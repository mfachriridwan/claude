# %% [markdown]
# # MSW recovery-pathway screening model, version 3
# **Screening LCA + TEA of six management options for 21 Indonesian cities/regencies, baseline year 2025**
#
# Functional unit: management of **1 tonne of mixed MSW as received at the facility gate** in 2025.
# Options: SL sanitary landfill + flare, S1 WtE, S2 RDF to cement kiln, S3 AD of food waste,
# S4 PHB from landfill gas, S5 integrated RDF + AD. Baselines: open dump (OD) and sanitary landfill (SL).
#
# What is new in v3
# - Every number is read from `data/*.csv` (built by `build_database_v3.py` from the update package).
#   The code holds no parameter values.
# - Tonnage: the parser reads the 2025 baseline columns and never projects a tonnage twice.
#   Overview totals whose year is unverified are used as reported, with a projected alternative as a scenario.
# - Composition: the RIPS sampling-year distribution is a proxy for 2025; it is normalised per stream and
#   the factor is recorded. No growth is applied to percentages.
# - Managed tonnage: two series that are never mixed silently: M1 status index, M2 local indicator first.
# - Rubber/leather DOCf is a declared data gap handled as a scenario (0 to 0.5), not the legacy 0.5.
# - Moisture: as-received values (calibrated assumption) in the main case; IPCC defaults as a scenario.
#
# Run: `python msw_pathway_model_v3.py` (or Runtime > Run all in Colab with the `data/` folder next to it).

# %%
import numpy as np, pandas as pd
import matplotlib
import matplotlib.pyplot as plt
from pathlib import Path

try:
    ROOT = Path(__file__).resolve().parent
except NameError:                                  # notebook / Colab
    ROOT = Path.cwd()
DATA, OUT = ROOT / "data", ROOT / "outputs"
OUT.mkdir(exist_ok=True)
if not (DATA / "input_kota_2025_updated.csv").exists():
    raise FileNotFoundError("Put the data/ folder (from build_database_v3.py) next to this file.")

K = pd.read_csv(DATA / "model_constants.csv").set_index("key").value
SEED, N_MC, YEAR = int(K.SEED), int(K.N_MC), int(K.BASE_YEAR)
rng = np.random.default_rng(SEED)

# %% [markdown]
# ## 1. Parser for the 2025 input
# The parser reads `input_kota_2025_updated.csv`. It refuses the old file (no 2025 columns), so the
# old tonnage logic cannot run by accident.

# %%
REQUIRED = ["Q2025_tpd", "M_dom_tpd_2025", "M_nd_tpd_2025", "tonnage_source_value_tpd", "tonnage_already_2025",
            "projection_exponent_years", "growth_rate_used", "growth_rate_basis", "Q2025_alt_tpd",
            "managed_share_status_index", "managed_share_local", "managed_share_local_lower", "grid_region",
            "lat_used", "lon_used", "composition_sampling_year"]


def read_input(path=DATA / "input_kota_2025_updated.csv"):
    raw = pd.read_csv(path)
    missing = [c for c in REQUIRED if c not in raw.columns]
    if missing:
        raise ValueError(f"{path.name} is not a v3 input; missing columns: {missing}")
    raw["tonnage_already_2025"] = raw.tonnage_already_2025.astype(str).str.lower().eq("true")
    return raw


raw = read_input()
print(f"{len(raw)} locations loaded")

# %% [markdown]
# ## 2. Parameters from CSV
# `PROP`: Table 2 (ten model fractions). `REG`: assumption register (central, low, high).

# %%
PROP = pd.read_csv(DATA / "table2_model_fraction_parameters.csv").set_index("fraction")
FR = list(PROP.index)
W, W_LO, W_HI = (PROP[c].to_numpy(float) for c in ("moisture_as_received", "moisture_as_received_low",
                                                   "moisture_as_received_high"))
W_IPCC = PROP.moisture_ipcc_default.to_numpy(float)
DOC, CARB, PHI = (PROP[c].to_numpy(float) for c in ("DOC_dry", "carbon_dry", "fossil_carbon_share"))
DOCF0 = PROP.DOCf_model.to_numpy(float)
HDRY, TAU = PROP.LHV_dry.to_numpy(float), PROP.tau.to_numpy(float)
APPLY_HK = PROP.apply_h_k.astype(str).str.lower().eq("true").to_numpy()
iF, iG, iW, iR, iP = (FR.index(f) for f in ("food", "garden", "wood", "rubber", "plastic"))

REG = pd.read_csv(DATA / "assumption_register.csv").set_index("key")
GWP = {"GWP100": (K.GWP100_CH4, K.GWP100_N2O), "GWP20": (K.GWP20_CH4, K.GWP20_N2O)}
LAM, RHO_CH4, LHV_CH4, AVAIL = K.LAM, K.RHO_CH4, K.LHV_CH4, K.AVAIL
GATE = {k[5:]: v for k, v in K.items() if k.startswith("GATE_")}
QREF = {k[5:]: v for k, v in K.items() if k.startswith("QREF_")}
P_EL_PSEL, PC_MAX = K.P_EL_PSEL, K.PC_MAX
EF_GRID = pd.read_csv(DATA / "grid_emission_factors.csv").set_index("grid_region").ef_kgCO2_per_kWh.to_dict()
KILN = {r.kiln: (r.lat, r.lon) for r in pd.read_csv(DATA / "cement_kilns.csv").itertuples()}

# RIPS categories -> model fractions (documented in data/taxonomy_crosswalk.csv)
MAP = {"food": ["sisa_makanan"], "garden": ["daun_kering"], "paper": ["kertas_kardus"], "wood": ["kayu"],
       "textile": ["kain"], "rubber": ["karet_kulit"], "plastic": ["plastik", "sterofoam"], "metal": ["logam"],
       "glass": ["kaca"], "other": ["b3", "elektronik", "lain_lain"]}


def km(a, b):                                    # great-circle distance
    la1, lo1, la2, lo2 = np.radians([a[0], a[1], b[0], b[1]])
    h = np.sin((la2 - la1) / 2) ** 2 + np.cos(la1) * np.cos(la2) * np.sin((lo2 - lo1) / 2) ** 2
    return 6371 * 2 * np.arcsin(np.sqrt(h))

# %% [markdown]
# ## 3. Harmonisation
# Composition: each stream is normalised to 100% (factor recorded), then streams are weighted by their
# 2025 masses. Blank cells are absent categories, never read as zero-valued measurements of a reported
# category (glass blank = not reported). Tonnage: read, not recomputed.

# %%
def harmonise(raw):
    rows = []
    for _, r in raw.iterrows():
        md, mn = float(r.M_dom_tpd_2025), float(r.M_nd_tpd_2025)

        def stream(pre):
            v = np.array([sum(float(r[f"{pre}_{c}"]) for c in MAP[f] if pd.notna(r.get(f"{pre}_{c}"))) for f in FR])
            return v / v.sum(), v.sum()
        sd, tot_d = stream("dom")
        sn, tot_n = stream("nd")
        s = (md * sd + mn * sn) / (md + mn)
        lumped = bool(r.organik_lumped_to_food)
        woody = (s[iG] == 0) and (s[iW] >= 0.10)                              # rule W
        rounded = bool(np.allclose(r[[c for c in raw.columns if c.startswith("dom_")
                                      and not c.endswith(("_2025", "_pct", "_factor", "_blank"))]]
                                   .astype(float).fillna(0) % 1, 0))
        xy = (r.lat_used, r.lon_used)
        d = {k: km(xy, v) for k, v in KILN.items()}; kiln = min(d, key=d.get)
        low_q = lumped or woody or rounded or r.composition_sampling_year < 2020
        g_rips = r.growth_rate_basis.startswith("RIPS")
        rows.append(dict(city_id=r.city_id, city=r.city_name, province=r.province,
                         comp_year=int(r.composition_sampling_year), Q2025=float(r.Q2025_tpd),
                         Q_src=float(r.tonnage_source_value_tpd), already=bool(r.tonnage_already_2025),
                         expo=int(r.projection_exponent_years), growth=float(r.growth_rate_used), g_rips=g_rips,
                         Q_alt=float(r.Q2025_alt_tpd), t_status=r.tonnage_year_status,
                         m_M1=r.managed_share_status_index, m_M2=(r.managed_share_local_lower if r.city_id == "kota_padang"
                                                                   else r.managed_share_local
                                                                   if pd.notna(r.managed_share_local)
                                                                   else r.managed_share_status_index),
                         norm_dom=100 / tot_d, norm_nd=100 / tot_n if tot_n > 0 else np.nan,
                         lumped=lumped, woody=woody, rounded=rounded, glass=r.glass_status,
                         alpha0=REG.central["alpha_lo"] if low_q else REG.central["alpha_hi"],
                         grid=r.grid_region, kiln=kiln, d_line=d[kiln],
                         **{f"s_{f}": v for f, v in zip(FR, s)}))
    H = pd.DataFrame(rows).set_index("city_id")
    H["ef_grid"] = H.grid.map(EF_GRID)
    return H


H = harmonise(raw)
SB = H[[f"s_{f}" for f in FR]].to_numpy()


def tri(u, lo, c, hi):                           # inverse CDF of the triangular distribution
    lo, c, hi = (np.asarray(x, float) for x in (lo, c, hi))
    span = np.where(hi > lo, hi - lo, 1.0); f = (c - lo) / span
    x = np.where(u < f, lo + np.sqrt(u * span * np.maximum(c - lo, 0)), hi - np.sqrt((1 - u) * span * np.maximum(hi - c, 0)))
    return np.where(hi > lo, x, c)


def draw(N, central=False, fix=None, moisture="as_received", vary=("param", "comp")):
    """Parameter arrays (length N). central=True gives central values (N = 1).
    vary: which uncertainty sources are sampled: 'param' (register + moisture + carbon value), 'comp' (Dirichlet)."""
    sample = (not central) and ("param" in vary)
    P = {k: (tri(rng.random(N), v.low, v.central, v.high) if sample else np.full(N, v.central)) for k, v in REG.iterrows()}
    if moisture == "ipcc":
        P["w"] = np.repeat(W_IPCC[None, :], N, 0)
    else:
        u = rng.random((N, 1)) if sample else np.full((N, 1), 0.5)
        P["w"] = tri(u, W_LO, W, W_HI) if sample else np.repeat(W[None, :], N, 0)
    P["pc"] = rng.uniform(0, PC_MAX, N) if sample else np.zeros(N)
    for k, v in (fix or {}).items(): P[k] = np.full(N, float(v))
    P["_comp"] = (not central) and ("comp" in vary)
    return P


def composition(i, P):
    """Rule W, else rule L (harmonisation priors), then Dirichlet sampling noise around the RIPS proxy."""
    c = H.iloc[i]; N = len(P["F"]); s = np.repeat(SB[i][None, :], N, 0)
    if c.woody:
        mv = s[:, iW] * P["yard"]; s[:, iG] += mv; s[:, iW] -= mv
    elif c.lumped:
        mv = s[:, iF] * P["gamma"]; s[:, iG] += mv; s[:, iF] -= mv
    if not P["_comp"]: return s
    g = rng.gamma(np.maximum(c.alpha0 * s, 1e-12)); return g / g.sum(1, keepdims=True)


def tonnage(i, P, rule="central"):
    """2025 tonnage. A tonnage already at 2025 is used directly; otherwise Q_src x (1+g)^expo, where only the
    fallback growth rate is uncertain. rule='alt' uses the projected overview alternative."""
    c = H.iloc[i]; N = len(P["F"])
    if rule == "alt":
        return np.full(N, c.Q_alt)
    if c.already:
        return np.full(N, c.Q2025)
    g = np.full(N, c.growth) if c.g_rips else P["g_default"]
    return c.Q_src * (1 + g) ** c.expo

# %% [markdown]
# ## 4. The model (equations unchanged from v2 except DOCf, LHV handling and moisture options)

# %%
PW = ["SL", "S1", "S2", "S3", "S4", "S5"]
NAMES = {"SL": "Sanitary landfill + flare", "S1": "WtE", "S2": "RDF", "S3": "AD", "S4": "PHB", "S5": "RDF + AD"}


def model(s, P, c, Q, policy="market", metric="GWP100", sink="SL"):
    gch4, gn2o = GWP[metric]; w = P["w"]; dm = 1 - w; col = lambda k: P[k][:, None]
    hk = np.where(APPLY_HK[None, :], col("h_k"), 1.0)
    hk[:, iP] = P["h_plastic_k"]
    h = HDRY * hk
    docf = np.repeat(DOCF0[None, :], len(s), 0); docf[:, iR] = P["docf_rubber"]
    efg = c.ef_grid * P["efg_k"]; dkm = c.d_line * P["tort"]
    crf = P["r"] * (1 + P["r"]) ** P["n"] / ((1 + P["r"]) ** P["n"] - 1)
    capex = lambda Kc, qref, q: Kc * (np.maximum(q, 1e-9) / AVAIL / qref) ** P["b"] * crf / (np.maximum(q, 1e-9) * 365)
    fossil = lambda m: 1e3 * (m * dm * CARB * PHI).sum(1) * 44 / 12

    def landfill(m, kind, inert=0.0):
        sl = kind == "SL"; t = m.sum(1) + inert
        ch4 = 1e3 * (m * dm * DOC * docf).sum(1) * P["doc_k"] * (P["mcf_sl"] if sl else P["mcf_od"]) * P["F"] * 16 / 12
        capt = ch4 * P["cap"] * sl
        emit = (ch4 - capt) * (1 - P["ox"] * sl)
        unit = P["c_sl"] * (np.maximum(Q * t, 5.0) / QREF["sl"]) ** (-P["e_sl"]) if sl else P["c_od"]
        return emit * gch4 + t * (P["anc_sl"] if sl else P["anc_od"]), t * unit, capt, ch4

    def rdf_line(m, t):
        r = m * np.minimum(TAU * col("tau_k"), 1.0); rej = m - r
        m_in, water = r.sum(1), (r * w).sum(1); dry = m_in - water
        m_out = np.minimum(m_in, dry / (1 - P["omega"]))
        e = (r * dm * h).sum(1) - LAM * (m_out - dry)
        e_net = e - (m_in - m_out) * P["q_dry"]
        m_del = m_out * e_net / e
        g = fossil(r) + t * (P["e_rdf"] * efg + P["anc_rdf"]) + m_del * dkm * P["ef_truck"] - P["psi"] * e_net * P["ef_coal"]
        cst = t * (capex(P["K_rdf"], QREF["rdf"], Q * t) + P["o_rdf"]) + m_del * dkm * P["c_truck"] - e_net * P["p_rdf"]
        return g, cst, rej, dict(rdf_t=m_del, rdf_ncv=e / m_out, rdf_gj=e_net, rdf_in=m_in, rdf_water=m_in - m_out)

    def ad(a):
        ch4 = a * 1e3 * dm[:, iF] * P["vs_ts"] * P["y_ch4"]
        el = ch4 * (1 - P["fug_ad"]) * LHV_CH4 / 3.6 * P["eta_chp"] * (1 - P["par_ad"])
        g = ch4 * RHO_CH4 * P["fug_ad"] * gch4 + a * P["anc_ad"] - el * efg
        cst = a * (capex(P["K_ad"], QREF["ad"], Q * a) + P["o_ad"]) - el * P["p_el"]
        return g, cst, dict(ad_kwh=el, ad_ch4_m3=ch4)

    X, G, C = {}, {}, {}
    G_od, C_od, _, X["L0_od"] = landfill(s, "OD")
    G["SL"], C["SL"], capt, X["L0_sl"] = landfill(s, "SL")
    X["moist"] = (s * w).sum(1); X["lhv"] = np.maximum((s * dm * h).sum(1) - LAM * X["moist"], 0)

    X["wte_kwh"] = X["lhv"] * 1e3 / 3.6 * P["eta_wte"]
    p_el = np.where((policy == "perpres109") & (Q >= GATE["q_psel"]), P_EL_PSEL, P["p_el"])
    lg, lc, _, _ = landfill(0 * s, sink, P["ash"])
    G["S1"] = fossil(s) + P["n2o_wte"] * gn2o + P["anc_wte"] - X["wte_kwh"] * efg + lg
    C["S1"] = capex(P["K_wte"], QREF["wte"], Q) + P["o_wte"] - X["wte_kwh"] * p_el + lc

    g, cst, rej, x2 = rdf_line(s, 1.0); lg, lc, _, _ = landfill(rej, sink)
    G["S2"], C["S2"] = g + lg, cst + lc; X.update({k + "_S2": v for k, v in x2.items()})

    food = np.zeros_like(s); food[:, iF] = s[:, iF] * P["kappa"]; a = food[:, iF]; X["ad_feed"] = a
    g, cst, x3 = ad(a); lg, lc, _, _ = landfill(s - food, sink, a * P["dig"])
    G["S3"] = g + lg + P["pre"] * (P["e_rdf"] * efg + P["anc_rdf"])
    C["S3"] = cst + lc + P["pre"] * (capex(P["K_rdf"], QREF["rdf"], Q) + P["o_rdf"]); X.update(x3)

    phb = capt / P["r_phb"]
    X["phb_kg"], X["phb_tpa"] = phb, phb * Q * 365 / 1e3
    X["phb_cost"] = P["c_phb"] * (np.maximum(X["phb_tpa"], 1.0) / 500.0) ** (-P["e_phb"])
    G["S4"] = G["SL"] + phb * (P["ef_phb"] - P["sub_pp"] * P["ef_pp"])
    C["S4"] = C["SL"] + phb * (X["phb_cost"] - P["p_phb"])

    g2, c2, rej, x5 = rdf_line(s - food, 1.0); lg, lc, _, _ = landfill(rej, sink, a * P["dig"])
    G["S5"], C["S5"] = g + g2 + lg, cst + c2 + lc; X.update({k + "_S5": v for k, v in x5.items()})
    X["landfilled_S5"] = rej.sum(1)

    one = np.ones(len(s), bool); near = dkm <= GATE["d_max"]
    ok = {"SL": one, "S1": (X["lhv"] >= GATE["lhv_min"]) & (Q >= GATE["q_wte"]),
          "S2": (X["rdf_ncv_S2"] >= GATE["ncv_rdf"]) & near, "S3": one,
          "S4": X["phb_tpa"] >= GATE["phb_min"], "S5": (X["rdf_ncv_S5"] >= GATE["ncv_rdf"]) & near}
    A = lambda d: np.column_stack([d[k] for k in PW])
    return dict(G=A(G), C=A(C), ok=A(ok), G_od=G_od, C_od=C_od, X=X)


def delta(R, bg):
    g0, c0 = (R["G_od"], R["C_od"]) if bg == "OD" else (R["G"][:, 0], R["C"][:, 0])
    return g0[:, None] - R["G"], R["C"] - c0[:, None]


def decide(R, pc):
    G, C, ok = R["G"], R["C"], R["ok"]
    score = np.where(ok, C + pc[:, None] * G / 1e3, np.inf)
    dom = (G[:, None, :] <= G[:, :, None]) & (C[:, None, :] <= C[:, :, None]) & \
          ((G[:, None, :] < G[:, :, None]) | (C[:, None, :] < C[:, :, None])) & ok[:, None, :]
    return score.argmin(1), ok & ~dom.any(2)

# %% [markdown]
# ## 5. Scenarios and central results
# `managed_M1` uses the RIPS status-index share (19 locations; Padang and Serang have none and are n.a.).
# `managed_M2` uses the local indicator where one is held (Padang: TPA-delivery lower bound 71.8%), else the
# status index. The two series use different indicator definitions and are reported side by side.

# %%
SCEN = {"market": {},
        "perpres109": dict(policy="perpres109"),
        "food_separated_at_source": dict(fix={"pre": 0.0}),
        "residues_to_open_dump": dict(sink="OD"),
        "managed_M1_status_index": dict(managed="M1"),
        "managed_M2_local_first": dict(managed="M2"),
        "GWP20": dict(metric="GWP20"),
        "moisture_IPCC_default": dict(moisture="ipcc"),
        "tonnage_overview_projected": dict(trule="alt"),
        "docf_rubber_0.5": dict(fix={"docf_rubber": 0.5})}


def setup(i, N, central, fix=None, managed=None, moisture="as_received", trule="central", vary=("param", "comp"), **kw):
    c = H.iloc[i]; P = draw(N, central, fix, moisture, vary); s = composition(i, P)
    Q = tonnage(i, P, trule)
    if managed:
        share = c.m_M1 if managed == "M1" else c.m_M2
        Q = Q * (share if pd.notna(share) else np.nan)
    return c, P, s, Q, kw


# Single-projection guard: the central tonnage the model uses must equal the CSV baseline.
for i in range(len(H)):
    _, P0, _, Q0, _ = setup(i, 1, True)
    assert abs(Q0[0] - H.iloc[i].Q2025) < 1e-3, f"tonnage mismatch for {H.index[i]} (double projection?)"

rows, char, best = [], [], []
for name, sc in SCEN.items():
    for i in range(len(H)):
        c, P, s, Q, kw = setup(i, 1, True, **sc)
        if np.isnan(Q).any(): continue
        R = model(s, P, c, Q, **kw)
        for bg in ("OD", "SL"):
            dG, dC = delta(R, bg)
            rows += [dict(city=c.city, scenario=name, baseline=bg, pathway=k, feasible=bool(R["ok"][0, j]),
                          G=R["G"][0, j], C=R["C"][0, j], dG=dG[0, j], dC=dC[0, j],
                          MAC=1e3 * dC[0, j] / dG[0, j] if dG[0, j] > 1e-9 else np.nan) for j, k in enumerate(PW)]
        best.append(dict(city=c.city, scenario=name, Q=Q[0],
                         **{f"best_at_{pc}": PW[decide(R, np.array([float(pc)]))[0][0]] for pc in (0, 25, 50, 100)}))
        if name == "market":
            X = {k: v[0] for k, v in R["X"].items()}; road = c.d_line * REG.central["tort"]
            qm1, qm2 = Q[0] * c.m_M1, Q[0] * c.m_M2
            char.append(dict(city=c.city, comp_year=c.comp_year, t_status=c.t_status, Q_2025=Q[0], Q_alt=c.Q_alt,
                             Q_managed_M1=qm1, Q_managed_M2=qm2, moisture=X["moist"], LHV=X["lhv"],
                             L0_od=X["L0_od"], L0_sl=X["L0_sl"], wte_kwh=X["wte_kwh"], rdf_yield=X["rdf_t_S2"],
                             rdf_ncv=X["rdf_ncv_S2"], ad_kwh=X["ad_kwh"], phb_kg=X["phb_kg"], phb_tpa=X["phb_tpa"],
                             phb_cost=X["phb_cost"], kiln=c.kiln, road_km=road,
                             gate_LHV=X["lhv"] >= GATE["lhv_min"], gate_WtE_scale=Q[0] >= GATE["q_wte"],
                             gate_PSEL_generated=Q[0] >= GATE["q_psel"],
                             gate_PSEL_M1=(qm1 >= GATE["q_psel"]) if pd.notna(qm1) else np.nan,
                             gate_PSEL_M2=(qm2 >= GATE["q_psel"]) if pd.notna(qm2) else np.nan,
                             gate_RDF=bool(X["rdf_ncv_S2"] >= GATE["ncv_rdf"]) and road <= GATE["d_max"],
                             gate_PHB=X["phb_tpa"] >= GATE["phb_min"], landfilled_S5=X["landfilled_S5"],
                             norm_dom=c.norm_dom, norm_nd=c.norm_nd, glass=c.glass,
                             flags="".join(f for f, b in (("L", c.lumped and not c.woody), ("W", c.woody),
                                                          ("R", c.rounded), ("G", c.glass == "folded_into_lain_lain"),
                                                          ("N", c.glass == "not_reported_blank"),
                                                          ("T", c.t_status == "overview_index_year_unverified")) if b)))
DET, CHAR, BEST = pd.DataFrame(rows), pd.DataFrame(char).set_index("city"), pd.DataFrame(best)
pd.set_option("display.width", 250); pd.set_option("display.max_columns", 40)
print(CHAR.round(2).to_string())
print("\nBest feasible pathway by carbon value (USD per tonne CO2e), market scenario")
print(BEST[BEST.scenario == "market"].set_index("city").drop(columns=["scenario", "Q"]).to_string())

# %% [markdown]
# ## 6. Monte Carlo: uncertainty, Pareto front, probability of being best

# %%
def run_mc(name, N=N_MC, vary=("param", "comp")):
    global rng
    out, samples = [], {}
    for i, cid in enumerate(H.index):
        rng = np.random.default_rng([SEED, i])
        c, P, s, Q, kw = setup(i, N, False, vary=vary, **SCEN[name])
        if np.isnan(Q).any(): continue
        R = model(s, P, c, Q, **kw); pick, front = decide(R, P["pc"])
        q = lambda a: dict(zip(("p5", "p50", "p95"), np.percentile(a, [5, 50, 95])))
        (dGo, dCo), (dGs, dCs) = delta(R, "OD"), delta(R, "SL")
        for j, k in enumerate(PW):
            stats = {f"{nm}_{p}": v for nm, a in (("G", R["G"]), ("C", R["C"]), ("dG_OD", dGo), ("dC_OD", dCo),
                                                   ("dG_SL", dGs), ("dC_SL", dCs)) for p, v in q(a[:, j]).items()}
            out.append(dict(city=c.city, scenario=name, pathway=k, p_feasible=R["ok"][:, j].mean(),
                            p_front=front[:, j].mean(), p_best=(pick == j).mean(),
                            **{f"p_best_pc{lo}_{hi}": (pick[(P["pc"] >= lo) & (P["pc"] < hi)] == j).mean()
                               for lo, hi in ((0, 10), (10, 50), (50, 100))}, **stats))
        samples[cid] = (P, s, R)
    return pd.DataFrame(out), samples


MC = {}
for name in SCEN:
    MC[name], smp = run_mc(name)
    if name == "market": SAMPLES = smp
MCALL = pd.concat(MC.values(), ignore_index=True)


def winners(df, col="p_best"):
    t = df.pivot(index="city", columns="pathway", values=col).reindex(H.city).dropna(how="all")[PW]
    return t.assign(best=t.idxmax(axis=1), p=t.max(axis=1))


for name, df in MC.items():
    print("\nProbability of being best | scenario:", name); print(winners(df).round(2).to_string())

# %% [markdown]
# ## 7. Uncertainty sources kept apart
# The composition prior (Dirichlet around the RIPS proxy, heuristic concentration) and the parameter
# uncertainty (register + moisture + carbon value) are run separately. The table reports the 90% interval
# width of GHG and cost under each source alone and under both.

# %%
def uncertainty_split(N=2000):
    acc = []
    for lab, vary in (("composition only", ("comp",)), ("parameters only", ("param",)), ("both", ("param", "comp"))):
        df, _ = run_mc("market", N, vary)
        df = df.assign(G_w90=df.G_p95 - df.G_p5, C_w90=df.C_p95 - df.C_p5, source=lab)
        acc.append(df.groupby(["source", "pathway"])[["G_w90", "C_w90"]].median())
    return pd.concat(acc).unstack(0)


USPLIT = uncertainty_split()
print("\nMedian 90% interval width over locations (G in kg CO2e/t, C in USD/t)\n", USPLIT.round(1).to_string())

# %% [markdown]
# ## 8. Sensitivity: Spearman rank correlation inside the Monte Carlo sample

# %%
def sensitivity():
    acc = []
    for cid, (P, s, R) in SAMPLES.items():
        Xin = pd.DataFrame({k: v for k, v in P.items() if not k.startswith("_") and k not in ("w", "pc") and np.ptp(v) > 0})
        Xin["moisture"] = R["X"]["moist"]
        for j, f in enumerate(FR): Xin["s_" + f] = s[:, j]
        Xin = Xin.loc[:, Xin.std() > 0].rank()
        for j, k in enumerate(PW):
            for nm in ("G", "C"):
                acc.append(Xin.corrwith(pd.Series(R[nm][:, j]).rank()).rename((k, nm)))
    return pd.concat(acc, axis=1).T.groupby(level=[0, 1]).agg(lambda x: x.abs().mean()).T


SENS = sensitivity()
for k in PW:
    print(k, "G:", SENS[(k, "G")].nlargest(5).round(2).to_dict(), "\n   C:", SENS[(k, "C")].nlargest(5).round(2).to_dict())

# %% [markdown]
# ## 9. Checks against measured values and Level II/III parameter coverage

# %%
c, P, s, Q, kw = setup(0, 1, True); X = model(s, P, c, Q)["X"]
assert abs(X["ad_feed"][0] + X["rdf_in_S5"][0] + X["landfilled_S5"][0] - 1) < 1e-9, "S5 mass balance broken"
assert X["rdf_t_S5"][0] <= X["rdf_in_S5"][0] - X["rdf_water_S5"][0] + 1e-12, "more RDF delivered than produced"
c_food = 1e3 * (1 - W[iF]) * CARB[iF]
c_gas = 1e3 * (1 - W[iF]) * REG.central["vs_ts"] * REG.central["y_ch4"] * RHO_CH4 * 12 / 16 / 0.6
assert c_gas < c_food, "AD yield exceeds the carbon available in food waste"
CHAR["HHV_dry"] = (CHAR.LHV + LAM * CHAR.moisture) / (1 - CHAR.moisture) + LAM * 9 * 0.06
span = lambda v, f: f"{CHAR[v].min():{f}} to {CHAR[v].max():{f}} (median {CHAR[v].median():{f}})"
BENCH = pd.DataFrame({
    "model, 21 locations": [span("moisture", ".2f"), span("LHV", ".1f"), span("HHV_dry", ".1f"), span("L0_sl", ".0f"),
                            span("rdf_yield", ".2f"), span("rdf_ncv", ".1f"), span("wte_kwh", ".0f"), f"{c_gas / c_food:.2f}"],
    "reference": ["0.53 to 0.56 at transfer points (Prabowo et al. 2019); 0.64 to 0.66 at a landfill (Fiki et al. 2022)",
                  "about 5 to 7 (HHV 6.9 to 9.0 measured, Prabowo et al. 2019; 6.5 assumed, Azis et al. 2021)",
                  "15.7 to 19.2, from HHV 6.9 to 9.0 as received at 53 to 56% moisture (Prabowo et al. 2019)",
                  "no Indonesian measurement; reported for transparency",
                  "0.20 to 0.42 (Indonesian RDF plants, GIZ 2023)",
                  "12.6 to 13.8 for biodried whole waste (GIZ 2023); sorted and dried SRF is higher",
                  "632 kWh/t at 35% gross conversion (Azis et al. 2021); model uses 18% net",
                  "must be below 1"]},
    index=["bulk moisture (-)", "LHV as received (MJ/kg)", "HHV of dry matter (MJ/kg)", "L0 at MCF = 1 (kg CH4/t)",
           "RDF yield (t/t MSW)", "RDF heating value (MJ/kg)", "WtE net electricity (kWh/t)",
           "carbon in biogas / carbon in food"])
print(BENCH.to_string())

# Share of each stream's wet mass (terminal partition) whose Level II/III parameters are known.
TERM = pd.read_csv(DATA / "composition_terminal_partition_2025.csv", dtype={"code": str})
FP = pd.read_csv(DATA / "fraction_parameters_L1_L2_L3.csv", dtype={"code": str})
TERM = TERM.merge(FP[["level", "code", "moisture_wet_fraction", "DOC_dry_fraction", "DOCf", "carbon_dry_fraction"]],
                  on=["level", "code"], how="left")
COVER = (TERM.assign(**{f"has_{p}": TERM[p].notna() * TERM.percent_wet_mass
                        for p in ("moisture_wet_fraction", "DOC_dry_fraction", "DOCf", "carbon_dry_fraction")})
         .groupby(["city_name", "stream"])[[f"has_{p}" for p in ("moisture_wet_fraction", "DOC_dry_fraction", "DOCf",
                                                                  "carbon_dry_fraction")]].sum().round(1))
print("\nPercent of wet mass with a known Level II/III parameter (terminal partition)\n", COVER.describe().round(1))

# %% [markdown]
# ## 10. Save tables and figures

# %%
for nm, df, idx in (("01_harmonised_inputs", H.round(4), True), ("02_characterisation_and_gates", CHAR.round(3), True),
                    ("03_deterministic_results", DET.round(3), False), ("04_best_pathway_by_carbon_value", BEST, False),
                    ("05_montecarlo_results", MCALL.round(4), False), ("06_sensitivity_spearman", SENS.round(3), True),
                    ("07_assumption_register_used", REG, True), ("08_fraction_properties_used", PROP, True),
                    ("09_validation_benchmarks", BENCH, True), ("10_uncertainty_split", USPLIT.round(2), True),
                    ("11_parameter_coverage_L2L3", COVER, True)):
    df.to_csv(OUT / f"{nm}.csv", index=idx)

COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}
fig, ax = plt.subplots(1, 3, figsize=(15, 6.5), sharey=True)
for a, key, ttl in zip(ax, ("market", "perpres109", "food_separated_at_source"),
                       ("Market case", "Perpres 109/2025 tariff for WtE\nat 1,000 t/day or more",
                        "What if food waste arrives\nalready separated")):
    winners(MC[key])[PW].iloc[::-1].plot.barh(stacked=True, ax=a, color=[COL[k] for k in PW], width=0.8, legend=False)
    a.set_title(ttl, fontsize=10); a.set_xlabel("Probability of being the best feasible pathway"); a.set_xlim(0, 1); a.set_ylabel("")
ax[2].legend([NAMES[k] for k in PW], loc="center left", bbox_to_anchor=(1.0, 0.5), frameon=False)
fig.tight_layout(); fig.savefig(OUT / "fig_probability_best.png", dpi=170); plt.close(fig)

fig, ax = plt.subplots(1, 2, figsize=(13, 5)); d = MC["market"]
for a, bg, ttl in zip(ax, ("OD", "SL"), ("Baseline: open dump", "Baseline: sanitary landfill")):
    for k in PW[(bg == "SL"):]:
        q = d[d.pathway == k]
        a.scatter(q[f"dC_{bg}_p50"], q[f"dG_{bg}_p50"], s=15 + 70 * q.p_feasible, c=COL[k], alpha=0.75, label=NAMES[k],
                  edgecolor="k", linewidth=0.3)
    a.axhline(0, c="k", lw=0.6); a.axvline(0, c="k", lw=0.6); a.set_title(ttl + " (one dot per location)", fontsize=10)
    a.set_xlabel("Extra cost over baseline, USD per tonne MSW (median)"); a.set_ylabel("GHG avoided, kg CO2e per tonne MSW (median)")
ax[0].legend(frameon=False, fontsize=8); fig.tight_layout(); fig.savefig(OUT / "fig_tradeoff.png", dpi=170); plt.close(fig)

fig, ax = plt.subplots(1, 2, figsize=(13, 5.5))
for a, nm, ttl in zip(ax, ("G", "C"), ("Life-cycle GHG", "Net levelised cost")):
    t = SENS.xs(nm, axis=1, level=1)[PW]; top = t.max(axis=1).nlargest(12).index[::-1]
    t.loc[top].plot.barh(ax=a, color=[COL[k] for k in PW], width=0.85, legend=False)
    a.set_xlabel("mean |Spearman rho| over 21 locations"); a.set_title(ttl + ": most influential inputs", fontsize=10)
ax[1].legend([NAMES[k] for k in PW], frameon=False, fontsize=8); fig.tight_layout()
fig.savefig(OUT / "fig_sensitivity.png", dpi=170); plt.close(fig)

# Robustness across the v3 data scenarios: most probable pathway per location
ROB = pd.DataFrame({nm: winners(MC[nm]).best for nm in SCEN}).reindex(H.city)
ROB.to_csv(OUT / "12_most_probable_by_scenario.csv")
print("\nMost probable pathway by scenario\n", ROB.to_string())
print("Done. Files written to", OUT.resolve())

"""
Core of the MSW recovery-pathway screening model (v3.1).

Screening LCA + TEA of six options per tonne of mixed MSW as received at the facility gate in 2025:
SL sanitary landfill + flare, S1 WtE, S2 RDF to cement kiln, S3 AD of food waste, S4 PHB from landfill gas,
S5 integrated RDF + AD. Baselines: open dump (OD) and sanitary landfill (SL).

Indicators per tonne MSW: G climate change (kg CO2e, GWP100 or GWP20), C net levelised cost (USD),
CED non-renewable (fossil) cumulative energy demand (MJ), LU land take (m2).

Every number comes from the CSV files in data/. The equations for G and C are those of v3; v3.1 adds CED and LU,
whose factors are drawn after all v3 draws so that G and C reproduce v3 exactly for the same seed.
"""
from pathlib import Path
from types import SimpleNamespace
import numpy as np
import pandas as pd

PW = ["SL", "S1", "S2", "S3", "S4", "S5"]
NAMES = {"SL": "Sanitary landfill + flare", "S1": "WtE", "S2": "RDF", "S3": "AD", "S4": "PHB", "S5": "RDF + AD"}
INDICATORS = ("G", "C", "CED", "LU")

# RIPS categories -> model fractions (documented in data/taxonomy_crosswalk.csv)
MAP = {"food": ["sisa_makanan"], "garden": ["daun_kering"], "paper": ["kertas_kardus"], "wood": ["kayu"],
       "textile": ["kain"], "rubber": ["karet_kulit"], "plastic": ["plastik", "sterofoam"], "metal": ["logam"],
       "glass": ["kaca"], "other": ["b3", "elektronik", "lain_lain"]}
RIPS_CATS = ["sisa_makanan", "daun_kering", "kertas_kardus", "kayu", "plastik", "logam", "kain", "karet_kulit",
             "sterofoam", "b3", "elektronik", "lain_lain", "kaca"]

REQUIRED = ["Q2025_tpd", "M_dom_tpd_2025", "M_nd_tpd_2025", "tonnage_source_value_tpd", "tonnage_already_2025",
            "projection_exponent_years", "growth_rate_used", "growth_rate_basis", "Q2025_alt_tpd",
            "managed_share_status_index", "managed_share_local", "managed_share_local_lower", "grid_region",
            "lat_used", "lon_used", "composition_sampling_year"]

SCENARIOS = {"market": {},
             "perpres109": dict(policy="perpres109"),
             "food_separated_at_source": dict(fix={"pre": 0.0}),
             "residues_to_open_dump": dict(sink="OD"),
             "managed_M1_status_index": dict(managed="M1"),
             "managed_M2_local_first": dict(managed="M2"),
             "GWP20": dict(metric="GWP20"),
             "moisture_IPCC_default": dict(moisture="ipcc"),
             "tonnage_overview_projected": dict(trule="alt"),
             "docf_rubber_0.5": dict(fix={"docf_rubber": 0.5})}


def km(a, b):
    """Great-circle distance in km between (lat, lon) pairs."""
    la1, lo1, la2, lo2 = np.radians([a[0], a[1], b[0], b[1]])
    h = np.sin((la2 - la1) / 2) ** 2 + np.cos(la1) * np.cos(la2) * np.sin((lo2 - lo1) / 2) ** 2
    return 6371 * 2 * np.arcsin(np.sqrt(h))


def tri(u, lo, c, hi):
    """Inverse CDF of the triangular distribution (returns c where lo == hi)."""
    lo, c, hi = (np.asarray(x, float) for x in (lo, c, hi))
    span = np.where(hi > lo, hi - lo, 1.0); f = (c - lo) / span
    x = np.where(u < f, lo + np.sqrt(u * span * np.maximum(c - lo, 0)), hi - np.sqrt((1 - u) * span * np.maximum(hi - c, 0)))
    return np.where(hi > lo, x, c)


def read_input(path):
    """Read a v3 city input. Refuses files without the 2025 baseline columns (e.g. the v2 input)."""
    path = Path(path)
    raw = pd.read_csv(path)
    missing = [c for c in REQUIRED if c not in raw.columns]
    if missing:
        raise ValueError(f"{path.name} is not a v3 input; missing columns: {missing}")
    raw["tonnage_already_2025"] = raw.tonnage_already_2025.astype(str).str.lower().eq("true")
    return raw


class MSWModel:
    """Loads parameters from data/ and evaluates the six options. One instance can run many city sets."""

    def __init__(self, data_dir="data", seed=None):
        D = Path(data_dir)
        self.D = D
        K = pd.read_csv(D / "model_constants.csv").set_index("key").value
        self.K = K
        self.SEED = int(K.SEED) if seed is None else int(seed)
        self.N_MC, self.YEAR = int(K.N_MC), int(K.BASE_YEAR)
        self.rng = np.random.default_rng(self.SEED)
        P = pd.read_csv(D / "table2_model_fraction_parameters.csv").set_index("fraction")
        self.PROP, self.FR = P, list(P.index)
        self.W, self.W_LO, self.W_HI = (P[c].to_numpy(float) for c in ("moisture_as_received", "moisture_as_received_low",
                                                                       "moisture_as_received_high"))
        self.W_IPCC = P.moisture_ipcc_default.to_numpy(float)
        self.DOC, self.CARB, self.PHI = (P[c].to_numpy(float) for c in ("DOC_dry", "carbon_dry", "fossil_carbon_share"))
        self.DOCF0 = P.DOCf_model.to_numpy(float)
        self.HDRY, self.TAU = P.LHV_dry.to_numpy(float), P.tau.to_numpy(float)
        self.APPLY_HK = P.apply_h_k.astype(str).str.lower().eq("true").to_numpy()
        self.iF, self.iG, self.iW, self.iR, self.iP = (self.FR.index(f) for f in ("food", "garden", "wood", "rubber", "plastic"))
        self.REG = pd.read_csv(D / "assumption_register.csv").set_index("key")
        lp = D / "lcia_factors.csv"
        self.LCIA = pd.read_csv(lp).set_index("key") if lp.exists() else None
        self.GWP = {"GWP100": (K.GWP100_CH4, K.GWP100_N2O), "GWP20": (K.GWP20_CH4, K.GWP20_N2O)}
        self.LAM, self.RHO_CH4, self.LHV_CH4, self.AVAIL = K.LAM, K.RHO_CH4, K.LHV_CH4, K.AVAIL
        self.GATE = {k[5:]: v for k, v in K.items() if k.startswith("GATE_")}
        self.QREF = {k[5:]: v for k, v in K.items() if k.startswith("QREF_")}
        self.P_EL_PSEL, self.PC_MAX = K.P_EL_PSEL, K.PC_MAX
        self.EF_GRID = pd.read_csv(D / "grid_emission_factors.csv").set_index("grid_region").ef_kgCO2_per_kWh.to_dict()
        self.KILN = {r.kiln: (r.lat, r.lon) for r in pd.read_csv(D / "cement_kilns.csv").itertuples()}
        self.H = None

    # ------------------------------------------------------------------ harmonisation
    def harmonise(self, raw):
        """Composition per stream normalised to 1, weighted by 2025 masses; tonnage read, not recomputed."""
        FR = self.FR
        rows = []
        for _, r in raw.iterrows():
            md, mn = float(r.M_dom_tpd_2025), float(r.M_nd_tpd_2025)

            def stream(pre):
                v = np.array([sum(float(r[f"{pre}_{c}"]) for c in MAP[f] if pd.notna(r.get(f"{pre}_{c}"))) for f in FR])
                return v / v.sum(), v.sum()
            sd, tot_d = stream("dom")
            sn, tot_n = stream("nd") if mn > 0 or any(pd.notna(r.get(f"nd_{c}")) for c in RIPS_CATS) else (sd, tot_d)
            s = (md * sd + mn * sn) / (md + mn)
            lumped = bool(r.organik_lumped_to_food) if "organik_lumped_to_food" in r else False
            woody = (s[self.iG] == 0) and (s[self.iW] >= 0.10)
            dcols = [f"dom_{c}" for c in RIPS_CATS if f"dom_{c}" in raw.columns]
            rounded = bool(np.allclose(pd.to_numeric(r[dcols], errors="coerce").astype(float).fillna(0) % 1, 0))
            xy = (r.lat_used, r.lon_used)
            if "kiln_road_km" in r and pd.notna(r.kiln_road_km):          # user-supplied road distance
                kiln, d_line = "user-supplied", float(r.kiln_road_km) / self.REG.central["tort"]
            else:
                d = {k: km(xy, v) for k, v in self.KILN.items()}; kiln = min(d, key=d.get); d_line = d[kiln]
            low_q = lumped or woody or rounded or r.composition_sampling_year < 2020
            rows.append(dict(city_id=r.city_id, city=r.city_name, province=r.get("province", ""),
                             comp_year=int(r.composition_sampling_year), Q2025=float(r.Q2025_tpd),
                             Q_src=float(r.tonnage_source_value_tpd), already=bool(r.tonnage_already_2025),
                             expo=int(r.projection_exponent_years), growth=float(r.growth_rate_used),
                             g_rips=str(r.growth_rate_basis).startswith(("RIPS", "user")),
                             Q_alt=float(r.Q2025_alt_tpd), t_status=r.get("tonnage_year_status", ""),
                             m_M1=r.managed_share_status_index,
                             m_M2=(r.managed_share_local_lower if r.city_id == "kota_padang"
                                   else r.managed_share_local if pd.notna(r.managed_share_local)
                                   else r.managed_share_status_index),
                             norm_dom=100 / tot_d, norm_nd=100 / tot_n if tot_n > 0 else np.nan,
                             lumped=lumped, woody=woody, rounded=rounded, glass=r.get("glass_status", ""),
                             alpha0=self.REG.central["alpha_lo"] if low_q else self.REG.central["alpha_hi"],
                             grid=r.grid_region, kiln=kiln, d_line=d_line,
                             **{f"s_{f}": v for f, v in zip(FR, s)}))
        H = pd.DataFrame(rows).set_index("city_id")
        H["ef_grid"] = H.grid.map(self.EF_GRID)
        return H

    def load_cities(self, raw):
        self.raw = raw
        self.H = self.harmonise(raw)
        self.SB = self.H[[f"s_{f}" for f in self.FR]].to_numpy()
        return self.H

    # ------------------------------------------------------------------ sampling
    def draw(self, N, central=False, fix=None, moisture="as_received", vary=("param", "comp")):
        rng = self.rng
        sample = (not central) and ("param" in vary)
        P = {k: (tri(rng.random(N), v.low, v.central, v.high) if sample else np.full(N, v.central))
             for k, v in self.REG.iterrows()}
        if moisture == "ipcc":
            P["w"] = np.repeat(self.W_IPCC[None, :], N, 0)
        else:
            u = rng.random((N, 1)) if sample else np.full((N, 1), 0.5)
            P["w"] = tri(u, self.W_LO, self.W, self.W_HI) if sample else np.repeat(self.W[None, :], N, 0)
        P["pc"] = rng.uniform(0, self.PC_MAX, N) if sample else np.zeros(N)
        for k, v in (fix or {}).items():
            if k in P: P[k] = np.full(N, float(v))
        P["_comp"] = (not central) and ("comp" in vary)
        P["_sample"] = sample
        P["_fix"] = fix or {}
        return P

    def draw_lcia(self, P):
        """CED and land-use factors, drawn after every v3 draw so that G and C are unchanged."""
        N = len(P["F"])
        if self.LCIA is None:
            return P
        for k, v in self.LCIA.iterrows():
            P[k] = tri(self.rng.random(N), v.low, v.central, v.high) if P["_sample"] else np.full(N, v.central)
        for k, v in P["_fix"].items():
            if k in self.LCIA.index: P[k] = np.full(N, float(v))
        return P

    def composition(self, i, P):
        c = self.H.iloc[i]; N = len(P["F"]); s = np.repeat(self.SB[i][None, :], N, 0)
        if c.woody:
            mv = s[:, self.iW] * P["yard"]; s[:, self.iG] += mv; s[:, self.iW] -= mv
        elif c.lumped:
            mv = s[:, self.iF] * P["gamma"]; s[:, self.iG] += mv; s[:, self.iF] -= mv
        if not P["_comp"]: return s
        g = self.rng.gamma(np.maximum(c.alpha0 * s, 1e-12)); return g / g.sum(1, keepdims=True)

    def tonnage(self, i, P, rule="central"):
        c = self.H.iloc[i]; N = len(P["F"])
        if rule == "alt":
            return np.full(N, c.Q_alt)
        if c.already:
            return np.full(N, c.Q2025)
        g = np.full(N, c.growth) if c.g_rips else P["g_default"]
        return c.Q_src * (1 + g) ** c.expo

    def setup(self, i, N, central, fix=None, managed=None, moisture="as_received", trule="central",
              vary=("param", "comp"), **kw):
        c = self.H.iloc[i]; P = self.draw(N, central, fix, moisture, vary); s = self.composition(i, P)
        Q = self.tonnage(i, P, trule)
        if managed:
            share = c.m_M1 if managed == "M1" else c.m_M2
            Q = Q * (share if pd.notna(share) else np.nan)
        P = self.draw_lcia(P)
        return c, P, s, Q, kw

    # ------------------------------------------------------------------ the model
    def model(self, s, P, c, Q, policy="market", metric="GWP100", sink="SL"):
        """Evaluate the six options for every draw. c needs attributes ef_grid and d_line (scalars or arrays)."""
        FR, iF, iP, iR = self.FR, self.iF, self.iP, self.iR
        DOC, CARB, PHI, TAU, LAM, AVAIL = self.DOC, self.CARB, self.PHI, self.TAU, self.LAM, self.AVAIL
        QREF, GATE = self.QREF, self.GATE
        gch4, gn2o = self.GWP[metric]; w = P["w"]; dm = 1 - w; col = lambda k: P[k][:, None]
        hk = np.where(self.APPLY_HK[None, :], col("h_k"), 1.0)
        hk[:, iP] = P["h_plastic_k"]
        h = self.HDRY * hk
        docf = np.repeat(self.DOCF0[None, :], len(s), 0); docf[:, iR] = P["docf_rubber"]
        efg = c.ef_grid * P["efg_k"]; dkm = c.d_line * P["tort"]
        crf = P["r"] * (1 + P["r"]) ** P["n"] / ((1 + P["r"]) ** P["n"] - 1)
        capex = lambda Kc, qref, q: Kc * (np.maximum(q, 1e-9) / AVAIL / qref) ** P["b"] * crf / (np.maximum(q, 1e-9) * 365)
        fossil = lambda m: 1e3 * (m * dm * CARB * PHI).sum(1) * 44 / 12
        lc_on = "pef_eta" in P
        if lc_on:   # CED conversion factors (MJ per unit)
            pef = 3.6 / P["pef_eta"] * (1 + P["up_fuel"]) * P["efg_k"]      # MJ fossil per kWh grid
            mj_diesel = (1 + P["up_diesel"]) / P["ef_diesel"]                 # MJ per kg CO2e of diesel use
            plant_lu = lambda fp: fp / (AVAIL * 365 * P["n"])                 # m2 per t input to a plant
        Z = np.zeros(len(s))

        def landfill(m, kind, inert=0.0):
            sl = kind == "SL"; t = m.sum(1) + inert
            ch4 = 1e3 * (m * dm * DOC * docf).sum(1) * P["doc_k"] * (P["mcf_sl"] if sl else P["mcf_od"]) * P["F"] * 16 / 12
            capt = ch4 * P["cap"] * sl
            emit = (ch4 - capt) * (1 - P["ox"] * sl)
            unit = P["c_sl"] * (np.maximum(Q * t, 5.0) / QREF["sl"]) ** (-P["e_sl"]) if sl else P["c_od"]
            anc = P["anc_sl"] if sl else P["anc_od"]
            if lc_on:
                ced = t * anc * mj_diesel
                if sl:
                    lu = (m.sum(1) / (P["rho_sl"] * P["h_sl"]) + inert / (P["rho_inert"] * P["h_sl"])) * P["f_gross_sl"]
                else:
                    lu = (m.sum(1) + inert) / (P["rho_od"] * P["h_od"])
            else:
                ced = lu = Z
            return emit * gch4 + t * anc, t * unit, capt, ch4, ced, lu

        def rdf_line(m, t):
            r = m * np.minimum(TAU * col("tau_k"), 1.0); rej = m - r
            m_in, water = r.sum(1), (r * w).sum(1); dry = m_in - water
            m_out = np.minimum(m_in, dry / (1 - P["omega"]))
            e = (r * dm * h).sum(1) - LAM * (m_out - dry)
            e_net = e - (m_in - m_out) * P["q_dry"]
            m_del = m_out * e_net / e
            g = fossil(r) + t * (P["e_rdf"] * efg + P["anc_rdf"]) + m_del * dkm * P["ef_truck"] - P["psi"] * e_net * P["ef_coal"]
            cst = t * (capex(P["K_rdf"], QREF["rdf"], Q * t) + P["o_rdf"]) + m_del * dkm * P["c_truck"] - e_net * P["p_rdf"]
            if lc_on:
                ced = (t * (P["e_rdf"] * pef + P["anc_rdf"] * mj_diesel) + m_del * dkm * P["ef_truck"] * mj_diesel
                       - P["psi"] * e_net * 1e3 * (1 + P["up_coal"]))
                lu = t * plant_lu(P["fp_rdf"])
            else:
                ced = lu = Z
            return g, cst, rej, dict(rdf_t=m_del, rdf_ncv=e / m_out, rdf_gj=e_net, rdf_in=m_in, rdf_water=m_in - m_out), ced, lu

        def ad(a):
            ch4 = a * 1e3 * dm[:, iF] * P["vs_ts"] * P["y_ch4"]
            el = ch4 * (1 - P["fug_ad"]) * self.LHV_CH4 / 3.6 * P["eta_chp"] * (1 - P["par_ad"])
            g = ch4 * self.RHO_CH4 * P["fug_ad"] * gch4 + a * P["anc_ad"] - el * efg
            cst = a * (capex(P["K_ad"], QREF["ad"], Q * a) + P["o_ad"]) - el * P["p_el"]
            ced = (a * P["anc_ad"] * mj_diesel - el * pef) if lc_on else Z
            lu = a * plant_lu(P["fp_ad"]) if lc_on else Z
            return g, cst, dict(ad_kwh=el, ad_ch4_m3=ch4), ced, lu

        X, G, C, E, L = {}, {}, {}, {}, {}
        G_od, C_od, _, X["L0_od"], E_od, L_od = landfill(s, "OD")
        G["SL"], C["SL"], capt, X["L0_sl"], E["SL"], L["SL"] = landfill(s, "SL")
        X["moist"] = (s * w).sum(1); X["lhv"] = np.maximum((s * dm * h).sum(1) - LAM * X["moist"], 0)

        X["wte_kwh"] = X["lhv"] * 1e3 / 3.6 * P["eta_wte"]
        p_el = np.where((policy == "perpres109") & (Q >= GATE["q_psel"]), self.P_EL_PSEL, P["p_el"])
        lg, lc, _, _, le, ll = landfill(0 * s, sink, P["ash"])
        G["S1"] = fossil(s) + P["n2o_wte"] * gn2o + P["anc_wte"] - X["wte_kwh"] * efg + lg
        C["S1"] = capex(P["K_wte"], QREF["wte"], Q) + P["o_wte"] - X["wte_kwh"] * p_el + lc
        E["S1"] = (P["anc_wte"] * mj_diesel - X["wte_kwh"] * pef + le) if lc_on else Z
        L["S1"] = (plant_lu(P["fp_wte"]) + ll) if lc_on else Z

        g, cst, rej, x2, re_, rl = rdf_line(s, 1.0); lg, lc, _, _, le, ll = landfill(rej, sink)
        G["S2"], C["S2"], E["S2"], L["S2"] = g + lg, cst + lc, re_ + le, rl + ll
        X.update({k + "_S2": v for k, v in x2.items()})

        food = np.zeros_like(s); food[:, iF] = s[:, iF] * P["kappa"]; a = food[:, iF]; X["ad_feed"] = a
        g, cst, x3, ae, al = ad(a); lg, lc, _, _, le, ll = landfill(s - food, sink, a * P["dig"])
        G["S3"] = g + lg + P["pre"] * (P["e_rdf"] * efg + P["anc_rdf"])
        C["S3"] = cst + lc + P["pre"] * (capex(P["K_rdf"], QREF["rdf"], Q) + P["o_rdf"]); X.update(x3)
        E["S3"] = (ae + le + P["pre"] * (P["e_rdf"] * pef + P["anc_rdf"] * mj_diesel)) if lc_on else Z
        L["S3"] = (al + ll + P["pre"] * plant_lu(P["fp_rdf"])) if lc_on else Z

        phb = capt / P["r_phb"]
        X["phb_kg"], X["phb_tpa"] = phb, phb * Q * 365 / 1e3
        X["phb_cost"] = P["c_phb"] * (np.maximum(X["phb_tpa"], 1.0) / 500.0) ** (-P["e_phb"])
        G["S4"] = G["SL"] + phb * (P["ef_phb"] - P["sub_pp"] * P["ef_pp"])
        C["S4"] = C["SL"] + phb * (X["phb_cost"] - P["p_phb"])
        E["S4"] = (E["SL"] + phb * (P["ef_phb"] / P["ef_natgas"] - P["sub_pp"] * P["ced_pp"])) if lc_on else Z
        L["S4"] = L["SL"]

        g2, c2, rej, x5, r5e, r5l = rdf_line(s - food, 1.0); lg, lc, _, _, le, ll = landfill(rej, sink, a * P["dig"])
        G["S5"], C["S5"] = g + g2 + lg, cst + c2 + lc
        E["S5"], L["S5"] = (ae + r5e + le, al + r5l + ll) if lc_on else (Z, Z)
        X.update({k + "_S5": v for k, v in x5.items()})
        X["landfilled_S5"] = rej.sum(1)

        one = np.ones(len(s), bool); near = dkm <= GATE["d_max"]
        ok = {"SL": one, "S1": (X["lhv"] >= GATE["lhv_min"]) & (Q >= GATE["q_wte"]),
              "S2": (X["rdf_ncv_S2"] >= GATE["ncv_rdf"]) & near, "S3": one,
              "S4": X["phb_tpa"] >= GATE["phb_min"], "S5": (X["rdf_ncv_S5"] >= GATE["ncv_rdf"]) & near}
        A = lambda d: np.column_stack([np.broadcast_to(d[k], (len(s),)) for k in PW])
        return dict(G=A(G), C=A(C), CED=A(E), LU=A(L), ok=A(ok), G_od=G_od, C_od=C_od,
                    CED_od=np.broadcast_to(E_od, (len(s),)), LU_od=np.broadcast_to(L_od, (len(s),)), X=X)

    @staticmethod
    def delta(R, bg, ind="G"):
        """Benefit vs baseline: for G, CED, LU positive = less than baseline; for C positive = extra cost."""
        base = R[f"{ind}_od"] if bg == "OD" else R[ind][:, 0]
        return (base[:, None] - R[ind]) if ind != "C" else (R[ind] - base[:, None])

    @staticmethod
    def decide(R, pc):
        G, C, ok = R["G"], R["C"], R["ok"]
        score = np.where(ok, C + pc[:, None] * G / 1e3, np.inf)
        dom = (G[:, None, :] <= G[:, :, None]) & (C[:, None, :] <= C[:, :, None]) & \
              ((G[:, None, :] < G[:, :, None]) | (C[:, None, :] < C[:, :, None])) & ok[:, None, :]
        return score.argmin(1), ok & ~dom.any(2)

    # ------------------------------------------------------------------ runs
    def check_single_projection(self):
        for i in range(len(self.H)):
            _, _, _, Q0, _ = self.setup(i, 1, True)
            assert abs(Q0[0] - self.H.iloc[i].Q2025) < 1e-3, f"tonnage mismatch for {self.H.index[i]} (double projection?)"

    def run_central(self, scenarios=None):
        scenarios = scenarios or SCENARIOS
        rows, char, best = [], [], []
        for name, sc in scenarios.items():
            for i in range(len(self.H)):
                c, P, s, Q, kw = self.setup(i, 1, True, **sc)
                if np.isnan(Q).any(): continue
                R = self.model(s, P, c, Q, **kw)
                for bg in ("OD", "SL"):
                    dG, dC = self.delta(R, bg, "G"), self.delta(R, bg, "C")
                    dE, dL = self.delta(R, bg, "CED"), self.delta(R, bg, "LU")
                    rows += [dict(city=c.city, scenario=name, baseline=bg, pathway=k, feasible=bool(R["ok"][0, j]),
                                  G=R["G"][0, j], C=R["C"][0, j], dG=dG[0, j], dC=dC[0, j],
                                  MAC=1e3 * dC[0, j] / dG[0, j] if dG[0, j] > 1e-9 else np.nan,
                                  CED=R["CED"][0, j], dCED=dE[0, j], LU=R["LU"][0, j], dLU=dL[0, j])
                             for j, k in enumerate(PW)]
                best.append(dict(city=c.city, scenario=name, Q=Q[0],
                                 **{f"best_at_{pc}": PW[self.decide(R, np.array([float(pc)]))[0][0]] for pc in (0, 25, 50, 100)}))
                if name == "market":
                    char.append(self._char_row(c, R, Q))
        return pd.DataFrame(rows), pd.DataFrame(char).set_index("city"), pd.DataFrame(best)

    def _char_row(self, c, R, Q):
        X = {k: v[0] for k, v in R["X"].items()}; road = c.d_line * self.REG.central["tort"]
        qm1, qm2 = Q[0] * c.m_M1, Q[0] * c.m_M2; G = self.GATE
        return dict(city=c.city, comp_year=c.comp_year, t_status=c.t_status, Q_2025=Q[0], Q_alt=c.Q_alt,
                    Q_managed_M1=qm1, Q_managed_M2=qm2, moisture=X["moist"], LHV=X["lhv"],
                    L0_od=X["L0_od"], L0_sl=X["L0_sl"], wte_kwh=X["wte_kwh"], rdf_yield=X["rdf_t_S2"],
                    rdf_ncv=X["rdf_ncv_S2"], ad_kwh=X["ad_kwh"], phb_kg=X["phb_kg"], phb_tpa=X["phb_tpa"],
                    phb_cost=X["phb_cost"], kiln=c.kiln, road_km=road,
                    gate_LHV=X["lhv"] >= G["lhv_min"], gate_WtE_scale=Q[0] >= G["q_wte"],
                    gate_PSEL_generated=Q[0] >= G["q_psel"],
                    gate_PSEL_M1=(qm1 >= G["q_psel"]) if pd.notna(qm1) else np.nan,
                    gate_PSEL_M2=(qm2 >= G["q_psel"]) if pd.notna(qm2) else np.nan,
                    gate_RDF=bool(X["rdf_ncv_S2"] >= G["ncv_rdf"]) and road <= G["d_max"],
                    gate_PHB=X["phb_tpa"] >= G["phb_min"], landfilled_S5=X["landfilled_S5"],
                    norm_dom=c.norm_dom, norm_nd=c.norm_nd, glass=c.glass,
                    flags="".join(f for f, b in (("L", c.lumped and not c.woody), ("W", c.woody), ("R", c.rounded),
                                                 ("G", c.glass == "folded_into_lain_lain"),
                                                 ("N", c.glass == "not_reported_blank"),
                                                 ("T", c.t_status == "overview_index_year_unverified")) if b))

    def run_mc(self, name, N=None, vary=("param", "comp"), scenarios=None, keep=False):
        N = N or self.N_MC
        sc = (scenarios or SCENARIOS)[name]
        out, samples = [], {}
        q = lambda a: dict(zip(("p5", "p50", "p95"), np.percentile(a, [5, 50, 95])))
        for i, cid in enumerate(self.H.index):
            self.rng = np.random.default_rng([self.SEED, i])
            c, P, s, Q, kw = self.setup(i, N, False, vary=vary, **sc)
            if np.isnan(Q).any(): continue
            R = self.model(s, P, c, Q, **kw); pick, front = self.decide(R, P["pc"])
            dd = {f"{ind}_{bg}": self.delta(R, bg, ind) for ind in INDICATORS for bg in ("OD", "SL")}
            for j, k in enumerate(PW):
                stats = {f"{nm}_{p}": v for nm, a in (("G", R["G"]), ("C", R["C"]), ("dG_OD", dd["G_OD"]),
                                                       ("dC_OD", dd["C_OD"]), ("dG_SL", dd["G_SL"]), ("dC_SL", dd["C_SL"]),
                                                       ("CED", R["CED"]), ("LU", R["LU"]))
                         for p, v in q(a[:, j]).items()}
                out.append(dict(city=c.city, scenario=name, pathway=k, p_feasible=R["ok"][:, j].mean(),
                                p_front=front[:, j].mean(), p_best=(pick == j).mean(),
                                **{f"p_best_pc{lo}_{hi}": (pick[(P["pc"] >= lo) & (P["pc"] < hi)] == j).mean()
                                   for lo, hi in ((0, 10), (10, 50), (50, 100))}, **stats))
            if keep: samples[cid] = (P, s, R)
        return pd.DataFrame(out), samples

    @staticmethod
    def winners(df, cities, col="p_best"):
        t = df.pivot(index="city", columns="pathway", values=col).reindex(cities).dropna(how="all")[PW]
        return t.assign(best=t.idxmax(axis=1), p=t.max(axis=1))

    def uncertainty_split(self, N=2000):
        acc = []
        for lab, vary in (("composition only", ("comp",)), ("parameters only", ("param",)), ("both", ("param", "comp"))):
            df, _ = self.run_mc("market", N, vary)
            df = df.assign(G_w90=df.G_p95 - df.G_p5, C_w90=df.C_p95 - df.C_p5, source=lab)
            acc.append(df.groupby(["source", "pathway"])[["G_w90", "C_w90"]].median())
        return pd.concat(acc).unstack(0)

    def spearman(self, samples):
        acc = []
        for cid, (P, s, R) in samples.items():
            skip = set(self.LCIA.index) if self.LCIA is not None else set()   # CED/LU factors cannot affect G or C
            Xin = pd.DataFrame({k: v for k, v in P.items() if not k.startswith("_") and k not in ("w", "pc")
                                and k not in skip and np.ptp(v) > 0})
            Xin["moisture"] = R["X"]["moist"]
            for j, f in enumerate(self.FR): Xin["s_" + f] = s[:, j]
            Xin = Xin.loc[:, Xin.std() > 0].rank()
            for j, k in enumerate(PW):
                for nm in ("G", "C"):
                    acc.append(Xin.corrwith(pd.Series(R[nm][:, j]).rank()).rename((k, nm)))
        return pd.concat(acc, axis=1).T.groupby(level=[0, 1]).agg(lambda x: x.abs().mean()).T

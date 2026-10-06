"""
One-city analysis: read a manually filled template (XLSX, or CSV files), validate it, apply optional local parameter
values, and run the full analysis for that city only.

Template (see templates/single_city_template.xlsx and the three CSV files with the same content):
  city_data         one row per field (field, value): tonnage and its year, grid, kiln distance or coordinates, ...
  composition       one row per RIPS category: wet-mass % for the domestic stream and, optionally, the non-domestic stream
  local_parameters  optional: your own central/low/high value for selected model parameters (e.g. measured moisture,
                    local landfill cost); blank rows keep the default from data/assumption_register.csv

Rules (reported back as warnings): tonnage is projected once to 2025, Q2025 = Qt (1+g)^(2025-t); composition is
normalised per stream and weighted by the 2025 tonnage of each stream; a blank composition cell means "not reported",
never zero; if only a central value is given for a parameter, its default relative range is kept around the new value.
"""
from pathlib import Path
import numpy as np
import pandas as pd
from .core import MSWModel, PW, NAMES, SCENARIOS, CARBON_VALUES, MAP, RIPS_CATS

# --------------------------------------------------------------------------------------------- template definition
# field: (required, unit, description EN, description ID)
CITY_FIELDS = {
    "city_name": (True, "-", "Name of the city or regency", "Nama kota/kabupaten"),
    "province": (False, "-", "Province", "Provinsi"),
    "grid_region": (True, "-", "Electricity grid: Jamali, Sumatera or Mahakam", "Sistem listrik: Jamali, Sumatera, atau Mahakam"),
    "tonnage_total_tpd": (True, "t/day", "Waste generated or delivered, wet weight (domestic + non-domestic)",
                          "Timbulan/sampah yang diterima, berat basah (domestik + non-domestik)"),
    "tonnage_nondomestic_tpd": (False, "t/day", "Part of the total that is non-domestic (blank = 0 or not separated)",
                                "Bagian non-domestik dari total (kosong = 0 atau tidak dipisah)"),
    "tonnage_year": (True, "year", "Year the tonnage refers to", "Tahun data tonase"),
    "growth_rate": (False, "1/yr", "Annual growth of tonnage as a fraction (0.017 = 1.7%/yr); blank = 1.73%/yr default",
                    "Laju pertumbuhan tonase per tahun, pecahan (0,017 = 1,7%/th); kosong = 1,73%/th"),
    "composition_year": (True, "year", "Year the composition was sampled", "Tahun sampling komposisi"),
    "organik_lumped": (False, "yes/no", "yes if food and garden waste are reported together in sisa_makanan",
                       "yes bila sampah makanan dan daun dilaporkan gabung di sisa_makanan"),
    "lat": (False, "deg", "Latitude of the planned facility (decimal degrees, south negative)", "Lintang lokasi fasilitas (desimal, selatan negatif)"),
    "lon": (False, "deg", "Longitude of the planned facility", "Bujur lokasi fasilitas"),
    "kiln_road_km": (False, "km", "Road distance to the nearest cement kiln (overrides lat/lon)",
                     "Jarak jalan ke pabrik semen terdekat (menggantikan lat/lon)"),
    "managed_share": (False, "0-1", "Share of the waste collected or managed today", "Porsi sampah yang terkelola saat ini"),
    "managed_share_definition": (False, "-", "What the managed share measures", "Definisi indikator terkelola"),
    "source": (False, "-", "Source document and page of these data", "Dokumen sumber dan halaman"),
    "monte_carlo_draws": (False, "-", "Number of Monte Carlo draws (blank = 4000)", "Jumlah iterasi Monte Carlo (kosong = 4000)"),
    "random_seed": (False, "-", "Random seed for reproducibility (blank = 20251)", "Seed acak agar dapat direproduksi (kosong = 20251)"),
}
CAT_LABEL = {"sisa_makanan": ("food waste", "sisa makanan"), "daun_kering": ("garden/leaves", "daun/ranting"),
             "kertas_kardus": ("paper and cardboard", "kertas dan kardus"), "kayu": ("wood", "kayu"),
             "plastik": ("plastics", "plastik"), "logam": ("metals", "logam"), "kain": ("textiles", "kain/tekstil"),
             "karet_kulit": ("rubber and leather", "karet dan kulit"), "sterofoam": ("styrofoam", "sterofoam"),
             "b3": ("household hazardous", "B3 rumah tangga"), "elektronik": ("e-waste", "elektronik"),
             "lain_lain": ("other/inert", "lain-lain"), "kaca": ("glass", "kaca")}
TO_FRACTION = {c: f for f, cs in MAP.items() for c in cs}

# parameters a researcher may know locally: (key, group)
LOCAL_KEYS = [("cap", "Landfill"), ("ox", "Landfill"), ("c_sl", "Landfill"), ("c_od", "Landfill"), ("mcf_od", "Landfill"),
              ("p_el", "Prices"), ("p_rdf", "Prices"), ("c_truck", "Prices"),
              ("eta_wte", "WtE"), ("K_wte", "WtE"), ("o_wte", "WtE"),
              ("K_rdf", "RDF"), ("o_rdf", "RDF"), ("tau_k", "RDF"), ("omega", "RDF"),
              ("kappa", "AD"), ("y_ch4", "AD"), ("y_pen_mech", "AD"), ("pre_ofmsw", "AD"), ("K_ad", "AD"), ("o_ad", "AD"),
              ("fug_ad", "AD"), ("c_phb", "PHB"), ("p_phb", "PHB"), ("r", "Finance"), ("n", "Finance"),
              ("gamma", "Composition")]
YES = {"yes", "y", "ya", "true", "1", "1.0"}


def _yes(v):
    return str(v).strip().lower() in YES


def _num(v):
    return pd.to_numeric(v, errors="coerce")


def parameter_table(M):
    """Rows of the local_parameters sheet with the model defaults."""
    rows = []
    for k, g in LOCAL_KEYS:
        src = M.REG if k in M.REG.index else M.SP
        r = src.loc[k]
        rows.append(dict(key=k, group=g, meaning=r.meaning, unit=r.unit, default_central=r.central, default_low=r.low,
                         default_high=r.high, your_central=np.nan, your_low=np.nan, your_high=np.nan, your_source=""))
    for j, f in enumerate(M.FR):
        rows.append(dict(key=f"moisture_{f}", group="Waste properties", meaning=f"Moisture of {f} as received (wet basis)",
                         unit="kg water/kg", default_central=M.W[j], default_low=M.W_LO[j], default_high=M.W_HI[j],
                         your_central=np.nan, your_low=np.nan, your_high=np.nan, your_source=""))
    for j, f in enumerate(M.FR):
        rows.append(dict(key=f"lhv_dry_{f}", group="Waste properties", meaning=f"Lower heating value of {f}, dry matter",
                         unit="MJ/kg dry", default_central=M.HDRY[j], default_low=np.nan, default_high=np.nan,
                         your_central=np.nan, your_low=np.nan, your_high=np.nan, your_source=""))
    return pd.DataFrame(rows)


def blank_template(M, example=None):
    """(city_data, composition, local_parameters) frames; `example` = dict from example_padang() fills an example column."""
    ex = example or {}
    cd = pd.DataFrame([dict(field=f, value="", unit=u, required="yes" if req else "no",
                            example_kota_padang=ex.get("fields", {}).get(f, ""), description_en=en, description_id=id_)
                       for f, (req, u, en, id_) in CITY_FIELDS.items()])
    exc = ex.get("composition", {})
    comp = pd.DataFrame([dict(category=c, label_en=CAT_LABEL[c][0], label_id=CAT_LABEL[c][1], model_fraction=TO_FRACTION[c],
                              domestic_pct=np.nan, nondomestic_pct=np.nan,
                              example_domestic_pct=exc.get(c, (np.nan, np.nan))[0],
                              example_nondomestic_pct=exc.get(c, (np.nan, np.nan))[1]) for c in RIPS_CATS])
    return cd, comp, parameter_table(M)


def example_padang(data_dir):
    """The Kota Padang RIPS values in template form (used as the example and for the reproduction check)."""
    from .core import read_input
    r = read_input(Path(data_dir) / "input_kota_2025_updated.csv").set_index("city_id").loc["kota_padang"]
    fields = dict(city_name=r.city_name, province=r.province, grid_region=r.grid_region,
                  tonnage_total_tpd=float(r.tonnage_source_value_tpd), tonnage_nondomestic_tpd=float(r.M_nd_tpd),
                  tonnage_year=int(r.tonnage_source_year), growth_rate=float(r.growth_rate_used),
                  composition_year=int(r.composition_sampling_year), organik_lumped="no", lat=r.lat_used, lon=r.lon_used,
                  kiln_road_km="", managed_share=r.managed_share_local_lower,
                  managed_share_definition="waste delivered to the TPA / generation (lower bound; provisional)",
                  source="RIPS Kota Padang (domestic and non-domestic composition)", monte_carlo_draws=4000,
                  random_seed=20251)
    comp = {c: (r[f"dom_{c}"], r[f"nd_{c}"]) for c in RIPS_CATS}
    return dict(fields=fields, composition=comp)


def filled_example(M, data_dir):
    """Template frames filled with the Padang example in the value columns."""
    ex = example_padang(data_dir)
    cd, comp, par = blank_template(M, ex)
    cd["value"] = cd.field.map(ex["fields"]).fillna("")
    comp["domestic_pct"] = comp.category.map({k: v[0] for k, v in ex["composition"].items()})
    comp["nondomestic_pct"] = comp.category.map({k: v[1] for k, v in ex["composition"].items()})
    return cd, comp, par


# --------------------------------------------------------------------------------------------- reading and validation
def read_template(path=None, city_csv=None, comp_csv=None, par_csv=None):
    """Read an XLSX template (sheets city_data, composition, local_parameters) or the CSV files."""
    if path is not None and str(path).lower().endswith((".xlsx", ".xls")):
        x = pd.ExcelFile(path)
        cd = pd.read_excel(x, "city_data"); comp = pd.read_excel(x, "composition")
        par = pd.read_excel(x, "local_parameters") if "local_parameters" in x.sheet_names else None
    else:
        cd = pd.read_csv(city_csv or path); comp = pd.read_csv(comp_csv)
        par = pd.read_csv(par_csv) if par_csv else None
    return cd, comp, par


def to_input(cd, comp, M):
    """Validate and convert to the one-row input format of MSWModel. Returns (raw, errors, warnings)."""
    err, warn = [], []
    f = {str(r.field).strip(): r.value for r in cd.itertuples()}
    get = lambda k: f.get(k, np.nan)
    blank = lambda v: v is None or (isinstance(v, float) and np.isnan(v)) or str(v).strip() in ("", "nan")
    for k, (req, *_) in CITY_FIELDS.items():
        if req and blank(get(k)):
            err.append(f"'{k}' is required")
    grid = str(get("grid_region")).strip()
    if grid not in M.EF_GRID:
        err.append(f"grid_region must be one of {list(M.EF_GRID)}")
    q = _num(get("tonnage_total_tpd")); qn = _num(get("tonnage_nondomestic_tpd")); qn = 0.0 if pd.isna(qn) else float(qn)
    if not (q > 0):
        err.append("tonnage_total_tpd must be a positive number")
    elif not (0 <= qn < q):
        err.append("tonnage_nondomestic_tpd must be between 0 and the total")
    for y in ("tonnage_year", "composition_year"):
        v = _num(get(y))
        if not (1990 <= v <= 2025):
            err.append(f"{y} must be a year between 1990 and 2025")
    comp = comp.set_index("category")
    dom = _num(comp.reindex(RIPS_CATS).domestic_pct); nd = _num(comp.reindex(RIPS_CATS).nondomestic_pct)
    for lab, s in (("domestic", dom), ("non-domestic", nd)):
        if s.notna().any():
            if (s < 0).any(): err.append(f"{lab} composition has negative values")
            tot = s.sum()
            if not (90 <= tot <= 110): err.append(f"{lab} composition sums to {tot:.2f}%; expected about 100%")
            elif abs(tot - 100) > 0.05: warn.append(f"{lab} composition sums to {tot:.2f}%; normalised (factor {100 / tot:.4f})")
    if dom.notna().sum() == 0:
        err.append("the domestic composition is empty")
    if qn > 0 and nd.notna().sum() == 0:
        warn.append("non-domestic tonnage given without a non-domestic composition: the domestic composition is used for both")
    lat, lon, kkm = _num(get("lat")), _num(get("lon")), _num(get("kiln_road_km"))
    if pd.isna(kkm) and (pd.isna(lat) or pd.isna(lon)):
        err.append("give lat and lon, or kiln_road_km")
    m = _num(get("managed_share"))
    if pd.notna(m) and not (0 <= m <= 1):
        err.append("managed_share must be between 0 and 1")
    if err:
        return None, err, warn
    t = int(_num(get("tonnage_year"))); g = _num(get("growth_rate"))
    if pd.isna(g):
        g, basis = M.REG.central["g_default"], "fallback median of RIPS rates (assumption)"
        if t < 2025: warn.append(f"growth_rate blank: default {g:.4f}/yr used (assumption)")
    else:
        g, basis = float(g), "user-supplied growth rate"
    expo = max(2025 - t, 0); fac = (1 + g) ** expo
    if int(_num(get("composition_year"))) < 2020:
        warn.append("composition sampled before 2020: used as a 2025 proxy with wider uncertainty (alpha 40)")
    if dom.isna().any() or nd.isna().any():
        miss = [c for c in RIPS_CATS if pd.isna(dom[c])]
        if miss: warn.append("not reported (zero share, modelling assumption): " + ", ".join(miss))
    out = dict(city_id="my_city", city_name=str(get("city_name")), province=str(get("province")), admin_level="",
               composition_mode="separate" if nd.notna().any() else "combined",
               organik_lumped_to_food=_yes(get("organik_lumped")), glass_folded_into_inert=False,
               source_pdf=str(get("source")), source_pages="", composition_sampling_year=int(_num(get("composition_year"))),
               tonnage_source_value_tpd=float(q), tonnage_source_year=t, tonnage_year_status="user_supplied",
               tonnage_already_2025=expo == 0, baseline_year=2025, growth_rate_used=g, growth_rate_basis=basis,
               projection_exponent_years=expo, M_dom_tpd=float(q) - qn, M_nd_tpd=qn,
               M_dom_tpd_2025=(float(q) - qn) * fac, M_nd_tpd_2025=qn * fac, Q2025_tpd=float(q) * fac,
               Q2025_alt_tpd=float(q) * fac, managed_share_status_index=np.nan, managed_share_local=m,
               managed_share_local_lower=np.nan, grid_region=grid, lat_used=lat, lon_used=lon, kiln_road_km=kkm,
               glass_status="reported_separately" if pd.notna(dom["kaca"]) else "not_reported_blank")
    for c in RIPS_CATS:
        out[f"dom_{c}"] = dom[c]; out[f"nd_{c}"] = nd[c]
    raw = pd.DataFrame([out])
    raw["tonnage_already_2025"] = raw.tonnage_already_2025.astype(bool)
    warn.append(f"2025 tonnage = {float(q):,.1f} x (1 + {g:.4f})^{expo} = {float(q) * fac:,.1f} t/day (projected once)")
    return raw, err, warn


def apply_local_parameters(M, par):
    """Overwrite defaults with the user's values. Returns a table of what was changed."""
    if par is None or len(par) == 0:
        return pd.DataFrame(columns=["key", "central", "low", "high", "source"])
    done = []
    M.W, M.W_LO, M.W_HI, M.HDRY = (np.array(a, dtype=float) for a in (M.W, M.W_LO, M.W_HI, M.HDRY))   # writable copies
    M.APPLY_HK = np.array(M.APPLY_HK, dtype=bool)
    for r in par.itertuples():
        c, lo, hi = _num(r.your_central), _num(r.your_low), _num(r.your_high)
        if pd.isna(c) and pd.isna(lo) and pd.isna(hi):
            continue
        key = str(r.key)
        d0, l0, h0 = _num(r.default_central), _num(r.default_low), _num(r.default_high)
        if pd.isna(c): c = d0
        if pd.isna(lo): lo = c * l0 / d0 if pd.notna(l0) and d0 else c
        if pd.isna(hi): hi = c * h0 / d0 if pd.notna(h0) and d0 else c
        lo, hi = min(lo, c), max(hi, c)
        if key.startswith("moisture_"):
            j = M.FR.index(key[9:]); hi = min(hi, 0.95)
            M.W[j], M.W_LO[j], M.W_HI[j] = c, lo, hi
        elif key.startswith("lhv_dry_"):
            j = M.FR.index(key[8:]); M.HDRY[j] = c; M.APPLY_HK[j] = False   # measured value: no legacy calibration
            lo = hi = c
        else:
            src = M.REG if key in M.REG.index else M.SP
            src.loc[key, ["central", "low", "high"]] = [c, lo, hi]
        done.append(dict(key=key, central=c, low=lo, high=hi, source=getattr(r, "your_source", "")))
    return pd.DataFrame(done)


# --------------------------------------------------------------------------------------------- analysis
def analyse(M, raw, N=4000, scenario_draws=2000, scenarios=None):
    """Everything for one city. Returns a dict of tables and arrays."""
    from .voi import voi_groups, voi_location
    from .thresholds import threshold_table
    H = M.load_cities(raw)
    M.check_single_projection()
    c = H.iloc[0]; Q = float(c.Q2025); A = dict(city=c.city, Q2025=Q, H=H)
    det, char, best = M.run_central({"market": {}})
    A["char"] = char.T.rename(columns={c.city: "value"})
    o, s = det[det.baseline == "OD"].set_index("pathway"), det[det.baseline == "SL"].set_index("pathway")
    A["central"] = pd.DataFrame({"option": [NAMES[k] for k in PW], "passes_gates": o.feasible.values,
                                 "GHG_kgCO2e_t": o.G.values, "avoided_vs_open_dump": o.dG.values,
                                 "avoided_vs_landfill": s.dG.values, "net_cost_USD_t": o.C.values,
                                 "abatement_cost_vs_landfill_USD_tCO2e": s.MAC.values,
                                 "fossil_CED_MJ_t": o.CED.values, "land_m2_t": o.LU.values}, index=PW)
    A["best_central"] = best.drop(columns=["scenario"]).T
    # Monte Carlo, main case
    mc, smp = M.run_mc("market", N=N, keep=True)
    P, sm, R = smp[raw.city_id.iloc[0]]
    A["samples"] = (P, sm, R)
    rows = []
    for k in PW:
        q = mc.set_index("pathway").loc[k]
        for pc in CARBON_VALUES:
            p, se = q[f"p_best_at_{pc}"], q[f"p_best_at_{pc}_se"]
            rows.append(dict(carbon_value=pc, option=k, p_best=p, se=se, ci95_low=max(p - 1.96 * se, 0), ci95_high=min(p + 1.96 * se, 1)))
    A["p_fixed"] = pd.DataFrame(rows)
    pcs = np.arange(0, 101, 5)
    A["p_curve"] = pd.DataFrame({pc: np.bincount(M.decide(R, np.full(N, float(pc)))[0], minlength=6) / N for pc in pcs},
                                index=PW).T
    A["curve_central"] = pd.DataFrame({k: o.C[k] + pcs * o.G[k] / 1e3 for k in PW}, index=pcs)
    A["mc"] = mc.set_index("pathway")[[f"{v}_{p}" for v in ("G", "C", "dG_SL", "CED", "LU") for p in ("p5", "p50", "p95")]
                                      + ["p_feasible"]]
    sp = M.spearman(smp)
    A["spearman"] = pd.concat({f"{k} {ind}": sp[(k, ind)].nlargest(6) for k in ("SL", "S1", "S5") for ind in ("G", "C")
                               if (k, ind) in sp.columns}, names=["output", "input"]).rename("abs_rho").reset_index()
    # scenarios and stress tests
    srows = []
    for name in (scenarios or SCENARIOS):
        df, _ = M.run_mc(name, N=scenario_draws)
        if len(df) == 0:
            srows.append(dict(scenario=name, note="not applicable (no data for this city)")); continue
        bc = best if name == "market" else M.run_central({"market": SCENARIOS[name]})[2]
        row = dict(scenario=name, **{f"central_best_at_{pc}": bc[f"best_at_{pc}"].iloc[0] for pc in (0, 25, 50, 100)})
        for pc in (25, 50, 100):
            q = df.set_index("pathway")[f"p_best_at_{pc}"]
            row[f"most_probable_at_{pc}"] = f"{q.idxmax()} ({q.max():.2f})"
        srows.append(row)
    A["scenarios"] = pd.DataFrame(srows)
    A["break_even"] = threshold_table(M, carbon_values=(25, 50, 100)).drop(columns=["city"])
    vg, vi = [], []
    for pc in (25, 50, 100):
        g, evpi = voi_groups(P, sm, R, M.FR, pc=pc)
        vg.append(g.assign(carbon_value=pc, evpi=evpi, usd_per_year=g.evppi_net * Q * 365, evpi_usd_per_year=evpi * Q * 365))
        d, _ = voi_location(P, sm, R, M.FR, pc=pc)
        vi.append(d.assign(carbon_value=pc, usd_per_year=d.evppi_net * Q * 365).head(10))
    A["voi_groups"] = pd.concat(vg, ignore_index=True); A["voi_inputs"] = pd.concat(vi, ignore_index=True)
    A["N"] = N
    return A


# --------------------------------------------------------------------------------------------- figures and report
COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}


def figures(A, M, outdir):
    import matplotlib.pyplot as plt
    out = Path(outdir); out.mkdir(parents=True, exist_ok=True); paths = {}
    s = A["H"].iloc[0][[f"s_{f}" for f in M.FR]].astype(float).values
    fig, ax = plt.subplots(figsize=(8, 3.2)); ax.bar(M.FR, 100 * s, color="#46719e")
    ax.set_ylabel("% of wet mass"); ax.set_title(f"{A['city']}: composition used (2025 tonnage-weighted)", fontsize=10)
    fig.tight_layout(); paths["composition"] = out / "fig1_composition.png"; fig.savefig(paths["composition"], dpi=160); plt.close(fig)
    ct = A["central"]
    fig, ax = plt.subplots(1, 2, figsize=(11, 3.6))
    ax[0].bar(PW, ct.GHG_kgCO2e_t, color=[COL[k] for k in PW]); ax[0].set_title("GHG, kg CO2e per t MSW", fontsize=10)
    ax[1].bar(PW, ct.net_cost_USD_t, color=[COL[k] for k in PW]); ax[1].set_title("Net cost, USD per t MSW", fontsize=10)
    for a in ax: a.axhline(0, c="k", lw=0.5)
    fig.tight_layout(); paths["central"] = out / "fig2_central.png"; fig.savefig(paths["central"], dpi=160); plt.close(fig)
    fig, ax = plt.subplots(1, 2, figsize=(11, 3.8))
    for k in PW:
        ls = "-" if bool(ct.loc[k, "passes_gates"]) else ":"
        ax[0].plot(A["curve_central"].index, A["curve_central"][k], ls, color=COL[k], label=NAMES[k])
        ax[1].plot(A["p_curve"].index, A["p_curve"][k], "-o", ms=2.5, color=COL[k], label=NAMES[k])
    ax[0].set_xlabel("Carbon value, USD/t CO2e"); ax[0].set_ylabel("Carbon-inclusive cost, USD/t")
    ax[0].set_title("Central values (dotted = fails a gate)", fontsize=9); ax[0].legend(fontsize=7, frameon=False)
    ax[1].set_xlabel("Fixed carbon value, USD/t CO2e"); ax[1].set_ylabel("P(best)"); ax[1].set_ylim(0, 1)
    ax[1].set_title(f"Monte Carlo, {A['N']:,} draws per carbon value", fontsize=9)
    fig.tight_layout(); paths["decision"] = out / "fig3_decision.png"; fig.savefig(paths["decision"], dpi=160); plt.close(fig)
    P, sm, R = A["samples"]
    fig, ax = plt.subplots(1, 2, figsize=(11, 3.6))
    for a, key, lab in zip(ax, ("G", "C"), ("GHG, kg CO2e/t", "Net cost, USD/t")):
        bp = a.boxplot([R[key][:, j] for j in range(6)], whis=(5, 95), showfliers=False, patch_artist=True)
        for patch, k in zip(bp["boxes"], PW): patch.set_facecolor(COL[k]); patch.set_alpha(0.6)
        a.set_xticks(range(1, 7), PW); a.set_title(lab + " (box: IQR, whiskers: 5-95%)", fontsize=9); a.axhline(0, c="k", lw=0.4)
    fig.tight_layout(); paths["uncertainty"] = out / "fig4_uncertainty.png"; fig.savefig(paths["uncertainty"], dpi=160); plt.close(fig)
    vg = A["voi_groups"]
    fig, ax = plt.subplots(1, 3, figsize=(13, 3.8), sharey=True)
    order = vg[vg.carbon_value == 100].sort_values("evppi_net").group.tolist()
    for a, pc in zip(ax, (25, 50, 100)):
        q = vg[vg.carbon_value == pc].set_index("group").reindex(order)
        a.barh([g.split(" (")[0] for g in order], q.evppi_net, color="#46719e")
        a.set_title(f"{pc} USD/t CO2e (EVPI {q.evpi.iloc[0]:.2f} USD/t)", fontsize=9); a.set_xlabel("EVPPI, USD per t MSW")
        a.tick_params(labelsize=7.5)
    fig.tight_layout(); paths["voi"] = out / "fig5_value_of_information.png"; fig.savefig(paths["voi"], dpi=160); plt.close(fig)
    return paths


CAVEATS = [
    "Ex-ante screening LCA/TEA: no facility is assumed to exist; results show which options are worth a feasibility study.",
    "Functional unit: 1 t of mixed MSW at the facility gate in 2025. Costs are class-5 estimates (USD/t).",
    "Decision criterion: carbon-inclusive cost C + pG/1000 at a fixed carbon value p; health and local pollution are not valued.",
    "P(best) is reported with its Monte Carlo standard error; a small lead means the choice depends on uncertain inputs.",
    "Composition is the sampling-year composition used as a 2025 proxy; categories left blank are not reported (zero share).",
    "Default parameters, their sources and status are in data/assumption_register.csv and data/scenario_parameters.csv.",
]


def write_report(A, path, inputs=None, warnings=None, overrides=None):
    path = Path(path)
    with pd.ExcelWriter(path, engine="openpyxl") as w:
        pd.DataFrame({"read_me": CAVEATS + [f"warning: {x}" for x in (warnings or [])]}).to_excel(w, sheet_name="README", index=False)
        if inputs is not None: inputs.T.to_excel(w, sheet_name="inputs_used")
        if overrides is not None and len(overrides): overrides.to_excel(w, sheet_name="local_parameters_used", index=False)
        A["char"].to_excel(w, sheet_name="characterisation")
        A["central"].to_excel(w, sheet_name="central_results")
        A["p_fixed"].to_excel(w, sheet_name="p_best_fixed_carbon", index=False)
        A["mc"].to_excel(w, sheet_name="monte_carlo_p5_p50_p95")
        A["scenarios"].to_excel(w, sheet_name="scenarios_stress_tests", index=False)
        A["break_even"].to_excel(w, sheet_name="break_even_vs_landfill", index=False)
        A["voi_groups"].to_excel(w, sheet_name="value_of_information", index=False)
        A["voi_inputs"].to_excel(w, sheet_name="voi_single_inputs", index=False)
        A["spearman"].to_excel(w, sheet_name="spearman_drivers", index=False)
    return path


# --------------------------------------------------------------------------------------------- projection to 2045
def project_city(data_dir, raw, par=None, seed=None, N=4000, year=2045):
    """2025 versus `year` for the one city, with the user's local values (read as 2025 values) escalated like the
    defaults. Returns a dict of tables; Monte Carlo draws are paired between the two years."""
    from .projection import apply_year, read_parameters, grid_factor, COMPONENTS
    pr = read_parameters(data_dir)

    def build(yr, comp=COMPONENTS):
        M = MSWModel(data_dir, seed=seed)
        apply_local_parameters(M, par)
        info = apply_year(M, raw, yr, comp if yr != int(pr["base_year"]) else (), pr=pr)
        return M, info

    M25, _ = build(int(pr["base_year"])); M45, info = build(year)
    out = dict(year=year, factors=pd.Series(info), parameters=pd.Series(pr))
    k = ["baseline", "pathway"]
    d25 = M25.run_central({"market": {}})[0].set_index(k); d45 = M45.run_central({"market": {}})[0].set_index(k)
    od = lambda d: d.xs("OD")
    t = pd.DataFrame({"G_2025": od(d25).G, f"G_{year}": od(d45).G, "C_2025": od(d25).C, f"C_{year}": od(d45).C,
                      "CED_2025": od(d25).CED, f"CED_{year}": od(d45).CED,
                      "avoided_vs_SL_2025": d25.xs("SL").dG, f"avoided_vs_SL_{year}": d45.xs("SL").dG,
                      "MAC_vs_SL_2025": d25.xs("SL").MAC, f"MAC_vs_SL_{year}": d45.xs("SL").MAC}).reindex(PW)
    t["G_change"] = t[f"G_{year}"] - t.G_2025; t["C_change"] = t[f"C_{year}"] - t.C_2025
    out["central"] = t
    dec = {}
    for lab, comp in (("grid only", ("grid",)), ("costs only", ("costs",)), ("tonnage only", ("tonnage",))):
        Mx, _ = build(year, comp); dx = od(Mx.run_central({"market": {}})[0].set_index(k))
        dec[("G", lab)] = dx.G - od(d25).G; dec[("C", lab)] = dx.C - od(d25).C
    dec = pd.DataFrame(dec).reindex(PW)
    dec[("G", "total")] = t.G_change; dec[("C", "total")] = t.C_change
    out["decomposition"] = dec.sort_index(axis=1)
    mc25, s25 = M25.run_mc("market", N=N, keep=True); mc45, s45 = M45.run_mc("market", N=N, keep=True)
    cid = raw.city_id.iloc[0]; R0, R1 = s25[cid][2], s45[cid][2]
    q = lambda a: np.percentile(a, [5, 50, 95])
    out["change_uncertainty"] = pd.DataFrame([dict(option=kk, indicator=ind, **dict(zip(("p5", "p50", "p95"), q(R1[ind][:, j] - R0[ind][:, j]))))
                                              for j, kk in enumerate(PW) for ind in ("G", "C")])
    rows = []
    for yr, mc in (("2025", mc25), (str(year), mc45)):
        for pc in CARBON_VALUES:
            s = mc.set_index("pathway")[f"p_best_at_{pc}"]
            rows.append(dict(year=yr, carbon_value=pc, most_probable=s.idxmax(), p=s.max(), **{f"P({x})": s[x] for x in PW}))
    out["p_best"] = pd.DataFrame(rows)
    path = []
    for yr in range(int(pr["base_year"]), int(pr["netzero_year"]) + 1, 5):
        Mx, _ = build(yr); dx = od(Mx.run_central({"market": {}})[0].set_index(k))
        for kk in PW:
            path.append(dict(year=yr, option=kk, grid_factor=grid_factor(pr, yr), G=dx.G[kk], C=dx.C[kk],
                             CIC50=dx.C[kk] + 50 * dx.G[kk] / 1e3, CIC100=dx.C[kk] + 100 * dx.G[kk] / 1e3))
    out["path"] = pd.DataFrame(path)
    return out


def projection_figure(PJ, outdir):
    import matplotlib.pyplot as plt
    p = Path(outdir) / "fig6_projection.png"; Y = PJ["year"]; t = PJ["central"]
    fig, ax = plt.subplots(1, 3, figsize=(14, 3.8)); x = np.arange(len(PW))
    for a, ind, lab in ((ax[0], "G", "GHG, kg CO2e/t"), (ax[1], "C", "Net cost, USD/t")):
        a.bar(x - 0.2, t[f"{ind}_2025"], 0.38, color=[COL[k] for k in PW], alpha=0.5, label="2025")
        a.bar(x + 0.2, t[f"{ind}_{Y}"], 0.38, color=[COL[k] for k in PW], label=str(Y), edgecolor="k", linewidth=0.3)
        a.set_xticks(x, PW); a.axhline(0, c="k", lw=0.5); a.set_title(f"{lab}: light 2025, dark {Y}", fontsize=9)
    for kk in PW:
        v = PJ["path"][PJ["path"].option == kk]; ax[2].plot(v.year, v.CIC100, "-o", ms=3, color=COL[kk], label=NAMES[kk])
    ax[2].axvline(Y, c="k", lw=0.6, ls=":"); ax[2].set_title("Carbon-inclusive cost at 100 USD/t CO2e, 2025-2060", fontsize=9)
    ax[2].legend(fontsize=7, frameon=False); fig.tight_layout(); fig.savefig(p, dpi=160); plt.close(fig)
    return p


def write_projection_report(PJ, path):
    with pd.ExcelWriter(path, engine="openpyxl") as w:
        pd.concat([PJ["parameters"].rename("value"), PJ["factors"].rename("value")]).to_frame().to_excel(w, sheet_name="drivers")
        PJ["central"].to_excel(w, sheet_name="central_2025_vs_target")
        PJ["decomposition"].to_excel(w, sheet_name="decomposition")
        PJ["change_uncertainty"].to_excel(w, sheet_name="change_uncertainty", index=False)
        PJ["p_best"].to_excel(w, sheet_name="p_best_fixed_carbon", index=False)
        PJ["path"].to_excel(w, sheet_name="year_path_2025_2060", index=False)
    return path

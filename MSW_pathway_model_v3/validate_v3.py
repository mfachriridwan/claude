"""
Validation checks for the v3 database and parser. Run after build_database_v3.py:
    python validate_v3.py
Writes outputs/validation_report.csv and outputs/validation_report.md and exits non-zero on any failure.
"""
import sys, tempfile
from pathlib import Path
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent
D, OUT = ROOT / "data", ROOT / "outputs"
OUT.mkdir(exist_ok=True)
R = []


def check(group, name, ok, detail=""):
    R.append(dict(group=group, check=name, result="PASS" if bool(ok) else "FAIL", detail=str(detail)))


inp = pd.read_csv(D / "input_kota_2025_updated.csv")
t2 = pd.read_csv(D / "table2_model_fraction_parameters.csv")
reg = pd.read_csv(D / "assumption_register.csv")
cross = pd.read_csv(D / "taxonomy_crosswalk.csv", dtype={"code": str, "parent_code": str})
fp = pd.read_csv(D / "fraction_parameters_L1_L2_L3.csv", dtype={"code": str, "parent_code": str})
bm = pd.read_csv(D / "composition_benchmarks_conditional.csv", dtype={"code": str, "parent_code": str})
c1 = pd.read_csv(D / "composition_level1_2025.csv", dtype={"code": str, "level1_code": str})
c2 = pd.read_csv(D / "composition_level2_2025.csv", dtype={"code": str, "level1_code": str})
term = pd.read_csv(D / "composition_terminal_partition_2025.csv", dtype={"code": str})
t5 = pd.read_csv(D / "table5_qmanaged_2025.csv")
pkg_l3 = pd.read_csv(ROOT / "sources/02_taxonomy_and_allocations/input_kota_level3_detail.csv", encoding="utf-8-sig",
                     dtype={"level3_code": str, "parent_code": str})

# ---- 1. Blank versus zero -------------------------------------------------------------------
sys.path.insert(0, str(ROOT))
src_params = pd.read_csv(ROOT / "sources/04_fraction_literature/fraction_level2_level3_parameters.csv", dtype={"code": str})
n_blank_src = src_params.DOCf.isna().sum()
n_blank_out = fp[fp.level > 1].DOCf.isna().sum()
check("blank vs zero", "Blank DOCf in source stays blank in output (not coerced to 0)",
      n_blank_out >= n_blank_src and (fp.DOCf == 0).sum() == 0, f"source blanks {n_blank_src}, output blanks {n_blank_out}")
check("blank vs zero", "Rubber/leather DOCf blank in material tables; model value only in register scenario",
      t2.set_index("fraction").loc["rubber", "DOCf_source_value"] != t2.set_index("fraction").loc["rubber", "DOCf_source_value"]
      and fp[fp.ipcc_proxy_category == "rubber_leather"].DOCf.isna().all()
      and "docf_rubber" in set(reg.key), "C12 docf_rubber 0 (0-0.5)")
check("blank vs zero", "Benchmark blanks are not zero (e.g. 4.3, 9.3, 9.4 remain blank)",
      bm.set_index("code").loc[["4.3", "9.3", "9.4"], "equal_housing_mean_pct_total_wet"].isna().all())
check("blank vs zero", "Glass blank (not reported) distinguished from glass folded or reported",
      set(inp.glass_status) == {"not_reported_blank", "folded_into_lain_lain", "reported_separately"}
      and inp.loc[inp.city_id == "kota_padang", "glass_status"].iloc[0] == "not_reported_blank")

# Parser behaviour on a synthetic row: blank glass must not change shares; zero glass is a reported zero.
from mswpath import MSWModel, read_input, PW
M = MSWModel(D)
raw0 = read_input(D / "input_kota_2025_updated.csv")
row = raw0.iloc[[0]].copy()
row_blank, row_zero = row.copy(), row.copy()
row_blank[["dom_kaca", "nd_kaca"]] = np.nan
row_zero[["dom_kaca", "nd_kaca"]] = 0.0
hb, hz = M.harmonise(row_blank), M.harmonise(row_zero)
fr_cols = [c for c in hb.columns if c.startswith("s_")]
check("blank vs zero", "Parser: blank glass and reported-zero glass give the same shares (no imputation)",
      np.allclose(hb[fr_cols].to_numpy(), hz[fr_cols].to_numpy()))
try:
    with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as f:
        pd.read_csv(ROOT / "sources/input_kota_v2_original.csv").to_csv(f.name, index=False)
        read_input(Path(f.name))
    check("blank vs zero", "Parser rejects the old input file (no 2025 columns)", False)
except ValueError as e:
    check("blank vs zero", "Parser rejects the old input file (no 2025 columns)", True, str(e)[:90])

# ---- 2. Taxonomy keys -----------------------------------------------------------------------
for nm, df in (("crosswalk", cross), ("parameters", fp), ("benchmarks", bm)):
    check("taxonomy", f"(level, code) unique in {nm}", not df.duplicated(["level", "code"]).any(), f"{len(df)} rows")
check("taxonomy", "Counts: 10 Level I, 36 Level II, 56 Level III",
      list(cross.level.value_counts().sort_index()) == [10, 36, 56])
keys = set(zip(cross.level, cross.code))
orphans = [(l, c, p) for l, c, p in zip(cross.level, cross.code, cross.parent_code)
           if l > 1 and (l - 1, p) not in keys and p != "6.*"]
check("taxonomy", "Every Level II/III parent exists (6.* cross-parent documented)", not orphans, orphans)
check("taxonomy", "Cross-parent metal rows carry a crosswalk note",
      cross[cross.code.str.startswith("6.x")].crosswalk_note.str.len().gt(0).all())
check("taxonomy", "Same code at different levels never collides (keys 'L{level}:{code}' unique)", cross.key.is_unique)

# ---- 3. Mass balance ------------------------------------------------------------------------
mb = (inp.M_dom_tpd_2025 + inp.M_nd_tpd_2025 - inp.Q2025_tpd).abs()
check("mass balance", "M_dom_2025 + M_nd_2025 = Q2025 (all 21)", (mb < 1e-3).all(), f"max error {mb.max():.2e} t/d")
f = inp.tonnage_projection_factor
check("mass balance", "Each stream scaled by the same factor",
      ((inp.M_dom_tpd * f - inp.M_dom_tpd_2025).abs() < 1e-3).all()
      and ((inp.M_nd_tpd.fillna(0) * f - inp.M_nd_tpd_2025).abs() < 1e-3).all())
s1 = c1.groupby(["city_id", "stream"]).percent_wet_mass.sum()
check("mass balance", "Level I sums to 100% per stream (42 streams)", ((s1 - 100).abs() < 1e-6).all(), f"max |dev| {(s1-100).abs().max():.1e}")
m1 = c1.groupby(["city_id", "stream"]).agg(t=("tpd_2025", "sum"), m=("stream_mass_tpd_2025", "first"))
check("mass balance", "Level I tonnes sum to the 2025 stream mass", ((m1.t - m1.m).abs() < 1e-3).all())
p1 = c1.set_index(["city_id", "stream", "code"]).percent_wet_mass
s2 = c2.groupby(["city_id", "stream", "level1_code"]).percent_wet_mass.sum()
err2 = max(abs(v - p1.loc[(a, b, c)]) for (a, b, c), v in s2.items())
check("mass balance", "Level II sums to its Level I parent", err2 < 1e-6, f"max |dev| {err2:.1e} pp")
st = term.groupby(["city_id", "stream"]).percent_wet_mass.sum()
check("mass balance", "Terminal partition sums to 100% per stream (no parent lost)", ((st - 100).abs() < 1e-5).all(),
      f"{term.code.nunique()} leaves; max |dev| {(st-100).abs().max():.1e}")

# ---- 4. Parent-child composition ------------------------------------------------------------
p2 = c2.set_index(["city_id", "stream", "code"]).percent_wet_mass
g3 = pkg_l3.groupby(["city_id", "stream", "parent_code"]).percent_wet_mass.sum()
errs = []
for (a, b, pc), v in g3.items():
    par = p2.loc[(a, b, "6.1")] + p2.loc[(a, b, "6.2")] if pc == "6.*" else p2.loc[(a, b, pc)]
    errs.append(abs(v - par))
check("parent-child", "Catalogued Level III children sum to their Level II parent (6.* to 6.1 + 6.2)",
      max(errs) < 1e-6, f"max |dev| {max(errs):.1e} pp")
cov = pkg_l3.groupby(["city_id", "stream"]).percent_wet_mass.sum()
check("parent-child", "Level III detail file is a partial coverage (not forced to 100%)", (cov < 100).all(),
      f"coverage {cov.min():.1f}-{cov.max():.1f}% of stream")
cs = bm[bm.sibling_set_complete].groupby(["level", "parent_code"]).conditional_share_within_parent.sum()
check("parent-child", "Conditional benchmark shares sum to 1 for complete sibling sets", ((cs - 1).abs() < 1e-3).all(),
      f"{len(cs)} complete sets")
check("parent-child", "No conditional share computed for incomplete sibling sets",
      bm[~bm.sibling_set_complete].conditional_share_within_parent.isna().all(),
      f"incomplete parents: {sorted(bm[~bm.sibling_set_complete & bm.equal_housing_mean_pct_total_wet.notna()].parent_code.unique())}")
check("parent-child", "Benchmarks labelled as foreign (Denmark, residual household waste)",
      bm.transfer_status.str.contains("not an Indonesian").all())

# ---- 5. Single projection to 2025 ------------------------------------------------------------
pred = inp.tonnage_source_value_tpd * (1 + inp.growth_rate_used) ** inp.projection_exponent_years
check("projection", "Q2025 = Qt (1+g)^(2025-t), applied once", ((pred - inp.Q2025_tpd).abs() < 1e-3).all())
already = inp.tonnage_already_2025.astype(str).str.lower() == "true"
check("projection", "Exponent is 0 where the tonnage is already 2025", (inp.loc[already, "projection_exponent_years"] == 0).all(),
      f"{already.sum()} rows already 2025")
ov = inp.tonnage_year_status == "overview_index_year_unverified"
check("projection", "Overview totals: year left blank (unverified), not labelled as 2025 observation",
      inp.loc[ov, "tonnage_source_year"].isna().all() and inp.loc[ov, "data_quality_flags"].str.contains("T1").all(),
      f"{ov.sum()} overview rows")
check("projection", "Projected alternative reported for every overview row with an older composition year",
      ((inp.loc[ov, "Q2025_alt_tpd"] >= inp.loc[ov, "Q2025_tpd"]) | (inp.loc[ov, "year_base"] == 2025)).all())
check("projection", "Composition carries no growth factor (shares only, treatment declared)",
      inp.composition_2025_treatment.str.contains("no trend").all())
check("projection", "Composition sampling year, tonnage source year and baseline year stored separately",
      {"composition_sampling_year", "tonnage_source_year", "baseline_year"} <= set(inp.columns))

# ---- 6. Qmanaged <= Qtotal ------------------------------------------------------------------
for col in ("Qmanaged_M1_tpd", "Qmanaged_M2_tpd"):
    v = t5[col].dropna(); q = t5.loc[v.index, "Q2025_tpd"]
    check("Qmanaged", f"{col} <= Q2025", (v <= q + 1e-9).all(), f"{len(v)} locations with a value")
check("Qmanaged", "Serang baseline uses observed 7.45%, the 14% target only as scenario",
      t5.set_index("city_id").loc["kab_serang", "series_M2_local_first"] == 0.0745
      and t5.set_index("city_id").loc["kab_serang", "target_scenario_share"] == 0.14)
check("Qmanaged", "Padang 93.71% labelled provisional; model series uses TPA-only lower bound",
      "provisional" in t5.set_index("city_id").loc["kota_padang", "local_status"]
      and abs(t5.set_index("city_id").loc["kota_padang", "series_M2_local_first"] - 465.08 / 647.57) < 1e-4)
check("Qmanaged", "Status-index series and local series kept in separate columns (no silent mixing)",
      t5.series_M1_status_index.isna().sum() == 2)

# ---- 7. Wet/dry basis -----------------------------------------------------------------------
ipcc_wet = {"food": 0.15, "garden": 0.20, "paper": 0.40, "wood": 0.43, "textile": 0.24, "rubber": 0.39}
x = t2.set_index("fraction")
dev = max(abs(x.loc[k, "DOC_wet_ipcc_check"] - v) for k, v in ipcc_wet.items())
check("wet/dry", "DOC_dry x (1 - IPCC moisture) reproduces IPCC wet DOC (single moisture correction)", dev < 0.011,
      f"max |dev| {dev:.3f}")
d = fp.dropna(subset=["DOC_dry_fraction", "moisture_wet_fraction"])
check("wet/dry", "L2/L3 DOC_wet_derived = DOC_dry x (1 - moisture)",
      np.allclose(d.DOC_wet_derived, d.DOC_dry_fraction * (1 - d.moisture_wet_fraction), atol=1e-4))
check("wet/dry", "All model LHV values declared on a dry basis; plastic uses the LHV (not HHV) median",
      x.LHV_basis.str.contains("dry").all() and x.loc["plastic", "LHV_dry"] == 30.5)
check("wet/dry", "Landfill DOCf and AD methane yield are separate parameters (no BMP as DOCf)",
      "y_ch4" in set(reg.key) and "BMP" in reg.set_index("key").loc["y_ch4", "meaning"])
check("wet/dry", "Grass, wood and soil separated: humid soil not given grass DOC; woody material DOCf 0.10",
      fp.set_index("code").loc["2.2.1", "DOC_dry_fraction"] == 0 and fp.set_index("code").loc["2.2.3", "DOCf"] == 0.1
      and fp.set_index("code").loc["2.2.4", "DOCf"] == 0.1)
check("wet/dry", "Composite products have no whole-product proxy (4.3, 4.4.1, 4.4.2, 5.3.2, WEEE, HHW, batteries)",
      fp.set_index("code").loc[["4.3", "4.4.1", "4.4.2", "5.3.2", "10.1", "10.2", "10.3"], "DOC_dry_fraction"].isna().all())
nf = inp[["dom_normalisation_factor", "nd_normalisation_factor"]]
check("wet/dry", "Normalisation factor recorded for every stream (Pemalang 100.10% -> factor 0.999)",
      nf.notna().all().all() and abs(inp.set_index("city_id").loc["kab_pemalang", "dom_normalisation_factor"] - 0.999001) < 1e-5)

# ---- 8. Provenance --------------------------------------------------------------------------
check("provenance", "Every input row has source document, pages and quality flags",
      inp.source_pdf.notna().all() and inp.source_pages.notna().all() and inp.data_quality_flags.str.len().gt(0).all())
check("provenance", "Every Table 2 parameter has a status column filled",
      t2[["moisture_ipcc_status", "moisture_as_received_status", "DOC_status", "DOCf_status", "carbon_status",
          "LHV_status", "tau_status"]].notna().all().all())
check("provenance", "Every register row has source, status (V/L/A) and evidence type",
      reg.source.notna().all() and reg.status.isin(list("VLA")).all() and reg.evidence_type.notna().all())
vals = fp[fp[["DOC_dry_fraction", "moisture_wet_fraction", "carbon_dry_fraction"]].notna().any(axis=1)]
check("provenance", "Every non-blank material parameter has a status, a source URL and a locator",
      vals.parameter_status.notna().all() and vals.carbon_source.notna().all() and vals.source_locator.notna().all(),
      f"{len(vals)} rows with values")
check("provenance", "Parent-category proxies keep their proxy status",
      fp[(fp.level > 1) & fp.ipcc_proxy_category.notna()].parameter_status.str.contains("proxy|missing").all())
check("provenance", "Benchmark values carry source table and geography",
      bm.dropna(subset=["equal_housing_mean_pct_total_wet"])[["source_table", "geography"]].notna().all().all())
check("provenance", "Heuristic Dirichlet concentrations declared as heuristic, not calibrated",
      reg.set_index("key").loc[["alpha_hi", "alpha_lo"], "evidence_type"].eq("heuristic").all())

# ---- 9. CED and land use (v3.1) ------------------------------------------------------------
lcia = pd.read_csv(D / "lcia_factors.csv")
check("CED/LU", "Every CED/LU factor has unit, source, status and evidence type",
      lcia[["unit", "source", "status", "evidence_type"]].notna().all().all() and lcia.status.isin(list("VLA")).all())
check("CED/LU", "Ranges are ordered (low <= central <= high)", ((lcia.low <= lcia.central) & (lcia.central <= lcia.high)).all())
M.load_cities(raw0)
det, _, _ = M.run_central({"market": {}})
m = det[det.baseline == "OD"]
lu = m.pivot(index="city", columns="pathway", values="LU")
check("CED/LU", "Land take is non-negative for every option and location", (lu >= 0).all().all())
check("CED/LU", "WtE takes less land than landfilling the same tonne (ash only)", (lu.S1 < lu.SL).all())
ced = m.pivot(index="city", columns="pathway", values="CED")
check("CED/LU", "WtE and RDF are net fossil-energy savers; landfill is a small consumer",
      (ced.S1 < 0).all() and (ced.S2 < 0).all() and (ced.SL > 0).all())
lfac = lcia.set_index("key").central
lu_hand = 1 / (lfac.rho_sl * lfac.h_sl) * lfac.f_gross_sl
check("CED/LU", "Landfill land take reproduces 1/(rho H) x gross factor", np.allclose(lu.SL, lu_hand),
      f"{lu_hand:.4f} m2/t")
gc = det.set_index(["city", "baseline", "pathway"])[["G", "C"]]
M2 = MSWModel(D); M2.LCIA = None; M2.load_cities(raw0)
det2, _, _ = M2.run_central({"market": {}})
check("CED/LU", "Adding CED/LU leaves G and C unchanged", np.allclose(gc.to_numpy(), det2.set_index(["city", "baseline", "pathway"])[["G", "C"]].to_numpy()))

# ---- 10. User template round trip (v3.1) ---------------------------------------------------
from mswpath.inputs import from_template, validate_template, example_from_v3
ex = example_from_v3(raw0, ["kab_pati", "kota_magelang"])
raw_t, _ = from_template(ex, M)
M.load_cities(raw_t); dt, _, _ = M.run_central({"market": {}})
M.load_cities(raw0); dr, _, _ = M.run_central({"market": {}})
key = ["city", "baseline", "pathway"]
a = dr[dr.city.isin(["Kab. Pati", "Kota Magelang"])].set_index(key)[["G", "C", "CED", "LU"]]
b = dt.set_index(key)[["G", "C", "CED", "LU"]].loc[a.index]
check("template", "Template round trip reproduces the v3 results (Pati, Kota Magelang)", np.allclose(a, b, atol=1e-3),
      f"max |diff| {np.abs(a.to_numpy() - b.to_numpy()).max():.1e}")
bad = ex.copy(); bad.loc[0, "pct_sisa_makanan"] = 300; bad.loc[1, "grid_region"] = "Java"
errs, _ = validate_template(bad, M)
check("template", "Validator rejects a composition far from 100% and an unknown grid", len(errs) >= 2, f"{len(errs)} errors")
blank = ex.copy(); blank.loc[0, "pct_kaca"] = np.nan
rb, _ = from_template(blank, M)
check("template", "Blank glass in the template is 'not reported', not zero", rb.loc[0, "glass_status"] == "not_reported_blank")
grow = ex.copy(); grow["tonnage_year"] = 2022; grow["tonnage_is_2025"] = "no"; grow["growth_rate"] = 0.02
rg, _ = from_template(grow, M)
check("template", "Template tonnage projected once: Q2025 = Qt (1.02)^3", np.allclose(rg.Q2025_tpd, ex.tonnage_tpd * 1.02 ** 3))

# ---- 11. v3.2: secondary-data corrections, realism parameters, decision outputs ------------------
ver = pd.read_csv(D / "secondary_data_verification.csv")
check("v3.2 data", "Every secondary-data item has a verification result, evidence and method",
      ver[["result", "evidence", "method"]].notna().all().all(), f"{len(ver)} items")
rk = reg.set_index("key")
check("v3.2 data", "Corrected register values are the ones used (o_rdf 18.4, p_rdf 1.15, cap low 0.20, c_sl low 9)",
      np.isclose(rk.central.o_rdf, 18.4) and np.isclose(rk.central.p_rdf, 1.15) and np.isclose(rk.low["cap"], 0.20)
      and np.isclose(rk.low.c_sl, 9))
gv = pd.read_csv(D / "grid_emission_factors.csv").set_index("grid_region").ef_kgCO2_per_kWh
check("v3.2 data", "Grid factors from ESDM 2018 (Jamali 0.877, Sumatera 0.832, Mahakam 1.128)",
      np.allclose(gv.reindex(["Jamali", "Sumatera", "Mahakam"]), [0.877, 0.832, 1.128]), gv.round(3).to_dict())
check("v3.2 data", "Status V kept only for values confirmed in the 2026-10 check (y_ch4, K_wte, o_wte, p_el)",
      set(reg[reg.status == "V"].key) <= {"y_ch4", "K_wte", "o_wte", "p_el"}, ", ".join(reg[reg.status == "V"].key))
check("v3.2 data", "Every register row records its v3.1 status and the 2026-10 verification outcome",
      reg[["status_v3_1", "verification_2026_10"]].notna().all().all())
sp = pd.read_csv(D / "scenario_parameters.csv").set_index("key")
check("v3.2 data", "Scenario/realism parameters have source, status and use (main case or scenario)",
      sp[["source", "status", "use"]].notna().all().all() and sp.use.isin(["main case", "scenario", "stress"]).all())
check("v3.2 data", "Main case: no glass imputation, no pseudocount, RDF NCV multiplier 1",
      sp.central.glass_imp == 0 and sp.central.pseudo == 0 and sp.central.rdf_ncv_k == 1)

M.load_cities(raw0)
d_main, _, _ = M.run_central({"market": {}})
d_opt, _, _ = M.run_central({"market": dict(fix={"y_pen_mech": 1.0, "pre_ofmsw": 0.0})})
km_ = ["city", "baseline", "pathway"]
a_, b_ = d_main.set_index(km_)[["G", "C"]], d_opt.set_index(km_)[["G", "C"]].loc[d_main.set_index(km_).index]
oth = a_.index.get_level_values("pathway").isin(["SL", "S1", "S2", "S4"])
check("v3.2 model", "AD-feed penalty changes only S3 and S5 (SL, S1, S2, S4 identical)",
      np.allclose(a_[oth], b_[oth]) and (np.abs(a_[~oth] - b_[~oth]).to_numpy().max() > 1))
check("v3.2 model", "AD-feed penalty makes S3 and S5 dearer and less climate-beneficial",
      (a_[~oth].C >= b_[~oth].C - 1e-9).all() and (a_[~oth].G >= b_[~oth].G - 1e-9).all())
i_rep = [k for k, v in enumerate(M.H.glass) if v != "not_reported_blank"][0]
i_nr = [k for k, v in enumerate(M.H.glass) if v == "not_reported_blank"]
c_, P_, s_, Q_, _ = M.setup(i_rep, 1, True, glass_impute=True)
check("v3.2 model", "Glass imputation leaves locations that report glass unchanged", np.allclose(s_[0], M.SB[i_rep]))
if i_nr:
    c_, P_, s_, Q_, _ = M.setup(i_nr[0], 1, True, glass_impute=True)
    check("v3.2 model", "Glass imputation adds glass only where glass is not reported, shares still sum to 1",
          s_[0, M.FR.index("glass")] > M.SB[i_nr[0]][M.FR.index("glass")] and abs(s_[0].sum() - 1) < 1e-12,
          f"{len(i_nr)} locations without glass")
c_, P_, s_, Q_, kw_ = M.setup(0, 1, True); X0 = M.model(s_, P_, c_, Q_)["X"]
c_, P_, s_, Q_, kw_ = M.setup(0, 1, True, rdf_stress=True); X1 = M.model(s_, P_, c_, Q_)["X"]
check("v3.2 model", "RDF NCV stress lowers delivered RDF energy by the multiplier (0.78)",
      np.isclose(X1["rdf_ncv_S2"][0] / X0["rdf_ncv_S2"][0], sp.central.rdf_ncv_k_scen),
      f"{X0['rdf_ncv_S2'][0]:.1f} -> {X1['rdf_ncv_S2'][0]:.1f} MJ/kg")
mc_, smp_ = M.run_mc("market", N=1000, keep=True)
sums = mc_.groupby("city")[[f"p_best_at_{pc}" for pc in (0, 2, 25, 50, 100)]].sum()
check("v3.2 decision", "P(best | carbon value) sums to 1 over options in every location", np.allclose(sums, 1.0),
      f"range {sums.min().min():.3f} to {sums.max().max():.3f}")
check("v3.2 decision", "Monte Carlo standard error of P(best) is at most 0.5/sqrt(N)",
      (mc_[[c for c in mc_.columns if c.endswith("_se")]] <= 0.5 / np.sqrt(1000) + 1e-12).all().all())
from mswpath.voi import voi_location, voi_groups
P0, s0, R0 = smp_[M.H.index[0]]
g0, evpi0 = voi_groups(P0, s0, R0, M.FR, pc=100)
v0, meta0 = voi_location(P0, s0, R0, M.FR, pc=100)
check("v3.2 decision", "EVPI >= 0 and every (net) EVPPI <= EVPI",
      evpi0 >= 0 and (g0.evppi_net <= evpi0 + 1e-6).all() and (v0.evppi_net <= evpi0 + 1e-6).all(), f"EVPI {evpi0:.2f} USD/t")
from mswpath.thresholds import break_even
be = break_even(M, 0, "S2", "p_rdf", 0.0, 6.0, 50)
if isinstance(be["break_even"], float):
    c_, P_, s_, Q_, kw_ = M.setup(0, 1, True); P_["p_rdf"] = np.array([be["break_even"]]); Rb = M.model(s_, P_, c_, Q_)
    gap = (Rb["C"] + 50 * Rb["G"] / 1e3)[0]; gap = gap[PW.index("S2")] - gap[PW.index("SL")]
    check("v3.2 decision", "Break-even value makes the option's carbon-inclusive cost equal SL's", abs(gap) < 0.05,
          f"S2 p_rdf* = {be['break_even']:.3f} USD/GJ, gap {gap:.3f} USD/t")
sys.path.insert(0, str(ROOT / "docs"))
try:
    import padang_calc; padang_calc.calc(D); ok_ = True; msg = ""
except AssertionError as e:
    ok_, msg = False, str(e)
check("v3.2 decision", "Explicit hand recalculation (Kota Padang) reproduces every model output", ok_, msg)
txt = "".join((ROOT / f).read_text() for f in ("mswpath/report.py", "mswpath/core.py", "make_colab_notebook.py", "run_discovery.py"))
check("v3.2 wording", "No 'statistical tie' and no 'social cost' used as the decision metric in code or tool",
      "statistical tie" not in txt.lower() and "lowest social cost" not in txt.lower() and "Social-cost" not in txt)

rep = pd.DataFrame(R)
rep.to_csv(OUT / "validation_report.csv", index=False)
with open(OUT / "validation_report.md", "w") as fh:
    fh.write(f"# Validation report (v3.2)\n\n{(rep.result == 'PASS').sum()} of {len(rep)} checks passed.\n\n")
    fh.write("| Group | Check | Result | Detail |\n|---|---|---|---|\n")
    for r in rep.itertuples():
        fh.write(f"| {r.group} | {r.check} | {r.result} | {r.detail} |\n")
print(rep.to_string())
print(f"\n{(rep.result == 'PASS').sum()} of {len(rep)} checks passed")
sys.exit(0 if (rep.result == "PASS").all() else 1)

"""
Build the v3 database for the MSW pathway model from the update package in `sources/`.

Every file in `data/` is written by this script, so each number can be traced back to a
source file and a rule. Run:  python build_database_v3.py

Conventions used in every output table
- A blank cell means "unknown or not applicable". It is never read as zero.
- Every value carries a status: measurement, literature default, proxy, assumption, scenario or gap.
- Taxonomy rows are keyed by (level, code). Codes are strings, so "6.10" and "6.1" never collide.
"""
from pathlib import Path
import numpy as np
import pandas as pd

ROOT = Path(__file__).resolve().parent
SRC, DATA = ROOT / "sources", ROOT / "data"
DATA.mkdir(exist_ok=True)
BASE_YEAR = 2025

IPCC2006 = "https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/5_Volume5/V5_2_Ch2_Waste_Data.pdf"
IPCC2019 = "https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/5_Volume5/19R_V5_3_Ch03_SWDS.pdf"
EDJ2015 = "https://doi.org/10.1016/j.wasman.2014.11.009"
EDJ2015_MS = "https://backend.orbit.dtu.dk/ws/files/119653490/Manuscript.pdf"
GOTZE2016 = "https://doi.org/10.1016/j.wasman.2016.01.008"


def read(rel, **kw):
    return pd.read_csv(SRC / rel, encoding="utf-8-sig", **kw)


def write(df, name):
    df.to_csv(DATA / name, index=False)
    print(f"  wrote data/{name:48s} {df.shape[0]:5d} rows x {df.shape[1]:3d} cols")


# ---------------------------------------------------------------------------------------------
# 1. City input with an auditable 2025 tonnage baseline
# ---------------------------------------------------------------------------------------------
RIPS_CATS = ["sisa_makanan", "daun_kering", "kertas_kardus", "kayu", "plastik", "logam", "kain",
             "karet_kulit", "sterofoam", "b3", "elektronik", "lain_lain", "kaca"]

# Approximate district-capital coordinates carried over from v2 (status A, used only for haul distance).
CENTROID = {"kota_semarang": (-6.99, 110.42), "kab_brebes": (-6.87, 109.04), "kab_nganjuk": (-7.60, 111.90),
            "kab_pati": (-6.75, 111.04), "kab_magelang": (-7.59, 110.22), "kab_pandeglang": (-6.31, 106.10),
            "kab_pemalang": (-6.89, 109.38), "kab_temanggung": (-7.32, 110.17), "kab_pekalongan": (-7.03, 109.59),
            "kab_sragen": (-7.43, 111.02), "kab_kutai_kartanegara": (-0.42, 116.98), "kab_kendal": (-6.92, 110.20),
            "kab_wonogiri": (-7.81, 110.92), "kota_pekalongan": (-6.89, 109.67), "kota_magelang": (-7.47, 110.22)}
GRID_OF = {"Sumatera Barat": "Sumatera", "Kalimantan Timur": "Mahakam"}   # every other province here is on Jamali

# Fallback growth rate: median of the six RIPS-specific rates (analyst assumption, register key g_default).
G_FALLBACK = 0.0173


def build_input():
    pkg = read("03_baseline_2025/input_kota_2025_claude_ready.csv")
    orig = read("input_kota_v2_original.csv")
    qm = read("03_baseline_2025/qmanaged_2025_source_audit.csv").set_index("city_id")
    comp_audit = read("03_baseline_2025/city_composition_source_audit.csv").set_index("city_id")
    assert list(orig.city_id) == list(pkg.city_id)

    out = orig.copy()                                         # the 54 original columns, unchanged
    rows = []
    for _, r in out.iterrows():
        cid = r.city_id
        md = float(r.M_dom_tpd)
        mn_blank = pd.isna(r.M_nd_tpd)
        mn = 0.0 if mn_blank else float(r.M_nd_tpd)
        qt = md + mn
        pages = str(r.source_pages)
        overview = "overview index for total" in pages
        if overview:
            # The total comes from the RIPS overview index. Its reference year is not shown on the
            # rows we hold, so the year is unverified. Central rule: the value is used as reported
            # (no projection). Sensitivity: the total dates from the composition year and is projected.
            t_status = "overview_index_year_unverified"
            t_year = np.nan
            already = True
        elif cid == "kab_pemalang":
            t_status = "RIPS_projection_for_2025"          # population projection x surveyed 0.31 kg/cap/d
            t_year = int(r.year_base)
            already = int(r.year_base) == BASE_YEAR
        else:
            t_status = "stated_in_source_table"
            t_year = int(r.year_base)
            already = int(r.year_base) == BASE_YEAR

        if pd.notna(r.growth):
            g, g_basis = float(r.growth), "RIPS-specific growth rate"
        else:
            g, g_basis = G_FALLBACK, "fallback: median of six RIPS rates (assumption, range 0.0065-0.0254)"

        if already:
            expo = 0
        else:
            expo = BASE_YEAR - int(t_year)
        factor = (1 + g) ** expo
        # Sensitivity for overview rows: as if the total dated from the composition year.
        expo_alt = BASE_YEAR - int(r.year_base)
        q_alt = qt * (1 + g) ** expo_alt if overview else qt * factor

        # Composition closure, per stream, on the source percentages (blank cells are not summed as zero:
        # they are absent categories; glass blank means "not reported").
        def closure(pre):
            vals = [r.get(f"{pre}_{c}") for c in RIPS_CATS]
            return float(np.nansum([np.nan if pd.isna(v) else float(v) for v in vals]))
        sd, sn = closure("dom"), closure("nd")

        if pd.isna(r.dom_kaca) and pd.isna(r.nd_kaca):
            glass = "folded_into_lain_lain" if bool(r.glass_folded_into_inert) else "not_reported_blank"
        else:
            glass = "reported_separately"

        flags = []
        if overview:
            flags.append("T1 tonnage year unverified (overview index); used as reported; see Q2025_alt")
        if t_status == "RIPS_projection_for_2025":
            flags.append("T2 2025 tonnage is a RIPS projection (population x per-capita rate), not a weighbridge measurement")
        if pd.isna(r.growth):
            flags.append("T3 growth rate is the fallback median (assumption)")
        if abs(sd - 100) > 0.05 or abs(sn - 100) > 0.05:
            flags.append("C1 source percentages do not close to 100 within 0.05; normalised")
        if r.composition_mode == "combined":
            flags.append("C2 one composition applied to both streams")
        if glass == "folded_into_lain_lain":
            flags.append("C3 glass folded into lain_lain")
        if glass == "not_reported_blank":
            flags.append("C4 glass not reported (blank, not zero)")
        if bool(r.organik_lumped_to_food):
            flags.append("C5 organic aggregate lumped to food (rule L)")
        if mn == 0 and r.composition_mode == "separate":
            flags.append("C6 non-domestic composition reported but non-domestic tonnage is zero")
        if int(r.year_base) < 2020:
            flags.append("C7 composition sampled before 2020")
        flags.append("C0 composition is the RIPS sampling-year distribution used as a 2025 proxy, not a 2025 measurement")

        # Managed share: keep the indicator families apart (Table 5).
        a = qm.loc[cid]
        si = a.status_index_share if pd.notna(a.status_index_share) else np.nan
        if pd.notna(si):
            si_status = ("legacy RIPS status-index value (original 14)" if pd.notna(r.managed_share)
                         else "candidate RIPS status-index value added in 2025 package")
        else:
            si_status = "not available in the RIPS status index"
        local, local_def, local_status, local_src = np.nan, "", "", ""
        local_lo = np.nan
        if cid == "kota_padang":
            local = 0.9371
            local_lo = round(465.08 / 647.57, 4)
            local_def = ("(141.76 t/d recovery + 465.08 t/d to TPA) / 647.57 t/d RIPS 2023 total; the 141.76 t/d may be a "
                         "recovery potential and the feasibility-study year may differ from the RIPS denominator")
            local_status = "derived, provisional (not an observed managed share)"
            local_src = a.city_specific_source
        elif cid == "kab_serang":
            local, local_def = 0.0745, "observed existing managed share; 14% is a 2025 service target"
            local_status, local_src = "local observed (existing condition)", a.city_specific_source
        elif pd.notna(a.city_specific_share):
            local = float(a.city_specific_share)
            local_def = a.definition_note
            local_status = "local indicator with a different definition from the status index"
            local_src = a.city_specific_source
        target = 0.14 if cid == "kab_serang" else np.nan

        lat, lon = (r.lat, r.lon) if pd.notna(r.lat) else CENTROID[cid]
        rows.append(dict(
            composition_sampling_year=int(comp_audit.loc[cid, "composition_sample_or_base_year"]),
            composition_source_level=comp_audit.loc[cid, "composition_source_level"],
            composition_basis="wet mass percentage (RIPS)",
            composition_2025_treatment="RIPS sampling-year composition held constant as 2025 proxy; no trend applied",
            dom_source_sum_pct=round(sd, 4), nd_source_sum_pct=round(sn, 4),
            dom_normalisation_factor=round(100 / sd, 6) if sd > 0 else np.nan,
            nd_normalisation_factor=round(100 / sn, 6) if sn > 0 else np.nan,
            glass_status=glass,
            M_nd_tpd_source_blank=mn_blank,
            tonnage_source_value_tpd=round(qt, 5),
            tonnage_source_year=t_year,
            tonnage_year_status=t_status,
            tonnage_already_2025=already,
            baseline_year=BASE_YEAR,
            growth_rate_used=g, growth_rate_basis=g_basis,
            projection_exponent_years=expo,
            tonnage_projection_factor=round(factor, 6),
            M_dom_tpd_2025=round(md * factor, 4), M_nd_tpd_2025=round(mn * factor, 4),
            Q2025_tpd=round(qt * factor, 4),
            Q2025_alt_tpd=round(q_alt, 4),
            Q2025_alt_rule=("overview total projected from the composition year (sensitivity)" if overview
                            else "same as central"),
            Q2025_tpd_package=pkg.loc[pkg.city_id == cid, "Q2025_tpd"].iloc[0],
            managed_share_status_index=si,
            managed_share_status_index_status=si_status,
            managed_share_local=local,
            managed_share_local_lower=local_lo,
            managed_share_local_definition=local_def,
            managed_share_local_status=local_status,
            managed_share_local_source=local_src,
            managed_share_target_scenario=target,
            Qmanaged_2025_status_index_tpd=round(si * qt * factor, 4) if pd.notna(si) else np.nan,
            Qmanaged_2025_local_tpd=round(local * qt * factor, 4) if pd.notna(local) else np.nan,
            Qmanaged_basis="scenario: share held constant and applied to Q2025 (not a 2025 observation)",
            lat_used=lat, lon_used=lon,
            coord_status=("provisional city centroid from source CSV [A]" if pd.notna(r.lat)
                          else "provisional district-capital coordinate [A]"),
            grid_region=GRID_OF.get(r.province, "Jamali"),
            data_quality_flags="; ".join(flags),
        ))
    out = pd.concat([out, pd.DataFrame(rows)], axis=1)
    write(out, "input_kota_2025_updated.csv")
    return out


# ---------------------------------------------------------------------------------------------
# 2. Table 5: managed waste, candidates and alternatives kept apart
# ---------------------------------------------------------------------------------------------
def build_table5(inp):
    rows = []
    for _, r in inp.iterrows():
        m1 = r.managed_share_status_index
        if r.city_id == "kota_padang":
            m2, m2_rule = r.managed_share_local_lower, "Padang lower bound: TPA delivery only (465.08 / 647.57)"
        elif pd.notna(r.managed_share_local):
            m2, m2_rule = r.managed_share_local, "local indicator"
        else:
            m2, m2_rule = m1, "status index (no local indicator held)"
        rows.append(dict(
            city_id=r.city_id, city_name=r.city_name, Q2025_tpd=r.Q2025_tpd,
            status_index_share=m1, status_index_status=r.managed_share_status_index_status,
            local_share=r.managed_share_local, local_share_lower=r.managed_share_local_lower,
            local_definition=r.managed_share_local_definition, local_status=r.managed_share_local_status,
            local_source=r.managed_share_local_source, target_scenario_share=r.managed_share_target_scenario,
            series_M1_status_index=m1, Qmanaged_M1_tpd=round(m1 * r.Q2025_tpd, 3) if pd.notna(m1) else np.nan,
            series_M2_local_first=m2, series_M2_rule=m2_rule,
            Qmanaged_M2_tpd=round(m2 * r.Q2025_tpd, 3) if pd.notna(m2) else np.nan,
            definition_difference_pp=(round(100 * (r.managed_share_local - m1), 2)
                                      if pd.notna(m1) and pd.notna(r.managed_share_local) else np.nan),
            share_basis="constant share applied to Q2025 (scenario, not an observation)",
        ))
    t5 = pd.DataFrame(rows)
    write(t5, "table5_qmanaged_2025.csv")
    return t5


# ---------------------------------------------------------------------------------------------
# 3. Table 2: properties of the ten model fractions (what the model reads)
# ---------------------------------------------------------------------------------------------
def build_table2():
    ip = read("03_baseline_2025/table2_fraction_properties_IPCC_2019.csv").set_index("fraction")
    key = {"food": "Food", "garden": "Garden", "paper": "Paper", "wood": "Wood", "textile": "Textile",
           "rubber": "Rubber_leather", "plastic": "Plastic", "metal": "Metal", "glass": "Glass", "other": "Other"}
    # As-received moisture (v2, analyst assumption calibrated to Indonesian bulk moisture at transfer points).
    asrec = {"food": (0.75, 0.65, 0.82), "garden": (0.55, 0.40, 0.65), "paper": (0.35, 0.15, 0.50),
             "wood": (0.25, 0.15, 0.40), "textile": (0.35, 0.15, 0.50), "rubber": (0.15, 0.05, 0.25),
             "plastic": (0.22, 0.05, 0.35), "metal": (0.05, 0.02, 0.08), "glass": (0.02, 0.01, 0.03),
             "other": (0.25, 0.10, 0.40)}
    # Legacy RDF transfer coefficients (v2). Paper, plastic, wood informed by Nasrullah et al. 2014 (C&I waste).
    tau_src = {"paper": "legacy; informed by Nasrullah et al. 2014 (commercial & industrial waste, Finland): basis differs from MSW",
               "plastic": "legacy; informed by Nasrullah et al. 2014 (commercial & industrial waste, Finland): basis differs from MSW",
               "wood": "legacy; informed by Nasrullah et al. 2014 (commercial & industrial waste, Finland): basis differs from MSW"}
    rows = []
    for f, k in key.items():
        p = ip.loc[k]
        docf_src = p.DOCf_fraction
        if f == "rubber":
            docf_m, docf_lo, docf_hi = 0.0, 0.0, 0.5
            docf_status = ("gap: no verified DOCf for the rubber/leather aggregate; scenario 0 (not degradable) "
                           "to 0.5 (IPCC 2006 generic default)")
        elif pd.isna(docf_src):
            docf_m = docf_lo = docf_hi = 0.0
            docf_status = "not applicable (no degradable organic carbon)"
        else:
            docf_m = docf_lo = docf_hi = float(docf_src)
            docf_status = "IPCC 2019 Refinement Table 3.0 default"
        if f == "plastic":
            h, h_basis = 30.5, "MJ/kg dry solids (TS)"
            h_status = "literature aggregate median (Gotze et al. 2016, Sect. 3.2.3); not resin-specific"
            h_src, apply_hk = GOTZE2016, False
        else:
            h, h_basis = float(p.dry_LHV_MJ_per_kg), "MJ/kg dry matter (hydrogen deducted)"
            h_status = ("legacy model assumption (v2 cites Tchobanoglous et al. 1993; not re-verified)"
                        if h > 0 else "zero: no combustible matter")
            h_src, apply_hk = "v2 model (legacy)", h > 0
        w, wlo, whi = asrec[f]
        rows.append(dict(
            fraction=f, ipcc_category=k,
            moisture_ipcc_default=float(p.moisture_wet_mass_fraction),
            moisture_ipcc_status="IPCC 2006 Table 2.4 default (waste as generated, before collection)",
            moisture_as_received=w, moisture_as_received_low=wlo, moisture_as_received_high=whi,
            moisture_as_received_status=("analyst assumption (v2), calibrated so bulk moisture matches Indonesian "
                                         "transfer-point measurements (Prabowo et al. 2019)"),
            DOC_dry=float(p.DOC_dry_mass_fraction), DOC_basis="dry-mass fraction",
            DOC_wet_ipcc_check=round(float(p.DOC_dry_mass_fraction) * (1 - float(p.moisture_wet_mass_fraction)), 4),
            DOC_status="IPCC 2006 Table 2.4 default",
            DOCf_source_value=docf_src, DOCf_model=docf_m, DOCf_model_low=docf_lo, DOCf_model_high=docf_hi,
            DOCf_status=docf_status,
            carbon_dry=float(p.total_carbon_dry_mass_fraction), fossil_carbon_share=float(p.fossil_carbon_share),
            carbon_status="IPCC 2006 Table 2.4 default",
            LHV_dry=h, LHV_basis=h_basis, LHV_status=h_status, LHV_source=h_src, apply_h_k=apply_hk,
            tau=float(p.RDF_transfer_fraction_tau),
            tau_status=tau_src.get(f, "legacy analyst assumption (v2)") if p.RDF_transfer_fraction_tau > 0
            else "zero: not sorted into RDF",
            source_ipcc_2006=IPCC2006 + " (Table 2.4)",
            source_ipcc_2019=IPCC2019 + " (Table 3.0)" if pd.notna(docf_src) else "",
            geography="global default (not Indonesia-specific)",
        ))
    t2 = pd.DataFrame(rows)
    write(t2, "table2_model_fraction_parameters.csv")
    return t2


# ---------------------------------------------------------------------------------------------
# 4. Assumption register, constants, grids, kilns (everything the model used to hard-code)
# ---------------------------------------------------------------------------------------------
REG = [  # id, key, central, low, high, unit, stage, meaning, source, status, evidence
 ("H11", "g_default", 0.0173, 0.0065, 0.0254, "1/yr", "H", "Tonnage growth rate where the RIPS gives none (median of six RIPS rates)", "RIPS of six locations", "A", "assumption"),
 ("H12", "gamma", 0.27, 0.04, 0.62, "-", "H", "Garden share of lumped 'organik' (median and range of 8 locations reporting both)", "input_kota; Edjabou et al. 2017", "A", "assumption"),
 ("H13", "yard", 1.0, 0.5, 1.0, "-", "H", "Share of 'kayu' treated as garden where kayu >= 10% and no leaf category exists", "RIPS notes", "A", "assumption"),
 ("H14", "alpha_hi", 80.0, 80.0, 80.0, "-", "H", "Dirichlet concentration, unflagged composition (heuristic, not statistically calibrated)", "Analyst heuristic", "A", "heuristic"),
 ("H15", "alpha_lo", 40.0, 40.0, 40.0, "-", "H", "Dirichlet concentration, flagged composition (heuristic, not statistically calibrated)", "Analyst heuristic", "A", "heuristic"),
 ("C8", "F", 0.50, 0.475, 0.525, "-", "C", "Methane fraction of landfill gas", "IPCC 2006 V5 Ch3", "L", "literature default"),
 ("C9", "doc_k", 1.0, 0.8, 1.2, "x", "C", "Multiplier on DOC x DOCf (default uncertainty +-20%)", "IPCC 2006 V5 Ch3 Table 3.5", "L", "literature default"),
 ("C10", "h_k", 0.90, 0.78, 1.02, "x", "C", "Calibration factor on legacy dry-matter LHV (not applied to the Gotze plastic value)", "Prabowo et al. 2019 (bulk HHV)", "V", "calibration"),
 ("C10b", "h_plastic_k", 1.0, 0.90, 1.10, "x", "C", "Uncertainty multiplier on plastic dry LHV (aggregate median, resin mix unknown)", "Analyst range around Gotze et al. 2016 median", "A", "assumption"),
 ("C11", "efg_k", 1.0, 0.7, 1.1, "x", "C", "Multiplier on grid emission factor (lower bound = decarbonising grid)", "ESDM factors via JCM 2022", "A", "assumption"),
 ("C12", "docf_rubber", 0.0, 0.0, 0.5, "-", "C", "DOCf of rubber/leather: gap scenario (0 = not degradable; 0.5 = IPCC 2006 generic default)", "IPCC 2006 V5 Ch3 (generic DOCf 0.5); no fraction-specific value", "A", "scenario (data gap)"),
 ("B5", "mcf_od", 0.80, 0.40, 0.80, "-", "B", "Methane correction factor, open dump", "IPCC 2006 V5 Ch3 Table 3.1", "L", "literature default"),
 ("B6", "mcf_sl", 1.00, 1.00, 1.00, "-", "B", "Methane correction factor, managed anaerobic landfill", "IPCC 2006 V5 Ch3 Table 3.1", "L", "literature default"),
 ("B7", "ox", 0.10, 0.00, 0.10, "-", "B", "Oxidation in cover soil, sanitary landfill", "IPCC 2006 V5 Ch3 Table 3.2", "L", "literature default"),
 ("B8", "cap", 0.50, 0.30, 0.80, "-", "B", "Lifetime landfill-gas collection efficiency, sanitary landfill", "Barlaz et al. 2009; Anshassi et al. 2022; Wei et al. 2024", "V", "literature range"),
 ("B9", "anc_od", 1.0, 0.0, 3.0, "kg CO2e/t", "B", "Diesel for dump operation", "Manfredi et al. 2009", "L", "literature"),
 ("B10", "anc_sl", 5.0, 2.0, 10.0, "kg CO2e/t", "B", "Diesel and materials for sanitary landfill operation", "Manfredi et al. 2009", "L", "literature"),
 ("W4", "eta_wte", 0.18, 0.14, 0.22, "-", "S1", "Net electrical efficiency of grate incinerator on low-LHV waste", "Astrup et al. 2015; Rand et al. 2000", "L", "literature"),
 ("W5", "n2o_wte", 0.05, 0.02, 0.08, "kg N2O/t", "S1", "N2O from continuous stoker incineration", "IPCC 2006 V5 Ch5 Table 5.6", "L", "literature default"),
 ("W6", "anc_wte", 10.0, 5.0, 30.0, "kg CO2e/t", "S1", "Auxiliary fuel and flue-gas reagents", "Astrup et al. 2009", "L", "literature"),
 ("W7", "ash", 0.25, 0.15, 0.30, "t/t", "S1", "Bottom ash and APC residue sent to landfill", "Rand et al. 2000", "L", "literature"),
 ("R6", "tau_k", 1.0, 0.85, 1.10, "x", "S2", "Multiplier on RDF transfer coefficients", "Nasrullah et al. 2014", "V", "assumption range"),
 ("R7", "omega", 0.20, 0.15, 0.25, "-", "S2", "Target moisture of RDF (off-taker limit 20-25%)", "GIZ ERiC-DKTI 2023", "V", "specification"),
 ("R8", "q_dry", 3.2, 2.6, 4.0, "GJ/t water", "S2", "Heat to evaporate water in the dryer (self-supplied by burning RDF)", "Engineering range for rotary dryers", "L", "engineering range"),
 ("R9", "e_rdf", 40.0, 20.0, 70.0, "kWh/t", "S2", "Electricity for shredding, screening, baling", "Engineering range for MT plants", "A", "assumption"),
 ("R10", "psi", 0.90, 0.80, 1.00, "-", "S2", "GJ coal displaced per GJ RDF in the kiln", "Silva et al. 2021; Khandelwal et al. 2019", "L", "literature"),
 ("R11", "ef_coal", 96.1, 94.6, 101.0, "kg CO2/GJ", "S2", "Combustion emission factor of displaced coal (sub-bituminous)", "IPCC 2006 V2 Ch2 Table 2.3", "L", "literature default"),
 ("R12", "ef_truck", 0.10, 0.06, 0.15, "kg CO2e/t-km", "S2", "Truck transport of RDF incl. empty return", "GLEC Framework", "L", "literature"),
 ("R13", "tort", 1.3, 1.2, 1.5, "x", "S2", "Road distance / straight-line distance to nearest kiln", "Analyst assumption", "A", "assumption"),
 ("R14", "anc_rdf", 3.0, 1.0, 6.0, "kg CO2e/t", "S2", "Diesel for loaders at the RDF plant", "Analyst assumption", "A", "assumption"),
 ("A6", "kappa", 0.70, 0.50, 0.90, "-", "S3", "Share of food waste captured into the digester", "Mayer et al. 2019", "L", "literature"),
 ("A7", "vs_ts", 0.87, 0.80, 0.92, "-", "S3", "Volatile solids / total solids of food waste", "Zhang et al. 2007", "L", "literature"),
 ("A8", "y_ch4", 0.36, 0.28, 0.44, "Nm3 CH4/kg VS", "S3", "Methane yield realised in a full-scale digester (BMP-type parameter, not landfill DOCf)", "Zhang et al. 2007", "L", "literature"),
 ("A9", "eta_chp", 0.36, 0.32, 0.40, "-", "S3", "Electrical efficiency of biogas engine", "Mayer et al. 2019", "L", "literature"),
 ("A10", "par_ad", 0.20, 0.10, 0.30, "-", "S3", "Own electricity use (pulping, mixing, dewatering)", "Mayer et al. 2019", "L", "literature"),
 ("A11", "fug_ad", 0.05, 0.01, 0.10, "-", "S3", "Fugitive methane from the biogas plant", "IPCC 2006 V5 Ch4", "L", "literature default"),
 ("A12", "anc_ad", 3.0, 1.0, 6.0, "kg CO2e/t", "S3", "Diesel for loaders at the AD plant (per t feed)", "Analyst assumption", "A", "assumption"),
 ("A13", "pre", 0.40, 0.25, 0.60, "-", "S3", "Front-end separation of food from mixed waste: share of RDF-line cost and electricity", "Analyst assumption", "A", "assumption"),
 ("A14", "dig", 0.20, 0.10, 0.35, "t/t feed", "S3", "Dewatered digestate sent to the residue landfill", "Analyst assumption", "A", "assumption"),
 ("P6", "r_phb", 2.30, 2.00, 2.80, "t CH4/t PHB", "S4", "Methane per tonne PHB (1.79 as carbon + 0.52 for process energy)", "Chidambarampadmavathy et al. 2017", "V", "literature"),
 ("P7", "ef_phb", 0.5, 0.0, 1.5, "kg CO2e/kg", "S4", "Nutrients and extraction chemicals", "Rostkowski et al. 2012", "L", "literature"),
 ("P8", "ef_pp", 1.9, 1.6, 3.4, "kg CO2e/kg", "S4", "Cradle-to-gate footprint of displaced polypropylene", "PlasticsEurope; Chidambarampadmavathy et al. 2017", "L", "literature"),
 ("P9", "sub_pp", 1.0, 0.7, 1.0, "kg/kg", "S4", "kg PP displaced per kg PHB", "Chidambarampadmavathy et al. 2017", "L", "literature"),
 ("T7", "r", 0.10, 0.08, 0.12, "1/yr", "T", "Real discount rate, same for all pathways", "Analyst assumption", "A", "assumption"),
 ("T8", "n", 20.0, 15.0, 30.0, "yr", "T", "Economic lifetime, same for all pathways", "Analyst assumption", "A", "assumption"),
 ("T9", "b", 0.70, 0.60, 0.85, "-", "T", "Capacity exponent for CAPEX scaling, same for all units", "Tsilemou & Panagiotakopoulos 2006", "L", "literature"),
 ("T10", "K_wte", 120e6, 100e6, 200e6, "USD @1000 t/d", "T", "WtE CAPEX at reference capacity", "Azis et al. 2021 (USD 102.2 M)", "A", "assumption"),
 ("T11", "o_wte", 27.0, 21.0, 37.0, "USD/t", "T", "WtE O&M", "Azis et al. 2021; GIZ 2017 in UNDP 2021", "V", "literature"),
 ("T12", "K_rdf", 12.3e6, 9e6, 16e6, "USD @300 t/d", "T", "RDF plant CAPEX at reference capacity (Rp 200 bn)", "GIZ ERiC-DKTI 2023", "V", "literature"),
 ("T13", "o_rdf", 12.0, 8.0, 21.0, "USD/t", "T", "RDF plant O&M", "GIZ ERiC-DKTI 2023; GIZ 2017 in UNDP 2021", "V", "literature"),
 ("T14", "K_ad", 7e6, 4e6, 14e6, "USD @100 t/d", "T", "AD plant CAPEX at reference feed capacity", "GIZ 2017 in UNDP 2021; Aleluia & Ferrao 2017", "L", "literature"),
 ("T15", "o_ad", 14.0, 10.6, 30.0, "USD/t feed", "T", "AD plant O&M", "GIZ 2017 in UNDP 2021", "V", "literature"),
 ("T16", "c_sl", 20.0, 12.0, 30.0, "USD/t @500 t/d", "T", "Full cost of sanitary landfill with gas collection and flare", "World Bank 2024", "V", "literature"),
 ("T17", "e_sl", 0.30, 0.20, 0.40, "-", "T", "Scale elasticity of landfill cost per tonne", "Tsilemou & Panagiotakopoulos 2006", "L", "literature"),
 ("T18", "c_od", 4.0, 2.0, 8.0, "USD/t", "T", "Cost of operating an open dump", "Analyst assumption", "A", "assumption"),
 ("T19", "c_phb", 6.8, 4.8, 9.6, "USD/kg @500 t/a", "T", "PHB production cost excl. methane feedstock", "Listewnik et al. in Chidambarampadmavathy 2017; Levett et al. 2016", "A", "assumption"),
 ("T20", "e_phb", 0.083, 0.05, 0.15, "-", "T", "Scale elasticity of PHB unit cost", "Levett et al. 2016", "A", "assumption"),
 ("T21", "p_el", 0.07, 0.05, 0.10, "USD/kWh", "T", "Market value of electricity (grid generation cost)", "Kepmen ESDM 169/2021 (BPP)", "L", "literature"),
 ("T22", "p_rdf", 1.6, 1.0, 2.6, "USD/GJ", "T", "Price of RDF at kiln gate", "Field data; GIZ 2023", "A", "assumption"),
 ("T23", "p_phb", 4.0, 2.5, 6.0, "USD/kg", "T", "Selling price of PHB", "Market reports; Levett et al. 2016", "L", "literature"),
 ("T24", "c_truck", 0.10, 0.05, 0.15, "USD/t-km", "T", "Cost of trucking RDF", "Analyst assumption", "A", "assumption"),
]

CONSTANTS = [  # key, value, unit, meaning, source
 ("GWP100_CH4", 27.0, "-", "GWP100 of non-fossil CH4", "IPCC AR6 WG1 Table 7.15"),
 ("GWP100_N2O", 273.0, "-", "GWP100 of N2O", "IPCC AR6 WG1 Table 7.15"),
 ("GWP20_CH4", 79.7, "-", "GWP20 of non-fossil CH4", "IPCC AR6 WG1 Table 7.15"),
 ("GWP20_N2O", 273.0, "-", "N2O kept at GWP100 value in the GWP20 case", "IPCC AR6 WG1 Table 7.15"),
 ("LAM", 2.443, "MJ/kg water", "Latent heat of vaporisation", "Physical constant"),
 ("RHO_CH4", 0.716, "kg/Nm3", "Density of methane", "Physical constant"),
 ("LHV_CH4", 35.9, "MJ/Nm3", "Lower heating value of methane", "Physical constant"),
 ("AVAIL", 0.85, "-", "Plant availability (nameplate = throughput / 0.85)", "Analyst assumption"),
 ("P_EL_PSEL", 0.20, "USD/kWh", "Perpres 109/2025 WtE tariff", "Perpres 109/2025"),
 ("PC_MAX", 100.0, "USD/t CO2e", "Highest carbon value sampled", "High-Level Commission on Carbon Prices 2017"),
 ("QREF_wte", 1000.0, "t/d", "Reference capacity WtE", "Azis et al. 2021"),
 ("QREF_rdf", 300.0, "t/d", "Reference capacity RDF", "GIZ ERiC-DKTI 2023"),
 ("QREF_ad", 100.0, "t/d", "Reference capacity AD", "GIZ 2017 in UNDP 2021"),
 ("QREF_sl", 500.0, "t/d", "Reference capacity sanitary landfill", "World Bank 2024"),
 ("GATE_lhv_min", 7.0, "MJ/kg", "G1 WtE minimum LHV as received", "Rand et al. 2000"),
 ("GATE_q_wte", 150.0, "t/d", "G2 WtE minimum supply", "Rand et al. 2000"),
 ("GATE_q_psel", 1000.0, "t/d", "G3 PSEL tariff eligibility", "Perpres 109/2025"),
 ("GATE_ncv_rdf", 12.56, "MJ/kg", "G4 RDF minimum NCV (3,000 kcal/kg)", "GIZ ERiC-DKTI 2023"),
 ("GATE_d_max", 300.0, "km", "G5 maximum road distance to kiln", "Analyst assumption"),
 ("GATE_phb_min", 500.0, "t PHB/yr", "G6 PHB minimum costed scale", "Chidambarampadmavathy et al. 2017"),
 ("SEED", 42, "-", "Random seed", "Model setting"),
 ("N_MC", 4000, "-", "Monte Carlo draws per location and scenario", "Model setting"),
 ("BASE_YEAR", 2025, "-", "Baseline year", "Study design"),
]

GRID = [("Jamali", 0.87, "ESDM factors via JCM/GEC 2022"), ("Sumatera", 0.94, "ESDM factors via JCM/GEC 2022"),
        ("Mahakam", 1.14, "ESDM factors via JCM/GEC 2022")]
KILN = [("Semen Padang (Indarung)", -0.96, 100.47), ("Indocement Citeureup", -6.49, 106.88), ("SBI Narogong", -6.48, 106.95),
        ("Semen Jawa Sukabumi", -6.98, 106.83), ("Cemindo Bayah", -6.94, 106.25), ("Indocement Palimanan", -6.70, 108.40),
        ("SBI Cilacap", -7.69, 109.02), ("Semen Bima Ajibarang", -7.42, 109.07), ("Semen Gresik Rembang", -6.86, 111.47),
        ("SIG Tuban", -6.87, 111.92), ("Imasco Puger Jember", -8.36, 113.47), ("Indocement Tarjun", -3.30, 116.07),
        ("Conch Tanjung", -2.10, 115.42)]


def build_register():
    reg = pd.DataFrame(REG, columns=["id", "key", "central", "low", "high", "unit", "stage", "meaning", "source",
                                     "status", "evidence_type"])
    write(reg, "assumption_register.csv")
    write(pd.DataFrame(CONSTANTS, columns=["key", "value", "unit", "meaning", "source"]), "model_constants.csv")
    write(pd.DataFrame(GRID, columns=["grid_region", "ef_kgCO2_per_kWh", "source"]), "grid_emission_factors.csv")
    k = pd.DataFrame(KILN, columns=["kiln", "lat", "lon"])
    k["coord_status"] = "approximate plant position [A]; verify before publication"
    write(k, "cement_kilns.csv")


# ---------------------------------------------------------------------------------------------
# 5. Taxonomy crosswalk and Level I/II/III material parameters
# ---------------------------------------------------------------------------------------------
L1_MODEL = {"1": "food", "2": "garden|wood (heterogeneous)", "3": "paper", "4": "paper", "5": "plastic", "6": "metal",
            "7": "glass", "8": "textile|rubber|wood|other (heterogeneous)", "9": "other", "10": "other"}
L2_MODEL = {"1.1": "food", "1.2": "food", "2.1": "food|other (unresolved)", "2.2": "garden|wood|other (heterogeneous)",
            "5.1": "plastic", "5.2": "plastic", "5.3": "plastic", "6.1": "metal", "6.2": "metal",
            "7.1": "glass", "7.2": "glass", "7.3": "glass", "8.1": "other", "8.2": "textile|rubber",
            "8.3": "other", "8.4": "wood", "8.5": "other", "9.1": "other", "9.2": "other", "9.3": "other",
            "9.4": "other", "9.5": "other", "10.1": "other", "10.2": "other", "10.3": "other"}
L3_MODEL = {"2.2.1": "other", "2.2.2": "garden", "2.2.3": "wood", "2.2.4": "wood", "8.2.1": "textile",
            "8.2.2": "rubber", "8.2.3": "rubber"}
RIPS_TO = {"food": "sisa_makanan (+ organik where lumped; rule L moves share gamma to garden)",
           "garden": "daun_kering (+ kayu where rule W applies)", "paper": "kertas_kardus", "wood": "kayu",
           "textile": "kain", "rubber": "karet_kulit", "plastic": "plastik + sterofoam", "metal": "logam",
           "glass": "kaca (or inside lain_lain in 5 locations)", "other": "b3 + elektronik + lain_lain"}
CROSS_NOTES = {
    ("2", "4.3"): "Local label 'Cartons, plates and cups' is broader than Edjabou Table 3 'Beverage cartons'; the Danish value is not copied to this code.",
    ("3", "4.4.1"): "Beverage cartons also appear inside 4.3 in the local label; definitional overlap with 4.3 must be resolved before any sorting campaign.",
    ("3", "6.x.1"): "Parent is all metal (6.*): crosses 6.1 and 6.2. Excluded from the terminal partition to avoid double counting; reported as a cross-cutting attribute.",
    ("3", "6.x.2"): "Parent is all metal (6.*): crosses 6.1 and 6.2. Excluded from the terminal partition to avoid double counting; reported as a cross-cutting attribute.",
    ("2", "6.1"): "Aluminium wrapping foil merged here, giving the 36 Level II groups stated in the source prose (printed Table 2 lists 37).",
    ("3", "8.1.3"): "Condom code duplicated in the printed source table; numbering corrected, meaning kept.",
    ("2", "10.2"): "WEEE categories printed under code 10.3 in the source table; renumbered to 10.2.x.",
    ("2", "1.1"): "Six Level III food refinements are a modelling interpretation, not listed explicitly in the source.",
    ("2", "1.2"): "Six Level III food refinements are a modelling interpretation, not listed explicitly in the source.",
}


def build_taxonomy_and_parameters():
    tax = read("02_taxonomy_and_allocations/waste_fraction_taxonomy.csv", dtype=str)
    tax["level"] = tax.level.astype(int)
    assert not tax.duplicated(["level", "code"]).any()
    par = read("04_fraction_literature/fraction_level2_level3_parameters.csv", dtype={"code": str, "parent_code": str})
    t2 = pd.read_csv(DATA / "table2_model_fraction_parameters.csv").set_index("ipcc_category")

    cross = []
    for _, r in tax.iterrows():
        lv, code = r.level, r.code
        if lv == 1:
            mf = L1_MODEL[code]
        elif lv == 2:
            mf = L2_MODEL.get(code, L1_MODEL[code.split(".")[0]])
        else:
            mf = L3_MODEL.get(code, L2_MODEL.get(r.parent_code, L1_MODEL.get(code.split(".")[0], "")))
        has_child = ((tax.parent_code == code) & (tax.level == lv + 1)).any()
        in_terminal = (lv == 3 and not code.startswith("6.x")) or (lv == 2 and (not has_child or code in ("6.1", "6.2")))
        cross.append(dict(level=lv, code=code, key=f"L{lv}:{code}", fraction=r.fraction,
                          parent_level=(lv - 1) if lv > 1 else np.nan,
                          parent_code=r.parent_code if isinstance(r.parent_code, str) else "",
                          parent_key=f"L{lv-1}:{r.parent_code}" if lv > 1 else "",
                          taxonomy_status=r.status, model_fraction=mf,
                          rips_source_categories=" / ".join(RIPS_TO[m] for m in mf.split(" ")[0].split("|") if m in RIPS_TO),
                          in_terminal_partition=in_terminal,
                          crosswalk_note=CROSS_NOTES.get((str(lv), code), "")))
    cross = pd.DataFrame(cross)
    write(cross, "taxonomy_crosswalk.csv")

    # Level I rows: parameters only where the Level I group maps to a single IPCC category.
    l1_ipcc = {"1": "Food", "3": "Paper", "4": "Paper", "5": "Plastic", "6": "Metal", "7": "Glass", "9": "Other"}
    rows = []
    for _, r in tax[tax.level == 1].iterrows():
        k = l1_ipcc.get(r.code)
        base = dict(level=1, code=r.code, fraction=r.fraction, parent_code="", ipcc_proxy_category=k or "")
        if k:
            p = t2.loc[k]
            base.update(moisture_wet_fraction=p.moisture_ipcc_default, DOC_dry_fraction=p.DOC_dry,
                        DOCf=p.DOCf_source_value, carbon_dry_fraction=p.carbon_dry, fossil_carbon_share=p.fossil_carbon_share,
                        parameter_status="IPCC 2006/2019 category default (global; not Indonesia-specific)",
                        carbon_source=IPCC2006, DOCf_source=IPCC2019 if pd.notna(p.DOCf_source_value) else "",
                        source_locator="Table 2.4 / Table 3.0",
                        scope_note=("Board includes coated/composite items; paper default applies to the fibre portion"
                                    if r.code == "4" else ""))
        else:
            base.update(parameter_status="missing; heterogeneous Level I group: needs component shares",
                        scope_note="Do not assign one IPCC category to this group")
        rows.append(base)
    l1 = pd.DataFrame(rows)
    p = pd.concat([l1, par.drop(columns=["taxonomy_status"])], ignore_index=True)

    # Composite products: a parent proxy is not a whole-product value.
    composite = {"4.3": "paperboard + polymer/aluminium", "4.4.1": "paperboard + PE + aluminium",
                 "4.4.2": "paperboard + polymer coating"}
    for c, comp in composite.items():
        i = p.index[p.code == c]
        p.loc[i, "fibre_portion_DOC_dry_proxy"] = p.loc[i, "DOC_dry_fraction"]
        p.loc[i, "fibre_portion_DOCf_proxy"] = p.loc[i, "DOCf"]
        for col in ["moisture_wet_fraction", "DOC_dry_fraction", "DOC_dry_lower", "DOC_dry_upper", "DOCf",
                    "carbon_dry_fraction", "fossil_carbon_share"]:
            p.loc[i, col] = np.nan
        p.loc[i, "parameter_status"] = f"missing for whole product ({comp}); paper proxy kept only for the fibre portion"
    p["needs_material_composition"] = p.code.isin(["1.2", "1.2.1", "1.2.2", "1.2.3", "4.3", "4.4.1", "4.4.2", "5.3.2",
                                                    "8.1", "8.1.2", "8.1.3", "8.2", "10", "10.1", "10.2", "10.3"]
                                                   ) | p.code.str.startswith("10.2.")
    p.loc[p.code.str.startswith("1.2"), "scope_note"] = "Food proxy can overestimate degradable carbon in bones and eggshells; component shares needed."
    p["DOCf_gap"] = p.ipcc_proxy_category.eq("rubber_leather")
    p.loc[p.DOCf_gap, "DOCf_note"] = "Rubber/leather DOCf not verified; left blank. Model uses scenario 0-0.5 (register C12)."
    # Single moisture correction: DOC_wet = DOC_dry x (1 - moisture). Blank if either input is blank.
    p["DOC_wet_derived"] = (p.DOC_dry_fraction * (1 - p.moisture_wet_fraction)).round(4)
    p["moisture_basis"] = "wet-mass fraction (IPCC default before collection)"
    p["DOC_basis"] = "dry-mass fraction"
    p["carbon_basis"] = "dry-mass fraction"
    p["LHV_dry_MJ_kg"] = np.nan
    p["LHV_status"] = "blank: no verified subfraction value (legacy Level I value only in Table 2)"
    p["RDF_transfer_tau"] = np.nan
    p["tau_status"] = "blank: no verified subfraction value"
    p["geography"] = np.where(p.parameter_status.str.contains("proxy|default"), "global IPCC default", "")
    p["sampling_year"] = ""
    p["uncertainty"] = np.where(p.DOC_dry_lower.notna(),
                                "DOC_dry range " + p.DOC_dry_lower.astype(str) + "-" + p.DOC_dry_upper.astype(str), "")
    p.insert(2, "key", "L" + p.level.astype(str) + ":" + p.code)
    p = p.merge(cross[["level", "code", "model_fraction", "taxonomy_status"]], on=["level", "code"], how="left")
    write(p, "fraction_parameters_L1_L2_L3.csv")
    return cross, p


# ---------------------------------------------------------------------------------------------
# 6. Literature benchmarks with conditional shares only for complete sibling sets
# ---------------------------------------------------------------------------------------------
def build_benchmarks(cross):
    b = read("04_fraction_literature/fraction_level2_level3_composition_benchmarks.csv",
             dtype={"code": str, "parent_code": str})
    l2 = read("02_taxonomy_and_allocations/input_kota_level2.csv", dtype={"level2_code": str})
    l3 = read("02_taxonomy_and_allocations/input_kota_level3_detail.csv", dtype={"level3_code": str})
    def const_prior(df, col):                 # a prior that differs by city is not one number: leave blank
        g = df.groupby(col).prior_share_within_parent
        return g.first().where(g.nunique() == 1).rename_axis("code")
    prior = pd.concat([const_prior(l2, "level2_code"), const_prior(l3, "level3_code")])
    b["parent_benchmark_pct"] = np.nan
    b["children_sum_pct"] = np.nan
    b["sibling_set_complete"] = False
    b["conditional_share_within_parent"] = np.nan
    for (lv, pc), grp in b.groupby(["level", "parent_code"]):
        complete = grp.equal_housing_mean_pct_total_wet.notna().all()
        s = grp.equal_housing_mean_pct_total_wet.sum(min_count=1)
        par = b.loc[(b.level == lv - 1) & (b.code == pc), "equal_housing_mean_pct_total_wet"]
        b.loc[grp.index, "children_sum_pct"] = s
        b.loc[grp.index, "parent_benchmark_pct"] = par.iloc[0] if len(par) else np.nan
        b.loc[grp.index, "sibling_set_complete"] = bool(complete)
        if complete and s > 0:
            b.loc[grp.index, "conditional_share_within_parent"] = (grp.equal_housing_mean_pct_total_wet / s).round(4)
    b["conditional_basis"] = np.where(b.sibling_set_complete,
                                      "p(child|parent) = child / sum(children); complete siblings, same Danish basis",
                                      "not computed: sibling set incomplete or no data (no renormalisation of a partial subset)")
    b["workbook_prior_share_within_parent"] = b.code.map(prior)
    b["workbook_prior_note"] = np.where(b.workbook_prior_share_within_parent.isna(),
                                        "workbook prior varies by city or is absent", "constant workbook prior")
    b["pct_basis"] = "percent of total residual household waste, wet mass (Denmark)"
    b["transfer_status"] = "foreign benchmark: not an Indonesian city measurement, not non-domestic waste"
    write(b, "composition_benchmarks_conditional.csv")
    return b


# ---------------------------------------------------------------------------------------------
# 7. Level I / II / terminal composition, rescaled to the 2025 stream masses
# ---------------------------------------------------------------------------------------------
L1_RULE = {"A01": "RIPS organic aggregate split 85/15 Food/Gardening (prior, no Indonesian basis)",
           "A02": "Paper/board 55/45 (approximation of Danish data; Edjabou Table 5 gives about 52.7/47.3)",
           "A03": "Lain-lain 80/20 Misc combustibles/Inert (prior, no source)"}
L2_PRIOR = {"1": ("A04", "Danish-derived ratio (Table 3, 79.4/20.6), rounded"),
            "2": ("A05", "prior 2/98 not supported by Table 3 (9.2/90.8)"),
            "3": ("-", "Danish-derived ratios (Table 3)"),
            "4": ("-", "workbook ratios; 4.3 uses a Danish 'beverage cartons' value under a broader local label"),
            "5": ("A06", "Danish-derived ratio (Table 3); styrofoam added to packaging"),
            "6": ("A07", "Danish-derived ratio (Table 3)"), "7": ("A08", "Danish-derived ratio (Table 3)"),
            "8": ("-", "direct RIPS textile/rubber plus share of lain-lain; remaining splits are workbook priors"),
            "9": ("-", "workbook ratios; 9.3/9.4 have no matched Danish observation"),
            "10": ("A09", "RIPS WEEE direct; B3 split 20/80 prior (Table 3 implies 28.6/71.4)")}
L3_PRIOR = {"1.1": "equal-share prior (no source)", "1.2": "equal-share prior (no source)",
            "2.1": "equal-share prior (no source)", "2.2": "prior 5/70/20/5 (not Table 4)",
            "3.7": "equal-share prior (Table 4 is uneven, tissue-dominated)", "4.4": "equal-share prior",
            "5.1": "Danish-derived (Table 4) with pseudocount for zeros", "5.3": "approximation of Table 4 (91.5/8.5)",
            "6.*": "approximation of Table 4 (57.8/42.2)", "7.1": "Danish-derived (Table 4)",
            "8.1": "prior 90/5/5 (no source)", "8.2": "RIPS textile direct; leather/rubber 20/80 prior",
            "10.2": "equal-share prior (EU categories define classes, not masses)"}


def build_compositions(inp, cross):
    l1 = read("02_taxonomy_and_allocations/input_kota_level1.csv", dtype={"level1_code": str})
    l2 = read("02_taxonomy_and_allocations/input_kota_level2.csv", dtype={"level1_code": str, "level2_code": str})
    l3 = read("02_taxonomy_and_allocations/input_kota_level3_detail.csv", dtype={"level3_code": str, "parent_code": str})
    m25 = inp.set_index("city_id")
    mass = {}
    for cid, r in m25.iterrows():
        mass[(cid, "domestic")] = r.M_dom_tpd_2025
        mass[(cid, "non_domestic")] = r.M_nd_tpd_2025

    def add(df, code_col, level):
        df = df.copy()
        df["stream_mass_tpd_source_year"] = df.stream_mass_tpd
        df["stream_mass_tpd_2025"] = [mass[(c, s)] for c, s in zip(df.city_id, df.stream)]
        df["tpd_2025"] = (df.percent_wet_mass * df.stream_mass_tpd_2025 / 100).round(6)
        df["level"] = level
        df["code"] = df[code_col]
        df["key"] = f"L{level}:" + df.code
        df["composition_year_note"] = "RIPS sampling-year shares applied to 2025 mass (proxy, not measured 2025)"
        return df.drop(columns=["stream_mass_tpd", "tpd"])

    c1 = add(l1, "level1_code", 1)
    c1["allocation_audit_id"] = np.select(
        [c1.mapping_rule.str.contains("85%"), c1.mapping_rule.str.contains("paper/cardboard"),
         c1.mapping_rule.str.contains("other waste")], ["A01", "A02", "A03"], "")
    c1["allocation_status"] = np.where(c1.allocation_audit_id == "", "mapped RIPS aggregate",
                                       c1.allocation_audit_id.map(L1_RULE))
    write(c1, "composition_level1_2025.csv")

    c2 = add(l2, "level2_code", 2)
    c2["allocation_audit_id"] = c2.level1_code.map(lambda k: L2_PRIOR[k][0])
    c2["allocation_status"] = c2.level1_code.map(lambda k: L2_PRIOR[k][1])
    write(c2, "composition_level2_2025.csv")

    # Terminal partition: Level III leaves (except the cross-parent metal split) + Level II leaves without children.
    term_codes = set(cross.loc[cross.in_terminal_partition & (cross.level == 2), "code"])
    t2 = c2[c2.code.isin(term_codes)].copy()
    t2["leaf_type"] = "Level II leaf (no Level III refinement in catalogue)"
    t2["allocation_status_leaf"] = t2.allocation_status
    t3 = add(l3[~l3.level3_code.str.startswith("6.x")], "level3_code", 3)
    t3["leaf_type"] = "Level III refinement (provisional catalogue)"
    t3["allocation_status_leaf"] = t3.parent_code.map(L3_PRIOR)
    keep = ["city_id", "city_name", "stream", "level", "code", "key", "percent_wet_mass", "tpd_2025",
            "stream_mass_tpd_2025", "leaf_type", "allocation_status_leaf", "composition_year_note", "source_pdf", "source_pages"]
    t2 = t2.rename(columns={"level2_fraction": "fraction"})
    t3 = t3.rename(columns={"level3_fraction": "fraction"})
    term = pd.concat([t2[keep + ["fraction"]], t3[keep + ["fraction"]]], ignore_index=True)
    # Explicit residual leaves: any gap between a Level II parent and its catalogued children.
    par = c2.set_index(["city_id", "stream", "code"]).percent_wet_mass
    s3 = t3.groupby(["city_id", "stream", "parent_code"]).percent_wet_mass.sum()
    res = []
    for (cid, st, pc), v in s3.items():
        gap = par.loc[(cid, st, pc)] - v
        if gap > 1e-6:
            res.append(dict(city_id=cid, stream=st, level=3, code=f"{pc}.R", key=f"L3:{pc}.R",
                            fraction=f"Unrefined remainder of {pc}", percent_wet_mass=gap,
                            leaf_type="explicit residual leaf", allocation_status_leaf="residual"))
    if res:
        term = pd.concat([term, pd.DataFrame(res)], ignore_index=True)
    meta = pd.DataFrame([dict(n_terminal_leaves=term.code.nunique(), n_residual_leaves=len({r["code"] for r in res}),
                              metal_rule="6.1/6.2 kept as leaves; 6.x.1/6.x.2 excluded (cross-parent)")])
    write(term, "composition_terminal_partition_2025.csv")
    write(meta, "composition_terminal_partition_summary.csv")
    return c1, c2, term


# ---------------------------------------------------------------------------------------------
# 8. CED and land-use factors (v3.1). Drawn by the model after every v3 draw.
# ---------------------------------------------------------------------------------------------
LCIA = [  # key, central, low, high, unit, indicator, meaning, source, status, evidence
 ("pef_eta", 0.32, 0.28, 0.36, "-", "CED", "Net thermal efficiency of the fossil generation displaced or consumed (primary energy = 3.6 / eta MJ per kWh)", "PT PLN Indonesia Power fleet thermal efficiency 32.09% in 2022 (annual report, seen as a search-result quotation only); Indramayu CFPP net heat rate about 2,460 kcal/kWh (= 35%) at full load", "L", "literature (to verify against the original report)"),
 ("up_fuel", 0.08, 0.03, 0.15, "-", "CED", "Upstream supply energy of power-plant fuel (mining, processing, transport) as a share of fuel energy", "Analyst assumption", "A", "assumption"),
 ("ef_diesel", 0.0741, 0.0741, 0.0741, "kg CO2/MJ", "CED", "Converts diesel-type ancillary burdens (kg CO2e) to diesel energy; the ancillary terms also contain materials, so this is an approximation", "IPCC 2006 Vol. 2 Ch. 1 Table 1.4 (gas/diesel oil 74,100 kg CO2/TJ)", "L", "literature default"),
 ("up_diesel", 0.15, 0.10, 0.25, "-", "CED", "Upstream (refinery, crude supply) energy of diesel as a share of its energy", "Analyst assumption", "A", "assumption"),
 ("up_coal", 0.05, 0.02, 0.10, "-", "CED", "Upstream (mining, transport) energy of the kiln coal displaced by RDF", "Analyst assumption", "A", "assumption"),
 ("ef_natgas", 0.0561, 0.0561, 0.0561, "kg CO2/MJ", "CED", "Converts the PHB nutrient/chemical burden (kg CO2e) to fossil MJ as if natural-gas based (proxy)", "IPCC 2006 Vol. 2 Ch. 1 Table 1.4 (natural gas 56,100 kg CO2/TJ); use as proxy is an assumption", "A", "proxy"),
 ("ced_pp", 75.0, 65.0, 85.0, "MJ/kg", "CED", "Cradle-to-gate fossil CED of displaced polypropylene incl. feedstock energy", "Order of magnitude of PlasticsEurope PP eco-profile; value not verified in this session", "L", "literature (to verify)"),
 ("rho_sl", 0.80, 0.60, 1.00, "t/m3", "LU", "In-place density of compacted MSW in a sanitary landfill", "Engineering range (analyst)", "A", "assumption"),
 ("h_sl", 20.0, 10.0, 30.0, "m", "LU", "Effective fill height of a sanitary landfill", "Engineering range (analyst)", "A", "assumption"),
 ("f_gross_sl", 1.30, 1.10, 1.50, "-", "LU", "Gross site area / fill area (roads, leachate ponds, buffer)", "Engineering range (analyst)", "A", "assumption"),
 ("rho_inert", 1.30, 1.00, 1.60, "t/m3", "LU", "In-place density of ash, digestate cake and other inert residues", "Engineering range (analyst)", "A", "assumption"),
 ("rho_od", 0.50, 0.35, 0.70, "t/m3", "LU", "Density of uncompacted waste in an open dump", "Engineering range (analyst)", "A", "assumption"),
 ("h_od", 5.0, 3.0, 10.0, "m", "LU", "Average heap height of an open dump", "Engineering range (analyst)", "A", "assumption"),
 ("fp_wte", 40.0, 20.0, 80.0, "m2 per t/d", "LU", "Site footprint of a WtE plant per t/d nameplate capacity", "Analyst assumption", "A", "assumption"),
 ("fp_rdf", 50.0, 25.0, 100.0, "m2 per t/d", "LU", "Site footprint of an RDF plant (incl. drying and storage) per t/d", "Analyst assumption", "A", "assumption"),
 ("fp_ad", 60.0, 30.0, 120.0, "m2 per t/d", "LU", "Site footprint of an AD plant (incl. digestate handling) per t/d feed", "Analyst assumption", "A", "assumption"),
]


def build_lcia():
    df = pd.DataFrame(LCIA, columns=["key", "central", "low", "high", "unit", "indicator", "meaning", "source", "status",
                                     "evidence_type"])
    df["note"] = np.where(df.indicator == "LU",
                          "Land take = landfill area consumed (permanent) + plant footprint over its lifetime, m2 per t MSW. "
                          "Cross-check: an Indonesian TPA design study (Talumelito, Gorontalo) implies about 0.045 m2/t; "
                          "central here 0.081 m2/t for MSW.",
                          "CED = non-renewable (fossil) cumulative energy demand, MJ per t MSW; negative = net saving. "
                          "Biogenic energy is not counted.")
    write(df, "lcia_factors.csv")
    return df


if __name__ == "__main__":
    print("Building v3 database from sources/")
    inp = build_input()
    build_table5(inp)
    build_table2()
    build_register()
    cross, _ = build_taxonomy_and_parameters()
    build_benchmarks(cross)
    build_compositions(inp, cross)
    build_lcia()
    print("Done.")

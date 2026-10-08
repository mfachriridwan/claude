"""Build docs/MSW_Methodology_v3_EN.pdf from the v3 data and model outputs. Run after the model."""
import pandas as pd, numpy as np
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.platypus import SimpleDocTemplate, Spacer, PageBreak, KeepTogether, CondPageBreak
from doccommon import *

d = load(); N = stats(d)
for f in (fig_flow, fig_metamodel): f()
for f in (fig_tonnage, fig_qmanaged, fig_robust, fig_hierarchy): f(d)
inp, t2, t5, reg = d["inp"], d["t2"], d["t5"], d["reg"]
story = []
A = story.append
TITLE = "MSW recovery pathways in Indonesia: methodology v3.2 (2025 baseline)"

# ---------------------------------------------------------------------------------------------- title
A(P("Screening LCA and TEA of Municipal Solid Waste<br/>Recovery Pathways in Indonesia", "title"))
A(P("Methodology v3.2: ex-ante decision support before facilities exist &mdash; 2025 tonnage baseline, auditable "
    "parameters with verified secondary data, carbon-inclusive cost at fixed carbon values, decision rules, break-even "
    "targets and the value of information", "subtitle"))
A(P("Muhammad Fachri Ridwan &mdash; Master of Environmental Management, The University of Queensland<br/>"
    "Supervisor: Prof. Anthony Halog &mdash; October 2026 &mdash; Companion code: msw_pathway_model_v3.py / "
    "MSW_pathway_model_v3.ipynb; user tool: MSW_Decision_Tool_Colab.ipynb", "subtitle"))
A(Spacer(1, 6))
st = N["stress"]
box = [P("<b>What v3.2 does and what it shows</b>", "box")] + bullets([
    "<b>Ex-ante decision support.</b> None of the six options exists as a facility in the 21 locations. The model is a "
    "screening tool for local governments, planners and funders <i>before</i> a feasibility study or tender: it tells them "
    "under which conditions each option is worth pursuing, which performance an option must reach to beat a sanitary "
    "landfill, and which local data are worth collecting first. This, not the ranking of 21 cases, is the contribution.",
    f"<b>Secondary data re-checked.</b> 23 secondary values were re-verified against their sources (Section 6.1): "
    f"{N['n_corrected']} were corrected (RDF O&amp;M and price, Sumatera grid factor, the WtE and RDF benchmarks), "
    "landfill-gas collection was widened to 0.20&ndash;0.80, and status V is now kept only for the four values confirmed in "
    "the source text. Benchmarks that could not be confirmed were removed.",
    "<b>Carbon-inclusive cost, fixed carbon values.</b> Options are ranked by C + pG/1000 (USD/t), called the "
    "carbon-inclusive cost because health and local pollution are not valued. Results are given for fixed carbon values "
    "0, 2 (the UU 7/2021 carbon tax), 25, 50 and 100 USD/t CO<sub>2</sub>e with Monte Carlo standard errors "
    f"(at most {N['se_max']:.3f}).",
    "<b>Main result.</b> At central values the sanitary landfill with flare has the lowest carbon-inclusive cost in all 21 "
    f"locations up to 50 USD/t; at 100 USD/t RDF + AD is best in {N['best100'].get('S5', 0)} locations and WtE in "
    f"{N['best100'].get('S1', 0)}. With uncertainty, RDF + AD is the most probable option at 100 USD/t in "
    f"{N['mp100']['market'].get('S5', 0)} locations (median P = {N['p_S5_100'][0]:.2f}). Its abatement cost against the landfill is "
    f"{N['s5_mac'][0]:.0f} ({N['s5_mac'][1]:.0f} to {N['s5_mac'][2]:.0f}) USD/t CO<sub>2</sub>e.",
    "<b>What decides.</b> Food separated at source makes AD preferable above about 30&ndash;45 USD/t; the Perpres 109/2025 "
    "tariff with at least 1,000 t/day and LHV of 7 MJ/kg makes WtE preferable; RDF + AD needs a high carbon value and a "
    f"kiln within about 300 km. The 100 USD/t result is fragile: the high moisture bound or a 22% lower RDF heating value "
    f"returns the choice to the landfill in {int(st.loc['moisture_high_bound', 100])} and "
    f"{int(st.loc['rdf_ncv_stress', 100])} of 21 locations.",
    f"<b>What to measure first.</b> The expected value of perfect information is {N['evpi'][50]:.2f} USD/t at 50 USD/t and "
    f"{N['evpi'][100]:.2f} USD/t at 100 USD/t (median); waste characterisation (moisture, composition) and landfill-gas "
    "performance carry most of it. These are the measurements a city should fund before committing to a technology.",
    "<b>AD on mixed waste made realistic.</b> Food separated mechanically from mixed waste is given 0.6 (0.35&ndash;0.85) of "
    "the methane yield of source-separated food and 5 (0&ndash;10) USD/t extra pre-treatment; the optimistic case is a scenario.",
    f"<b>Reproducible.</b> {N['valid'][0]} of {N['valid'][1]} validation checks pass, including an explicit hand "
    "recalculation for Kota Padang; a Colab notebook runs everything for new cities (Section 12)."], "box")
A(boxed(box))

# ---------------------------------------------------------------------------------------------- 1
A(P("1 Purpose, scope of the update and vocabulary of evidence", "h1"))
A(P("<b>Purpose: ex-ante decision support.</b> Indonesian cities must choose among WtE, RDF, AD and improved landfilling "
    "before any such facility is built, usually with no local performance data. This study is therefore a modelling "
    "study: it does not evaluate existing plants but estimates, before investment, under which city and waste-system "
    "conditions each option would become preferable. Its outputs are designed for the stakeholders who make that choice "
    "(city and regency environment agencies, Bappeda, the Ministry of Environment, PLN and cement companies as offtakers, "
    "and funders): (i) decision rules stated in observable conditions, (ii) break-even targets that a tender or pilot "
    "must reach, and (iii) a ranking of the data worth collecting first, expressed in USD per year. Results are "
    "screening-level and must be confirmed by a feasibility study."))
A(P("This document updates the v2 methodology with the research package of October 2026 (folders 02 to 04). The package "
    "was read in the order its START_HERE file prescribes: the v2 code and PDF, then the assumption audit "
    "(ASSUMPTION_SOURCE_AUDIT.md), which overrides the interpretation of the older allocation workbook, then the 2025 "
    "baseline files, then the fraction-literature files. The SHA-256 of every source file was checked against the package "
    "manifest before use. The equations, system boundary and decision rule of v2 are kept; what changes is where numbers come "
    "from, how their status is recorded and how the 2025 baseline is formed."))
A(P("Every value in the database carries one of the following statuses. A blank cell always means <i>unknown or not "
    "applicable</i>; it is never read as zero."))
A(table([["Status", "Meaning", "Example"],
         ["local measurement", "Reported for the location by its RIPS document", "RIPS wet-mass composition, Kab. Bogor 2022"],
         ["literature default", "Published default for a category, not for the location", "IPCC 2006 Table 2.4 DOC of food, 0.38 (dry)"],
         ["parent-category proxy", "A parent default assigned to a sub-fraction; not a sub-fraction measurement", "Food DOC for 1.2.3 unavoidable animal-derived food"],
         ["foreign benchmark", "Measured elsewhere on a different waste stream", "Edjabou et al. (2015) Danish residual household waste"],
         ["assumption / prior", "Analyst choice, to be defended or replaced", "Organic split 85/15 (A01); moisture as received"],
         ["scenario", "Alternative value run to show sensitivity, never a baseline", "Serang 14% target; rubber DOCf 0.5"],
         ["gap", "No defensible number found; left blank, scenario if the model needs one", "Rubber/leather DOCf; WEEE material composition"]],
        [3.2, 7.0, 6.8]))
A(P("Table 1: Vocabulary of evidence used in every CSV and table of this document.", "cap"))

# ---------------------------------------------------------------------------------------------- 2
A(P("2 Goal, functional unit and system boundary (unchanged)", "h1"))
A(P("The research question remains: <i>under what Indonesian city and waste-system conditions does each recovery pathway "
    "become preferable?</i> The functional unit is the management of 1 tonne of mixed MSW, wet weight, as received at the "
    "gate of the treatment facility, with the composition of the location, in the reference year 2025. Six options are "
    "compared within one LCA/TEA boundary, from the facility gate to final disposal of residues and delivery of products, "
    "with system expansion for displaced electricity, coal and polypropylene: sanitary landfill with flare (SL), WtE (S1), "
    "RDF to cement kiln (S2), AD of food waste (S3), PHB from landfill gas (S4) and integrated RDF + AD (S5). Two baselines "
    "are reported: open dump (OD) and sanitary landfill (SL). Climate change is assessed with GWP100 of IPCC AR6 (GWP20 as a "
    "sensitivity case); cost is a net levelised cost in USD per tonne (AACE class 5). These choices and their justification "
    "are those of v2, Sections 2 and 3."))

# ---------------------------------------------------------------------------------------------- 3
A(P("3 Data harmonisation for the 2025 baseline", "h1"))
A(P("3.1 Three years, kept apart", "h2"))
A(P("Each location now carries a composition sampling year t<sub>c</sub>, a tonnage source year t (blank when it is "
    "unverified) and the baseline year 2025. Tonnage is harmonised with"))
A(eqrow(r"Q_{2025}=Q_t\,(1+g)^{\,2025-t}\qquad M^{d}_{2025}=M^{d}_t\,(1+g)^{\,2025-t},\quad M^{n}_{2025}=M^{n}_t\,(1+g)^{\,2025-t}", "q2025", "1"))
A(P("with g the RIPS growth rate where one is given (6 locations) and otherwise the median of those six rates, 1.73%/yr "
    "(assumption H11, range 0.65 to 2.54%). Domestic and non-domestic streams use the same factor, so "
    "M<super>d</super><sub>2025</sub> + M<super>n</super><sub>2025</sub> = Q<sub>2025</sub>. The exponent is zero when the "
    "tonnage already refers to 2025. The flag <i>tonnage_already_2025</i> is set in the CSV and read by the parser, which "
    "uses Q<sub>2025</sub> directly; an assertion in the model stops the run if the tonnage it computes differs from the CSV "
    "baseline (single-projection guard)."))
ov = inp[inp.tonnage_year_status == "overview_index_year_unverified"]
rows = [["Class", "n", "Rule (central)", "Locations"]]
for st, rule in (("stated_in_source_table", "projected once from the stated year"),
                 ("overview_index_year_unverified", "used as reported; alternative projected from t<sub>c</sub>"),
                 ("RIPS_projection_for_2025", "used as reported; flagged as a projection")):
    q = inp[inp.tonnage_year_status == st]
    rows.append([st.replace("_", " "), str(len(q)), rule, ", ".join(q.city_name)])
A(table(rows, [3.6, 0.7, 4.2, 8.5]))
A(P("Table S1: Tonnage-year classes. The overview class also covers Kota Semarang, Kab. Sragen and Kab. Kutai Kartanegara, "
    "whose source pages cite the same overview index although v2 and the package projected them from the composition year.",
    "cap"))
A(P("<b>Decision on overview totals.</b> The package treated five overview totals as 2025 values on the basis of a keyword in "
    "free-text notes, while three other rows citing the same overview index were projected. Neither the year of the overview "
    "index nor its relation to the composition year is shown in the rows held. v3 therefore applies one rule to all eight: the "
    "total is used as reported (no growth applied, so no double projection if it is already current) and its year is left "
    "blank with flag T1. The alternative, that the total dates from the composition year, is computed for every row "
    f"(Q2025_alt_tpd) and run as the scenario <i>tonnage_overview_projected</i>. It raises tonnage by up to "
    f"{100 * (ov.Q2025_alt_tpd / ov.Q2025_tpd - 1).max():.0f}% (Kab. Kendal, composition 2014) and changes no most-probable "
    "pathway (Section 8). Kab. Pemalang's 2025 tonnage is itself a RIPS projection (population &times; 0.31 kg/cap/day) and "
    "is flagged T2."))
A(figure(fig_tonnage(d), 14.5, "Figure 1: 2025 tonnage by location and tonnage-year class. Hatched: the projected alternative for overview totals. "
    "Ticks: reported source value Q<sub>t</sub>."))

A(P("3.2 Composition: a sampling-year proxy, normalised per stream", "h2"))
A(P("RIPS compositions are wet-mass percentages measured in the sampling year with the SNI 19-3964-1994 method. No source "
    "gives a defensible annual composition trend, so the shares are used for 2025 unchanged and labelled <i>RIPS "
    "sampling-year composition held constant as 2025 proxy</i> (flag C0). Growth is never applied to percentages. Rounded "
    "source totals (99.99 to 100.10%) are normalised per stream k and the factor is stored:"))
A(eqrow(r"s^{(k)}_j=\frac{x^{(k)}_j}{\sum_i x^{(k)}_i},\qquad f^{(k)}=\frac{100}{\sum_i x^{(k)}_i},\qquad "
        r"s_j=\frac{M^{d}_{2025}\,s^{(d)}_j+M^{n}_{2025}\,s^{(n)}_j}{M^{d}_{2025}+M^{n}_{2025}}", "norm", "2"))
A(P("Kab. Pemalang (100.10%) has f = 0.9990. Blank category cells are absent categories: glass is <i>reported</i> in 15 "
    "locations, <i>folded into lain-lain</i> in 5 and <i>not reported</i> in Kota Padang, and the three cases are flagged "
    "separately (C3, C4). Rules L (organic aggregate split with share &gamma;) and W (kayu treated as garden) of v2 are kept as "
    "sampled harmonisation priors. Semarang and Nganjuk report a non-domestic composition but no non-domestic tonnage; it "
    "receives zero weight (flag C6). All compositions refer to waste at generation or collection, not at the landfill gate."))

A(P("3.3 Managed waste and Table 5", "h2"))
A(P("The managed share &mu; is used only in the managed-tonnage scenarios, where it scales the plant size: "
    "Q<sub>managed</sub> = &mu; Q<sub>2025</sub>. Holding &mu; constant from its source year is a scenario assumption, not a "
    "2025 observation. The package filled the seven blanks of v2 with candidates whose definitions differ; v3 keeps every "
    "candidate visible instead of choosing by numerical similarity:"))
A(table([["Indicator family", "Locations", "Definition and status"],
         ["RIPS status index (M1)", "14 legacy + 5 candidates (Bogor, Denpasar, Cianjur, Indramayu, Semarang)",
          "Same index series as v2 Table 5. Its definition and denominator are not documented in the package rows; comparable "
          "only within the series."],
         ["Local service / handling indicators", "Bogor 35.15%, Denpasar 96.91%, Cianjur 30.36%, Indramayu 61.52%, Semarang 59.43%",
          "Urban managed waste, reduction + handling, or service level. Different numerators and denominators from the index; "
          "kept as alternatives, not merged."],
         ["Padang, derived", "93.71% (upper), 71.82% (TPA delivery only)",
          "(141.76 recovery + 465.08 to TPA) / 647.57. Source is a draft feasibility study (p. 94); the recovery may be a "
          "potential and the denominator is the 2023 RIPS total. Provisional; not an observed managed share."],
         ["Serang, local observed", "7.45% (14% target as scenario only)",
          "Existing condition from the master-plan executive summary (p. 154); the 14% is a 2025 service target (pp. 180-181)."]],
        [3.4, 5.6, 8.0]))
A(P("Table S2: Families of managed-waste indicators held in the database.", "cap"))
A(P("Two series are run and reported side by side. <b>M1</b> uses the status index only: 19 locations, Padang and Serang n.a. "
    "<b>M2</b> uses the local indicator where one is held (Padang at its 71.82% lower bound, because the recovery numerator "
    "is unverified) and the status index elsewhere; it mixes definitions by construction and is labelled so. Neither series "
    "implies that all 21 locations have the same indicator. Qmanaged &le; Q<sub>2025</sub> holds for every value."))
rows = [["Location", "Q2025 t/d", "Status index", "Local", "M1 t/d", "M2 t/d", "Status of the local value"]]
for r in t5.itertuples():
    loc = (f"{100*r.local_share:.2f}%" + (f" ({100*r.local_share_lower:.2f}% low)" if pd.notna(r.local_share_lower) else "")
           if pd.notna(r.local_share) else "-")
    rows.append([r.city_name, f"{r.Q2025_tpd:,.0f}", f"{100*r.status_index_share:.2f}%" if pd.notna(r.status_index_share) else "n.a.",
                 loc, fmt(r.Qmanaged_M1_tpd), fmt(r.Qmanaged_M2_tpd),
                 (r.local_status if isinstance(r.local_status, str) else "")[:70]])
A(table(rows, [3.2, 1.5, 1.6, 2.6, 1.3, 1.3, 5.5]))
A(P("Table 5: Managed waste in 2025 (constant-share scenario). Status-index values of the 14 original locations are carried "
    "over from v2 without re-audit of their definition.", "cap"))
A(figure(fig_qmanaged(d), 13.5, "Figure 2: Managed share by indicator family. Grey lines join the two values held for the same location."))

A(P("3.4 Waste-fraction taxonomy: Level I, II and III", "h2"))
A(P("The taxonomy follows the tiered approach of Edjabou et al. (2015): 10 Level I material groups, 36 harmonised Level II "
    "fractions (aluminium foil merged into metal packaging, which gives the 36 stated in the source prose while the printed "
    "Table 2 lists 37) and a catalogue of 56 Level III refinements. The catalogue is <b>provisional</b>: the printed source "
    "table has duplicated codes (condoms), WEEE printed under 10.3, index notation for plastics and metals, and no explicit "
    "food refinements. The six Level III food sub-fractions are a modelling interpretation. The 56 entries are neither 56 "
    "measurements nor a complete terminal partition."))
A(P("All joins use the key (level, code), stored as text so that codes such as 6.1 and 6.10 cannot collide. The crosswalk "
    "(<i>taxonomy_crosswalk.csv</i>) records for every key its parent, the model fraction it maps to and a note where the "
    "mapping is not one-to-one. Three cases matter. (i) The ferrous/non-ferrous split (6.x.1, 6.x.2) has the parent "
    "<i>all metal</i> and crosses 6.1 and 6.2; it is excluded from the terminal partition and kept as a cross-cutting "
    "attribute. (ii) The local label of 4.3, <i>cartons, plates and cups</i>, is broader than the Danish <i>beverage "
    "cartons</i>, and beverage cartons appear again as 4.4.1; no Danish value is copied to 4.3. (iii) Level I "
    "<i>Gardening</i> and <i>Miscellaneous combustibles</i> are heterogeneous and do not map to one IPCC category."))
A(P("<b>Mass balance.</b> The Level III detail file holds only catalogued refinements and covers 76.8 to 93.7% of each "
    "stream; it is not forced to 100%. Where a full terminal partition is needed, it is built from the 54 Level III leaves "
    "(56 minus the two cross-parent metal rows) and the 24 Level II fractions without children (including 6.1 and 6.2): "
    "<b>78 leaves</b>, each stream summing to 100%. Because every catalogued parent is fully split by its priors, no residual "
    "leaf is needed (the code would create one, labelled <i>X.R unrefined remainder</i>, if a gap appeared)."))
A(P("<b>Benchmarks and conditional shares.</b> The Danish single- and multi-family values (Edjabou et al. 2015, Tables 3 "
    "and 4) are percentages of total residual household waste. They are a foreign benchmark: not an Indonesian city "
    "measurement and not non-domestic waste. A conditional share within the parent is computed only for complete sibling sets "
    "on the same basis:"))
A(eqrow(r"p(c\,|\,\pi)=\frac{x_c}{\sum_{c'\in\mathrm{ch}(\pi)}x_{c'}}\qquad \mathrm{only\ if\ every\ } x_{c'} \mathrm{\ is\ observed}", "cond", "3"))
cs = d["bm"][d["bm"].sibling_set_complete]
A(P(f"This holds for {cs.groupby(['level','parent_code']).ngroups} sibling sets (food, gardening, paper, plastic, metal, "
    "glass, miscellaneous combustibles and special waste at Level II; packaging plastic, plastic film and packaging glass at "
    "Level III). Board (4.3 missing), inert (9.3, 9.4 missing), garden waste (2.2.4), miscellaneous paper (3.7.4, 3.7.5) and "
    "miscellaneous board (4.4.1, 4.4.2) are incomplete and receive no conditional share; partial subsets are not renormalised. "
    "Reported zeros are kept; a pseudocount for a Dirichlet prior would be a separate modelling decision."))
A(P("<b>Allocation priors.</b> The Level I/II/III compositions (<i>composition_level*_2025.csv</i>) rescale the package "
    "allocations to the 2025 stream masses and label every split with its audit ID. The audit found that several are priors "
    "without an Indonesian or literature basis (A01 organic 85/15, A03 lain-lain 80/20, A05 gardening 2/98 where Table 3 "
    "gives 9.2/90.8, A09 B3 20/80 where Table 3 implies 28.6/71.4, and the equal-share Level III splits); others are rounded "
    "Danish ratios (A04, A06 to A08). These allocations are database content for future calibration. <b>They are not used by "
    "the model</b>, which computes on the ten model fractions mapped directly from the RIPS categories (Figure 6)."))
A(figure(fig_hierarchy(d), 15.5, "Figure 3: Status of material parameters by level (left) and availability of Danish composition benchmarks (right)."))

A(P("3.5 Material parameters (Table 2 and the detailed table)", "h2"))
A(P("IPCC (2006) Volume 5 Table 2.4 supplies default dry matter, DOC, total carbon and fossil-carbon share for the parent "
    "categories; the 2019 Refinement Table 3.0 supplies DOCf: 0.70 for food and garden/grass, 0.50 for paper, textile and "
    "nappies and 0.10 for wood and branches. DOC is on a dry basis and multiplies dry mass; the wet value is"))
A(eqrow(r"\mathrm{DOC}_{wet}=\mathrm{DOC}_{dry}\,(1-w)\qquad \mathrm{CH_4}=10^3\,k_D\,\mathrm{MCF}\,F\,\frac{16}{12}\sum_j m_j(1-w_j)\,\mathrm{DOC}_{dry,j}\,\mathrm{DOCf}_j", "docwet", "4"))
A(P("so moisture is applied once. With IPCC moisture the check reproduces the IPCC wet DOC values (food 0.15, garden 0.20, "
    "paper 0.40, wood 0.43, textile 0.24, rubber 0.39) within 0.005. Landfill DOCf is not the biochemical methane potential of "
    "a digester: AD uses its own yield parameter (A8)."))
rows = [["Fraction", "w IPCC", "w as received (range)", "DOC dry", "DOCf (status)", "C", "&phi;", "h dry MJ/kg (status)", "&tau; (status)"]]
short = lambda s: ("IPCC 2019" if s.startswith("IPCC") else "gap: scenario 0-0.5" if s.startswith("gap") else
                   "n/a" if s.startswith("not") else s)
for r in t2.itertuples():
    hs = "Gotze 2016, TS" if r.fraction == "plastic" else ("legacy" if r.LHV_dry > 0 else "-")
    ts = "legacy (Nasrullah)" if "Nasrullah" in r.tau_status else ("legacy A" if r.tau > 0 else "-")
    rows.append([r.fraction, f"{r.moisture_ipcc_default:.2f}", f"{r.moisture_as_received:.2f} ({r.moisture_as_received_low:.2f}-{r.moisture_as_received_high:.2f})",
                 f"{r.DOC_dry:.2f}", ("blank; " if pd.isna(r.DOCf_source_value) and r.fraction == "rubber" else
                                      f"{r.DOCf_model:.2f} ") + f"({short(r.DOCf_status)})",
                 f"{r.carbon_dry:.2f}", f"{r.fossil_carbon_share:.2f}", f"{r.LHV_dry:.1f} ({hs})", f"{r.tau:.2f} ({ts})"])
A(table(rows, [1.6, 1.1, 2.6, 1.2, 2.8, 0.9, 0.9, 2.9, 2.8]))
A(P("Table 2: Properties of the ten model fractions (<i>table2_model_fraction_parameters.csv</i>). w, DOC, C and &phi;: "
    "IPCC 2006 Table 2.4 defaults, global, not Indonesia-specific. As-received moisture: analyst assumption carried from v2, "
    "calibrated to bulk moisture at Indonesian transfer points (Prabowo et al. 2019). DOCf: IPCC 2019 Table 3.0. h: legacy "
    "values cited in v2 to Tchobanoglous et al. (1993), not re-verified, multiplied by k<sub>h</sub>; plastic: aggregate "
    "median LHV of 30.5 MJ/kg TS (G&ouml;tze et al. 2016, Section 3.2.3), not resin-specific and not multiplied by "
    "k<sub>h</sub>. &tau;: legacy RDF transfer coefficients; paper, plastic and wood informed by measurements on commercial "
    "and industrial waste (Nasrullah et al. 2014), the others analyst assumptions."))
A(P("<b>Moisture decision.</b> IPCC moisture describes waste as generated; the functional unit is waste as received at the "
    "gate, wetter after collection. The main case keeps the v2 as-received values, which bring bulk moisture into the "
    "measured Indonesian range, and the IPCC defaults are run as the scenario <i>moisture_IPCC_default</i>. Both are "
    "internally consistent because DOC, carbon and heating value always multiply the same dry mass. The choice matters "
    "(Section 8): it is the single data assumption that changes the most probable pathway most often."))
A(P("<b>Detailed parameters</b> (<i>fraction_parameters_L1_L2_L3.csv</i>, 102 rows). Level II/III values are IPCC parent "
    "proxies and keep that status; IPCC did not measure these sub-fractions. Grass, woody material and soil are separated: "
    "plant material takes the garden proxy (DOCf 0.70), woody plant material and straw the wood proxy (DOCf 0.10) and humid "
    "soil the inert proxy (DOC 0). Composite products receive no whole-product value: coated cartons and cups (4.3, 4.4.1, "
    "4.4.2) keep the paper proxy only for their fibre portion, and composite film, batteries, other HHW, WEEE, tampons and "
    "condoms are blank until their material composition is known. Animal-derived food keeps the food proxy with a warning that "
    "bones and eggshells lower its degradable carbon. Rubber and leather have no verified DOCf and stay blank. LHV and "
    "&tau; are blank for all sub-fractions. Metal and glass moisture is blank at Level II/III because contamination moisture "
    "was not measured; the IPCC dry-matter default of 100% applies at Level I only. Literature from 2026 (the Malang plastic "
    "study) is retained as a material benchmark, not as 2025 composition data for the study locations."))
cov = d["cover"]
A(P(f"Weighted by the terminal composition, a Level II/III DOC value is known for {cov.has_DOC_dry_fraction.min():.0f} to "
    f"{cov.has_DOC_dry_fraction.max():.0f}% of each stream's wet mass, moisture and carbon for "
    f"{cov.has_moisture_wet_fraction.min():.0f} to {cov.has_moisture_wet_fraction.max():.0f}% and DOCf for "
    f"{cov.has_DOCf.min():.0f} to {cov.has_DOCf.max():.0f}% (<i>outputs/11_parameter_coverage_L2L3.csv</i>). A Level II "
    "model cannot yet be run without imputation, which is why the model stays on the ten model fractions."))

# ---------------------------------------------------------------------------------------------- 4
A(PageBreak())
A(P("4 Model and metamodel", "h1"))
A(P("The computation is split into a data layer and a model layer (Figure 5). The data layer turns the package into "
    "status-labelled CSV files; the model layer reads them, harmonises, characterises, evaluates the six options, applies the "
    "gates and the decision rule, and propagates uncertainty. Figure 6 is the metamodel: the data entities and which of them "
    "feed results."))
A(figure(fig_flow(), 16.5, "Figure 5: Model flow. Every arrow into the model layer is a CSV file."))
A(figure(fig_metamodel(), 16.5, "Figure 6: Metamodel. The upper chain produces results; the lower chain (Level I/II/III) is reporting and calibration "
    "content and never changes a result."))
A(P("4.1 Equations", "h2"))
A(P("Symbols as in v2: s<sub>j</sub> share of fraction j; w<sub>j</sub> moisture; m<sub>j</sub> tonnes of fraction j sent to "
    "a unit per tonne MSW; h<sub>j</sub> dry-matter LHV; C<sub>j</sub> dry carbon; &phi;<sub>j</sub> fossil share. "
    "Characterisation:"))
A(eqrow(r"M=\sum_j s_jw_j\qquad H=\mathrm{max}\left[0,\ \sum_j k_{h,j}\,s_jh_j(1-w_j)-\lambda M\right]\qquad "
        r"E_{fos}(m)=10^3\,\frac{44}{12}\sum_j m_j(1-w_j)C_j\varphi_j", "char", "5"))
A(P("with k<sub>h,j</sub> = k<sub>h</sub> for legacy heating values and k<sub>h,plastic</sub> for plastic (register C10, "
    "C10b). Landfill (both baselines and every residue landfill):"))
A(eqrow(r"G_{LF}(m,m_{in})=\mathrm{CH_4}(m,\mathrm{MCF})\,(1-\eta)(1-OX)\,GWP_{CH_4}+a_{LF}\left(\sum_jm_j+m_{in}\right)", "glf", "6"))
A(P("Pathways (S5 combines the S2 and S3 units; rejects and digestate go to the residue landfill):"))
A(eqrow(r"G_1=E_{fos}(s)+n_{N_2O}GWP_{N_2O}+a_W-E_{el}EF_{grid}+G_{LF}(0,\alpha_{ash}),\quad E_{el}=\frac{10^3}{3.6}H\eta_W", "g1", "7"))
A(eqrow(r"G_2=E_{fos}(r)+e_REF_{grid}+a_R+M_{del}\,d\,ef_{truck}-\psi E_{net}EF_{coal}+G_{LF}(s-r),\quad r_j=\tau_jk_\tau s_j", "g2", "8"))
A(P("For AD the methane volume of food separated from mixed waste is V = 10<super>3</super> a (1&minus;w<sub>food</sub>) "
    "(VS/TS) y<sub>CH4</sub> k<sub>mech</sub>, where k<sub>mech</sub> = 0.6 (0.35&ndash;0.85) is the yield of mechanically "
    "separated food relative to source-separated food (Seruga et al. 2020, full scale, about 0.84; Basinas et al. lower), and "
    "the cost includes c<sub>pre</sub> = 5 (0&ndash;10) USD per tonne of feed for depackaging and grit removal. Both are 1 and 0 "
    "in the scenarios <i>AD_feed_optimistic</i> and <i>food_separated_at_source</i>."))
A(eqrow(r"G_3=V\rho_{CH_4}f_{AD}GWP_{CH_4}+a\,a_A+\pi(e_REF_{grid}+a_R)-E_{AD}EF_{grid}+G_{LF}(s-a\,e_{food},\delta a)", "g3", "9"))
A(eqrow(r"G_4=G_{SL}+P\,(ef_{PHB}-\sigma\,ef_{PP}),\quad P=\eta\,\mathrm{CH_4}(s,1)/R\qquad G_5=G_{AD}(a)+G_{RDF}(s-a\,e_{food})+G_{LF}(\mathrm{rej},\delta a)", "g45", "10"))
A(P("Techno-economic assessment and decision:"))
A(eqrow(r"CRF=\frac{r(1+r)^n}{(1+r)^n-1},\quad c^{cap}_u=\frac{CRF\,K_{ref}\,(q/0.85/q_{ref})^b}{365\,q},\quad "
        r"CIC_k=C_k+p\,G_k/10^3,\quad MAC_k=10^3\,\frac{C_k-C_0}{G_0-G_k}", "tea", "11"))
A(P("The gates G1 to G6 (WtE LHV &ge; 7 MJ/kg and Q &ge; 150 t/d, PSEL tariff at Q &ge; 1,000 t/d, RDF NCV &ge; 12.56 MJ/kg, "
    "kiln within 300 km road distance, PHB &ge; 500 t/yr) are read from <i>model_constants.csv</i>. Feasible options are "
    "screened by Pareto dominance on (G, C). The best option is the feasible option with the lowest <b>carbon-inclusive cost</b> "
    "CIC<sub>k</sub> (USD per tonne MSW). CIC is not a full social cost: it values only greenhouse gases, at a carbon value p "
    "that is a <i>policy choice</i>. p is therefore not sampled as an uncertain parameter; results are reported at fixed "
    "values p = 0, 2 (Indonesian carbon tax, Rp 30/kg CO<sub>2</sub>e, UU 7/2021), 25, 50 and 100 USD/t CO<sub>2</sub>e. "
    "For each p the probability of being best is the share of N = 4,000 Monte Carlo draws in which an option has the "
    "lowest CIC, reported with its Monte Carlo standard error &radic;(P(1&minus;P)/N) &le; 0.008. A small lead between two "
    "options is not a statistical tie: the standard error is small, and the lead measures how much the choice depends on "
    "inputs that are still uncertain (decision uncertainty), which Section 9 decomposes."))

# ---------------------------------------------------------------------------------------------- 5
A(P("5 Uncertainty: priors, sampling noise and scenarios kept apart", "h1"))
A(P("Three kinds of uncertainty are distinguished. (1) <b>Composition</b>: a Dirichlet distribution around the RIPS proxy, "
    "S ~ Dir(&alpha;<sub>0</sub> s), with &alpha;<sub>0</sub> = 80, or 40 where the data are flagged. These concentrations are "
    "<i>heuristics</i>, not statistically calibrated: no replicate sorting campaigns exist for these locations to estimate "
    "them. The Dirichlet therefore represents prior uncertainty about how well an 8-day sample of a past year represents "
    "2025, not measured sampling variability. (2) <b>Parameters</b>: triangular distributions from the register (Section 6), "
    "a common wetness draw for moisture, the harmonisation priors &gamma; and yard, the rubber DOCf gap (0 to 0.5) and the "
    "realism parameters of AD on mixed waste. (3) <b>Data scenarios and stress tests</b>: discrete alternatives that are not "
    "given probabilities (Table S4). The carbon value is a policy setting and is fixed, not sampled (Section 4.1)."))
A(P("<b>Missing categories.</b> A category that a RIPS does not report (glass in Kota Padang) has zero share in the main "
    "case. This is a modelling assumption, not a measurement of zero. The scenario <i>glass_imputed</i> gives such a "
    "location a glass share drawn from the 15 locations that report glass (median 1.2%, 10th&ndash;90th percentile "
    "0.7&ndash;4.5%), and <i>dirichlet_pseudocount</i> adds 0.5 to every Dirichlet concentration so that no fraction can be "
    "exactly zero in a draw. Neither changes the most probable option at any carbon value (Table S6)."))
us = d["usplit"]
rows = [["Option", "G: composition only", "G: parameters only", "G: both", "C: composition only", "C: parameters only", "C: both"]]
for k in PW:
    rows.append([NAMES[k]] + [f"{us.loc[k, (a, b)]:.0f}" for a in ("G_w90", "C_w90") for b in ("composition only", "parameters only", "both")])
A(table(rows, [3.2, 2.3, 2.3, 1.7, 2.6, 2.6, 2.3]))
A(P("Table S3: Median width of the 90% interval over 21 locations when only one source of uncertainty is sampled "
    "(G in kg CO<sub>2</sub>e/t, C in USD/t; 2,000 draws, market case).", "cap"))
A(P("Parameter uncertainty dominates cost; composition contributes a large share of the spread in GHG for WtE (through the "
    "plastic share) but less than the parameters for the landfill-based options, where gas collection efficiency dominates."))
A(table([["Scenario", "What changes", "Status"],
         ["market", "Main case: market prices, no support, residues to sanitary landfill, GWP100, as-received moisture", "main"],
         ["perpres109", "WtE electricity at USD 0.20/kWh where Q &ge; 1,000 t/d", "policy"],
         ["food_separated_at_source", "Food arrives separated: no front-end sorting (&pi; = 0), k<sub>mech</sub> = 1, c<sub>pre</sub> = 0", "what-if"],
         ["AD_feed_optimistic", "Mixed-waste AD as good as source-separated food (k<sub>mech</sub> = 1, c<sub>pre</sub> = 0)", "what-if"],
         ["residues_to_open_dump", "Residues dumped instead of landfilled (illegal; sensitivity only)", "what-if"],
         ["managed_M1_status_index", "Plant size = status-index share &times; Q2025 (19 locations)", "data scenario"],
         ["managed_M2_local_first", "Plant size = local indicator where held, else status index (21; mixed definitions)", "data scenario"],
         ["GWP20", "20-year horizon for methane", "method"],
         ["moisture_IPCC_default", "IPCC moisture (as generated) instead of as-received values", "data scenario"],
         ["tonnage_overview_projected", "Overview totals projected from the composition year", "data scenario"],
         ["docf_rubber_0.5", "Rubber/leather DOCf at the IPCC 2006 generic default 0.5", "data gap"],
         ["glass_imputed", "Glass share imputed where glass is not reported", "data gap"],
         ["dirichlet_pseudocount", "Pseudocount 0.5 added to every Dirichlet concentration", "method"],
         ["moisture_high_bound", "Moisture as received at the upper end of every fraction range", "stress test"],
         ["rdf_ncv_stress", "RDF energy &times; 0.78 (NCV about 13&ndash;14 MJ/kg, below the 15&ndash;16.7 reported at Cilacap)", "stress test"],
         ["phb_large_scale_cost", "PHB at an aspirational large-scale cost (1.3 USD/kg, no scale penalty)", "what-if"]],
        [4.0, 10.6, 2.4]))
A(P("Table S4: Scenarios. Each uses the same random numbers per location, so differences come only from what the scenario "
    "changes. With 4,000 draws the Monte Carlo standard error of a probability is at most 0.008.", "cap"))

# ---------------------------------------------------------------------------------------------- 6
A(P("6 Assumption register", "h1"))
A(P("Status: V = confirmed against the source text in the October 2026 verification (Section 6.1); L = literature value "
    "with a cited source that was not, or only partly, confirmed; A = analyst assumption. Evidence type follows Table 1. "
    "The register is <i>data/assumption_register.csv</i>; the table below is generated from it."))
rows = [["ID", "Key", "Meaning", "Central (range)", "Unit", "Source", "St."]]
for r in reg.itertuples():
    rng_ = num(r.central) + (f" ({num(r.low)} to {num(r.high)})" if r.low != r.high else "")
    rows.append([r.id, r.key, r.meaning, rng_, r.unit, r.source, r.status])
A(table(rows, [0.9, 1.6, 6.0, 2.4, 1.6, 3.9, 0.6]))
A(P(f"Table 3: Assumption register ({len(reg)} entries; CED/LU factors in Table 8, realism and scenario parameters in "
    "<i>data/scenario_parameters.csv</i>). New or changed in v3: H14-H15 (heuristic Dirichlet concentrations made "
    "explicit), C10b (plastic LHV range), C12 (rubber DOCf gap scenario). Changed in v3.2: see Table 3a.", "cap"))
A(P("6.1 Verification of secondary data (October 2026)", "h2"))
A(P("Every secondary value that drives a result or a benchmark was re-checked against its source. Full texts could not be "
    "downloaded in the working environment, so the check used publisher abstracts, indexed records and search-engine "
    "extracts of the sources; the method of each check is recorded in <i>data/secondary_data_verification.csv</i>. A value "
    "was <i>confirmed</i> when the number appeared in the source record, <i>corrected</i> when the source gave a different "
    "number, and <i>not verified</i> when it could not be found; unverified benchmark values were removed rather than kept. "
    "The previous value is kept in the file for audit."))
ver = d["ver"]
rows = [["Item", "Value used (v3.2)", "Previous value", "Result", "Evidence"]]
for r in ver.itertuples():
    rows.append([r.item, str(r.value_used), str(r.previous_value), r.result, str(r.evidence)[:170]])
A(table(rows, [3.0, 3.0, 2.8, 1.9, 6.3]))
A(P("Table 3a: Secondary-data verification. Status V in the register is kept only for the four values confirmed in the "
    "source record (food-waste methane yield, WtE CAPEX and O&amp;M, electricity price); other values formerly marked V are "
    "now L. The 21 RIPS compositions and tonnages were not re-verified because the source PDFs were not available; their "
    "row-level page locators are kept in the input file.", "cap"))

# ---------------------------------------------------------------------------------------------- 7
A(P("7 Verification and validation", "h1"))
A(P("Because the facilities do not exist, the model cannot be validated against their measured performance. Four layers "
    "are therefore kept apart, following the usual distinction between verification (is the model computed as specified?) "
    "and validation (does it represent the system well enough for its purpose?)."))
v = d["val"]
A(P(f"<b>(a) Verification of the code and data.</b> <i>validate_v3.py</i> ran {len(v)} checks; {N['valid'][0]} passed. They "
    "include an explicit hand recalculation of every result for Kota Padang (the worked example), which must reproduce "
    "the model to 10<super>&minus;6</super>."))
rows = [["Group", "Checks", "Examples of what is tested"]]
ex = {"blank vs zero": "blank DOCf stays blank; parser treats blank and reported-zero glass without imputation; old file rejected",
      "taxonomy": "(level, code) unique in 3 tables; 10/36/56 counts; every parent exists; 6.* documented",
      "mass balance": "M_dom + M_nd = Q2025; Level I = 100%; Level II = parent; terminal partition = 100% (78 leaves)",
      "parent-child": "Level III = Level II parent; detail file partial (76.8-93.7%); conditional shares only for complete sets",
      "projection": "Q2025 = Qt(1+g)^expo once; exponent 0 when already 2025; overview year blank and flagged",
      "Qmanaged": "M1, M2 <= Q2025; Serang 7.45% not 14%; Padang provisional with lower bound",
      "wet/dry": "DOC_dry(1-w) = IPCC wet DOC; LHV dry basis, LHV not HHV; DOCf separate from AD yield; soil/wood separated",
      "provenance": "every row has source, locator and status; proxies keep proxy status; heuristics labelled",
      "CED/LU": "factors have source and status; land take non-negative; landfill area = 1/(rho H) x gross; G and C unchanged",
      "template": "user-template round trip reproduces results; invalid input rejected; blank glass not zero; single projection",
      "v3.2 data": "corrected values in use; ESDM 2018 grid factors; status V only where confirmed; every item has evidence",
      "v3.2 model": "AD penalty changes only S3/S5; glass imputation only where not reported; RDF NCV stress applied",
      "v3.2 decision": "P(best) sums to 1; standard errors bounded; EVPPI <= EVPI; break-even closes the gap; Padang hand calculation",
      "v3.2 wording": "no 'statistical tie' or 'social cost' as decision metric in code and tool",
      "one-city tool": "Padang template reproduces the database results; CSV = XLSX; blank template rejected; local values applied",
      "projection 2045": "grid path; WtE change = kWh x EF x (1 - factor); escalation changes costs only; single tonnage projection; notebook = full run"}
for g, q in v.groupby("group", sort=False):
    rows.append([g, f"{(q.result == 'PASS').sum()}/{len(q)}", ex.get(g, "")])
A(table(rows, [2.6, 1.3, 13.1]))
A(P("Table S5: Verification checks by group (full list in <i>outputs/validation_report.md</i>). The model also asserts the "
    "single projection for every location, the S5 mass balance and that biogas carbon is below food carbon.", "cap"))
A(P("<b>(b) Benchmark validation.</b> Intermediate outputs that do not depend on a facility existing (moisture, heating "
    "value, RDF yield and quality, WtE electricity per tonne) were compared with independent Indonesian values from the "
    "verified sources. The verdict is computed, not judged (Table 4)."))
b = d["bench"]
rows = [["Quantity", "Model, 21 locations", "Reference (verified source)", "Verdict"]] + \
       [[i, r.model_21_locations, r.reference, r.verdict] for i, r in b.iterrows()]
A(table(rows, [3.2, 3.2, 7.6, 3.0]))
A(P("Table 4: Benchmark validation (main case). The RDF heating value lies at the upper end of, and partly above, the "
    "Cilacap range; WtE electricity overlaps the Indonesian design values at their lower end. Both are tested further in "
    "the stress tests.", "cap"))
A(P("<b>(c) Stress tests.</b> Assumptions without a local measurement were pushed to the ends of their plausible range and "
    "the number of locations whose most probable option changes was counted at each carbon value (Table S6)."))
st = N["stress"]
rows = [["Stress test or scenario"] + [f"{pc} USD/t" for pc in st.columns]]
for s_, r in st.iterrows():
    rows.append([SCN.get(s_, s_)] + [f"{int(x)}" for x in r.values])
A(table(rows, [5.2] + [2.36] * len(st.columns)))
A(P("Table S6: Number of the 21 locations whose most probable option changes relative to the market case. The PHB "
    "row is an aspirational what-if, not a stress test.", "cap"))
A(P(f"No stress test changes the decision up to 25 USD/t. At 100 USD/t the RDF + AD lead is fragile: the high moisture "
    f"bound and a 22% lower RDF heating value each return {int(st.loc['moisture_high_bound', 100])} and "
    f"{int(st.loc['rdf_ncv_stress', 100])} locations to the landfill. The drier IPCC moisture moves "
    f"{int(st.loc['moisture_IPCC_default', 50])} locations away from the landfill at 50 USD/t. Glass imputation and the "
    "pseudocount change at most two locations, at 100 USD/t only."))
A(P("<b>(d) Field validation, ex ante.</b> When a city moves towards a facility, the model should be checked against (i) a "
    "local waste characterisation with proximate analysis (moisture, ash, heating value as received) and (ii) performance "
    "data from analogous Indonesian facilities: the Cilacap and Jeranjang RDF plants for RDF yield and quality, the "
    "Bantargebang and Putri Cempo pilots for WtE output, and landfill-gas collection measured at a sanitary landfill. "
    "Section 9 ranks these measurements by their value of information, so that the field work is ordered by how much "
    "it can change the decision."))

# ---------------------------------------------------------------------------------------------- 8
A(PageBreak())
A(P("8 Results for the 21 locations (2025 baseline)", "h1"))
ch = d["char"]
A(P(f"LHV as received is {N['lhv'][0]:.1f} to {N['lhv'][1]:.1f} MJ/kg (median {N['lhv'][2]:.1f}). At central values "
    f"{N['g1']} locations pass the WtE heating-value gate G1 and {N['g12']} pass G1 and the scale gate G2. "
    f"{N['psel_gen']} locations reach 1,000 t/day on generated waste; on managed waste none does in series M1 and "
    f"{N['psel_m2']} does in series M2 (Kab. Bogor, whose local indicator gives 1,016 t/d)."))
rows = [["Option", "G", "&Delta;G vs open dump", "&Delta;G vs landfill", "C", "MAC vs OD", "MAC vs SL", "Pass"]] + pathway_table(d)
A(table(rows, [2.8, 2.2, 2.4, 2.3, 1.9, 2.0, 2.4, 1.0]))
A(P("Table 6: Pathway results per tonne of MSW, central values, market case: median (minimum to maximum) over the 21 "
    "locations, including those failing a gate (last column: number that pass). G, &Delta;G in kg CO<sub>2</sub>e/t; C in "
    "USD/t; MAC in USD/t CO<sub>2</sub>e.", "cap"))
pb = d["pbest"]; pb = pb[pb.scenario == "market"]
bst = d["best"][d["best"].scenario == "market"].set_index("city")
rows = [["Location", "Flags", "Central best: 0 / 25 / 50 / 100", "P(SL) at 50", "P(S5) at 50", "P(SL) at 100", "P(S5) at 100",
         "P(S1) at 100", "Most probable at 100"]]
for c_ in ch.itertuples():
    g = pb[pb.city == c_.city].set_index(["carbon_value", "pathway"])
    f = lambda pc, k: f"{g.loc[(pc, k), 'p_best']:.2f} &plusmn; {1.96 * g.loc[(pc, k), 'se']:.2f}"
    q100 = g.xs(100).p_best
    rows.append([c_.city, c_.flags if isinstance(c_.flags, str) else "",
                 " / ".join(bst.loc[c_.city, f"best_at_{pc}"] for pc in (0, 25, 50, 100)),
                 f(50, "SL"), f(50, "S5"), f(100, "SL"), f(100, "S5"), f(100, "S1"), f"{q100.idxmax()} ({q100.max():.2f})"])
A(table(rows, [3.0, 0.9, 2.5, 1.65, 1.65, 1.65, 1.65, 1.65, 2.35]))
A(P("Table 7: Decision results per location, market case. Central best: lowest carbon-inclusive cost at central values for "
    "carbon values of 0, 25, 50 and 100 USD/t CO<sub>2</sub>e. P(k) at p: probability that option k has the lowest "
    "carbon-inclusive cost at the fixed carbon value p, &plusmn; the 95% Monte Carlo interval (4,000 draws). At 0, 2 and "
    "25 USD/t the landfill is most probable everywhere (<i>outputs/15_p_best_fixed_carbon_value.csv</i>). Flags: L lumped "
    "organics, W kayu as garden, R rounded percentages, G glass in lain-lain, N glass not reported, T tonnage year unverified.",
    "cap"))
A(figure(OUT / "fig_probability_best.png", 17, "Figure 7: Probability of being the best feasible option at fixed carbon "
         "values (top) and in three scenarios at 50 USD/t (bottom)."))
A(figure(fig_robust(d, 100), 16, "Figure 8: Most probable option per location in each scenario at 100 USD/t CO<sub>2</sub>e "
         "(colour) and its probability (number)."))
m50, m100 = N["mp50"], N["mp100"]
A(P("<b>What the numbers say.</b>", "body"))
for t_ in bullets([
    f"<b>Landfill first.</b> Sanitary landfill with flare has the lowest carbon-inclusive cost at central values in all 21 "
    f"locations at 0, 25 and 50 USD/t, and is the most probable option everywhere up to 50 USD/t (median P = "
    f"{N['mpp50'].median():.2f} at 50 USD/t). At the current carbon tax (about 2 USD/t) no recovery option is preferred.",
    f"<b>RDF + AD at a high carbon value.</b> At 100 USD/t RDF + AD is best at central values in {N['best100'].get('S5',0)} "
    f"locations and most probable in {m100['market'].get('S5',0)} (median P = {N['p_S5_100'][0]:.2f}, range "
    f"{N['p_S5_100'][1]:.2f} to {N['p_S5_100'][2]:.2f}). Its abatement cost against the landfill is {N['s5_mac'][0]:.0f} "
    f"({N['s5_mac'][1]:.0f} to {N['s5_mac'][2]:.0f}) USD/t CO<sub>2</sub>e, so it becomes preferable only above that carbon value.",
    f"<b>WtE</b> needs a heating value of at least 7 MJ/kg, 1,000 t/day and the Perpres 109/2025 tariff together; with the "
    f"tariff it is most probable at 50 USD/t in {m50['perpres109'].get('S1',0)} locations. Without the tariff it wins only "
    "where the grid is coal-heavy and the waste dry (Kab. Kutai Kartanegara at 100 USD/t).",
    f"<b>AD alone</b> becomes most probable in {m50['food_separated_at_source'].get('S3',0)} locations at 50 USD/t if food "
    "arrives separated at source. For mixed waste it never does, even when the mixed-waste penalty is removed "
    f"(<i>AD_feed_optimistic</i>: landfill most probable in {m50['AD_feed_optimistic'].get('SL',0)} locations at 50 USD/t). "
    "Source separation, not digester technology, is the condition for AD.",
    f"<b>PHB</b> is not preferred at the cost assumed for a first plant. Only the aspirational large-scale cost "
    f"(1.3 USD/kg) would make it most probable in {m50['phb_large_scale_cost'].get('S4',0)} locations; that is a research "
    "target, not a planning value.",
    f"<b>Managed tonnage.</b> On the status-index series (M1) the landfill is most probable at 50 USD/t in "
    f"{m50['managed_M1_status_index'].get('SL',0)} of 19 locations, on the local-first series (M2) in "
    f"{m50['managed_M2_local_first'].get('SL',0)} of 21.",
    f"<b>Climate metric.</b> With GWP20 the landfill loses its lead at 50 USD/t everywhere (RDF + AD "
    f"{m50['GWP20'].get('S5',0)}, WtE {m50['GWP20'].get('S1',0)}): the choice of time horizon is as consequential as the carbon value."]):
    A(t_)
A(figure(OUT / "fig_tradeoff.png", 16, "Figure 9: GHG avoided against extra cost, Monte Carlo medians, one dot per location and option."))
A(figure(OUT / "fig_sensitivity.png", 16, "Figure 10: Inputs that drive the results: mean absolute Spearman rank correlation over the locations."))
A(P("For climate the landfill-gas collection efficiency dominates every option that landfills residues, followed by the "
    "plastic share (fossil CO<sub>2</sub> from WtE), the garden share and moisture. For cost: landfill cost, WtE and AD "
    "CAPEX, RDF O&amp;M, the AD front-end share and PHB cost and price. These are the values to verify first."))


# ---------------------------------------------------------------------------------------------- 10-12 (v3.1)
A(PageBreak())
A(P("9 Break-even targets and the value of information", "h1"))
A(P("Facilities do not yet exist, so their performance is uncertain. Two analyses turn that uncertainty into guidance for "
    "stakeholders. <b>Break-even targets</b>: with all other inputs central, one input is swept and the value at which an "
    "option's carbon-inclusive cost equals the landfill's is found (<i>mswpath.thresholds</i>). These are targets a tender, a "
    "pilot or an offtake contract must reach. <b>Value of information</b>: for a fixed carbon value the net benefit of option "
    "k in draw i is NB<sub>ik</sub> = &minus;CIC<sub>ik</sub>. The expected value of perfect information,"))
A(eqrow(r"EVPI=\mathrm{E}_i\left[\max_k NB_{ik}\right]-\max_k \mathrm{E}_i\left[NB_{ik}\right],\qquad "
        r"EVPPI(X)=\mathrm{E}_X\left[\max_k \mathrm{E}(NB_k\,|\,X)\right]-\max_k \mathrm{E}\left[NB_k\right]", "voi", "12"))
A(P("is the most a city should pay, per tonne, to remove all uncertainty before choosing; EVPPI is the same for one group of "
    "inputs X that one measurement campaign would resolve. E(NB<sub>k</sub> | X) is estimated by regression on X (Strong et al. "
    "2014; quadratic, additive in the inputs of a group), and the EVPPI of random noise is subtracted as a bias floor. "
    "Multiplied by the 2025 tonnage, the values are in USD per year."))
rows = [["Input (S5 RDF + AD vs SL, 100 USD/t)", "Central", "Better if", "Break-even: median (range)", "n crossing", "never", "already"]]
A(table(rows + thr_summary(d, "S5", 100), [4.6, 1.4, 1.5, 4.2, 1.7, 1.4, 1.6]))
rows = [["Input (S5 RDF + AD vs SL, 50 USD/t)", "Central", "Better if", "Break-even: median (range)", "n crossing", "never", "already"]]
A(table(rows + thr_summary(d, "S5", 50), [4.6, 1.4, 1.5, 4.2, 1.7, 1.4, 1.6]))
A(P("Table 7a: Break-even values for RDF + AD against the landfill over the 21 locations (units as in the register: "
    "k<sub>mech</sub> &ndash;; c<sub>pre</sub> and RDF O&amp;M USD/t; RDF price USD/GJ; collection efficiency &ndash;). "
    "'never': no value in the tested range closes the gap; 'already': RDF + AD is cheaper over the whole range; 'n crossing': "
    "locations where a break-even exists. Other options in <i>outputs/17_break_even_thresholds_vs_SL.csv</i>.", "cap"))
A(P("At 50 USD/t RDF + AD matches the landfill only if RDF O&amp;M falls well below the verified 18.4 USD/t, the RDF price "
    "rises several-fold above today's 1.15 USD/GJ, or landfill-gas collection is poor (below about 0.25). At 100 USD/t it is "
    "already cheaper in most locations and stays so unless RDF O&amp;M exceeds about 29 USD/t or collection exceeds about 0.66. "
    "For a city these are concrete conditions to write into a tender: a maximum gate fee for RDF processing, a minimum kiln "
    "price, and a measured collection efficiency for the landfill alternative."))
vg = N["voi_top"]
rows = [["Measurement group", "25 USD/t", "50 USD/t", "100 USD/t", "Max over locations at 100, USD/yr"]]
order = vg[100].group.tolist()
for g_ in order:
    r_ = [g_]
    for pc in (25, 50, 100):
        q = vg[pc].set_index("group").loc[g_]
        r_.append(f"{q.median_evppi:.3f}")
    r_.append(f"{vg[100].set_index('group').loc[g_].max_usd_per_year:,.0f}")
    rows.append(r_)
rows.append(["EVPI (all inputs), median USD/t"] + [f"{N['evpi'][pc]:.2f}" for pc in (25, 50, 100)] +
            [f"{N['evpi_yr_max'][100]:,.0f}"])
A(table(rows, [6.4, 2.2, 2.2, 2.2, 4.0]))
A(P("Table 7b: Value of information, median EVPPI over 21 locations in USD per tonne MSW (market case, 4,000 draws). The "
    "last column multiplies by the location's 2025 tonnage.", "cap"))
A(figure(OUT / "fig_voi.png", 16.5, "Figure 10a: Value of information by measurement group at three carbon values: median over "
         "21 locations (bar) and maximum (dot)."))
A(P(f"Uncertainty is worth little at 25 USD/t (EVPI {N['evpi'][25]:.2f} USD/t), because the landfill wins almost regardless. "
    f"At 100 USD/t the EVPI rises to {N['evpi'][100]:.2f} USD/t, about {N['evpi_yr'][100]:,.0f} USD per year for the median "
    "location, and waste characterisation (as-received moisture and composition) and landfill-gas performance carry most "
    "of it, followed by WtE performance and cost. Group values are conservative (additive regression within a group, noise "
    "floor subtracted), so they need not add up to the EVPI. The rule for a city is direct: if a local waste-characterisation campaign with "
    "proximate analysis, or a landfill-gas pumping test, costs less than its EVPPI for one year, it should be done before "
    "any technology commitment. AD-specific data matter only where food can be separated at source."))

A(P("10 Fossil energy (CED) and land take (v3.1)", "h1"))
A(P("Two indicators are added to climate change and cost. Both reuse the inventory of the existing model, so they "
    "add no new flows, only conversion factors (<i>data/lcia_factors.csv</i>, drawn after all v3 parameters so that G "
    "and C are unchanged; validation check). <b>CED</b> is the non-renewable (fossil) cumulative energy demand in MJ "
    "per tonne MSW; biogenic energy is not counted and a negative value is a net saving:"))
A(eqrow(r"\mathrm{CED}_k=\sum \mathrm{kWh}_{in}\,\mathrm{PEF}+\sum \frac{a_i}{ef_{diesel}}(1+u_d)"
        r"-E_{el}\,\mathrm{PEF}-\psi E_{net}(1+u_c)-P\,\sigma\,\mathrm{CED}_{PP},\qquad \mathrm{PEF}=\frac{3.6}{\eta_{fossil}}(1+u_f)\,k_{EF}", "ced", "12"))
A(P("PEF is the primary fossil energy per kWh of grid electricity, scaled with the same multiplier k<sub>EF</sub> as the "
    "grid emission factor so that a decarbonising grid lowers both. Diesel-type ancillary burdens a<sub>i</sub> (kg "
    "CO<sub>2</sub>e) are converted with the IPCC diesel factor. <b>Land take</b> is the landfill area consumed "
    "permanently plus plant footprints over their lifetime, in m<super>2</super> per tonne MSW:"))
A(eqrow(r"LU_k=\left(\frac{m_{LF}}{\rho_{SL}H_{SL}}+\frac{m_{inert}}{\rho_{inert}H_{SL}}\right)f_{gross}"
        r"+\sum_u \frac{fp_u\,m_u}{0.85\cdot 365\,n},\qquad LU_{OD}=\frac{m}{\rho_{OD}H_{OD}}", "lu", "13"))
lc = d["lcia"]
rows = [["Key", "Meaning", "Central (range)", "Unit", "Source", "St."]]
for r in lc.itertuples():
    rows.append([r.key, r.meaning, f"{r.central:g}" + (f" ({r.low:g} to {r.high:g})" if r.low != r.high else ""), r.unit, r.source, r.status])
A(table(rows, [1.6, 5.6, 2.2, 1.5, 5.3, 0.6]))
A(P("Table 8: CED and land-use factors. Most land-use factors are analyst engineering ranges; the thermal efficiency "
    "and the polypropylene CED are literature values still to be verified against their originals (status L). An "
    "Indonesian TPA design study (Talumelito, Gorontalo) implies about 0.045 m<super>2</super>/t, below the central "
    "0.081 m<super>2</super>/t used here.", "cap"))
rows = [["Option", "CED, GJ/t: median (range)", "Saving vs open dump, GJ/t", "Land take, m2/t: median (range)", "Saved vs open dump, m2/t"]] + lcia_table(d)
A(table(rows, [3.0, 3.8, 3.0, 4.0, 3.2]))
A(P("Table 9: Fossil CED and land take per tonne MSW, central values, market case, 21 locations.", "cap"))
A(P(f"WtE, RDF and RDF + AD save {-N['ced_med']['S1']/1e3:.1f}, {-N['ced_med']['S2']/1e3:.1f} and "
    f"{-N['ced_med']['S5']/1e3:.1f} GJ of fossil energy per tonne (displaced grid electricity and kiln coal), AD alone "
    f"{-N['ced_med']['S3']/1e3:.1f} GJ and PHB {-N['ced_med']['S4']/1e3:.1f} GJ (displaced polypropylene); the sanitary "
    f"landfill consumes {N['ced_med']['SL']:.0f} MJ for operation. Land take is dominated by landfilling: an open dump "
    f"uses about 0.40 m<super>2</super>/t, a sanitary landfill {N['lu_med']['SL']:.3f}, RDF + AD {N['lu_med']['S5']:.3f} "
    f"and WtE {N['lu_med']['S1']:.3f} m<super>2</super>/t, mostly for ash. The energy ranking follows the climate ranking; "
    "land take adds a reason to divert waste from landfill where land is scarce, such as Java."))
A(figure(OUT / "fig_ced_landuse.png", 15.5, "Figure 11: Fossil CED and land take of each option, Monte Carlo medians over the 21 locations."))

A(P("11 Conditions under which each option becomes preferable: scenario discovery", "h1"))
A(P("The research question asks for conditions, not for a ranking of 21 cases. The model was therefore run over 40,000 "
    "combinations of conditions that a planner can observe or choose: carbon value (0 to 100 USD/t CO<sub>2</sub>e), "
    "waste to the facility (50 to 3,000 t/day, log-uniform), road distance to a cement kiln (10 to 400 km), grid region, "
    "availability of the Perpres 109/2025 tariff, source separation of food, composition, moisture, landfill-gas "
    "collection and landfill cost; all other parameters were sampled from the register. Composition was drawn from a "
    f"Dirichlet distribution centred on the mean of the 21 RIPS compositions with concentration {d['meta']['dirichlet_alpha0']:.0f} fitted to their "
    "between-city spread (method of moments), so the rules describe compositions like those observed and are "
    "extrapolations outside them. For each draw the best feasible option (lowest carbon-inclusive cost) was recorded. A "
    f"classification tree of depth 4 reproduces that choice in {100*d['cart_acc']['acc_test']:.0f}% of held-out draws "
    f"(majority-class baseline {100*d['cart_acc']['baseline']:.0f}%); PRIM (Friedman and Fisher 1999) searched for boxes "
    "of conditions in which one option is best with high density."))
A(table([["Rule (conditions)", "Best option", "Purity", "Share of draws"]] + rules_table(d), [10.2, 2.8, 1.5, 2.5]))
A(P("Table 10: Leaves of the decision tree covering at least 3% of draws (<i>outputs/discovery_cart_rules.csv</i>). "
    "Purity: share of the draws in the leaf where the predicted option is indeed best. Q tpd: t/day to the facility; "
    "kiln km: road distance.", "cap"))
pr = d["prim"]
rows = [["Option", "Base rate", "Coverage", "Density", "Box (conditions)"]]
for r in pr.itertuples():
    rows.append([NAMES[r.option], f"{r.base_rate:.3f}", fmt(r.coverage, ".2f"), fmt(r.density, ".2f"),
                 r.box if isinstance(r.box, str) else r.note])
A(table(rows, [2.6, 1.4, 1.4, 1.4, 10.2]))
A(P("Table 11: PRIM boxes. Coverage: share of all draws where the option is best that fall in the box; density: "
    "share of draws in the box where it is best. RDF alone and PHB are best in fewer than 3% of draws.", "cap"))
im = d["imp"]
_pr = d["prim"].set_index("option")
prim_box_text = lambda k: str(_pr.loc[k, "box"]).replace("_", " ").replace("; ", ", ")
prim_den = lambda k: float(_pr.loc[k, "density"])
prim_lim = lambda k: str(_pr.loc[k, "box"]).split(";")[0].split(" to ")[-1] + " USD/t"
A(P("<b>Reading the rules.</b> The carbon value (importance "
    f"{im['carbon_value']:.2f}), source separation of food ({im['food_separated']:.2f}), plant scale "
    f"({im['Q_tpd']:.2f}) and kiln distance ({im['kiln_km']:.2f}) carry the decision; heating value and the tariff matter "
    "only for WtE. For <b>mixed waste without the WtE tariff</b>, the sanitary landfill with flare is best in "
    f"{100*N['disc_mix'].get('SL',0):.0f}% of draws and RDF + AD in {100*N['disc_mix'].get('S5',0):.0f}%. The rules are:"))
for t in bullets([
    f"<b>Sanitary landfill with flare</b> is preferred for mixed waste without the WtE tariff in most of the condition "
    f"space; its densest PRIM box is a carbon value below about {prim_lim('SL')} with no tariff and no source separation "
    f"(density {prim_den('SL'):.2f}).",
    "<b>RDF + AD</b> is the only recovery option that can beat the landfill for mixed waste without the tariff, and only "
    f"in a narrow region: {prim_box_text('S5')} (PRIM density {prim_den('S5'):.2f}). It never forms a tree leaf, i.e. it "
    "is never the majority choice in any broad region of conditions.",
    f"<b>WtE</b> is preferred when the Perpres 109/2025 tariff applies, at least 1,000 t/day are delivered and the LHV is "
    f"at least 7 MJ/kg (PRIM density {prim_den('S1'):.2f}); without the tariff it is rarely best.",
    f"<b>AD alone</b> is preferred when food waste arrives separated at source and the carbon value exceeds about "
    f"30&ndash;45 USD/t (tree split at 28 USD/t; PRIM: {prim_box_text('S3')}).",
    "<b>RDF alone and PHB</b> are best in fewer than 3% of all draws: they are dominated by RDF + AD and by flaring."]):
    A(t)
A(figure(OUT / "fig_condition_maps.png", 15.5, "Figure 12: Condition maps. Each cell shows the most frequent best "
         "option and the share of draws in which it is best, separately for mixed waste without tariff, mixed waste "
         "with the Perpres 109/2025 tariff and food separated at source."))
A(P(f"<b>Sobol indices.</b> To complement the rank correlations, first-order and total Sobol indices (Saltelli sampling, "
    f"N = 512 base samples, {d['sobol'].input.nunique() - 1} register inputs plus the common wetness draw; composition fixed at each location's value) "
    "were computed for every location and averaged."))
sg = N["sobol_gap"]
rows = [["Output", "Most influential inputs: total index ST (first-order S1)"]]
nm = {"wetness": "moisture (wetness)", "cap": "landfill-gas collection", "o_rdf": "RDF O&amp;M", "K_ad": "AD CAPEX",
      "p_rdf": "RDF price", "c_sl": "landfill cost", "doc_k": "DOC x DOCf multiplier", "gamma": "garden share of organik",
      "efg_k": "grid factor multiplier", "eta_wte": "WtE efficiency", "h_k": "LHV calibration", "ox": "cover oxidation"}
for lab, t in (("Carbon-inclusive cost gap RDF + AD minus landfill at 50 USD/t", N["sobol_gap"]), ("GHG of the sanitary landfill", N["sobol_gsl"]),
               ("GHG of WtE", N["sobol_gs1"])):
    rows.append([lab, "; ".join(f"{nm.get(r.input, r.input)} {r.ST:.2f} ({r.S1:.2f})" for r in t.itertuples())])
A(table(rows, [5.0, 12.0]))
A(P("Table 12: Sobol indices, mean over the 21 locations (<i>outputs/discovery_sobol_mean.csv</i>).", "cap"))
A(P("Moisture as received and landfill-gas collection efficiency together explain about half of the variance of the "
    "carbon-inclusive cost gap between RDF + AD and the landfill, which is why the moisture scenario of Section 8 can reverse the "
    "ranking. These two quantities, together with RDF operating cost and price, are the data to measure first."))
A(figure(OUT / "fig_sobol.png", 15, "Figure 13: Sobol indices for the carbon-inclusive cost gap RDF + AD minus landfill and for "
         "the GHG of the landfill, mean over 21 locations."))

A(P("12 A reproducible decision tool for researchers and planners", "h1"))
A(P("The model is packaged as the Python package <i>mswpath</i> (core model, input template and validator, scenario "
    "discovery, reports and an interactive form) and exposed through <i>MSW_Decision_Tool_Colab.ipynb</i>, which runs in "
    "Google Colab without installation. The notebook (i) reproduces the results for the 21 RIPS locations; (ii) accepts "
    "a one-row-per-city template (<i>templates/city_input_template.xlsx</i>, with an English/Indonesian column guide), "
    "validates it, applies the same single-projection and normalisation rules and reports every assumption it applies; "
    "(iii) offers an interactive form for one city; (iv) re-runs scenario discovery and applies the decision tree to a "
    "city described by the user; (v) reports break-even targets and the value of information for a chosen city; and (vi) writes Excel and HTML reports in English or Indonesian that carry the caveats "
    "and the data-quality warnings. A round-trip test confirms that two RIPS locations entered through the template "
    "reproduce the database results. Versions are pinned in <i>requirements.txt</i>, random seeds are fixed and "
    "<i>CITATION.cff</i> gives the citation."))
A(P("<b>One-city notebook for researchers and readers.</b> <i>MSW_Single_City_Colab.ipynb</i> runs the complete analysis "
    "for a single city whose data are entered by hand, either in <i>templates/single_city_template.xlsx</i> (sheets "
    "<i>city_data</i>, <i>composition</i> and the optional <i>local_parameters</i>; the same content is provided as three CSV "
    "files) or typed directly into the notebook. Domestic and non-domestic compositions and tonnages can be given "
    "separately; local measured values (for example the moisture of food waste, the landfill cost or the RDF price) "
    "replace the defaults and are listed in the report. The notebook shows each step (characterisation, central results, "
    "decision at fixed carbon values, uncertainty, scenarios, break-even targets and value of information) and exports an "
    "Excel report. With the Kota Padang example and the default seed it reproduces the worked example exactly; this is a "
    "validation check."))

# ---------------------------------------------------------------------------------------------- 9
A(P("13 Limits of validity", "h1"))
for t in bullets([
    "<b>Ex-ante screening.</b> No option is built in the study locations, so results are estimates for decision support "
    "before investment, not an evaluation of plants. Break-even targets and the value of information say what a "
    "feasibility study must confirm; they do not replace it.",
    "<b>Secondary-data check.</b> The October 2026 verification used abstracts, indexed records and search extracts because "
    "full texts could not be downloaded; values marked L should be checked against the full texts before publication.",
    "<b>Temporal proxy.</b> Compositions sampled 2014 to 2025 represent 2025 without a trend; eight tonnages have an unverified "
    "year. Results describe a 2025 baseline <i>conditional</i> on these proxies, not a measured 2025 state.",
    "<b>Managed-waste definitions.</b> No single indicator exists for all 21 locations. Rankings on managed tonnage are valid "
    "only within series M1, and series M2 mixes definitions. Padang's share is provisional.",
    "<b>Moisture and heating value.</b> No fraction-level moisture or LHV has been measured for the study locations; the "
    "LHV values are legacy. The moisture scenarios and stress tests show that this choice can reverse the ranking between "
    "landfill and RDF + AD at high carbon values, and the value-of-information analysis ranks local proximate analysis "
    "first.",
    "<b>Level II/III.</b> Level II/III compositions are model allocations and their parameters are proxies; they cannot support "
    "fraction-level conclusions (for example on recycling of specific resins) until local sorting data exist.",
    "<b>Foreign benchmarks.</b> Danish residual household waste is not Indonesian mixed MSW and not non-domestic waste.",
    "<b>Global defaults.</b> IPCC defaults are global and describe waste as generated, not at the landfill gate.",
    "<b>Carbon-inclusive cost.</b> Only greenhouse gases are valued; health, air pollution, odour, leachate and jobs are not. "
    "CED and land take are reported but not monetised.",
    "<b>Screening level.</b> One impact category in the decision; no time dynamics of landfill gas; class-5 costs not escalated; provisional "
    "coordinates; independent triangular inputs; Status L values still to be checked against the originals.",
    "<b>Coverage.</b> 21 locations, 12 in Central Java; conclusions about Indonesian conditions in general need more regions."]):
    A(t)

A(P("14 Data gaps carried forward", "h1"))
A(P("Measured fraction-level moisture and LHV (local proximate analysis); DOCf for rubber and leather; material composition "
    "of coated board, composite film, batteries, HHW and WEEE; Level III food and miscellaneous-paper sorting data; the year "
    "of the overview tonnage index; the definition and denominator of the RIPS status index; whether Padang's 141.76 t/d is "
    "actual or potential recovery; chlorine and sulphur of RDF; verified plant and kiln coordinates; the numerical tables of "
    "Riber et al. (2009). The full list with the decision taken for each item is in CHANGELOG_AND_SOURCE_DECISIONS.md."))

# ---------------------------------------------------------------------------------------------- refs
A(P("References", "h1"))
REFS = [
 "Anshassi, M., Smallwood, T., Townsend, T.G. (2022). Life cycle GHG emissions of MSW landfilling versus incineration: expected outcomes based on US landfill gas collection regulations. Waste Management 142, 44-54.",
 "Astrup, T., M&oslash;ller, J., Fruergaard, T. (2009). Incineration and co-combustion of waste: accounting of greenhouse gases and global warming contributions. Waste Management &amp; Research 27, 789-799.",
 "Astrup, T.F., Tonini, D., Turconi, R., Boldrin, A. (2015). Life cycle assessment of thermal waste-to-energy technologies: review and recommendations. Waste Management 37, 104-115.",
 "Azis, M.M., Kristanto, J., Purnomo, C.W. (2021). A techno-economic evaluation of municipal solid waste (MSW) conversion to energy in Indonesia. Sustainability 13, 7232.",
 "Barlaz, M.A., Chanton, J.P., Green, R.B. (2009). Controls on landfill gas collection efficiency: instantaneous and lifetime performance. Journal of the Air &amp; Waste Management Association 59, 1399-1404.",
 "Chidambarampadmavathy, K., Karthikeyan, O.P., Heimann, K. (2017). Sustainable bio-plastic production through landfill methane recycling. Renewable and Sustainable Energy Reviews 71, 555-562.",
 "Edjabou, M.E., Jensen, M.B., G&ouml;tze, R., Pivnenko, K., Petersen, C., Scheutz, C., Astrup, T.F. (2015). Municipal solid waste composition: sampling methodology, statistical analyses, and case study evaluation. Waste Management 36, 12-23. https://doi.org/10.1016/j.wasman.2014.11.009",
 "Edjabou, M.E., Mart&iacute;n-Fern&aacute;ndez, J.A., Scheutz, C., Astrup, T.F. (2017). Statistical analysis of solid waste composition data: arithmetic mean, standard deviation and correlation coefficients. Waste Management 69, 13-23.",
 "European Union (2012). Directive 2012/19/EU on waste electrical and electronic equipment (WEEE).",
 "G&ouml;tze, R., Boldrin, A., Scheutz, C., Astrup, T.F. (2016). Physico-chemical characterisation of material fractions in household waste: overview of data in literature. Waste Management 49, 3-14. https://doi.org/10.1016/j.wasman.2016.01.008",
 "GIZ ERiC-DKTI (2023). Kajian Analisis Potensi Off-taker Refuse Derived Fuel (RDF). Jakarta.",
 "IPCC (2006). 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Volume 5 Waste, Chapter 2 (Table 2.4) and Chapter 3.",
 "IPCC (2019). 2019 Refinement to the 2006 IPCC Guidelines, Volume 5, Chapter 3 (Table 3.0); corrected July 2023.",
 "IPCC (2021). Climate Change 2021: The Physical Science Basis (AR6 WG1), Chapter 7, Table 7.15.",
 "Nasrullah, M., Vainikka, P., Hannula, J., Hurme, M., K&auml;rki, J. (2014). Mass, energy and material balances of SRF production process. Part 1: SRF produced from commercial and industrial waste. Waste Management 34, 1398-1407.",
 "Prabowo, B., Simanjuntak, F.S.H., Saldi, Z.S., Samyudia, Y., Widjojo, I.J. (2019). Assessment of waste to energy technology in Indonesia: a techno-economical perspective on a 1000 ton/day scenario. International Journal of Technology 10, 1228-1234.",
 "Raharjo, S., Ariska, R. (2022). Waste-to-energy study for Padang (Jurnal Presipitasi). Reviewed; its heating values are not Padang laboratory data.",
 "Riber, C., Petersen, C., Christensen, T.H. (2009). Chemical composition of material fractions in Danish household waste. Waste Management 29, 1251-1257. https://doi.org/10.1016/j.wasman.2008.09.013",
 "Republic of Indonesia. UU 18/2008; UU 7/2021; Perpres 109/2025; SNI 19-3964-1994; RIPS of the 21 cities/regencies (row-level locators in input_kota_2025_updated.csv).",
 "Tchobanoglous, G., Theisen, H., Vigil, S. (1993). Integrated Solid Waste Management. McGraw-Hill.",
 "Zhang, R., El-Mashad, H.M., Hartman, K., et al. (2007). Characterization of food waste as feedstock for anaerobic digestion. Bioresource Technology 98, 929-935.",
 "Seruga, P., et al. (2020). Anaerobic digestion performance: separate collected vs. mechanical segregated organic fractions of municipal solid waste as feedstock. Energies 13.",
 "Basinas, P., et al. (2020). Assessment of high-solid mesophilic and thermophilic anaerobic digestion of mechanically-separated municipal solid waste. Environmental Research.",
 "Basinas, P., et al. (2021). Dry anaerobic digestion of the fine particle fraction of mechanically-sorted organic fraction of municipal solid waste in laboratory and pilot reactor. Waste Management.",
 "Yuliani, M., et al. (2022). Kajian tekno-ekonomi penerapan insinerator waste-to-energy di Indonesia (kasus pada Kota X). Jurnal Teknologi Lingkungan.",
 "Zeng, J.-C., et al. (2024). Environmental, energy, and techno-economic assessment of waste-to-energy incineration. Sustainability.",
 "Febijanto, I., et al. (2024). Municipal solid waste reduction through incineration for electricity purposes and its environmental performance: a case study in Bantargebang, West Java, Indonesia. Evergreen.",
 "Ministry of Energy and Mineral Resources (ESDM) (2018). Grid emission factors of the Indonesian electricity systems, as listed for the Joint Crediting Mechanism by GEC (jcmsbsd30_emission_factor).",
 "Strong, M., Oakley, J.E., Brennan, A. (2014). Estimating multiparameter partial expected value of perfect information from a probabilistic sensitivity analysis sample: a nonparametric regression approach. Medical Decision Making 34, 311-326.",
 "Friedman, J.H., Fisher, N.I. (1999). Bump hunting in high-dimensional data. Statistics and Computing 9, 123-143.",
 "Saltelli, A., Annoni, P., Azzini, I., Campolongo, F., Ratto, M., Tarantola, S. (2010). Variance based sensitivity analysis of model output: design and estimator for the total sensitivity index. Computer Physics Communications 181, 259-270.",
 "Breiman, L., Friedman, J.H., Olshen, R.A., Stone, C.J. (1984). Classification and Regression Trees. Wadsworth.",
 "Frischknecht, R., Wyss, F., B&uuml;sser Kn&ouml;pfel, S., L&uuml;tzkendorf, T., Balouktsi, M. (2015). Cumulative energy demand in LCA: the energy harvested approach. International Journal of Life Cycle Assessment 20, 957-969.",
 "Further references for register entries (Manfredi et al. 2009; Mayer et al. 2019; Rand et al. 2000; Silva et al. 2021; Tsilemou &amp; Panagiotakopoulos 2006; World Bank 2024; Levett et al. 2016; Rostkowski et al. 2012; Wei et al. 2024) are as listed in the v2 methodology.",
]
for r in REFS:
    A(P(r, "ref"))

doc = SimpleDocTemplate(str(DOCS / "MSW_Methodology_v3_EN.pdf"), pagesize=A4, leftMargin=2 * cm, rightMargin=2 * cm,
                        topMargin=1.8 * cm, bottomMargin=1.6 * cm, title="MSW Methodology v3",
                        author="Muhammad Fachri Ridwan")
doc.build(story, onFirstPage=page_deco(TITLE), onLaterPages=page_deco(TITLE))
print("wrote", DOCS / "MSW_Methodology_v3_EN.pdf")

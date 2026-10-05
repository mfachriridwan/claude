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
TITLE = "MSW recovery pathways in Indonesia: methodology v3 (2025 baseline)"

# ---------------------------------------------------------------------------------------------- title
A(P("Screening LCA and TEA of Municipal Solid Waste<br/>Recovery Pathways in Indonesia", "title"))
A(P("Methodology v3: 2025 tonnage baseline, Level I/II/III taxonomy, auditable material parameters, "
    "managed-waste candidates and updated results for 21 locations", "subtitle"))
A(P("Muhammad Fachri Ridwan &mdash; Master of Environmental Management, The University of Queensland<br/>"
    "Supervisor: Prof. Anthony Halog &mdash; October 2026 &mdash; Companion code: msw_pathway_model_v3.py / "
    "MSW_pathway_model_v3.ipynb", "subtitle"))
A(Spacer(1, 6))
mp = N["mp"]
box = [P("<b>What changed in v3 and what it shows</b>", "box")] + bullets([
    "<b>One source of truth.</b> Every parameter, constant, coordinate and threshold is read from <i>data/*.csv</i>, "
    "which <i>build_database_v3.py</i> builds from the update package. The model file holds no numbers. "
    f"{N['valid'][0]} of {N['valid'][1]} validation checks pass (Section 7).",
    "<b>2025 baseline with three separate years.</b> Composition sampling year, tonnage source year and baseline year are "
    "stored separately. Tonnage is projected once, Q<sub>2025</sub> = Q<sub>t</sub>(1+g)<super>2025&minus;t</super>; the "
    "parser refuses the old file, so a second projection cannot happen. "
    f"{(inp.tonnage_year_status == 'overview_index_year_unverified').sum()} overview totals have an unverified year; they are "
    "used as reported and a projected alternative is run as a scenario. They are not labelled as 2025 observations.",
    "<b>Composition is a proxy.</b> RIPS percentages from the sampling year (2014 to 2025) are normalised per stream "
    "(factor recorded) and used for 2025 without any trend. They are not a measured 2025 composition.",
    "<b>Managed waste (Table 5) kept in two series.</b> M1 is the RIPS status index (19 locations); M2 uses a local "
    "indicator where one is held. The two use different definitions; differences reach 86 percentage points. Padang's "
    "93.71% is provisional (the 141.76 t/d recovery may be a potential); Serang's 14% is a target, not a baseline.",
    "<b>Taxonomy and parameters.</b> 10 Level I groups, 36 Level II fractions and a provisional catalogue of 56 Level III "
    "refinements, keyed by (level, code). Level II/III parameters are parent-category proxies or explicit gaps; Level II/III "
    "allocations are priors and never enter the model results. Rubber/leather DOCf is a declared gap (scenario 0 to 0.5).",
    "<b>Results barely move; their status is clearer.</b> At central values sanitary landfill with flare has the lowest "
    f"social cost everywhere up to USD 25/t CO<sub>2</sub>e; RDF + AD is best in {N['best50'].get('S5', 0)} locations at "
    f"USD 50 and {N['best100'].get('S5', 0)} at USD 100. Over the sampled carbon range, landfill is most probable in "
    f"{mp.get('SL', 0)} locations and RDF + AD in {mp.get('S5', 0)}, but {N['n_ties']} of the 21 leads are below 0.05. "
    "The ranking is sensitive to the moisture assumption: with IPCC default moisture RDF + AD becomes most probable in "
    f"{N['mp_all']['moisture_IPCC_default'].get('S5', 0)} locations.",
    "<b>Still provisional.</b> Composition proxies of different years, legacy heating values and RDF transfer coefficients, "
    "status L literature values and class-5 costs. Section 9 states the limits of validity."], "box")
A(boxed(box))

# ---------------------------------------------------------------------------------------------- 1
A(P("1 Scope of the update and vocabulary of evidence", "h1"))
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
A(eqrow(r"G_3=V\rho_{CH_4}f_{AD}GWP_{CH_4}+a\,a_A+\pi(e_REF_{grid}+a_R)-E_{AD}EF_{grid}+G_{LF}(s-a\,e_{food},\delta a)", "g3", "9"))
A(eqrow(r"G_4=G_{SL}+P\,(ef_{PHB}-\sigma\,ef_{PP}),\quad P=\eta\,\mathrm{CH_4}(s,1)/R\qquad G_5=G_{AD}(a)+G_{RDF}(s-a\,e_{food})+G_{LF}(\mathrm{rej},\delta a)", "g45", "10"))
A(P("Techno-economic assessment and decision:"))
A(eqrow(r"CRF=\frac{r(1+r)^n}{(1+r)^n-1},\quad c^{cap}_u=\frac{CRF\,K_{ref}\,(q/0.85/q_{ref})^b}{365\,q},\quad "
        r"SC_k=C_k+p\,G_k/10^3,\quad MAC_k=10^3\,\frac{C_k-C_0}{G_0-G_k}", "tea", "11"))
A(P("The gates G1 to G6 (WtE LHV &ge; 7 MJ/kg and Q &ge; 150 t/d, PSEL tariff at Q &ge; 1,000 t/d, RDF NCV &ge; 12.56 MJ/kg, "
    "kiln within 300 km road distance, PHB &ge; 500 t/yr) are read from <i>model_constants.csv</i>. Feasible options are "
    "screened by Pareto dominance on (G, C), and the probability of being best is the share of Monte Carlo draws in which an "
    "option has the lowest social cost with the carbon value p sampled uniformly from 0 to 100 USD/t CO<sub>2</sub>e."))

# ---------------------------------------------------------------------------------------------- 5
A(P("5 Uncertainty: priors, sampling noise and scenarios kept apart", "h1"))
A(P("Three kinds of uncertainty are distinguished. (1) <b>Composition</b>: a Dirichlet distribution around the RIPS proxy, "
    "S ~ Dir(&alpha;<sub>0</sub> s), with &alpha;<sub>0</sub> = 80, or 40 where the data are flagged. These concentrations are "
    "<i>heuristics</i>, not statistically calibrated: no replicate sorting campaigns exist for these locations to estimate "
    "them. The Dirichlet therefore represents prior uncertainty about how well an 8-day sample of a past year represents "
    "2025, not measured sampling variability. (2) <b>Parameters</b>: triangular distributions from the register (Section 6), "
    "a common wetness draw for moisture, the harmonisation priors &gamma; and yard, the rubber DOCf gap (0 to 0.5) and the "
    "carbon value. (3) <b>Data scenarios</b>: discrete alternatives that are not given probabilities (Table S4)."))
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
         ["food_separated_at_source", "No front-end sorting cost for AD (&pi; = 0)", "what-if"],
         ["residues_to_open_dump", "Residues dumped instead of landfilled (illegal; sensitivity only)", "what-if"],
         ["managed_M1_status_index", "Plant size = status-index share &times; Q2025 (19 locations)", "data scenario"],
         ["managed_M2_local_first", "Plant size = local indicator where held, else status index (21; mixed definitions)", "data scenario"],
         ["GWP20", "20-year horizon for methane", "method"],
         ["moisture_IPCC_default", "IPCC moisture (as generated) instead of as-received values", "data scenario"],
         ["tonnage_overview_projected", "Overview totals projected from the composition year", "data scenario"],
         ["docf_rubber_0.5", "Rubber/leather DOCf at the IPCC 2006 generic default 0.5", "data gap"]],
        [4.0, 10.6, 2.4]))
A(P("Table S4: Scenarios. Each uses the same random numbers per location, so differences come only from what the scenario "
    "changes. With 4,000 draws, probability differences below about 0.03 are noise.", "cap"))

# ---------------------------------------------------------------------------------------------- 6
A(P("6 Assumption register", "h1"))
A(P("Status: V = checked against the source text during this project; L = literature value recalled from the cited source "
    "and still to be checked against the original before submission; A = analyst assumption. Evidence type follows Table 1. "
    "The register is <i>data/assumption_register.csv</i>; the table below is generated from it."))
rows = [["ID", "Key", "Meaning", "Central (range)", "Unit", "Source", "St."]]
for r in reg.itertuples():
    rng_ = num(r.central) + (f" ({num(r.low)} to {num(r.high)})" if r.low != r.high else "")
    rows.append([r.id, r.key, r.meaning, rng_, r.unit, r.source, r.status])
A(table(rows, [0.9, 1.6, 6.0, 2.4, 1.6, 3.9, 0.6]))
A(P("Table 3: Assumption register (61 entries). New or changed in v3: H14-H15 (heuristic Dirichlet concentrations made "
    "explicit), C10b (plastic LHV range), C12 (rubber DOCf gap scenario, previously a fixed 0.5).", "cap"))

# ---------------------------------------------------------------------------------------------- 7
A(P("7 Validation", "h1"))
v = d["val"]
A(P(f"<i>validate_v3.py</i> ran {len(v)} checks on the database and the parser; all {N['valid'][0]} passed. The groups "
    "requested in the brief were all run:"))
rows = [["Group", "Checks", "Examples of what is tested"]]
ex = {"blank vs zero": "blank DOCf stays blank; parser treats blank and reported-zero glass without imputation; old file rejected",
      "taxonomy": "(level, code) unique in 3 tables; 10/36/56 counts; every parent exists; 6.* documented",
      "mass balance": "M_dom + M_nd = Q2025; Level I = 100%; Level II = parent; terminal partition = 100% (78 leaves)",
      "parent-child": "Level III = Level II parent; detail file partial (76.8-93.7%); conditional shares only for complete sets",
      "projection": "Q2025 = Qt(1+g)^expo once; exponent 0 when already 2025; overview year blank and flagged",
      "Qmanaged": "M1, M2 <= Q2025; Serang 7.45% not 14%; Padang provisional with lower bound",
      "wet/dry": "DOC_dry(1-w) = IPCC wet DOC; LHV dry basis, LHV not HHV; DOCf separate from AD yield; soil/wood separated",
      "provenance": "every row has source, locator and status; proxies keep proxy status; heuristics labelled"}
for g, q in v.groupby("group", sort=False):
    rows.append([g, f"{(q.result == 'PASS').sum()}/{len(q)}", ex.get(g, "")])
A(table(rows, [2.6, 1.3, 13.1]))
A(P("Table S5: Validation checks by group (full list in <i>outputs/validation_report.md</i>). In addition the model asserts "
    "the single projection for every location, the S5 mass balance and that biogas carbon is below food carbon.", "cap"))
b = d["bench"]
rows = [["Quantity", "Model, 21 locations", "Reference"]] + [[i, r.iloc[0], r.iloc[1]] for i, r in b.iterrows()]
A(table(rows, [3.6, 4.0, 9.4]))
A(P("Table 4: Model outputs against published Indonesian values (main case). The model's bulk moisture is at the dry end "
    "and its heating value at the high end of the measured range, as in v2.", "cap"))

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
mc = d["mc"]; w = mc[mc.scenario == "market"].pivot(index="city", columns="pathway", values="p_best").reindex(ch.city)
bst = d["best"][d["best"].scenario == "market"].set_index("city")
rows = [["Location", "Flags"] + PW + ["0", "25", "50", "100"]]
for c_ in ch.itertuples():
    p = w.loc[c_.city]; srt = np.sort(p.values)
    row = [c_.city, c_.flags if isinstance(c_.flags, str) else ""]
    for k in PW:
        s_ = f"{p[k]:.2f}"
        if p[k] == srt[-1]: s_ = f"<b>{s_}</b>" + ("*" if srt[-1] - srt[-2] < 0.05 else "")
        row.append(s_)
    row += [bst.loc[c_.city, f"best_at_{pc}"] for pc in (0, 25, 50, 100)]
    rows.append(row)
A(table(rows, [3.4, 1.0] + [1.15] * 6 + [1.1] * 4))
A(P("Table 7: Probability of being the best feasible option in the market case (carbon value sampled from 0 to 100 USD/t), "
    "and the best option at central values for carbon values of 0, 25, 50 and 100. Bold: most probable; *: lead below 0.05. "
    "Flags: L lumped organics, W kayu as garden, R rounded percentages, G glass in lain-lain, N glass not reported, T tonnage "
    "year unverified.", "cap"))
A(figure(OUT / "fig_probability_best.png", 17, "Figure 7: Probability of being the best feasible option in three scenarios."))
A(figure(fig_robust(d), 15.5, "Figure 8: Most probable option per location in each scenario (colour) and its probability (number)."))
mpa = N["mp_all"]
A(P("<b>What the numbers say.</b>", "body"))
for t in bullets([
    f"<b>Landfill first, RDF + AD at moderate carbon values.</b> Sanitary landfill with flare has the lowest social cost in "
    f"all 21 locations at 0 and 25 USD/t. RDF + AD is best in {N['best50'].get('S5',0)} locations at USD 50 and "
    f"{N['best100'].get('S5',0)} at USD 100; its abatement cost against landfill is {N['s5_mac'][0]:.0f} "
    f"({N['s5_mac'][1]:.0f} to {N['s5_mac'][2]:.0f}) USD/t CO<sub>2</sub>e. Over the sampled carbon range the two are "
    f"close: {N['n_ties']} of 21 leads are below 0.05.",
    f"<b>WtE</b> needs a heating value of at least 7 MJ/kg, 1,000 t/day and the Perpres 109/2025 tariff together; with the "
    f"tariff it is most probable in {mpa['perpres109'].get('S1',0)} locations (Serang, Semarang, Brebes). Brebes qualifies "
    "on generated waste only; 2.3% of it is managed (status index). Semarang's tonnage is an overview total of unverified "
    "year.",
    f"<b>AD alone</b> becomes most probable in {mpa['food_separated_at_source'].get('S3',0)} locations if food arrives "
    "separated, and almost never for mixed waste. <b>PHB</b> is not preferred anywhere; it avoids only 14 (11 to 20) kg "
    "CO<sub>2</sub>e/t against a landfill at more than USD 990/t CO<sub>2</sub>e.",
    f"<b>Managed tonnage.</b> On the status-index series (M1) landfill is most probable in {mpa['managed_M1_status_index'].get('SL',0)} "
    f"of 19 locations; on the local-first series (M2) in {mpa['managed_M2_local_first'].get('SL',0)} of 21. Plants are "
    "expensive at today's managed tonnage under both definitions.",
    "<b>Data scenarios.</b> Projecting the overview totals and raising rubber DOCf to 0.5 change no most-probable option. "
    f"IPCC default moisture changes it in most locations: RDF + AD becomes most probable in "
    f"{mpa['moisture_IPCC_default'].get('S5',0)} locations, because drier waste carries more degradable carbon into the "
    "landfill baseline and more heat into RDF. GWP20 has a similar effect "
    f"({mpa['GWP20'].get('S5',0)} RDF + AD, {mpa['GWP20'].get('S1',0)} WtE)."]):
    A(t)
A(figure(OUT / "fig_tradeoff.png", 16, "Figure 9: GHG avoided against extra cost, Monte Carlo medians, one dot per location and option."))
A(figure(OUT / "fig_sensitivity.png", 16, "Figure 10: Inputs that drive the results: mean absolute Spearman rank correlation over the locations."))
A(P("For climate the landfill-gas collection efficiency dominates every option that landfills residues, followed by the "
    "plastic share (fossil CO<sub>2</sub> from WtE), the garden share and moisture. For cost: landfill cost, WtE and AD "
    "CAPEX, RDF O&amp;M, the AD front-end share and PHB cost and price. These are the values to verify first."))

# ---------------------------------------------------------------------------------------------- 9
A(P("9 Limits of validity", "h1"))
for t in bullets([
    "<b>Temporal proxy.</b> Compositions sampled 2014 to 2025 represent 2025 without a trend; eight tonnages have an unverified "
    "year. Results describe a 2025 baseline <i>conditional</i> on these proxies, not a measured 2025 state.",
    "<b>Managed-waste definitions.</b> No single indicator exists for all 21 locations. Rankings on managed tonnage are valid "
    "only within series M1, and series M2 mixes definitions. Padang's share is provisional.",
    "<b>Moisture and heating value.</b> No fraction-level moisture or LHV has been measured for the study locations; the "
    "as-received moisture is calibrated to two published samples and the LHV values are legacy. The moisture scenario shows "
    "that this choice can reverse the ranking between landfill and RDF + AD. Local proximate analysis is the priority.",
    "<b>Level II/III.</b> Level II/III compositions are model allocations and their parameters are proxies; they cannot support "
    "fraction-level conclusions (for example on recycling of specific resins) until local sorting data exist.",
    "<b>Foreign benchmarks.</b> Danish residual household waste is not Indonesian mixed MSW and not non-domestic waste.",
    "<b>Global defaults.</b> IPCC defaults are global and describe waste as generated, not at the landfill gate.",
    "<b>Screening level.</b> One impact category; no time dynamics of landfill gas; class-5 costs not escalated; provisional "
    "coordinates; independent triangular inputs; Status L values still to be checked against the originals.",
    "<b>Coverage.</b> 21 locations, 12 in Central Java; conclusions about Indonesian conditions in general need more regions."]):
    A(t)

A(P("10 Data gaps carried forward", "h1"))
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
 "Further references for register entries (Manfredi et al. 2009; Mayer et al. 2019; Rand et al. 2000; Silva et al. 2021; Tsilemou &amp; Panagiotakopoulos 2006; World Bank 2024; Levett et al. 2016; Rostkowski et al. 2012; Wei et al. 2024) are as listed in the v2 methodology.",
]
for r in REFS:
    A(P(r, "ref"))

doc = SimpleDocTemplate(str(DOCS / "MSW_Methodology_v3_EN.pdf"), pagesize=A4, leftMargin=2 * cm, rightMargin=2 * cm,
                        topMargin=1.8 * cm, bottomMargin=1.6 * cm, title="MSW Methodology v3",
                        author="Muhammad Fachri Ridwan")
doc.build(story, onFirstPage=page_deco(TITLE), onLaterPages=page_deco(TITLE))
print("wrote", DOCS / "MSW_Methodology_v3_EN.pdf")

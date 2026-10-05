"""Build docs/MSW_Manuscript_v3_EN.pdf, a journal-style research article drafted from the v3 results."""
import pandas as pd, numpy as np
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.platypus import SimpleDocTemplate, Spacer, PageBreak
from doccommon import *

d = load(); N = stats(d)
inp, t2, t5, ch, mc, det = d["inp"], d["t2"], d["t5"], d["char"], d["mc"], d["det"]
mpa = N["mp_all"]
x = det[(det.scenario == "market")]
med = lambda bg, k, c: x[(x.baseline == bg) & (x.pathway == k)][c]
rng3 = lambda s, f=".0f": f"{s.median():{f}} ({s.min():{f}}&ndash;{s.max():{f}})"
ov = inp[inp.tonnage_year_status == "overview_index_year_unverified"]
cov = d["cover"]; us = d["usplit"]
story = []; A = story.append
RUN = "Provenance-aware screening of MSW recovery pathways in Indonesia"

A(P("Draft manuscript for submission to a Q1 waste-management journal &mdash; not peer reviewed &mdash; October 2026", "subtitle"))
A(Spacer(1, 4))
A(P("When does resource recovery beat a sanitary landfill? A provenance-aware screening LCA and TEA of six "
    "municipal solid waste pathways for 21 Indonesian cities and regencies", "title"))
A(P("Muhammad Fachri Ridwan<super>a,*</super>, Anthony Halog<super>a</super>", "subtitle"))
A(P("<super>a</super> The University of Queensland, Brisbane, Australia (affiliation details to be completed)<br/>"
    "<super>*</super> Corresponding author. E-mail: mfachri0411@gmail.com<br/>"
    "<i>Author list, affiliations and contributions to be confirmed by the authors before submission.</i>", "subtitle"))
A(Spacer(1, 6))

abstract = (
    "Indonesian local governments must choose between sanitary landfilling and recovery pathways with waste data that are "
    "sparse, of different years and of uneven definition. We present an open, provenance-aware screening model that compares "
    "six options per tonne of mixed municipal solid waste (MSW) at the facility gate in 2025: sanitary landfill with flare, "
    "waste-to-energy (WtE), refuse-derived fuel (RDF) for cement kilns, anaerobic digestion (AD) of food waste, "
    "polyhydroxybutyrate (PHB) from landfill gas, and integrated RDF + AD. All pathways share one LCA and TEA boundary, "
    "including residue disposal. Composition data from local master plans (RIPS) of 21 cities and regencies were harmonised to "
    "a 2025 baseline with a single, audited projection of tonnage, and every value was labelled as local measurement, "
    "literature default, proxy, assumption, scenario or gap. Feasibility gates, Pareto screening and the probability of being "
    "best over a sampled carbon value (0&ndash;100 USD/t CO<sub>2</sub>e) were combined with Monte Carlo simulation. At central "
    f"values sanitary landfill with flare had the lowest social cost in all locations up to 25 USD/t CO<sub>2</sub>e, whereas "
    f"RDF + AD was best in {N['best50'].get('S5',0)} locations at 50 USD/t and {N['best100'].get('S5',0)} at 100 USD/t, with an "
    f"abatement cost of {N['s5_mac'][0]:.0f} ({N['s5_mac'][1]:.0f}&ndash;{N['s5_mac'][2]:.0f}) USD/t CO<sub>2</sub>e against "
    "landfill. WtE was preferred only with the Perpres 109/2025 tariff in three large locations, AD only for source-separated "
    "food waste, and PHB nowhere. Scenario discovery over 40,000 combinations of conditions condensed these results into "
    "decision rules: for mixed waste, landfill with flare is preferred below about 40 USD/t CO<sub>2</sub>e and wherever no "
    "cement kiln lies within about 300 km, RDF + AD above about 55 USD/t with a kiln in reach, WtE only with the tariff, "
    "at least 1,000 t/day and an LHV of at least 7 MJ/kg, and AD where food is separated at source. RDF + AD and WtE also "
    "saved the most fossil energy (about 4 GJ/t), and every recovery option reduced land take relative to landfilling. "
    "The ranking between landfill and RDF + AD was robust to the tonnage-year and data-gap "
    "scenarios but not to the moisture assumption: with IPCC default moisture instead of as-received values, RDF + AD became "
    f"most probable in {mpa['moisture_IPCC_default'].get('S5',0)} of 21 locations. Measured as-received moisture and heating "
    "value, and a common definition of managed waste, are therefore the data that would most improve such decisions. "
    "The model is released as a Google Colab tool that researchers and planners can run for their own cities.")
A(P("<b>Abstract</b>", "h2")); A(P(abstract, "body"))
A(P("<b>Highlights</b>", "h2"))
for t in bullets(["Six MSW options compared per tonne within one LCA/TEA boundary for 21 Indonesian locations",
                  "Every input labelled as measurement, default, proxy, assumption, scenario or gap",
                  "Landfill with flare is cheapest below 25 USD/t CO2e; RDF + AD leads at 50-100 USD/t",
                  "WtE needs the Perpres 109/2025 tariff and 1,000 t/day; PHB is not preferred anywhere",
                  "Decision rules from scenario discovery state when each option wins",
                  "The moisture assumption, not tonnage year or data gaps, can reverse the ranking"]):
    A(t)
A(P("<b>Keywords:</b> municipal solid waste; life cycle assessment; techno-economic assessment; refuse-derived fuel; "
    "anaerobic digestion; data provenance; scenario discovery; Indonesia", "body"))

# ------------------------------------------------------------------------------------------- 1
A(P("1. Introduction", "h1"))
A(P("Comparing waste-treatment options per tonne of waste, with credits for the energy and materials they displace, is "
    "established practice in waste LCA (Laurent et al., 2014; Astrup et al., 2015; Khandelwal et al., 2019). Such comparisons "
    "are informative for decisions only if three conditions hold: the options deliver the same function within equivalent "
    "system boundaries (ISO, 2006), every assumption is stated, and uncertainty is carried to the conclusion (Clavreul et al., "
    "2012; Bisinella et al., 2017). The first condition is often violated when recovery pathways treat only part of the waste "
    "and the remainder silently leaves the system; the third is weakened when composition, which is itself uncertain, is "
    "treated as known (Edjabou et al., 2017; Bisinella et al., 2017)."))
A(P("For Indonesian cities and regencies a fourth condition becomes critical: the provenance of the data. Local waste "
    "master plans (Rencana Induk Pengelolaan Sampah, RIPS) report composition and tonnage measured in different years with "
    "different category definitions, tonnage totals whose reference year is not always stated, and managed-waste indicators "
    "that use different numerators and denominators. National policy meanwhile creates scale-dependent incentives: Perpres "
    "109/2025 grants a WtE electricity tariff to facilities of at least 1,000 t/day. Fraction-level properties such as moisture, "
    "heating value and degradable organic carbon (DOC) are rarely measured locally, so models fall back on global IPCC "
    "defaults (IPCC, 2006, 2019) or on values from other countries, for example the detailed Danish household-waste "
    "characterisation of Edjabou et al. (2015). When such values enter a model without a status label, a proxy can be "
    "mistaken for a measurement and an analyst's assumption for a city observation."))
A(P("This study asks under what Indonesian city and waste-system conditions each recovery pathway becomes preferable, and "
    "which data limitations could change the answer. Its contributions are: (i) a screening model in which six options manage "
    "the whole tonne within one boundary, with sanitary landfill as both a baseline and an option; (ii) a reproducible data "
    "layer that harmonises 21 RIPS records to a 2025 baseline with a single, audited tonnage projection and labels every value "
    "by its evidence type; (iii) a three-level waste-fraction taxonomy with explicit parameter gaps, kept separate from the "
    "computation so that unverified allocation priors cannot influence results; and (iv) a decision analysis that reports the "
    "probability of each option being best over a range of carbon values, together with data scenarios that isolate the effect "
    "of individual provenance decisions; and (v) scenario discovery that turns the model into explicit decision rules, "
    "released with the model as a reproducible Google Colab tool for other cities."))

# ------------------------------------------------------------------------------------------- 2
A(P("2. Materials and methods", "h1"))
A(P("2.1 Goal, scope and functional unit", "h2"))
A(P("The functional unit is the management of 1 t of mixed MSW, wet weight, as received at the gate of the treatment "
    "facility, with the composition of the location, in 2025. Six options are compared: sanitary landfill with gas collection "
    "and flare (SL), WtE by grate incineration (S1), RDF co-processed in the nearest cement kiln (S2), wet AD of food waste "
    "separated from mixed waste (S3), PHB produced from captured landfill gas (S4) and an integrated plant in which an RDF line "
    "also separates food for a digester (S5). Every option includes its process energy and materials, direct emissions "
    "(fossil CO<sub>2</sub>, CH<sub>4</sub>, N<sub>2</sub>O), the landfill of its residues (ash, rejects, uncaptured waste, "
    "digestate), RDF haulage and the credit for the product it displaces (grid electricity, coal, polypropylene). Collection, "
    "capital goods, biogenic CO<sub>2</sub>, landfill carbon storage and recycling credits are excluded for all options. "
    "Results are expressed against two baselines, an open dump (OD) and a sanitary landfill, as &Delta;G<sub>k</sub> = "
    "G<sub>0</sub> &minus; G<sub>k</sub>. Climate change is assessed with GWP100 of IPCC AR6 (non-fossil CH<sub>4</sub> 27.0, "
    "N<sub>2</sub>O 273), GWP20 being a sensitivity case. Costs are net levelised costs in USD per tonne (AACE class 5)."))
A(P("2.2 Study locations and data harmonisation to 2025", "h2"))
A(P(f"The 21 locations comprise {(inp.admin_level=='kota').sum()} cities (kota) and {(inp.admin_level=='kabupaten').sum()} "
    "regencies (kabupaten) in West Sumatra, West Java, Banten, Central Java, East Java, Bali and East Kalimantan, with 2025 "
    f"tonnage from {inp.Q2025_tpd.min():.0f} to {inp.Q2025_tpd.max():,.0f} t/day. For each location three years are stored "
    "separately: the composition sampling year t<sub>c</sub> (2014&ndash;2025), the tonnage source year t and the baseline year. "
    "Tonnage is projected once,"))
A(eqrow(r"Q_{2025}=Q_t\,(1+g)^{\,2025-t}", "m_q", "1"))
A(P("with the RIPS growth rate g where available and otherwise the median of the six reported rates (1.73% yr<super>&minus;1"
    "</super>). Domestic and non-domestic tonnage use the same factor. The model parser reads Q<sub>2025</sub> directly and "
    "asserts that the tonnage it uses equals the database value, which prevents a second projection. "
    f"Eight totals come from an overview index whose reference year could not be verified; they are used as reported, their "
    f"year is left blank, and a projection from the composition year (up to +{100*(ov.Q2025_alt_tpd/ov.Q2025_tpd-1).max():.0f}%) "
    "is run as a scenario. Composition is used as a proxy for 2025 without any trend, normalised per stream with the factor "
    "recorded, and the two streams are weighted by their 2025 tonnages:"))
A(eqrow(r"s_j=\frac{M^{d}_{2025}\,s^{(d)}_j+M^{n}_{2025}\,s^{(n)}_j}{M^{d}_{2025}+M^{n}_{2025}},\qquad "
        r"s^{(k)}_j=x^{(k)}_j/\sum_i x^{(k)}_i", "m_s", "2"))
A(P("The 12 RIPS categories (plus glass) are mapped to ten IPCC-aligned model fractions. Two harmonisation rules handle "
    "inconsistent category definitions: where organics are reported as one aggregate, a share &gamma; (median 0.27, range "
    "0.04&ndash;0.62 from the locations that report both) is moved to garden waste, and where wood is at least 10% and no leaf "
    "category exists, it is treated as garden waste. Managed waste, which determines plant size in the managed-tonnage "
    "scenarios, is held in two series that are never merged silently: the RIPS status index (M1, 19 locations) and a series "
    "that uses local service or handling indicators where available (M2). The two differ by up to "
    f"{t5.definition_difference_pp.max():.0f} percentage points for the same location (Fig. 1). A derived share for Padang "
    "(93.7%) depends on a recovery figure that may be a potential, so only its disposal-only lower bound (71.8%) enters M2; a "
    "14% target for Serang is treated as a scenario, not a baseline."))
A(figure(fig_qmanaged(d), 12.5, "Fig. 1. Managed share of 2025 waste by indicator family. The RIPS status index and local "
         "indicators measure different things; grey lines join values for the same location."))
A(P("2.3 Waste-fraction taxonomy and material parameters", "h2"))
A(P("Following the tiered approach of Edjabou et al. (2015), the database holds 10 Level I groups, 36 Level II fractions and "
    "a provisional catalogue of 56 Level III refinements, keyed by (level, code). Level II/III compositions obtained by "
    "allocating RIPS aggregates with priors are retained for future calibration but are not used in the computation, because "
    "several priors have no Indonesian or literature basis. Danish benchmark compositions were converted to conditional "
    "shares only for complete sibling sets on the same basis (11 sets). Parameters of the ten model fractions are the IPCC "
    "(2006) Table 2.4 defaults for DOC, total carbon and fossil share and the IPCC (2019) Table 3.0 DOCf values (0.70 food and "
    "garden, 0.50 paper and textile, 0.10 wood). DOC is on a dry basis and multiplies dry mass, so moisture is applied once "
    "(DOC<sub>wet</sub> = DOC<sub>dry</sub>(1 &minus; w)). Rubber and leather have no verified DOCf; the gap is sampled between "
    "0 and the generic IPCC default of 0.5. Plastic uses the aggregate median LHV of 30.5 MJ/kg dry solids reported by "
    "G&ouml;tze et al. (2016); other dry-matter heating values and the RDF transfer coefficients are legacy assumptions. "
    "Moisture as received (food 0.75, plastic 0.22, paper 0.35) is an assumption calibrated to bulk moisture measured at "
    "Indonesian transfer points (Prabowo et al., 2019); IPCC moisture, which describes waste as generated, is run as a "
    f"scenario. At Level II/III a DOC value is known for only {cov.has_DOC_dry_fraction.min():.0f}&ndash;"
    f"{cov.has_DOC_dry_fraction.max():.0f}% and a DOCf for {cov.has_DOCf.min():.0f}&ndash;{cov.has_DOCf.max():.0f}% of the "
    "wet mass, which is why the computation remains at the ten model fractions (Fig. 2)."))
A(figure(fig_metamodel(), 16, "Fig. 2. Metamodel of the data and computation. The upper chain produces results; the Level "
         "I/II/III chain is reporting and calibration content that cannot alter results."))
A(P("2.4 Life-cycle inventory and cost model", "h2"))
A(P("For a stream with fraction masses m<sub>j</sub> per tonne MSW, methane generated in a landfill, fossil CO<sub>2</sub> "
    "on combustion and the lower heating value as received are"))
A(eqrow(r"\mathrm{CH_4}=10^3\,k_D\,\mathrm{MCF}\,F\,\frac{16}{12}\sum_j m_j(1-w_j)\mathrm{DOC}_j\mathrm{DOCf}_j,\quad "
        r"E_{fos}=10^3\frac{44}{12}\sum_j m_j(1-w_j)C_j\varphi_j,\quad H=\sum_j k_{h,j}s_jh_j(1-w_j)-\lambda\sum_j s_jw_j", "m_c", "3"))
A(P("The landfill module applies MCF = 0.8 and no gas collection for the open dump, and MCF = 1, lifetime collection "
    "efficiency &eta; = 0.5 (0.3&ndash;0.8) and 10% cover oxidation for the sanitary landfill. WtE uses a net electrical "
    "efficiency of 0.18 (0.14&ndash;0.22). The RDF line sorts a fixed share of each fraction, dries the product to 20% moisture "
    "with its own fuel, hauls it to the nearest kiln and credits displaced coal at 0.9 GJ per GJ; rejects are landfilled. AD "
    "captures 70% of food waste, converts it with a realised yield of 0.36 Nm<super>3</super> CH<sub>4</sub>/kg VS and 5% "
    "fugitive loss, and charges a front-end separation step as 40% of the RDF-line cost. PHB is produced from captured gas at "
    "2.3 t CH<sub>4</sub>/t PHB and credited against polypropylene. Costs combine annualised CAPEX scaled with capacity to the "
    "power b = 0.7, O&amp;M, residue landfill cost that falls with landfill size, haulage and product revenue. All 61 "
    "parameters, their ranges and their sources are listed in the assumption register (Supplementary Table S3)."))
A(P("Two further indicators reuse the same inventory. Fossil cumulative energy demand (CED) converts grid electricity "
    "with a primary-energy factor of 3.6/&eta; MJ/kWh (&eta; = 0.32, range 0.28&ndash;0.36, plus fuel supply), diesel-type "
    "ancillary burdens with the IPCC diesel factor, displaced kiln coal by its energy content plus supply, and displaced "
    "polypropylene with its cradle-to-gate CED; biogenic energy is not counted (Frischknecht et al., 2015). Land take is "
    "the landfill area consumed per tonne, 1/(&rho;H) times a gross-area factor (&rho; = 0.8 t/m<super>3</super>, H = 20 m "
    "for a sanitary landfill; 0.5 t/m<super>3</super> and 5 m for an open dump), plus plant footprints over their "
    "lifetime. Most land-use factors are engineering assumptions and are sampled over wide ranges."))
A(P("2.5 Decision analysis, uncertainty and scenarios", "h2"))
A(P("An option is removed where it fails a gate: WtE requires LHV &ge; 7 MJ/kg and at least 150 t/day (Rand et al., 2000); "
    "RDF requires a net calorific value of at least 12.56 MJ/kg and a kiln within 300 km by road; PHB requires at least "
    "500 t/yr. Among feasible options, the social cost SC<sub>k</sub> = C<sub>k</sub> + p G<sub>k</sub>/10<super>3</super> is "
    "computed for a carbon value p sampled uniformly between 0 and 100 USD/t CO<sub>2</sub>e, following the logic of "
    "stochastic multicriteria acceptability analysis (Lahdelma and Salminen, 2001). The probability of being best is the share "
    "of 4,000 Monte Carlo draws per location in which an option has the lowest social cost. Composition is sampled from a "
    "Dirichlet distribution S ~ Dir(&alpha;<sub>0</sub>s) with heuristic concentrations (&alpha;<sub>0</sub> = 80, or 40 for "
    "flagged data) that are not statistically calibrated; parameters are sampled from triangular distributions. The two sources "
    "were also sampled separately to show their contributions. Ten scenarios were run with common random numbers: the market "
    "case, the Perpres 109/2025 tariff, food separated at source, residues to an open dump, the two managed-tonnage series, "
    "GWP20, IPCC moisture, projected overview tonnage and rubber DOCf of 0.5. Sensitivity was measured by Spearman rank "
    "correlation within the Monte Carlo sample and by Sobol indices (Saltelli et al., 2010) for every location."))
A(P("2.6 Scenario discovery", "h2"))
A(P("To answer the research question in terms of conditions rather than cases, the model was run over 40,000 "
    "combinations of conditions a planner can observe or choose: carbon value (0&ndash;100 USD/t CO<sub>2</sub>e), waste to the "
    "facility (50&ndash;3,000 t/day), road distance to a cement kiln (10&ndash;400 km), grid region, availability of the WtE "
    "tariff, source separation of food, composition, moisture, landfill-gas collection and landfill cost, with the other "
    "parameters sampled from the register. Composition was drawn from a Dirichlet distribution centred on the mean RIPS "
    f"composition with a concentration of {d['meta']['dirichlet_alpha0']:.0f} fitted to the between-city spread. The best "
    "feasible option of each draw was explained with a classification tree of depth 4 (Breiman et al., 1984) and with PRIM "
    "boxes (Friedman and Fisher, 1999), following the scenario-discovery approach of decision making under deep "
    "uncertainty."))
A(P("2.7 Verification", "h2"))
A(P(f"An automated suite of {N['valid'][1]} checks (all passed) tests blank-versus-zero handling, uniqueness of taxonomy keys, "
    "domestic plus non-domestic mass balance, parent-child sums at every level, the single projection to 2025, managed "
    "tonnage not exceeding total tonnage, wet/dry consistency (the dry DOC and IPCC moisture reproduce the IPCC wet DOC within "
    "0.005) and the presence of a source, locator and status for every value. Model outputs were compared with published "
    "Indonesian measurements (Table 1)."))

# ------------------------------------------------------------------------------------------- 3
A(P("3. Results", "h1"))
A(P("3.1 Waste characteristics and feasibility", "h2"))
b = d["bench"]
rows = [["Quantity", "Model (21 locations)", "Reference"]] + [[i, r.iloc[0], r.iloc[1]] for i, r in b.iterrows()]
A(table(rows, [3.4, 4.0, 9.6]))
A(P("Table 1. Characteristics of the harmonised 2025 waste compared with published Indonesian values.", "cap"))
A(P(f"Bulk moisture as received ranged from {ch.moisture.min():.2f} to {ch.moisture.max():.2f} and LHV from "
    f"{N['lhv'][0]:.1f} to {N['lhv'][1]:.1f} MJ/kg (median {N['lhv'][2]:.1f}), at the dry and energetic end of measured "
    f"Indonesian values (Table 1). {N['g1']} of 21 locations passed the WtE heating-value gate and {N['g12']} passed both the "
    f"heating-value and the supply gate. {N['psel_gen']} locations generate at least 1,000 t/day, but on managed waste none "
    f"does in series M1 and only Kab. Bogor does in series M2. Methane potential at MCF = 1 was {ch.L0_sl.min():.0f}&ndash;"
    f"{ch.L0_sl.max():.0f} kg CH<sub>4</sub>/t, and the RDF yield {ch.rdf_yield.min():.2f}&ndash;{ch.rdf_yield.max():.2f} "
    "t/t, within the range reported for Indonesian RDF plants."))
A(P("3.2 Climate and cost performance", "h2"))
rows = [["Option", "G", "&Delta;G vs open dump", "&Delta;G vs landfill", "C", "MAC vs OD", "MAC vs SL", "Pass"]] + pathway_table(d)
A(table(rows, [2.8, 2.2, 2.4, 2.3, 1.9, 2.0, 2.4, 1.0]))
A(P("Table 2. Results per tonne of MSW at central values, market case: median (range) over 21 locations. G and &Delta;G in "
    "kg CO<sub>2</sub>e/t, C in USD/t, MAC in USD/t CO<sub>2</sub>e. Pass: number of locations passing the gates.", "cap"))
A(P(f"Against an open dump, WtE avoided {rng3(med('OD','S1','dG'))} kg CO<sub>2</sub>e/t and RDF + AD "
    f"{rng3(med('OD','S5','dG'))}; RDF alone {rng3(med('OD','S2','dG'))} and AD alone {rng3(med('OD','S3','dG'))}, because "
    f"each leaves part of the waste for the landfill. A sanitary landfill with flare already avoided {rng3(med('OD','SL','dG'))}. "
    f"Net costs were {rng3(med('OD','SL','C'))} USD/t for the landfill, {rng3(med('OD','S3','C'))} for AD, "
    f"{rng3(med('OD','S2','C'))} for RDF, {rng3(med('OD','S5','C'))} for RDF + AD and {rng3(med('OD','S1','C'))} for WtE at "
    f"market electricity value. PHB avoided only {rng3(med('SL','S4','dG'))} kg CO<sub>2</sub>e/t relative to the landfill it "
    f"is built on, at an abatement cost of {rng3(med('SL','S4','MAC'))} USD/t CO<sub>2</sub>e (Table 2; Fig. 3)."))
A(figure(OUT / "fig_tradeoff.png", 15.5, "Fig. 3. GHG avoided against extra cost relative to the open dump (left) and the "
         "sanitary landfill (right); Monte Carlo medians, one point per location and option; point size increases with the "
         "probability of passing the gates."))
A(P("3.3 Preferred options", "h2"))
A(P(f"At central values the sanitary landfill had the lowest social cost in all 21 locations at carbon values of 0 and "
    f"25 USD/t CO<sub>2</sub>e. RDF + AD was best in {N['best50'].get('S5',0)} locations at 50 USD/t and "
    f"{N['best100'].get('S5',0)} at 100 USD/t, while WtE was best only in Kab. Kutai Kartanegara at 100 USD/t, where no kiln "
    f"lies within reach for RDF. With the carbon value sampled, the landfill was the most probable choice in "
    f"{N['mp'].get('SL',0)} locations and RDF + AD in {N['mp'].get('S5',0)}, but in {N['n_ties']} locations the lead was below "
    "0.05 and both options are statistically close (Fig. 4). Averaged over locations, RDF + AD was on the Pareto front in "
    f"{100*N['avg'].loc['S5','p_front']:.0f}% of draws and the landfill in {100*N['avg'].loc['SL','p_front']:.0f}%; RDF + AD "
    f"was best in {100*N['avg'].loc['S5','p_best_pc50_100']:.0f}% of draws with carbon values of 50&ndash;100 USD/t."))
A(figure(OUT / "fig_probability_best.png", 16.5, "Fig. 4. Probability of being the best feasible option in the market "
         "case, with the Perpres 109/2025 WtE tariff, and with food waste separated at source."))
A(P(f"With the Perpres 109/2025 tariff, WtE became the most probable option in {mpa['perpres109'].get('S1',0)} locations "
    "(Kab. Serang, Kota Semarang and Kab. Brebes). Brebes qualifies on generated waste only, of which 2.3% is managed "
    "according to the status index, and Semarang's tonnage is an overview total of unverified year. If food waste arrived "
    f"separated, AD alone became most probable in {mpa['food_separated_at_source'].get('S3',0)} locations; for mixed waste the "
    "cost of separating food removes its advantage. On managed tonnage the landfill was the most probable option in "
    f"{mpa['managed_M1_status_index'].get('SL',0)} of 19 locations (M1) and {mpa['managed_M2_local_first'].get('SL',0)} of 21 "
    "(M2): at the tonnages managed today every recovery plant is small and expensive."))
A(P("3.4 Robustness to data provenance", "h2"))
A(P("Projecting the eight overview totals from their composition year and raising rubber DOCf to 0.5 changed no "
    "most-probable option (Fig. 5). The moisture assumption did: with IPCC default moisture, RDF + AD became the most probable "
    f"option in {mpa['moisture_IPCC_default'].get('S5',0)} locations, because drier waste carries more degradable carbon into "
    "the landfill and more energy into the RDF. A 20-year horizon for methane had a similar effect "
    f"({mpa['GWP20'].get('S5',0)} locations for RDF + AD and {mpa['GWP20'].get('S1',0)} for WtE). Sending residues to an open dump, "
    "which is illegal, made AD and RDF + AD cheaper in several locations because dumping residues is cheap."))
A(figure(fig_robust(d), 14.5, "Fig. 5. Most probable option per location in each scenario (colour) and its probability of "
         "being best (number). n.a.: no status-index value for the managed-tonnage series M1."))
A(P("3.5 Drivers of uncertainty", "h2"))
A(P("For climate results, the lifetime landfill-gas collection efficiency dominated every option that landfills residues, "
    "followed by the plastic share (fossil CO<sub>2</sub> in WtE), the garden share and moisture; for cost, the landfill cost, "
    "WtE and AD capital costs, RDF operating cost, the AD front-end share and PHB cost and price dominated (Supplementary "
    f"Fig. S1). Sampling composition alone produced a median 90% interval of {us.loc['S1', ('G_w90','composition only')]:.0f} "
    f"kg CO<sub>2</sub>e/t for WtE against {us.loc['S1', ('G_w90','parameters only')]:.0f} for parameters alone, whereas for the "
    f"landfill parameters dominated ({us.loc['SL', ('G_w90','parameters only')]:.0f} against "
    f"{us.loc['SL', ('G_w90','composition only')]:.0f}). Composition hardly affected cost."))

A(P(f"Sobol indices give the same picture: moisture as received (total index {N['sobol_gap'].iloc[0].ST:.2f}) and "
    f"landfill-gas collection ({N['sobol_gap'].iloc[1].ST:.2f}) explain about half of the variance of the social-cost gap "
    "between RDF + AD and the landfill at 50 USD/t CO<sub>2</sub>e, followed by RDF operating cost and price."))
A(P("3.6 Fossil energy and land take", "h2"))
rows = [["Option", "CED, GJ/t: median (range)", "Saving vs open dump", "Land take, m2/t: median (range)", "Saved vs open dump"]] + lcia_table(d)
A(table(rows, [3.0, 3.8, 2.6, 4.2, 2.6]))
A(P("Table 3. Fossil cumulative energy demand and land take per tonne of MSW, central values, market case.", "cap"))
A(P(f"RDF + AD, WtE and RDF saved {-N['ced_med']['S5']/1e3:.1f}, {-N['ced_med']['S1']/1e3:.1f} and "
    f"{-N['ced_med']['S2']/1e3:.1f} GJ of fossil energy per tonne by displacing grid electricity and kiln coal, AD alone "
    f"{-N['ced_med']['S3']/1e3:.1f} GJ and PHB {-N['ced_med']['S4']/1e3:.1f} GJ (Table 3). An open dump took about 0.40 "
    f"m<super>2</super> of land per tonne and a sanitary landfill {N['lu_med']['SL']:.3f} m<super>2</super>; RDF + AD reduced "
    f"this to {N['lu_med']['S5']:.3f} and WtE to {N['lu_med']['S1']:.3f} m<super>2</super>/t. Energy and climate rankings "
    "agree; land take is an additional argument for diversion where land is scarce."))
A(P("3.7 Conditions under which each option becomes preferable", "h2"))
A(table([["Rule (conditions)", "Best option", "Purity", "Share"]] + rules_table(d, 0.04), [10.4, 2.8, 1.4, 1.6]))
A(P(f"Table 4. Leaves of the decision tree covering at least 4% of the 40,000 draws. The tree reproduces the best "
    f"option in {100*d['cart_acc']['acc_test']:.0f}% of held-out draws (majority-class baseline "
    f"{100*d['cart_acc']['baseline']:.0f}%). Purity: share of draws in the leaf where the predicted option is best.", "cap"))
A(P(f"Carbon value, source separation of food, scale and kiln distance carried the decision (tree importances "
    f"{d['imp']['carbon_value']:.2f}, {d['imp']['food_separated']:.2f}, {d['imp']['Q_tpd']:.2f} and {d['imp']['kiln_km']:.2f}). "
    f"For mixed waste without the WtE tariff the landfill was best in {100*N['disc_mix'].get('SL',0):.0f}% of draws and "
    f"RDF + AD in {100*N['disc_mix'].get('S5',0):.0f}%. The landfill was preferred below about 40 USD/t CO<sub>2</sub>e (purity "
    "0.95) and, at any carbon value, where no kiln lay within about 300 km. RDF + AD was preferred above about 55 USD/t with "
    "a kiln in reach; PRIM located its box at carbon values above 51 USD/t, kiln distances below 296 km and gas collection "
    "below 0.68. WtE won almost only in the box defined by the tariff, at least 1,000 t/day and an LHV of at least 7 MJ/kg "
    "(density 0.97). AD alone won where food arrived separated and carbon was valued above about 30 USD/t. RDF alone and "
    "PHB were best in fewer than 3% of draws (Table 4; Fig. 6)."))
A(figure(OUT / "fig_condition_maps.png", 15.5, "Fig. 6. Condition maps: most frequent best option and its frequency (%) on "
         "grids of two conditions, for mixed waste without tariff, mixed waste with the Perpres 109/2025 tariff, and food "
         "separated at source."))

# ------------------------------------------------------------------------------------------- 4
A(P("4. Discussion", "h1"))
A(P("4.1 Conditions under which each pathway becomes preferable", "h2"))
A(P("The scenario discovery of Section 3.7 states the answer to the research question compactly; the 21 locations "
    "illustrate it. "
    "The screening identifies conditions rather than a single winner. A well-operated sanitary landfill with flaring is the "
    "least-cost compliant option wherever carbon is valued below about 25 USD/t CO<sub>2</sub>e, which includes Indonesia's "
    "current carbon tax of about 2 USD/t (UU 7/2021). Integrated RDF + AD becomes preferable once carbon is valued at roughly "
    f"{N['s5_mac'][0]:.0f} USD/t, provided a cement kiln is within reach and the plant receives enough waste; it is the fair "
    "counterpart of WtE because both treat the whole tonne, yet it costs less and passes the RDF quality gate in most locations. "
    "WtE requires the conjunction of a heating value above 7 MJ/kg, a supply of at least 1,000 t/day and the Perpres 109/2025 "
    "tariff; without the tariff it is preferred only where no kiln is accessible. AD alone is a pathway for source-separated "
    "food and market waste, not for mixed waste. PHB from landfill gas adds little climate benefit over flaring and does not "
    "cover its production cost at current prices, consistent with techno-economic assessments of methane-based PHB "
    "(Levett et al., 2016)."))
A(P("4.2 Why provenance matters", "h2"))
A(P("The data scenarios show which provenance decisions are consequential. The year assigned to overview tonnage totals "
    "changes plant scale by up to a fifth but no decision, and the unresolved DOCf of rubber and leather is immaterial because "
    "the fraction is small. Moisture, in contrast, is decisive. IPCC moisture values describe waste as generated; waste "
    "arriving at a facility in a humid climate is wetter, and the as-received values used here bring bulk moisture into the "
    "measured range (Prabowo et al., 2019). Because DOC, fossil carbon and heating value all multiply the same dry mass, moisture "
    "moves the landfill baseline and the recovery options in opposite directions. Managed-waste indicators are equally "
    "consequential for policy: the same location may report 11% or 97% depending on whether a status index or a "
    "reduction-plus-handling indicator is used, so the eligibility of a location for a 1,000 t/day WtE plant on managed waste "
    "depends on a definition rather than on the waste. Reporting both series, instead of filling gaps with numerically "
    "similar values, keeps this visible."))
A(P("The separation between the computation and the Level II/III hierarchy is a deliberate design choice. Fine-grained "
    "taxonomies are valuable for planning recycling and for designing sorting campaigns (Edjabou et al., 2015), but in the "
    "absence of local sorting data their sub-fraction shares are priors, and their properties are parent-category proxies. "
    "Allowing such priors to drive results would add apparent resolution without information. The database therefore records "
    "where the gaps lie, for example the material composition of coated cartons, composite films, batteries and WEEE, and the "
    "share of wet mass for which a parameter is known, so that future measurements can be added without changing the model."))
A(P("4.3 Implications for local planning", "h2"))
A(P("Three practical implications follow. First, upgrading open dumps to sanitary landfills with gas collection is the "
    "largest and cheapest single step in climate terms (about 425 kg CO<sub>2</sub>e/t at about 18 USD/t), and the quality of gas "
    "collection is the parameter that most influences every comparison. Second, recovery investment decisions should be "
    "conditioned on an explicit carbon value and on access to a cement kiln, rather than on technology preference. Third, "
    "before committing to WtE under Perpres 109/2025, local governments should verify heating value by proximate analysis "
    "of waste as received and confirm the tonnage that is actually collected, because both gates are close to their thresholds "
    "in several locations."))
A(P("4.4 Limitations", "h2"))
A(P("The study is a screening assessment. Compositions sampled between 2014 and 2025 represent 2025 without a trend, and "
    "eight tonnage totals have an unverified year. Fraction-level moisture and heating values have not been measured for the "
    "study locations; several heating values and the RDF transfer coefficients are legacy assumptions, and some literature "
    "values remain to be checked against their originals. Only climate change is assessed; landfill gas is assigned without "
    "time dynamics; costs are class-5 estimates without escalation; coordinates are provisional; inputs are treated as "
    "independent; and 12 of the 21 locations are in Central Java. The Dirichlet concentrations are heuristics, and the Danish "
    "benchmarks describe residual household waste, not Indonesian mixed MSW. Land-use factors and several CED factors "
    "are assumptions, and the decision rules approximate the model (70% accuracy) within the range of observed "
    "compositions. The next steps are local proximate analysis, a "
    "common managed-waste definition across RIPS documents, and full inventories in a dedicated LCA software to extend the "
    "assessment to other impact categories."))

# ------------------------------------------------------------------------------------------- 5
A(P("5. Conclusions", "h1"))
A(P("A provenance-aware screening of six MSW options for 21 Indonesian cities and regencies shows that, with equal system "
    "boundaries, a sanitary landfill with flaring is the least-cost option at low carbon values, integrated RDF + AD becomes "
    "preferable at carbon values of about 40&ndash;50 USD/t CO<sub>2</sub>e where a cement kiln is accessible, WtE depends on "
    "the Perpres 109/2025 tariff and large generated tonnage, AD alone suits source-separated food waste, and PHB from landfill "
    "gas is not competitive. Scenario discovery condenses these findings into rules that planners can check against their "
    "own carbon value, scale, kiln access, tariff and source separation. Labelling every input by its evidence type, projecting tonnage once and keeping managed-waste "
    "definitions apart did not overturn these conditions, but it revealed that the moisture of waste as received can. Measured "
    "as-received moisture and heating value, and a harmonised definition of managed waste, are the data investments that would "
    "most improve waste-infrastructure decisions in Indonesian cities."))

A(P("Data and code availability", "h2"))
A(P("The database, model, notebook, validation suite and the scripts that generate this manuscript are provided in the "
    "MSW_pathway_model_v3 package (build_database_v3.py, msw_pathway_model_v3.py, MSW_pathway_model_v3.ipynb, validate_v3.py). "
    "Row-level RIPS source locators are given in input_kota_2025_updated.csv. The package mswpath and the notebook "
    "MSW_Decision_Tool_Colab.ipynb let other researchers and planners run the model in Google Colab for their own cities "
    "from a one-row-per-city template, re-run the scenario discovery and download Excel/HTML reports in English or "
    "Indonesian."))
A(P("CRediT authorship contribution statement (draft)", "h2"))
A(P("Muhammad Fachri Ridwan: Conceptualization, Data curation, Methodology, Software, Formal analysis, Writing &ndash; original "
    "draft. Anthony Halog: Conceptualization, Supervision, Writing &ndash; review and editing. To be confirmed by the authors."))
A(P("Declaration of generative AI in the writing process", "h2"))
A(P("During preparation of this draft the authors used Claude (Anthropic) to restructure the model code, generate the data "
    "pipeline and draft text from model outputs. The authors must review and edit the content and take full responsibility "
    "for the publication."))
A(P("Declaration of competing interest", "h2"))
A(P("To be completed by the authors."))

A(P("References", "h1"))
REFS = [
 "Astrup, T.F., Tonini, D., Turconi, R., Boldrin, A., 2015. Life cycle assessment of thermal waste-to-energy technologies: review and recommendations. Waste Manag. 37, 104&ndash;115.",
 "Azis, M.M., Kristanto, J., Purnomo, C.W., 2021. A techno-economic evaluation of municipal solid waste (MSW) conversion to energy in Indonesia. Sustainability 13, 7232.",
 "Bisinella, V., G&ouml;tze, R., Conradsen, K., Damgaard, A., Christensen, T.H., Astrup, T.F., 2017. Importance of waste composition for life cycle assessment of waste management solutions. J. Clean. Prod. 164, 1180&ndash;1191.",
 "Clavreul, J., Guyonnet, D., Christensen, T.H., 2012. Quantifying uncertainty in LCA-modelling of waste management systems. Waste Manag. 32, 2482&ndash;2495.",
 "Edjabou, M.E., Jensen, M.B., G&ouml;tze, R., Pivnenko, K., Petersen, C., Scheutz, C., Astrup, T.F., 2015. Municipal solid waste composition: sampling methodology, statistical analyses, and case study evaluation. Waste Manag. 36, 12&ndash;23. https://doi.org/10.1016/j.wasman.2014.11.009",
 "Edjabou, M.E., Mart&iacute;n-Fern&aacute;ndez, J.A., Scheutz, C., Astrup, T.F., 2017. Statistical analysis of solid waste composition data: arithmetic mean, standard deviation and correlation coefficients. Waste Manag. 69, 13&ndash;23.",
 "G&ouml;tze, R., Boldrin, A., Scheutz, C., Astrup, T.F., 2016. Physico-chemical characterisation of material fractions in household waste: overview of data in literature. Waste Manag. 49, 3&ndash;14. https://doi.org/10.1016/j.wasman.2016.01.008",
 "IPCC, 2006. 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 5 Waste. IGES, Hayama.",
 "IPCC, 2019. 2019 Refinement to the 2006 IPCC Guidelines for National Greenhouse Gas Inventories, Vol. 5, Ch. 3. IPCC, Switzerland.",
 "IPCC, 2021. Climate Change 2021: The Physical Science Basis. Contribution of Working Group I to the Sixth Assessment Report. Cambridge University Press.",
 "ISO, 2006. ISO 14040 and ISO 14044: Environmental management &ndash; Life cycle assessment. International Organization for Standardization, Geneva.",
 "Khandelwal, H., Dhar, H., Thalla, A.K., Kumar, S., 2019. Application of life cycle assessment in municipal solid waste management: a worldwide critical review. J. Clean. Prod. 209, 630&ndash;654.",
 "Lahdelma, R., Salminen, P., 2001. SMAA-2: stochastic multicriteria acceptability analysis for group decision making. Oper. Res. 49, 444&ndash;454.",
 "Laurent, A., Bakas, I., Clavreul, J., et al., 2014. Review of LCA studies of solid waste management systems &ndash; Part I: lessons learned and perspectives. Waste Manag. 34, 573&ndash;588.",
 "Levett, I., Birkett, G., Davies, N., et al., 2016. Techno-economic assessment of poly-3-hydroxybutyrate (PHB) production from methane &ndash; the case for thermophilic bioprocessing. J. Environ. Chem. Eng. 4, 3724&ndash;3733.",
 "Prabowo, B., Simanjuntak, F.S.H., Saldi, Z.S., Samyudia, Y., Widjojo, I.J., 2019. Assessment of waste to energy technology in Indonesia: a techno-economical perspective on a 1000 ton/day scenario. Int. J. Technol. 10, 1228&ndash;1234.",
 "Rand, T., Haukohl, J., Marxen, U., 2000. Municipal Solid Waste Incineration: Requirements for a Successful Project. World Bank Technical Paper 462, Washington, DC.",
 "Republic of Indonesia, 2021. Undang-Undang No. 7/2021 (carbon tax). Republic of Indonesia, 2025. Peraturan Presiden No. 109/2025 (waste-to-energy).",
 "Saltelli, A., Annoni, P., Azzini, I., Campolongo, F., Ratto, M., Tarantola, S., 2010. Variance based sensitivity analysis of model output. Design and estimator for the total sensitivity index. Comput. Phys. Commun. 181, 259&ndash;270.",
 "Breiman, L., Friedman, J.H., Olshen, R.A., Stone, C.J., 1984. Classification and Regression Trees. Wadsworth, Belmont, CA.",
 "Friedman, J.H., Fisher, N.I., 1999. Bump hunting in high-dimensional data. Stat. Comput. 9, 123&ndash;143.",
 "Frischknecht, R., Wyss, F., B&uuml;sser Kn&ouml;pfel, S., L&uuml;tzkendorf, T., Balouktsi, M., 2015. Cumulative energy demand in LCA: the energy harvested approach. Int. J. Life Cycle Assess. 20, 957&ndash;969.",
]
for r in REFS:
    A(P(r, "ref"))

A(PageBreak())
A(P("Supplementary material", "h1"))
A(P("Table S1. Harmonised 2025 inputs and gates by location (central values).", "h2"))
rows = [["Location", "t<sub>c</sub>", "Flags", "Q2025 t/d", "M", "LHV", "L0 SL", "RDF NCV", "km", "G1", "G3", "Most probable (p)"]]
w = mc[mc.scenario == "market"].pivot(index="city", columns="pathway", values="p_best")
for r in ch.itertuples():
    p = w.loc[r.city]
    rows.append([r.city, str(r.comp_year), r.flags if isinstance(r.flags, str) else "", f"{r.Q_2025:,.0f}", f"{r.moisture:.2f}",
                 f"{r.LHV:.1f}", f"{r.L0_sl:.0f}", f"{r.rdf_ncv:.1f}", f"{r.road_km:.0f}", "yes" if r.gate_LHV else "no",
                 "yes" if r.gate_PSEL_generated else "no", f"{p.idxmax()} ({p.max():.2f})"])
A(table(rows, [3.3, 0.9, 1.0, 1.5, 0.9, 0.9, 1.1, 1.3, 0.9, 0.8, 0.8, 2.6]))
A(P("Flags: L lumped organics, W wood as garden, R rounded percentages, G glass in residual, N glass not reported, T tonnage "
    "year unverified. M bulk moisture; LHV in MJ/kg; L0 in kg CH<sub>4</sub>/t at MCF = 1; km road distance to kiln; G1 WtE "
    "heating-value gate; G3 PSEL scale on generated waste.", "cap"))
A(P("Table S2. Managed waste by indicator family: see Table 5 of the methodology (MSW_Methodology_v3_EN.pdf). "
    "Table S3. Assumption register: data/assumption_register.csv and Table 3 of the methodology.", "body"))
A(figure(OUT / "fig_sensitivity.png", 15.5, "Fig. S1. Mean absolute Spearman rank correlation between inputs and results "
         "over the 21 locations."))

doc = SimpleDocTemplate(str(DOCS / "MSW_Manuscript_v3_EN.pdf"), pagesize=A4, leftMargin=2.2 * cm, rightMargin=2.2 * cm,
                        topMargin=1.8 * cm, bottomMargin=1.6 * cm, title="MSW recovery pathways in Indonesia (manuscript)",
                        author="Muhammad Fachri Ridwan; Anthony Halog")
doc.build(story, onFirstPage=page_deco(RUN), onLaterPages=page_deco(RUN))
print("wrote", DOCS / "MSW_Manuscript_v3_EN.pdf")

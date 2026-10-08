"""Build docs/MSW_Manuscript_v3_EN.pdf, a journal-style research article drafted from the v3 results."""
import pandas as pd, numpy as np
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.platypus import SimpleDocTemplate, Spacer, PageBreak
from doccommon import *

d = load(); N = stats(d)
inp, t2, t5, ch, mc, det = d["inp"], d["t2"], d["t5"], d["char"], d["mc"], d["det"]
mpa = N["mp50"]; mp100 = N["mp100"]; st = N["stress"]
x = det[(det.scenario == "market")]
med = lambda bg, k, c: x[(x.baseline == bg) & (x.pathway == k)][c]
rng3 = lambda s, f=".0f": f"{s.median():{f}} ({s.min():{f}}&ndash;{s.max():{f}})"
ov = inp[inp.tonnage_year_status == "overview_index_year_unverified"]
cov = d["cover"]; us = d["usplit"]
story = []; A = story.append
RUN = "Ex-ante screening of MSW recovery pathways in Indonesia"

A(P("Draft manuscript for submission to a Q1 waste-management journal &mdash; not peer reviewed &mdash; October 2026", "subtitle"))
A(Spacer(1, 4))
A(P("Deciding before building: ex-ante screening of municipal solid waste recovery pathways for 21 Indonesian "
    "cities and regencies, with decision rules, break-even targets and the value of information", "title"))
A(P("Muhammad Fachri Ridwan<super>a,*</super>, Anthony Halog<super>a</super>", "subtitle"))
A(P("<super>a</super> The University of Queensland, Brisbane, Australia (affiliation details to be completed)<br/>"
    "<super>*</super> Corresponding author. E-mail: mfachri0411@gmail.com<br/>"
    "<i>Author list, affiliations and contributions to be confirmed by the authors before submission.</i>", "subtitle"))
A(Spacer(1, 6))

abstract = (
    "Indonesian local governments must decide on waste-to-energy (WtE), refuse-derived fuel (RDF), anaerobic digestion (AD) "
    "or improved landfilling before any such facility exists locally, with waste data that are sparse, of different years and "
    "of uneven definition. We present an open, provenance-aware ex-ante screening model that tells these stakeholders under "
    "which city and waste-system conditions each option becomes preferable, which performance it must reach, and which data "
    "are worth collecting first. Six options are compared per tonne of mixed municipal solid waste at the facility gate in "
    "2025 within one LCA and TEA boundary: sanitary landfill with flare, WtE, RDF for cement kilns, AD of food waste, "
    "polyhydroxybutyrate (PHB) from landfill gas and integrated RDF + AD. Data from the waste master plans of 21 cities and "
    "regencies were harmonised to 2025 with a single audited tonnage projection; every value is labelled by evidence type, "
    "and 23 secondary values were re-verified against their sources. Options are ranked by a carbon-inclusive cost at fixed "
    "carbon values with Monte Carlo uncertainty. At central values the sanitary landfill had the lowest carbon-inclusive cost "
    f"in all locations up to 50 USD/t CO<sub>2</sub>e; at 100 USD/t RDF + AD was best in {N['best100'].get('S5',0)} locations "
    f"(abatement cost {N['s5_mac'][0]:.0f}, range {N['s5_mac'][1]:.0f}&ndash;{N['s5_mac'][2]:.0f} USD/t CO<sub>2</sub>e) and WtE "
    "in one. Scenario discovery over 40,000 combinations of conditions gave decision rules: AD is preferable only where food "
    "is separated at source and carbon is valued above about 30&ndash;45 USD/t; WtE only with the Perpres 109/2025 tariff, at "
    "least 1,000 t/day and a heating value of at least 7 MJ/kg; RDF + AD only at high carbon values with a cement kiln within "
    "about 300 km; and the landfill elsewhere. The RDF + AD result at 100 USD/t is fragile: the upper moisture bound or a 22% "
    f"lower RDF heating value returned {int(st.loc['moisture_high_bound',100])} of 21 locations to the landfill. The expected "
    f"value of perfect information rose from {N['evpi'][25]:.2f} to {N['evpi'][100]:.2f} USD/t between 25 and 100 USD/t "
    "CO<sub>2</sub>e and was dominated by waste characterisation and landfill-gas performance, which identifies the "
    "measurements to fund before a technology is chosen. The model is released as a Google Colab decision tool.")
A(P("<b>Abstract</b>", "h2")); A(P(abstract, "body"))
A(P("<b>Highlights</b>", "h2"))
for t in bullets(["Ex-ante screening of six MSW options for 21 Indonesian locations before any facility exists",
                  "Every input labelled by evidence type; 23 secondary values re-verified against sources",
                  "Landfill with flare is preferred up to 50 USD/t CO2e; RDF + AD only near 100 USD/t",
                  "AD needs source separation; WtE needs the Perpres 109/2025 tariff and 1,000 t/day",
                  "Break-even targets and value of information tell cities what to require and measure"]):
    A(t)
A(P("<b>Keywords:</b> municipal solid waste; life cycle assessment; techno-economic assessment; refuse-derived fuel; "
    "anaerobic digestion; scenario discovery; value of information; ex-ante assessment; Indonesia", "body"))

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
A(P("A further feature of the Indonesian setting shapes what a model can contribute. Most recovery pathways do not yet "
    "exist at the scale of a city or regency, so decisions must be taken <i>ex ante</i>, before any local performance data "
    "exist. Indonesian techno-economic studies have typically assessed one technology at one scale or for one city, for "
    "example WtE incineration at 1,000 t/day (Prabowo et al., 2019; Azis et al., 2021; Yuliani et al., 2022). What local "
    "governments, planning agencies, offtakers and funders need before a feasibility study is different: a comparison of "
    "all realistic options under their own conditions, the performance an option would have to reach to be worth pursuing, "
    "and guidance on which local data would most reduce the risk of a wrong choice."))
A(P("This study therefore asks: <i>under what Indonesian city and waste-system conditions does each recovery pathway become "
    "preferable?</i> Its contribution is an ex-ante decision-support model with five elements: (i) six options that manage "
    "the whole tonne within one LCA/TEA boundary, with the sanitary landfill as both baseline and option; (ii) a reproducible "
    "data layer that harmonises 21 RIPS records to 2025 with a single audited projection, labels every value by evidence type "
    "and records the verification of each secondary value; (iii) decision results at fixed, policy-relevant carbon values "
    "with Monte Carlo standard errors; (iv) scenario discovery that states the answer as rules in observable conditions; and "
    "(v) break-even targets and a value-of-information analysis that translate uncertainty into what a city should require "
    "in a tender and what it should measure first. All of it is released as a Google Colab tool for other cities."))

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
    "Moisture as received (food 0.75, plastic 0.22, paper 0.35) is an assumption that gives a bulk moisture of about 0.5, "
    "close to the 0.55 measured for raw MSW at Cilacap before biodrying; IPCC moisture, which describes waste as generated, "
    "and an upper moisture bound are run as a scenario and a stress test. "
    f" At Level II/III a DOC value is known for only {cov.has_DOC_dry_fraction.min():.0f}&ndash;"
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
    "efficiency &eta; = 0.5 (0.2&ndash;0.8; lower values are common where food waste dominates) and 10% cover oxidation for the sanitary landfill. WtE uses a net electrical "
    "efficiency of 0.18 (0.14&ndash;0.22). The RDF line sorts a fixed share of each fraction, dries the product to 20% moisture "
    "with its own fuel, hauls it to the nearest kiln and credits displaced coal at 0.9 GJ per GJ; rejects are landfilled. AD "
    "captures 70% of food waste and converts it with a yield of 0.36 Nm<super>3</super> CH<sub>4</sub>/kg VS for "
    "source-separated food (Zhang et al., 2007) and 5% fugitive loss. Because food separated mechanically from mixed waste "
    "digests less well and carries plastics and grit, its yield is multiplied by 0.6 (0.35&ndash;0.85; Seruga et al., 2020; "
    "Basinas et al., 2020, 2021) and 5 (0&ndash;10) USD per tonne of feed are added for pre-treatment; a front-end separation "
    "step is charged as 40% of the RDF-line cost. PHB is produced from captured gas at "
    "2.3 t CH<sub>4</sub>/t PHB and credited against polypropylene. Costs combine annualised CAPEX scaled with capacity to the "
    f"power b = 0.7, O&amp;M, residue landfill cost that falls with landfill size, haulage and product revenue. All {len(d['reg'])} "
    "parameters, their ranges and their sources are listed in the assumption register (Supplementary Table S3). Secondary "
    "values were re-verified against their sources in October 2026 (Supplementary Table S4): RDF O&amp;M (18.4 USD/t) and the "
    "RDF price at the kiln (1.15 USD/GJ) were corrected to Indonesian values, the Sumatera grid factor to 0.832 kg "
    "CO<sub>2</sub>/kWh (ESDM, 2018), and benchmark values that could not be confirmed were removed."))
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
    "500 t/yr. Among feasible options the decision criterion is the <i>carbon-inclusive cost</i> CIC<sub>k</sub> = C<sub>k</sub> "
    "+ p G<sub>k</sub>/10<super>3</super> (USD/t). It is not a full social cost, because only greenhouse gases are valued. The "
    "carbon value p is a policy choice rather than an uncertain quantity, so results are reported at fixed values: 0, 2 (the "
    "Indonesian carbon tax of Rp 30/kg CO<sub>2</sub>e under UU 7/2021), 25, 50 and 100 USD/t CO<sub>2</sub>e. For each p, the "
    "probability of being best is the share of 4,000 Monte Carlo draws per location in which an option has the lowest CIC; "
    "its Monte Carlo standard error is at most 0.008, so differences between options reflect decision uncertainty, not "
    "sampling noise. Composition is sampled from a Dirichlet distribution S ~ Dir(&alpha;<sub>0</sub>s) with heuristic "
    "concentrations (&alpha;<sub>0</sub> = 80, or 40 for flagged data); parameters from triangular distributions. A category a "
    "RIPS does not report is given zero share, a modelling assumption tested by imputing glass where it is missing and by a "
    "Dirichlet pseudocount. Scenarios were run with common random numbers: the market case, the Perpres 109/2025 tariff, food "
    "separated at source, optimistic AD feed, residues to an open dump, two managed-tonnage series, GWP20, IPCC moisture, "
    "projected overview tonnage, rubber DOCf of 0.5, glass imputation, a pseudocount and an aspirational PHB cost; two stress "
    "tests set moisture at its upper bound and lower the RDF heating value by 22% (to about 13&ndash;14 MJ/kg). Sensitivity was "
    "measured by Spearman rank correlation and by Sobol indices (Saltelli et al., 2010)."))
A(P("<b>Break-even targets and value of information.</b> For each location, option and key input, the input was swept "
    "with all others central to find the value at which the option's CIC equals the landfill's. For a fixed p, the expected "
    "value of perfect information is EVPI = E[max<sub>k</sub> NB<sub>k</sub>] &minus; max<sub>k</sub> E[NB<sub>k</sub>] with "
    "NB = &minus;CIC, and the partial value EVPPI(X) for a group of inputs X resolved by one measurement campaign was estimated "
    "by regressing NB on X (Strong et al., 2014), with the value obtained for random noise subtracted. Multiplied by the 2025 "
    "tonnage, EVPPI is the most a city should pay per year to resolve X before choosing."))
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
A(P("2.7 Verification and validation", "h2"))
A(P("Because none of the facilities exists, the model cannot be validated against plant data; four layers were kept apart. "
    f"(a) Verification: an automated suite of {N['valid'][1]} checks ({N['valid'][0]} passed) tests blank-versus-zero "
    "handling, taxonomy keys, mass balances, the single projection, wet/dry consistency, provenance, the corrected secondary "
    "values, the realism parameters, the decision outputs and an explicit hand recalculation of every result for one city. "
    "(b) Benchmark validation: intermediate outputs that do not require a facility (moisture, heating value, RDF yield and "
    "quality, WtE electricity per tonne) were compared with verified Indonesian values (Table 1). (c) Stress tests: "
    "assumptions without local measurement were pushed to their bounds. (d) Field validation was framed as a protocol ordered "
    "by the value of information (Section 4.3)."))

# ------------------------------------------------------------------------------------------- 3
A(P("3. Results", "h1"))
A(P("3.1 Waste characteristics and feasibility", "h2"))
b = d["bench"]
rows = [["Quantity", "Model (21 locations)", "Reference (verified)", "Verdict"]] + \
       [[i, r.model_21_locations, r.reference, r.verdict] for i, r in b.iterrows()]
A(table(rows, [3.0, 3.2, 7.6, 2.8]))
A(P("Table 1. Benchmark validation: characteristics of the harmonised 2025 waste against verified Indonesian values. The "
    "verdict is computed from the model median and range.", "cap"))
A(P(f"Bulk moisture as received ranged from {ch.moisture.min():.2f} to {ch.moisture.max():.2f} and LHV from "
    f"{N['lhv'][0]:.1f} to {N['lhv'][1]:.1f} MJ/kg (median {N['lhv'][2]:.1f}), within the verified Indonesian range; the RDF "
    f"heating value lies at the upper end of the Cilacap values and WtE electricity at the lower end of Indonesian design "
    f"values (Table 1). {N['g1']} of 21 locations passed the WtE heating-value gate and {N['g12']} passed both the "
    f"heating-value and the supply gate. {N['psel_gen']} locations generate at least 1,000 t/day, but on managed waste none "
    f"does in series M1 and only Kab. Bogor does in series M2. Methane potential at MCF = 1 was {ch.L0_sl.min():.0f}&ndash;"
    f"{ch.L0_sl.max():.0f} kg CH<sub>4</sub>/t, and the RDF yield {ch.rdf_yield.min():.2f}&ndash;{ch.rdf_yield.max():.2f} "
    "t/t."))
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
A(P("3.3 Preferred options at fixed carbon values", "h2"))
pbm = d["pbest"]; pbm = pbm[pbm.scenario == "market"]
rows = [["Carbon value (USD/t CO<sub>2</sub>e)", "Best at central values (locations)", "Most probable (locations)",
         "Median P(most probable)", "Median P(SL)", "Median P(S5)"]]
for pc in (0, 2, 25, 50, 100):
    q = pbm[pbm.carbon_value == pc]
    cb = N.get(f"best{pc}", None)
    rows.append([str(pc), ", ".join(f"{k} {v}" for k, v in cb.items()) if cb else "as at 0",
                 ", ".join(f"{k} {v}" for k, v in N[f"mp{pc}"]["market"].items()), f"{N[f'mpp{pc}'].median():.2f}",
                 f"{q[q.pathway == 'SL'].p_best.median():.2f}", f"{q[q.pathway == 'S5'].p_best.median():.2f}"])
A(table(rows, [2.6, 3.4, 3.4, 2.5, 2.2, 2.2]))
A(P("Table 2a. Decision results at fixed carbon values, market case (21 locations, 4,000 draws each; Monte Carlo standard "
    "error of every probability at most 0.008). Per-location values in Supplementary Table S1.", "cap"))
A(P(f"At central values the sanitary landfill had the lowest carbon-inclusive cost in all 21 locations at 0, 25 and 50 USD/t "
    f"CO<sub>2</sub>e, and it was the most probable option everywhere up to 50 USD/t (Table 2a). At the current carbon tax "
    f"(about 2 USD/t) no recovery option was preferred anywhere. At 100 USD/t RDF + AD was best at central values in "
    f"{N['best100'].get('S5',0)} locations and the most probable option in {mp100['market'].get('S5',0)} (median probability "
    f"{N['p_S5_100'][0]:.2f}, range {N['p_S5_100'][1]:.2f}&ndash;{N['p_S5_100'][2]:.2f}); WtE was best only in Kab. Kutai "
    "Kartanegara, where the Mahakam grid is the most carbon-intensive and no kiln lies within reach for RDF. Averaged over "
    f"locations, RDF + AD was on the Pareto front in {100*N['avg'].loc['S5','p_front']:.0f}% of draws and the landfill in "
    f"{100*N['avg'].loc['SL','p_front']:.0f}%: the two are the efficient options, and the carbon value decides between them."))
A(figure(OUT / "fig_probability_best.png", 16.5, "Fig. 4. Probability of being the best feasible option at fixed carbon "
         "values (top) and, at 50 USD/t CO<sub>2</sub>e, with the Perpres 109/2025 WtE tariff, with optimistic AD feed and with "
         "food waste separated at source (bottom)."))
A(P(f"With the Perpres 109/2025 tariff, WtE became the most probable option at 50 USD/t in {mpa['perpres109'].get('S1',0)} "
    "locations, all of them generating at least 1,000 t/day; whether they collect that much is a separate question, since "
    "managed tonnage is far lower. If food waste arrived separated at source, AD alone became most probable in "
    f"{mpa['food_separated_at_source'].get('S3',0)} locations at 50 USD/t. For mixed waste it did not, even when the "
    f"mixed-waste penalty was removed (landfill most probable in {mpa['AD_feed_optimistic'].get('SL',0)} locations): source "
    "separation, not digester performance, is the condition for AD. On managed tonnage the landfill was most probable at "
    f"50 USD/t in {mpa['managed_M1_status_index'].get('SL',0)} of 19 locations (M1) and "
    f"{mpa['managed_M2_local_first'].get('SL',0)} of 21 (M2). PHB was preferred nowhere at first-plant cost; only an "
    f"aspirational large-scale cost of 1.3 USD/kg made it most probable in {mpa['phb_large_scale_cost'].get('S4',0)} locations, "
    "a research target rather than a planning value."))
A(P("3.4 Robustness: data scenarios and stress tests", "h2"))
rows = [["Scenario or stress test"] + [f"{pc}" for pc in st.columns]]
for s_, r in st.iterrows():
    rows.append([SCN.get(s_, s_)] + [str(int(x)) for x in r.values])
A(table(rows, [5.5] + [2.0] * len(st.columns)))
A(P("Table 2b. Number of the 21 locations whose most probable option changes relative to the market case, by carbon value "
    "(USD/t CO<sub>2</sub>e).", "cap"))
A(P("Projecting the eight overview totals, raising rubber DOCf to 0.5, imputing glass where it is not reported and adding a "
    "Dirichlet pseudocount changed the most probable option in at most two locations, and only at 100 USD/t. Moisture was "
    f"decisive. With the drier IPCC moisture, {int(st.loc['moisture_IPCC_default',50])} locations left the landfill at 50 USD/t; "
    f"with the upper moisture bound, {int(st.loc['moisture_high_bound',100])} of the RDF + AD locations returned to the "
    f"landfill at 100 USD/t, and a 22% lower RDF heating value did the same in {int(st.loc['rdf_ncv_stress',100])}. A 20-year "
    f"horizon for methane moved every location away from the landfill at 50 USD/t (RDF + AD {mpa['GWP20'].get('S5',0)}, WtE "
    f"{mpa['GWP20'].get('S1',0)}). The low-carbon-value result (landfill first) is robust; the high-carbon-value result "
    "(RDF + AD) depends on the moisture and RDF quality of the local waste (Fig. 5)."))
A(figure(fig_robust(d, 100), 15, "Fig. 5. Most probable option per location in each scenario at 100 USD/t CO<sub>2</sub>e "
         "(colour) and its probability (number). n.a.: no status-index value for the managed-tonnage series M1."))
A(P("3.5 Drivers of uncertainty", "h2"))
A(P("For climate results, the lifetime landfill-gas collection efficiency dominated every option that landfills residues, "
    "followed by the plastic share (fossil CO<sub>2</sub> in WtE), the garden share and moisture; for cost, the landfill cost, "
    "WtE and AD capital costs, RDF operating cost, the AD front-end share and PHB cost and price dominated (Supplementary "
    f"Fig. S1). Sampling composition alone produced a median 90% interval of {us.loc['S1', ('G_w90','composition only')]:.0f} "
    f"kg CO<sub>2</sub>e/t for WtE against {us.loc['S1', ('G_w90','parameters only')]:.0f} for parameters alone, whereas for the "
    f"landfill parameters dominated ({us.loc['SL', ('G_w90','parameters only')]:.0f} against "
    f"{us.loc['SL', ('G_w90','composition only')]:.0f}). Composition hardly affected cost."))

_sg = N["sobol_gap"].set_index("input").ST
A(P(f"Sobol indices give the same picture: landfill-gas collection (total index {_sg.get('cap', float('nan')):.2f}) and "
    f"moisture as received ({_sg.get('wetness', float('nan')):.2f}) explain about half of the variance of the carbon-inclusive cost gap "
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
_pr = d["prim"].set_index("option")
A(P(f"Carbon value, source separation of food, scale and heating value carried the decision (tree importances "
    f"{d['imp']['carbon_value']:.2f}, {d['imp']['food_separated']:.2f}, {d['imp']['Q_tpd']:.2f} and {d['imp'].get('LHV', 0):.2f}). "
    f"For mixed waste without the WtE tariff the landfill was best in {100*N['disc_mix'].get('SL',0):.0f}% of draws and "
    f"RDF + AD in {100*N['disc_mix'].get('S5',0):.0f}%. Every tree leaf for mixed waste without the tariff predicts the "
    "landfill. RDF + AD formed no leaf; PRIM located its best box at "
    f"{str(_pr.loc['S5','box']).replace('_', ' ')} with a density of only {_pr.loc['S5','density']:.2f}, i.e. it is preferable "
    "in a narrow region at high carbon values with a kiln in reach. WtE won in the box defined by the tariff, at least "
    f"1,000 t/day and an LHV of at least 7 MJ/kg (density {_pr.loc['S1','density']:.2f}). AD alone won where food arrived "
    "separated and carbon was valued above about 30&ndash;45 USD/t. RDF alone and PHB were best in fewer than 3% of draws "
    "(Table 4; Fig. 6)."))
A(figure(OUT / "fig_condition_maps.png", 15.5, "Fig. 6. Condition maps: most frequent best option and its frequency (%) on "
         "grids of two conditions, for mixed waste without tariff, mixed waste with the Perpres 109/2025 tariff, and food "
         "separated at source."))
A(P("3.8 Break-even targets and the value of information", "h2"))
A(table([["Input", "Central", "Better if", "Break-even: median (range)", "n crossing", "never", "already"]] +
        thr_summary(d, "S5", 50), [3.0, 1.6, 1.6, 4.4, 1.9, 1.6, 1.9]))
A(P("Table 5. Break-even values for RDF + AD against the landfill at 50 USD/t CO<sub>2</sub>e over 21 locations: the value "
    "of one input (others central) at which the two carbon-inclusive costs are equal. y_pen_mech: relative methane yield of "
    "mechanically separated food; pre_ofmsw: pre-treatment cost (USD/t feed); p_rdf: RDF price (USD/GJ); o_rdf: RDF O&amp;M "
    "(USD/t); cap: landfill-gas collection efficiency. 'never': no value in the tested range suffices.", "cap"))
A(P("At 50 USD/t RDF + AD would match the landfill if RDF O&amp;M fell from 18.4 to about 9 USD/t, if kilns paid about "
    "three times today's RDF price, or if the landfill alternative collected less than about a quarter of its gas; no "
    "plausible improvement of the AD feed alone is enough. These are the conditions a city would have to secure, through a "
    "processing contract, an offtake agreement or a realistic appraisal of its landfill, before RDF + AD is worth tendering."))
vg = N["voi_top"]
rows = [["Measurement group", "25", "50", "100", "Max at 100, USD/yr"]]
for g_ in vg[100].group:
    rows.append([g_] + [f"{vg[pc].set_index('group').loc[g_].median_evppi:.3f}" for pc in (25, 50, 100)] +
                [f"{vg[100].set_index('group').loc[g_].max_usd_per_year:,.0f}"])
rows.append(["EVPI (all inputs)"] + [f"{N['evpi'][pc]:.2f}" for pc in (25, 50, 100)] + [f"{N['evpi_yr_max'][100]:,.0f}"])
A(table(rows, [7.0, 1.8, 1.8, 1.8, 3.6]))
A(P("Table 6. Value of information: median EVPPI over 21 locations (USD per tonne MSW) at carbon values of 25, 50 and "
    "100 USD/t CO<sub>2</sub>e, and the maximum over locations in USD per year at 100 USD/t.", "cap"))
A(P(f"The value of information was small at 25 USD/t (median EVPI {N['evpi'][25]:.2f} USD/t) because the landfill wins "
    f"almost regardless, and grew to {N['evpi'][100]:.2f} USD/t at 100 USD/t, about {N['evpi_yr'][100]:,.0f} USD per year for "
    "the median location. Waste characterisation (as-received moisture and composition) and landfill-gas performance carried "
    "most of it, followed by WtE performance and cost; the RDF line, AD and finance parameters mattered little (Table 6; "
    "Fig. 7). Group values are conservative: the regression is additive within a group and the noise floor is subtracted, "
    "so the groups need not add up to the EVPI."))
A(figure(OUT / "fig_voi.png", 16, "Fig. 7. Value of information by measurement group: median over 21 locations (bar) and "
         "maximum (dot), at carbon values of 25, 50 and 100 USD/t CO<sub>2</sub>e."))

# ------------------------------------------------------------------------------------------- 4
A(P("4. Discussion", "h1"))
A(P("4.1 Conditions under which each pathway becomes preferable", "h2"))
A(P("The answer to the research question is a set of conditions rather than a single winner. A well-operated sanitary "
    "landfill with gas collection and flaring is the least-cost option wherever carbon is valued at 50 USD/t CO<sub>2</sub>e "
    "or less, which includes Indonesia's current carbon tax of about 2 USD/t. Integrated RDF + AD becomes preferable only "
    f"when carbon is valued near its abatement cost of about {N['s5_mac'][0]:.0f} USD/t, a cement kiln is within reach and the "
    "waste is not too wet; it treats the whole tonne, like WtE, at a lower cost in most locations. WtE requires the "
    "conjunction of a heating value of at least 7 MJ/kg, at least 1,000 t/day and the Perpres 109/2025 tariff. AD alone is a "
    "pathway for source-separated food and market waste, not for mixed waste, even with optimistic assumptions on the "
    "mixed-waste feed. PHB from landfill gas adds little climate benefit over flaring and does not cover its production cost "
    "at first-plant scale, consistent with techno-economic assessments of methane-based PHB (Levett et al., 2016)."))
A(P("4.2 Contribution: decision support before facilities exist", "h2"))
A(P("Because none of these facilities exists in the study locations, the model cannot tell a city how a plant <i>will</i> "
    "perform; it can tell the city which plants are worth studying, what they would have to achieve and what to measure "
    "first. This changes the role of uncertainty analysis from a robustness appendix to the main product. The fixed-carbon-"
    "value results show stakeholders where the decision is clear (landfill first at today's carbon prices) and where it "
    "depends on policy (above about 70 USD/t). The break-even targets convert model parameters into contract terms that a "
    "city, an RDF offtaker or a funder can negotiate. The value-of-information analysis prices local data in USD per year, "
    "so that a city can compare the cost of a waste-characterisation campaign or a landfill-gas pumping test with the "
    "expected cost of choosing wrongly. To our knowledge, combining provenance labels, fixed-carbon-value decisions, "
    "scenario discovery, break-even targets and the value of information in one open tool for Indonesian cities has not "
    "been reported; we offer it as a template for ex-ante waste-infrastructure appraisal where local data are scarce."))
A(P("4.3 Why provenance matters", "h2"))
A(P("The data scenarios show which provenance decisions are consequential. The year assigned to overview tonnage totals "
    "changes plant scale by up to a fifth but no decision, and the unresolved DOCf of rubber and leather is immaterial because "
    "the fraction is small. Moisture, in contrast, is decisive. IPCC moisture values describe waste as generated; waste "
    "arriving at a facility in a humid climate is wetter; the raw MSW at Cilacap had 55% moisture before biodrying. Because DOC, fossil carbon and heating value all multiply the same dry mass, moisture "
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
A(P("4.4 Implications for stakeholders", "h2"))
_od_sl = x[(x.baseline == "OD") & (x.pathway == "SL")]
A(P("For <b>city and regency governments</b>, upgrading open dumps to sanitary landfills with gas collection is the largest "
    f"and cheapest single climate step (about {_od_sl.dG.median():.0f} kg CO<sub>2</sub>e/t at about {_od_sl.C.median():.0f} "
    "USD/t), and it keeps later options open. Before tendering a recovery plant they should fund, in this order, a waste "
    "characterisation with proximate analysis of waste as received and a measurement of landfill-gas collection, because "
    "these carry most of the value of information. For <b>national agencies</b>, the results show that the carbon value and "
    "the WtE tariff, not technology, decide which recovery option is preferable; a common definition of managed waste would "
    "make the 1,000 t/day eligibility of Perpres 109/2025 verifiable. For <b>cement companies and PLN</b> as offtakers, the "
    "break-even RDF price and the WtE tariff define the terms under which recovery becomes viable. For <b>funders</b>, the "
    "fragility of the RDF + AD result at high carbon values argues for staged investment conditioned on measured waste "
    "properties. Source separation of food is the precondition for AD and should be planned as a programme, not assumed."))
A(P("4.5 Limitations", "h2"))
A(P("The study is an ex-ante screening assessment, not an evaluation of plants. Compositions sampled between 2014 and "
    "2025 represent 2025 without a trend, and eight tonnage totals have an unverified year. Fraction-level moisture and "
    "heating values have not been measured for the study locations; several heating values and the RDF transfer coefficients "
    "are legacy assumptions. The secondary-data verification relied on abstracts, indexed records and search extracts because "
    "full texts could not be retrieved; values marked L remain to be checked against the full texts. The decision criterion "
    "values only greenhouse gases; health, air pollution, leachate and employment are not valued, and CED and land take are "
    "reported but not monetised. Landfill gas is assigned without time dynamics; costs are class-5 estimates; coordinates are "
    "provisional; inputs are treated as independent; and 12 of the 21 locations are in Central Java. The Dirichlet "
    "concentrations are heuristics, a non-reported category is set to zero (tested by imputation), and the decision rules "
    f"approximate the model ({100*d['cart_acc']['acc_test']:.0f}% accuracy) within the range of observed compositions. The "
    "value-of-information estimates use a regression approximation and depend on the assumed input ranges."))

A(P("4.6 Methodological challenges and future work", "h2"))
INF = pd.read_csv(OUT / "parameter_influence.csv").set_index("key")
_gb = float(pd.read_csv(OUT / "parameter_influence_base_gap.csv", index_col=0).value["gap50"])
_ver = pd.read_csv(DATA / "secondary_data_verification.csv"); _reg = d["reg"]
_prim = d["prim"].set_index("option")
_g = lambda k: f"{INF.loc[k, 'gap50_lo']:+.1f} to {INF.loc[k, 'gap50_hi']:+.1f}"
_n = lambda k: int(max(INF.loc[k, "n_change100_lo"], INF.loc[k, "n_change100_hi"]))
_vg = N["voi_top"][100]
A(P("The limitations above are not equally consequential. Table 7 ranks the methodological challenges of this study by how "
    "much they can change the answer to the research question, using the model itself as the yardstick: the one-at-a-time "
    "swing of the carbon-inclusive cost gap between RDF + AD and the landfill at 50 USD/t CO<sub>2</sub>e (base value "
    f"{_gb:+.1f} USD/t; negative means RDF + AD is cheaper), the number of locations whose preferred option changes at "
    "100 USD/t, the stress tests (Table 2b) and the value of information (Table 6). The ranking shows that the most "
    "consequential challenges are data challenges that local measurement can resolve, rather than structural features of the "
    "model."))
rows = [["#", "Challenge", "Evidence from this study", "How it was handled here", "What would resolve it"],
        ["1", "Landfill-gas collection efficiency is not measured at any study site",
         f"Gap {_g('cap')} USD/t over its range (0.20&ndash;0.80); preferred option changes in up to {_n('cap')} locations at 100 USD/t; "
         f"second-largest group EVPPI ({_vg.set_index('group').median_evppi.iloc[1]:.2f} USD/t at 100 USD/t)",
         "Wide range from the literature; scenario and VOI analysis", "Pumping tests or flux measurements at operating sanitary landfills (priority 1)"],
        ["2", "Moisture and heating value of waste as received are not measured locally",
         f"Gap {_g('wetness')} USD/t between moisture bounds; upper bound returns {int(st.loc['moisture_high_bound', 100])} of 21 "
         f"locations to the landfill at 100 USD/t; largest group EVPPI ({_vg.set_index('group').median_evppi.iloc[0]:.2f} USD/t)",
         "Calibrated assumption, common wetness draw, IPCC and high-bound scenarios",
         "Waste characterisation with proximate analysis at the facility gate, wet and dry season (priority 1)"],
        ["3", "Composition is a sampling-year proxy with inconsistent categories and no replicates",
         f"Sampling years {inp.composition_sampling_year.min()}&ndash;{inp.composition_sampling_year.max()}; lumped organics and "
         "missing glass in some RIPS; Dirichlet concentration is heuristic",
         "Provenance labels, harmonisation rules L/W, glass imputation and pseudocount scenarios (at most 2 changes)",
         "Standardised sorting campaigns (SNI 19-3964-1994) with replicates to calibrate the concentration"],
        ["4", "Secondary data verified only at abstract level; RIPS source files not re-checked",
         f"{(_reg.status == 'V').sum()} of {len(_reg)} register values confirmed (status V); {(_reg.status == 'L').sum()} literature values "
         f"still status L; {len(_ver)} items re-checked, {int(_ver.result.str.startswith('corrected').sum())} corrected",
         "Item-level verification file with evidence and method; corrected values used",
         "Full-text check of every status L value and of the RIPS tables before publication"],
        ["5", "Plant costs are class-5 estimates with few Indonesian data points",
         f"RDF O&amp;M: gap {_g('o_rdf')} USD/t; AD CAPEX: {_g('K_ad')} USD/t, against a base gap of {_gb:.1f} USD/t",
         "Wide triangular ranges, Monte Carlo, break-even targets", "Cost data from Indonesian RDF, AD and WtE projects and tenders"],
        ["6", "No facility exists, so outcomes cannot be validated", "Validation limited to verification, benchmarks of intermediate quantities and stress tests",
         "Four validation layers; framing as ex-ante decision support", "Monitoring of pilot plants and re-running the model with their data"],
        ["7", "Static per-tonne inventory", "No first-order decay of landfill gas; no kiln decarbonisation; grid credit evaluated at one year",
         "2045 projection with grid path and escalation", "Time-resolved landfill model and lifetime-averaged grid credits"],
        ["8", "Inputs sampled independently", "Moisture, heating value and DOC are linked physically; costs co-vary",
         "One common wetness draw for all fractions", "Correlated sampling (e.g. copulas) for moisture-LHV-DOC and for costs"],
        ["9", "Single valued impact in the decision", "Carbon-inclusive cost values only GHG; air pollution, health, leachate, jobs and acceptance excluded",
         "CED and land take reported; limits stated", "Multi-criteria extension with local air-quality and social indicators"],
        ["10", "Decision depends on a policy carbon value and hard feasibility thresholds",
         f"Landfill preferred up to 50 USD/t, RDF + AD near 100 USD/t; result at 100 USD/t fragile (stress tests change "
         f"{int(st.loc['rdf_ncv_stress', 100])} of 21)", "Fixed carbon values with standard errors; stress tests",
         "Explicit policy scenarios for carbon price and tariff trajectories"],
        ["11", "Decision rules approximate the model", f"Tree accuracy {100 * d['cart_acc']['acc_test']:.0f}%; PRIM density for RDF + AD "
         f"{_prim.loc['S5', 'density']:.2f}; valid within the observed composition range", "Rules reported with purity, coverage and density",
         "More locations, especially outside Java, to widen the condition space"],
        ["12", "Limited regional coverage", f"{(inp.province == 'Jawa Tengah').sum()} of 21 locations in Central Java; one each in Sumatra, Kalimantan and Bali",
         "One-city tool and template for new locations", "Applying the tool to cities in Sumatra, Kalimantan, Sulawesi and eastern Indonesia"]]
A(table(rows, [0.5, 3.0, 4.6, 4.0, 4.5]))
A(P("Table 7. Methodological challenges ranked by their measured influence on the decision (one-at-a-time ranges from "
    "outputs/parameter_influence.csv; group EVPPI at 100 USD/t CO<sub>2</sub>e from Table 6).", "cap"))
A(P("Future work should therefore proceed in the order of Table 7. The first two items, landfill-gas collection and the "
    "properties of waste as received, carry most of the value of information and are field measurements rather than model "
    "developments; a city should commission them whenever their cost is below the corresponding EVPPI per year. Full-text verification of the remaining "
    "literature values (item 4) is a precondition for publication. Correlated sampling, a time-resolved landfill model and "
    "multi-criteria extension (items 7&ndash;9) are model developments that matter mainly for the high-carbon-value results. "
    "Applying the released one-city tool to new regions (item 12) is the most direct way to test whether the decision rules "
    "hold beyond Java."))

# ------------------------------------------------------------------------------------------- 5
A(P("5. Conclusions", "h1"))
A(P("An ex-ante, provenance-aware screening of six MSW options for 21 Indonesian cities and regencies, made before any of "
    "the facilities exists, shows that with equal system boundaries a sanitary landfill with flaring is the least-cost option "
    "up to about 50 USD/t CO<sub>2</sub>e, integrated RDF + AD becomes preferable only near 100 USD/t where a cement kiln is "
    "accessible and the waste is not too wet, WtE depends on the Perpres 109/2025 tariff and at least 1,000 t/day, AD alone "
    "requires source-separated food, and PHB from landfill gas is not competitive. Scenario discovery states these "
    "conditions as rules, break-even analysis turns them into targets for tenders and offtake contracts, and the value of "
    "information shows that measured as-received waste properties and landfill-gas collection are the data to fund first. "
    "Released as an open Colab tool, the model gives local governments, national agencies, offtakers and funders a common, "
    "auditable basis for deciding which recovery pathway to pursue before they build."))

A(P("Data and code availability", "h2"))
A(P("The database, model, notebook, validation suite and the scripts that generate this manuscript are provided in the "
    "MSW_pathway_model_v3 package (build_database_v3.py, msw_pathway_model_v3.py, MSW_pathway_model_v3.ipynb, validate_v3.py). "
    "Row-level RIPS source locators are given in input_kota_2025_updated.csv. The package mswpath and the notebook "
    "MSW_Decision_Tool_Colab.ipynb let other researchers and planners run the model in Google Colab for their own cities "
    "from a one-row-per-city template, re-run the scenario discovery and download Excel/HTML reports in English or "
    "Indonesian. A one-city notebook, MSW_Single_City_Colab.ipynb, with an input template in XLSX and CSV form "
    "(templates/single_city_template.xlsx), lets readers enter the data of one city by hand, including their own measured "
    "parameter values, and reproduces every result of this article for that city; with the example data it reproduces the "
    "Kota Padang worked example."))
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
 "Basinas, P., et al., 2020. Assessment of high-solid mesophilic and thermophilic anaerobic digestion of mechanically-separated municipal solid waste. Environ. Res.",
 "Basinas, P., et al., 2021. Dry anaerobic digestion of the fine particle fraction of mechanically-sorted organic fraction of municipal solid waste in laboratory and pilot reactor. Waste Manag.",
 "ESDM (Ministry of Energy and Mineral Resources), 2018. Grid emission factors of the Indonesian electricity systems (as listed for the Joint Crediting Mechanism, GEC).",
 "Seruga, P., et al., 2020. Anaerobic digestion performance: separate collected vs. mechanical segregated organic fractions of municipal solid waste as feedstock. Energies 13.",
 "Strong, M., Oakley, J.E., Brennan, A., 2014. Estimating multiparameter partial expected value of perfect information from a probabilistic sensitivity analysis sample: a nonparametric regression approach. Med. Decis. Making 34, 311&ndash;326.",
 "Yuliani, M., et al., 2022. Kajian tekno-ekonomi penerapan insinerator waste-to-energy di Indonesia (kasus pada Kota X). J. Teknol. Lingkung.",
 "Zhang, R., El-Mashad, H.M., Hartman, K., et al., 2007. Characterization of food waste as feedstock for anaerobic digestion. Bioresour. Technol. 98, 929&ndash;935.",
 "Breiman, L., Friedman, J.H., Olshen, R.A., Stone, C.J., 1984. Classification and Regression Trees. Wadsworth, Belmont, CA.",
 "Friedman, J.H., Fisher, N.I., 1999. Bump hunting in high-dimensional data. Stat. Comput. 9, 123&ndash;143.",
 "Frischknecht, R., Wyss, F., B&uuml;sser Kn&ouml;pfel, S., L&uuml;tzkendorf, T., Balouktsi, M., 2015. Cumulative energy demand in LCA: the energy harvested approach. Int. J. Life Cycle Assess. 20, 957&ndash;969.",
]
for r in REFS:
    A(P(r, "ref"))

A(PageBreak())
A(P("Supplementary material", "h1"))
A(P("Table S1. Harmonised 2025 inputs and gates by location (central values).", "h2"))
rows = [["Location", "t<sub>c</sub>", "Flags", "Q2025 t/d", "M", "LHV", "L0 SL", "RDF NCV", "km", "G1", "G3", "Most probable at 100 (p)"]]
w = pbm[pbm.carbon_value == 100].pivot(index="city", columns="pathway", values="p_best")
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
    "Table S3. Assumption register: data/assumption_register.csv and Table 3 of the methodology. Table S4. Secondary-data "
    "verification: data/secondary_data_verification.csv and Table 3a of the methodology. Per-location probabilities at every "
    "carbon value with standard errors: outputs/15_p_best_fixed_carbon_value.csv.", "body"))
A(figure(OUT / "fig_sensitivity.png", 15.5, "Fig. S1. Mean absolute Spearman rank correlation between inputs and results "
         "over the 21 locations."))

doc = SimpleDocTemplate(str(DOCS / "MSW_Manuscript_v3_EN.pdf"), pagesize=A4, leftMargin=2.2 * cm, rightMargin=2.2 * cm,
                        topMargin=1.8 * cm, bottomMargin=1.6 * cm, title="MSW recovery pathways in Indonesia (manuscript)",
                        author="Muhammad Fachri Ridwan; Anthony Halog")
doc.build(story, onFirstPage=page_deco(RUN), onLaterPages=page_deco(RUN))
print("wrote", DOCS / "MSW_Manuscript_v3_EN.pdf")

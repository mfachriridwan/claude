# Changelog and source decisions: MSW Pathway Model v2 → v3

Date: 5 October 2026. Scope: the update package `Claude_MSW_Update_Package` (folders 01–04). Every source file was checked against its SHA-256 in `sources/PACKAGE_MANIFEST.json` before use. All numbers below are produced by `build_database_v3.py`, `msw_pathway_model_v3.py` and `validate_v3.py`; none were typed by hand into the data.

## 1. What changed

### Data (`data/`, built by `build_database_v3.py`)
| File | Content | New in v3 |
|---|---|---|
| `input_kota_2025_updated.csv` | 21 locations; the 54 original columns kept unchanged, plus 41 v3 columns | Separate composition, tonnage-source and baseline years; single projection; `Q2025_alt_tpd`; normalisation factors per stream; glass status; managed-share families; coordinates used; quality flags |
| `table5_qmanaged_2025.csv` | Table 5 | Status-index (M1) and local-first (M2) series, Padang bounds, Serang target as a scenario |
| `table2_model_fraction_parameters.csv` | Table 2 (10 model fractions) | Status for each parameter; IPCC and as-received moisture side by side; rubber DOCf left blank (gap); plastic LHV from Götze et al. 2016 |
| `assumption_register.csv` | 61 register entries (was hard-coded) | H14/H15 heuristic Dirichlet α, C10b plastic LHV range, C12 rubber DOCf scenario |
| `model_constants.csv`, `grid_emission_factors.csv`, `cement_kilns.csv` | Constants, gates, reference capacities, GWP, grid factors, kiln positions | Previously hard-coded in the model |
| `taxonomy_crosswalk.csv` | 102 keys (10/36/56), parent, model fraction, terminal-partition flag, notes | New |
| `fraction_parameters_L1_L2_L3.csv` | Material parameters for Level I/II/III with status, basis, source and locator | New (Level I rows added; composite products blanked) |
| `composition_benchmarks_conditional.csv` | Danish benchmarks with conditional shares for complete sibling sets only | New |
| `composition_level1_2025.csv`, `composition_level2_2025.csv` | Package allocations rescaled to 2025 stream masses, with audit IDs | New |
| `composition_terminal_partition_2025.csv` | Full terminal partition: 78 leaves, each stream = 100% | New |

### Code
- `msw_pathway_model_v3.py` and `MSW_pathway_model_v3.ipynb` (executed, no errors). The model file now holds no parameter values.
- The parser requires the v3 columns and refuses the v2 file. It reads `Q2025_tpd` and `tonnage_already_2025`, and asserts for each location that the tonnage it uses equals the CSV baseline (single-projection guard).
- Composition is normalised per stream and weighted by the **2025** stream masses.
- Moisture: either the as-received values (main case) or the IPCC values (scenario `moisture_IPCC_default`).
- DOCf for rubber/leather comes from register C12 (central 0, range 0–0.5). It is no longer a fixed 0.5.
- `h_k` is applied only to legacy heating values. Plastic uses the Götze value with its own range (C10b).
- New scenarios: `managed_M1_status_index`, `managed_M2_local_first`, `moisture_IPCC_default`, `tonnage_overview_projected`, `docf_rubber_0.5`.
- New outputs: `10_uncertainty_split.csv` (composition prior and parameters sampled separately), `11_parameter_coverage_L2L3.csv` and `12_most_probable_by_scenario.csv`.
- `validate_v3.py`: 49 checks. All 49 pass.

### Documents
- `docs/MSW_Methodology_v3_EN.pdf` (15 pages): Table 2, Table 5, harmonisation equations, model flow and metamodel, sources, uncertainty, register, validation, results and limits of validity.
- `docs/MSW_Manuscript_v3_EN.pdf` (10 pages): a journal-style draft (abstract, highlights, IMRaD, supplementary table). The author list, affiliations and competing-interest statement still need to be confirmed.
- Both PDFs are generated from the CSV outputs by `docs/make_*.py`.

## 2. Tonnage decisions (2025 baseline)

Rule: `Q2025 = Qt (1 + g)^(2025 − t)`, applied once. The domestic and non-domestic streams use the same factor.

| Class | n | Central treatment | Locations |
|---|---|---|---|
| Stated in source table | 12 | Projected once from the stated year (exponent 0 when t = 2025) | Padang, Bogor, Serang, Denpasar, Cianjur, Indramayu, Pati, Pandeglang, Temanggung, Kab. Pekalongan, Kota Pekalongan, Kota Magelang |
| Overview index, year unverified | 8 | Used as reported, with the year left blank (flag T1). Scenario: projected from the composition year (`Q2025_alt_tpd`) | Semarang, Brebes, Nganjuk, Kab. Magelang, Sragen, Kutai Kartanegara, Kendal, Wonogiri |
| RIPS projection for 2025 | 1 | Used as reported; flagged T2 (population × 0.31 kg/cap/day) | Pemalang |

**Decision on overview totals.** The package treated five overview totals as 2025 values because the word "overview" appeared in free-text notes. It projected three other rows (Semarang, Sragen, Kutai Kartanegara) whose `source_pages` cite the same overview index. The year of that index is not shown in any row we hold, so v3 applies one rule to all eight rows. Each total is used as reported, so it cannot be projected twice, and it is never labelled a 2025 observation. The projected alternative is run as a scenario.
- Effect on central tonnage compared with the package: Semarang 1,252.8 → 1,190.0 t/d, Sragen 469.1 → 402.0 t/d, Kutai Kartanegara 399.5 → 373.0 t/d.
- The other 18 locations are unchanged.
- The scenario raises tonnage by up to 21% (Kendal) and changes no most-probable pathway.

**Fallback growth rate:** 1.73%/yr, the median of the six RIPS rates. This is an assumption (H11, range 0.65–2.54%). Its uncertainty is sampled only where the exponent is greater than 0.

## 3. Composition decisions
- Each RIPS sampling-year composition (2014–2025) is used as a proxy for 2025. No trend is applied and no growth factor touches the percentages (flag C0). It is not a measured 2025 composition.
- Each stream is normalised separately and its factor recorded. Pemalang (100.10%) gets factor 0.9990. The other streams close within 99.99–100.03%.
- Blank is kept distinct from zero. Glass is reported in 15 locations, folded into lain-lain in 5, and not reported in Kota Padang (blank, not zero).
- Semarang and Nganjuk report a non-domestic composition with zero non-domestic tonnage, so that composition gets no weight (flag C6).
- Rules L (organic aggregate, γ = 0.27, range 0.04–0.62) and W (kayu treated as garden) are kept from v2 as sampled harmonisation priors.
- All compositions refer to generation or collection, not the landfill gate.

## 4. Qmanaged / Table 5 decisions
The definitions differ, so v3 keeps two series and never merges them silently. A constant share applied to Q2025 is a **scenario**, not a 2025 observation.

| Location | Status index (M1) | Local value | Decision |
|---|---|---|---|
| Kota Padang | none | 93.71% derived = (141.76 + 465.08) / 647.57 | **Provisional, not observed.** The source is a draft feasibility study (p. 94), and 141.76 t/d may be a recovery *potential*. The denominator is the 2023 RIPS total, so its consistency with the FS year is unverified. M2 uses the TPA-delivery lower bound 465.08 / 647.57 = **71.82%**, and 93.71% is shown as the upper value. Excluded from M1. |
| Kab. Serang | none | 7.45% observed (existing) | Baseline 7.45%. The **14% figure is a 2025 service target** (pp. 180–181) and is used only as a scenario. Excluded from M1. |
| Kab. Bogor | 22.10% (candidate) | 35.15% urban managed waste | Both kept: M1 uses 22.10%, M2 uses 35.15%. |
| Kota Denpasar | 10.73% (candidate) | 96.91% reduction + handling | Both kept; they differ by 86 percentage points. |
| Kab. Cianjur | 2.67% (candidate) | 30.36% regency service level | Both kept. |
| Kab. Indramayu | 8.59% (candidate) | 61.52% (12.72% reduction + 48.80% handling) | Both kept. |
| Kota Semarang | 8.27% (candidate) | 59.43% service level | Both kept. |
| 14 others | v2 values | none | Carried over. Their definition is not re-audited in this update. |

The five status-index candidates came from the RIPS status index, chosen to keep the v2 series comparable. Their definition and denominator are not documented in the package rows, so they are comparable only within that series. Qmanaged ≤ Q2025 holds for every value (checked).

## 5. Taxonomy decisions
- The taxonomy has 10 Level I groups, 36 Level II fractions and 56 Level III refinements, keyed by `(level, code)`. Codes are stored as text.
- The 56 Level III entries are a **provisional catalogue**. They are not 56 measurements, not a literal reproduction of Edjabou Table 2, and not a terminal partition. The six food sub-fractions are a modelling interpretation.
- The Level III detail file is not forced to 100%; it covers 76.8–93.7% of each stream.
- For a full terminal partition, v3 combines 54 Level III leaves with the 24 Level II fractions that have no children (including 6.1 and 6.2). That gives **78 leaves**. No residual leaf was needed, because every catalogued parent is fully split by its priors; the code would create an `X.R` leaf otherwise.
- The ferrous/non-ferrous split (6.x.*) has the parent "all metal" and crosses 6.1 and 6.2. It is excluded from the terminal partition and documented in the crosswalk.
- Code 4.3 ("cartons, plates and cups") is broader than Edjabou's "beverage cartons", and beverage cartons appear again as 4.4.1. No Danish value is copied to 4.3, and the overlap is noted.
- Danish SF/MF benchmarks are labelled as a foreign benchmark for residual household waste: not Indonesian, not non-domestic.
- Conditional shares are computed only for the 11 complete sibling sets. Board, inert, 2.2, 3.7 and 4.4 are incomplete and are not renormalised.
- The workbook allocation priors are kept with audit IDs. A01, A03, A05, A09 and the equal-share Level III splits are priors with no basis. A04 and A06–A08 are rounded Danish ratios. **The model does not use them.** It computes on the ten model fractions mapped directly from the RIPS categories.
- The Dirichlet α values of 80 and 40 are labelled as heuristics, not calibrated. The composition prior and the parameter uncertainty are reported separately (`10_uncertainty_split.csv`).

## 6. Parameter decisions (Table 2 and detailed table)
| Item | v2 | v3 | Reason |
|---|---|---|---|
| DOC, carbon, fossil share | IPCC 2006 Table 2.4 | Unchanged, with status recorded | Global defaults, labelled as such |
| DOCf | IPCC 2019 (rubber 0.50) | IPCC 2019 Table 3.0; **rubber/leather blank**, model scenario 0–0.5 (central 0) | No verified DOCf for the aggregate, so the legacy 0.50 is not reused |
| Moisture | As-received values (assumption) | Main case keeps the as-received values; **IPCC defaults (as generated) run as a scenario** | The functional unit is waste at the gate. IPCC moisture describes waste before collection. Both options keep DOC, carbon and LHV on the same dry mass |
| Plastic LHV | 33.0 × k_h (= 29.7) | **30.5 MJ/kg TS**, Götze et al. 2016 aggregate median (LHV, not HHV), without k_h; range ±10% (C10b) | Literature value on a matching dry basis |
| Other dry LHV, τ | Legacy | Legacy, labelled "not re-verified". τ for paper, plastic and wood is based on C&I-waste data (Nasrullah 2014) | No verified sub-fraction or MSW-specific values |
| Level II/III values | n/a | IPCC parent proxies keep "proxy" status. Blank means unknown | IPCC does not measure these sub-fractions |
| Garden / wood / soil | n/a | Plant material: garden proxy (DOCf 0.70). Woody material and straw: wood (0.10). Humid soil: inert (DOC 0) | No grass DOC for mineral soil, and no DOCf 0.70 for branches |
| Composite products | n/a | 4.3, 4.4.1, 4.4.2: whole-product values blank, with a paper proxy for the fibre portion only. 5.3.2, batteries, HHW, WEEE, tampons, condoms: blank | Material composition is needed |
| Animal-derived food | n/a | Food proxy kept, with a warning about bones and eggshells | Proxy bias stated |
| Metal/glass moisture (L2/L3) | n/a | Blank (contamination moisture not measured). The IPCC 100% dry matter applies at Level I only | As in the source file |
| DOC wet | n/a | `DOC_wet = DOC_dry × (1 − moisture)`, applied once | Avoids a double moisture correction |
| AD yield vs DOCf | separate | Still separate (register A8 is labelled as a BMP-type parameter) | Landfill DOCf ≠ BMP |
| 2026 literature (Malang) | n/a | Kept as a material/plastic benchmark outside the 21 locations | Publication year is not composition year |

## 7. Effect on results (v2 → v3, market case)
- Most probable pathway (sampled carbon value 0–100 USD/t): v2 had SL 18, S5 3; v3 has SL 17, S5 4. The only change is Kab. Brebes (SL 0.49 → S5 0.47), which is a statistical tie in both versions.
- No change under Perpres 109 (S1 in Serang, Semarang, Brebes), food separated at source (S3 in 16), or GWP20 (S5 20, S1 1).
- At central values, SL is best everywhere at 0 and 25 USD/t. S5 is best in 18 locations at 50 USD/t and in 20 at 100 USD/t.
- LHV rises by about 0.1 MJ/kg (plastic LHV change). L0 falls slightly (rubber DOCf 0.5 → 0; Denpasar −4 kg CH₄/t).
- **The new data scenarios show what the ranking depends on.** Projecting the overview tonnage and setting rubber DOCf to 0.5 change nothing. IPCC moisture makes S5 the most probable pathway in 20 of 21 locations. Under the managed-tonnage series, SL is most probable in 18/19 locations (M1) and 20/21 (M2).

## 8. Data gaps (left open; no numbers invented)
1. Fraction-level moisture and LHV measured as received in the study locations (local proximate analysis).
2. DOCf for rubber and leather.
3. Material composition of coated board and cartons, composite film, batteries, HHW, WEEE, tampons and condoms.
4. Level III sorting data for food, miscellaneous paper and board, and garden (2.2.4), plus Indonesian Level II data in general.
5. The year of the RIPS overview tonnage index, and the definition and denominator of the RIPS status index.
6. Whether Padang's 141.76 t/d recovery is actual or potential, and the year of the FS numbers.
7. Chlorine and sulphur of RDF (gate not testable).
8. Verified plant and kiln coordinates.
9. The numerical tables of Riber et al. (2009), plus Indonesian measurements for metal and glass contamination moisture.
10. Status-L literature values in the register still need checking against the original sources.

## 9. What is still assumption (status A or heuristic)
- The as-received moisture values.
- The legacy dry LHV and τ values.
- The growth fallback.
- γ and the yard rule.
- The Dirichlet α values.
- Holding the managed share constant.
- Padang's lower bound as the M2 value.
- Using overview totals as reported.
- All Level II/III allocation priors (database only).
- Analyst cost and engineering assumptions in the register (status A).
- The provisional coordinates.

## 10. Validation actually run (`outputs/validation_report.md`)
49 of 49 checks passed.

| Group | Checks |
|---|---|
| Blank vs zero | 6 |
| Taxonomy keys | 7 |
| Mass balance | 6 |
| Parent–child | 5 |
| Single projection | 6 |
| Qmanaged ≤ Qtotal | 5 |
| Wet/dry basis | 7 |
| Provenance | 7 |

In addition, the model asserts three things for every run:
- the single projection, for every location;
- the S5 mass balance;
- that biogas carbon stays below food carbon.

The notebook was executed end to end without errors.

---

# v3.1 (5 October 2026): conditions, energy, land, reproducible tool

## What was added
| Item | Files | Notes |
|---|---|---|
| Python package `mswpath` | `mswpath/core.py`, `inputs.py`, `discovery.py`, `report.py`, `ui.py` | The model is a class (`MSWModel`) that holds no parameter values. G and C reproduce v3 **exactly** (regression test against the v3 outputs: maximum difference 0.0). |
| Fossil cumulative energy demand (CED) | `data/lcia_factors.csv`, `outputs/13_ced_landuse_central.csv` | Non-renewable (fossil) MJ per t MSW. Reuses the existing inventory with conversion factors: PEF = 3.6/η × (1 + fuel supply) × grid multiplier; IPCC diesel factor for ancillary burdens; coal energy plus supply; polypropylene cradle-to-gate CED. Biogenic energy is not counted. |
| Land take | same | m² per t MSW: landfill area consumed, 1/(ρH) × gross factor, plus plant footprints over their lifetime. |
| Scenario discovery | `run_discovery.py`, `outputs/discovery_*` | 40,000 combinations of city and system conditions. Rules come from a CART tree (depth 4; accuracy 0.70 vs a majority-class baseline of 0.43) and PRIM boxes, shown on condition maps. |
| Sobol indices | `outputs/discovery_sobol_*` | Computed per location: N = 512 base samples, 58 register inputs plus the wetness draw, with composition fixed at each location's value. |
| Colab decision tool | `MSW_Decision_Tool_Colab.ipynb`, `templates/`, `requirements.txt`, `CITATION.cff` | Reproduce the 21 locations, upload your own cities (template + validator), use an interactive form, see the decision rules, and download Excel/HTML reports in English or Indonesian. |
| Validation | `validate_v3.py` | 60 checks, all passing (49 from v3, 7 new CED/LU checks, 4 new template checks). |

## Source decisions for CED and land use
- **Network limitation.** Fetching web pages was blocked by the network policy in this session. Only search-result snippets could be read, so no CED or land-use factor is marked as verified (status V).
- **Thermal efficiency of displaced fossil generation:** 0.32 (range 0.28–0.36). Two values from search snippets support it, neither opened in full: PT PLN Indonesia Power's 2022 fleet thermal efficiency of 32.09% (annual report), and a net heat rate of about 2,460 kcal/kWh (≈ 35%) at the Indramayu plant. Status **L**: verify against the originals.
- **IPCC 2006 fuel factors:** diesel 74.1 kg CO₂/GJ and natural gas 56.1 kg CO₂/GJ, status L. Converting ancillary kg CO₂e to diesel MJ is an approximation, because those terms also contain materials.
- **Polypropylene CED:** 75 MJ/kg (range 65–85), the order of magnitude of the PlasticsEurope eco-profile. Status **L**, not verified in this session. It matters only for S4 PHB.
- **Land-use factors:** density, fill height, gross-area factor and plant footprints are **analyst engineering ranges (A)**. As a cross-check, an Indonesian TPA design study (Talumelito, Gorontalo, from a search snippet) implies about 0.045 m²/t, against 0.081 m²/t central here.
- **Effect on G and C:** the CED/LU factors are drawn after every v3 draw, so G and C are unchanged (verified by test).

## Main new results
- **CED (central, market case):**
  - RDF + AD saves −4.5 GJ/t, WtE −4.2 GJ/t, RDF −3.8 GJ/t.
  - PHB saves −0.6 GJ/t and AD −0.5 GJ/t.
  - The sanitary landfill consumes +0.08 GJ/t.
- **Land take:**
  - Open dump 0.40 m²/t and sanitary landfill 0.081 m²/t.
  - RDF + AD 0.045 m²/t and WtE 0.019 m²/t.
- **Decision rules for mixed waste without the WtE tariff:**
  - Landfill with flare is best in 62% of draws and RDF + AD in 27%.
  - Landfill with flare is preferred below about 40 USD/t CO₂e (purity 0.95), and at any carbon value where no kiln lies within about 300 km.
  - RDF + AD is preferred above about 55 USD/t when a kiln is within about 300 km.
- **Other decision rules:**
  - WtE only with the tariff, at least 1,000 t/day and LHV ≥ 7 MJ/kg (PRIM density 0.97).
  - AD alone where food is separated at source and carbon is valued above about 30 USD/t.
  - RDF alone and PHB are best in fewer than 3% of draws.
- **Sobol, carbon-inclusive cost gap between RDF + AD and the landfill at 50 USD/t:** moisture as received (ST 0.28) and landfill-gas collection (0.23) dominate, followed by RDF O&M (0.10). This confirms the v3 moisture scenario.

## Limits specific to v3.1
- The decision rules approximate the model: 70% accuracy on held-out draws.
- The rules hold for compositions like the 21 RIPS compositions; outside that range they are extrapolations.
- Land-use and several CED factors are assumptions.
- No license file has been added; the authors must choose one before public release.
- If the GitHub repository is private, the Colab "clone" step fails. The notebook then asks for the ZIP to be uploaded instead.

---

# v3.2 (5 October 2026): verified secondary data, carbon-inclusive cost, ex-ante decision support

## Purpose restated
None of the options (WtE, RDF, AD, PHB, RDF + AD) exists as a facility in the 21 locations. The model is **ex-ante decision support**: it tells local governments, planning agencies, offtakers (PLN, cement companies) and funders, *before* a feasibility study or tender, under which conditions each option becomes preferable (the research question), what performance it must reach, and which data to collect first. This is the stated novelty in the methodology (Section 1) and the manuscript (Introduction, Section 4.2).

## Secondary data re-verified (`data/secondary_data_verification.csv`, 23 items)
Full texts could not be downloaded (web fetch blocked), so the check used abstracts, indexed records and search extracts. The method is recorded per item. Unverified benchmark values were removed rather than kept.

| Item | v3.1 | v3.2 | Basis |
|---|---|---|---|
| RDF O&M `o_rdf` | 12 (8–21) USD/t | **18.4 (12–24.5)** | Rp 300–400k/t processed (Indonesian cost study) |
| RDF price at kiln `p_rdf` | 1.6 (1.0–2.6) USD/GJ | **1.15 (0.58–1.34)** | Rp 150–350k/t RDF (Bantargebang, news) |
| Landfill-gas collection `cap` | 0.50 (0.30–0.80) | **0.50 (0.20–0.80)**, status A | Lower in high-food-waste landfills |
| Landfill cost `c_sl` | 20 (12–30) | **20 (9–30)** | Rp 145k/t controlled landfill (BPK) |
| Grid Sumatera | 0.94 | **0.832** kg CO₂/kWh | ESDM 2018 via JCM/GEC |
| Grid Jamali / Mahakam | 0.87 / 1.14 | 0.877 / 1.128 | same source, unrounded |
| WtE benchmark | 632 kWh/t (Azis 2021) | **385–473 kWh/t** | 632 is not in the source; Yuliani 2022 and the Azis abstract |
| RDF NCV benchmark | 12.6–13.8 MJ/kg (GIZ) | **15–16.7 MJ/kg** | Cilacap RDF |
| Moisture benchmark | 0.53–0.56 (Prabowo), 0.64–0.66 (Fiki) | **0.554** (Cilacap raw MSW) | the old values could not be confirmed |
| Confirmed (status V) | many | **only `y_ch4`, `K_wte`, `o_wte`, `p_el`** | the others are now L; the old status is kept in `status_v3_1` |

The 21 RIPS compositions and tonnages were not re-verified (the source PDFs are not available in this session).

## Model changes
- **Carbon-inclusive cost** replaces "social cost": CIC = C + pG/1000. It values only greenhouse gases.
- **Fixed carbon values** replace the sampled carbon value as the main result: 0, 2 (UU 7/2021 carbon tax), 25, 50 and 100 USD/t CO₂e.
  - Each result is reported as P(best) with its Monte Carlo standard error (≤ 0.008) and a 95% interval.
  - The "statistical tie" wording is removed. A small lead is decision uncertainty, not sampling noise.
- **AD on mixed waste made realistic** (`data/scenario_parameters.csv`):
  - methane yield × `y_pen_mech` = 0.6 (0.35–0.85) (Seruga 2020; Basinas 2020, 2021);
  - pre-treatment `pre_ofmsw` = 5 (0–10) USD/t feed;
  - the scenario `AD_feed_optimistic` removes both.
- **Missing categories.** A non-reported category is zero in the main case; this is a modelling assumption. The scenarios `glass_imputed` (glass share drawn from the 15 locations that report it) and `dirichlet_pseudocount` (0.5) test it.
- **Stress tests:**
  - `moisture_high_bound`: every moisture at its upper bound.
  - `rdf_ncv_stress`: RDF energy × 0.78.
  - `phb_large_scale_cost`: an aspirational what-if, not a planning value.
- **Break-even targets** (`mswpath/thresholds.py`, `outputs/17_*`).
- **Value of information**: EVPI and EVPPI by measurement group (`mswpath/voi.py`, `run_voi.py`, `outputs/voi_*`).
- **Benchmarks** now carry computed verdicts (`outputs/09_validation_benchmarks.csv`).
- **New outputs:** `15_p_best_fixed_carbon_value.csv`, `16_stress_tests_ranking_changes.csv`, `17_break_even_thresholds_vs_SL.csv`, `18_scenario_parameters_used.csv`.
- **Validation:** 78 checks, all passing. They include 18 new v3.2 checks and an explicit hand recalculation of every Padang result.
- **v3.2 does not reproduce v3.1.** This is intended: the corrected data and the AD realism change the results.

## Main results (market case)
- **Central values:**
  - The landfill with flare is best in all 21 locations at 0, 25 and 50 USD/t.
  - At 100 USD/t, RDF + AD is best in 20 locations and WtE in 1 (Kutai Kartanegara).
- **Most probable option:**
  - The landfill is most probable everywhere up to 50 USD/t.
  - At 100 USD/t, RDF + AD is most probable in 18 locations (median P = 0.60).
- **Abatement cost** of RDF + AD vs the landfill: median 73 (range 53–99) USD/t CO₂e. In v3.1 it was about 42; the change comes from the corrected RDF data and the AD penalty.
- **Robustness:**
  - The 100 USD/t result is fragile: `moisture_high_bound` and `rdf_ncv_stress` each return 17 of 21 locations to the landfill.
  - IPCC moisture moves 7 locations away from the landfill at 50 USD/t.
  - Glass imputation and the pseudocount change at most 2 locations, and only at 100 USD/t.
- **Decision rules** (40,000 draws; tree accuracy 0.78 vs a 0.51 baseline):
  - The landfill is preferred for mixed waste without the tariff in 79% of draws.
  - RDF + AD forms no tree leaf. Its PRIM box needs a carbon value above about 64 USD/t and a kiln within about 300 km (density 0.42).
  - AD is preferred only with source separation and a carbon value above about 30–45 USD/t.
  - WtE is preferred only with the tariff, at least 1,000 t/day and LHV ≥ 7 MJ/kg.
- **Value of information** (median EVPI):
  - 0.24 USD/t at 25, 1.05 at 50 and 3.18 at 100 USD/t.
  - At 100 USD/t, waste characterisation and landfill-gas performance dominate.

## Limits specific to v3.2
- **Verification depth.** The secondary-data check is at abstract/snippet level. Status L values should be checked against the full texts before submission.
- **VOI method.** The EVPPI uses a regression approximation, and group values are conservative.

## v3.2 addendum: one-city notebook and manual-input template
- **`MSW_Single_City_Colab.ipynb`** (built by `make_single_city_colab.py`): the full analysis for one city, step by step.
  - Input options: a template upload, values typed in the notebook, or the Kota Padang example.
  - Steps: characterisation, central results, P(best) at fixed carbon values with standard errors, uncertainty and drivers, scenarios and stress tests, break-even targets, value of information.
  - Output: an Excel report plus figures.
- **Templates** (built by `make_single_city_template.py`):
  - `templates/single_city_template.xlsx` with sheets `guide_panduan`, `city_data`, `composition` and `local_parameters`. Input cells are yellow, example cells grey, and there are drop-down lists and a composition TOTAL row.
  - The same content as three CSV files.
  - Filled Kota Padang examples in XLSX and CSV.
- **Template contents.**
  - Domestic and non-domestic composition and tonnage can be entered separately.
  - Optional local values (moisture or dry LHV per fraction, landfill cost, prices, CAPEX/O&M, AD yield, ...) replace the defaults. If only a central value is given, the default relative range is kept.
- **Module:** `mswpath/single.py`.
- **Validation:** 5 new checks; 83 of 83 pass.
  - The Padang template reproduces the database results.
  - CSV and XLSX give the same input.
  - A blank template is rejected.
  - Blank glass is "not reported".
  - Local values are applied, and a fresh model keeps the defaults.
- **Known limitation:** the composition TOTAL formula is computed when the file is opened in Excel or Google Sheets. It could not be pre-calculated here because LibreOffice is unavailable in the build environment. The model ignores that row.

## v3.2 addendum: projection to 2045
- **What it computes.** The six options in 2045 compared with the existing condition of 2025, for all 21 locations.
  - Files: `run_projection_2045.py`, `mswpath/projection.py`, `data/projection_2045_parameters.csv` and `outputs/proj2045_*`.
  - Reports: two separate documents, `docs/Projection_2045_Environmental_EN.pdf` and `docs/Projection_2045_Economic_EN.pdf`.
- **Drivers (user-specified):**
  - Grid emission factor declines linearly to zero in 2060, with δ = 1/35 per year. In 2045 it is 0.429 × the 2025 value.
  - Real escalation of the electricity value: 2%/yr.
  - Real escalation of labour-driven O&M: 1%/yr.
  - Real escalation of landfill and open-dump cost: 2%/yr. This is the third rate of the "2, 1 and 2%" set; this interpretation is to be confirmed.
  - Tonnage grows at each location's rate.
- **Held constant:** CAPEX in real terms, composition, landfill-gas collection, coal displaced in kilns, and the Perpres tariff in real terms.
- **Method:**
  - Each driver is also run alone, to decompose the change.
  - The Monte Carlo draws are paired between the two years.
  - A year path from 2025 to 2060 is computed.
- **Main results** (medians over 21 locations):
  - **GHG per tonne:**
    - WtE rises by 177 kg CO₂e/t, from 130 to 296.
    - AD rises by 11.
    - RDF falls by 20.
    - RDF + AD changes by −1.
    - Landfill and PHB are unchanged.
  - **Net cost per tonne:**
    - Landfill rises from 18.4 to 25.9 USD/t.
    - WtE falls from 57.9 to 50.8 USD/t, because of higher electricity revenue and larger plants.
    - RDF, AD and RDF + AD rise by 5–8 USD/t.
  - **Preferred option:** landfill up to 50 USD/t in both years. At 100 USD/t, RDF + AD is most probable in 19 locations in 2045 (18 in 2025).
- **Validation:** 4 new checks; 87 of 87 pass.

## v3.2 addendum: combined projection report, projection in Colab, mathematical model
- **One projection report.** The environmental and economic reports are merged into `docs/Projection_2045_EN.pdf`:
  - shared key findings and method;
  - Part A covers environmental performance and Part B economic performance;
  - a joint reading for the decision closes the report.
  - The two separate PDFs were removed.
- **Projection in the one-city notebook.** Step 9 of `MSW_Single_City_Colab.ipynb` adds the 2045 projection, computed by `mswpath.single.project_city`. Its outputs:
  - central results for 2025 and 2045;
  - the change by driver;
  - the uncertainty of the change (paired draws);
  - P(best) at fixed carbon values;
  - the year path from 2025 to 2060;
  - an Excel report.
  - Local parameter values are read as 2025 values and escalated. `mswpath.projection.apply_year` makes this possible.
- **`docs/MSW_Mathematical_Model_EN.pdf`** (built by `docs/make_math_model_pdf.py`) contains all 45 equations as implemented. It covers:
  - harmonisation, characterisation and the landfill module;
  - the TEA building blocks and the six options;
  - CED and land take, and the feasibility gates;
  - decision analysis and uncertainty;
  - Spearman and Sobol sensitivity, and scenario discovery (CART, PRIM);
  - EVPI/EVPPI and break-even targets;
  - the 2045 projection;
  - symbol and value tables generated from `data/*.csv`, with a map from symbols to data keys.
- **Validation:** 1 new check (the one-city projection reproduces the 21-location projection for Padang).

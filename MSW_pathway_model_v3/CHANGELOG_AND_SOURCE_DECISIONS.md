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

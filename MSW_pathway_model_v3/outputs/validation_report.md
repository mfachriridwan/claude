# Validation report (v3)

49 of 49 checks passed.

| Group | Check | Result | Detail |
|---|---|---|---|
| blank vs zero | Blank DOCf in source stays blank in output (not coerced to 0) | PASS | source blanks 53, output blanks 56 |
| blank vs zero | Rubber/leather DOCf blank in material tables; model value only in register scenario | PASS | C12 docf_rubber 0 (0-0.5) |
| blank vs zero | Benchmark blanks are not zero (e.g. 4.3, 9.3, 9.4 remain blank) | PASS |  |
| blank vs zero | Glass blank (not reported) distinguished from glass folded or reported | PASS |  |
| blank vs zero | Parser: blank glass and reported-zero glass give the same shares (no imputation) | PASS |  |
| blank vs zero | Parser rejects the old input file (no 2025 columns) | PASS | tmpqm9hwbjx.csv is not a v3 input; missing columns: ['Q2025_tpd', 'M_dom_tpd_2025', 'M_nd_ |
| taxonomy | (level, code) unique in crosswalk | PASS | 102 rows |
| taxonomy | (level, code) unique in parameters | PASS | 102 rows |
| taxonomy | (level, code) unique in benchmarks | PASS | 92 rows |
| taxonomy | Counts: 10 Level I, 36 Level II, 56 Level III | PASS |  |
| taxonomy | Every Level II/III parent exists (6.* cross-parent documented) | PASS | [] |
| taxonomy | Cross-parent metal rows carry a crosswalk note | PASS |  |
| taxonomy | Same code at different levels never collides (keys 'L{level}:{code}' unique) | PASS |  |
| mass balance | M_dom_2025 + M_nd_2025 = Q2025 (all 21) | PASS | max error 1.00e-04 t/d |
| mass balance | Each stream scaled by the same factor | PASS |  |
| mass balance | Level I sums to 100% per stream (42 streams) | PASS | max |dev| 1.0e-08 |
| mass balance | Level I tonnes sum to the 2025 stream mass | PASS |  |
| mass balance | Level II sums to its Level I parent | PASS | max |dev| 1.0e-08 pp |
| mass balance | Terminal partition sums to 100% per stream (no parent lost) | PASS | 78 leaves; max |dev| 7.0e-08 |
| parent-child | Catalogued Level III children sum to their Level II parent (6.* to 6.1 + 6.2) | PASS | max |dev| 3.0e-08 pp |
| parent-child | Level III detail file is a partial coverage (not forced to 100%) | PASS | coverage 76.8-93.7% of stream |
| parent-child | Conditional benchmark shares sum to 1 for complete sibling sets | PASS | 11 complete sets |
| parent-child | No conditional share computed for incomplete sibling sets | PASS | incomplete parents: ['2.2', '3.7', '4', '4.4', '9'] |
| parent-child | Benchmarks labelled as foreign (Denmark, residual household waste) | PASS |  |
| projection | Q2025 = Qt (1+g)^(2025-t), applied once | PASS |  |
| projection | Exponent is 0 where the tonnage is already 2025 | PASS | 10 rows already 2025 |
| projection | Overview totals: year left blank (unverified), not labelled as 2025 observation | PASS | 8 overview rows |
| projection | Projected alternative reported for every overview row with an older composition year | PASS |  |
| projection | Composition carries no growth factor (shares only, treatment declared) | PASS |  |
| projection | Composition sampling year, tonnage source year and baseline year stored separately | PASS |  |
| Qmanaged | Qmanaged_M1_tpd <= Q2025 | PASS | 19 locations with a value |
| Qmanaged | Qmanaged_M2_tpd <= Q2025 | PASS | 21 locations with a value |
| Qmanaged | Serang baseline uses observed 7.45%, the 14% target only as scenario | PASS |  |
| Qmanaged | Padang 93.71% labelled provisional; model series uses TPA-only lower bound | PASS |  |
| Qmanaged | Status-index series and local series kept in separate columns (no silent mixing) | PASS |  |
| wet/dry | DOC_dry x (1 - IPCC moisture) reproduces IPCC wet DOC (single moisture correction) | PASS | max |dev| 0.005 |
| wet/dry | L2/L3 DOC_wet_derived = DOC_dry x (1 - moisture) | PASS |  |
| wet/dry | All model LHV values declared on a dry basis; plastic uses the LHV (not HHV) median | PASS |  |
| wet/dry | Landfill DOCf and AD methane yield are separate parameters (no BMP as DOCf) | PASS |  |
| wet/dry | Grass, wood and soil separated: humid soil not given grass DOC; woody material DOCf 0.10 | PASS |  |
| wet/dry | Composite products have no whole-product proxy (4.3, 4.4.1, 4.4.2, 5.3.2, WEEE, HHW, batteries) | PASS |  |
| wet/dry | Normalisation factor recorded for every stream (Pemalang 100.10% -> factor 0.999) | PASS |  |
| provenance | Every input row has source document, pages and quality flags | PASS |  |
| provenance | Every Table 2 parameter has a status column filled | PASS |  |
| provenance | Every register row has source, status (V/L/A) and evidence type | PASS |  |
| provenance | Every non-blank material parameter has a status, a source URL and a locator | PASS | 71 rows with values |
| provenance | Parent-category proxies keep their proxy status | PASS |  |
| provenance | Benchmark values carry source table and geography | PASS |  |
| provenance | Heuristic Dirichlet concentrations declared as heuristic, not calibrated | PASS |  |

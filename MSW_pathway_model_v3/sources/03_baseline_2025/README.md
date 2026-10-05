# MSW pathway model: baseline 2025 input package

This package is a non-destructive supplement to `output/input_kota.csv`. It makes the year transformation, managed-waste selection, and parameter provenance explicit so it can be reviewed or supplied to Claude.

## Files to provide to Claude

- `input_kota_2025_claude_ready.csv` - 21 city/regency records. Original columns are preserved. Use `M_dom_tpd_2025`, `M_nd_tpd_2025`, `Q2025_tpd`, `managed_share_recommended`, and `Qmanaged_2025_tpd` as the 2025 baseline fields.
- `table2_fraction_properties_IPCC_2019.csv` - fraction properties for the methodology Table 2, including scope and source status.
- `qmanaged_2025_source_audit.csv` - all recommended and alternative managed shares, including the seven previously empty Table 5 values.
- `city_composition_source_audit.csv` - the local RIPS composition source and its 2025 treatment for each location.
- `validation.json` - machine-readable checks.

## Tonnage harmonisation

For each location the package calculates:

`Q2025 = Qt x (1 + g)^(2025 - t)`

where `t` is `year_base`, unless the RIPS total is labelled as an overview/current total. In that case `mass_year_used = 2025`, the exponent is zero, and the reported total is retained. For a missing growth rate, the documented model fallback is 1.73%/year. Domestic and non-domestic mass are projected by the same factor, so `M_dom_tpd_2025 + M_nd_tpd_2025 = Q2025_tpd`.

Composition is deliberately **not** extrapolated. Each RIPS composition is retained as its sampled/base-year wet-mass distribution because the sources provide no defensible annual composition trend. This must be reported as a baseline assumption, not interpreted as a measured 2025 composition.

## Managed-waste rule for Table 5

The existing 14 Table 5 values are based on the RIPS status index. To keep that series comparable, the same index measure was retained for Bogor, Denpasar, Cianjur, Indramayu, and Semarang. The local RIPS service/management indicators are retained in separate alternative columns because their denominators and definitions differ materially from the status index.

Padang and Serang were absent from that index. Padang uses a local derived operational share of 93.71%, calculated as `(141.76 + 465.08) / 647.57`; Serang uses the observed local share of 7.45%. Serang's 14% 2025 target is supplied as a scenario only and is not used as an observed baseline.

## Table 2 parameter rule

The current inventory guidance is the **2019 Refinement to the 2006 IPCC Guidelines**, whose Volume 5 Waste chapter was corrected in July 2023. The refinement updates DOCf; the 2006 Table 2.4 remains the source for default dry matter, DOC, total carbon, and fossil-carbon fraction.

The package therefore uses IPCC defaults for moisture/dry matter, DOC, total carbon, and fossil-carbon share. It uses 2019 IPCC DOCf values of 0.70 for food and garden/grass, 0.50 for paper and textile, and 0.10 for wood. Rubber/leather is intentionally not assigned a new DOCf because the aggregate mixes materials with different biodegradability; plastics, metal, glass, and inert residuals do not have DOCf values.

`dry_LHV_MJ_per_kg` and `RDF_transfer_fraction_tau` are retained from the existing model only as **legacy assumptions**. IPCC does not provide those two parameters in the cited tables. Replace them only with a source that matches the fraction definition and declares wet/dry basis, sampling location, and measurement year.

## Source hierarchy used

1. City/regency-specific RIPS evidence for mass, composition, and local management indicators.
2. Indonesia-wide peer-reviewed or government evidence where a compatible local measurement is unavailable.
3. IPCC global defaults only for parameters not measured locally, and labelled as defaults.

The RIPS files searched did not provide a consistently measured, fraction-level moisture/LHV series for the 21 study locations. It would be methodologically weak to manufacture city-specific physical properties from composition percentages alone.

## Primary references

- IPCC (2006), Volume 5 Waste, Chapter 2, Table 2.4: https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/5_Volume5/V5_2_Ch2_Waste_Data.pdf
- IPCC (2019 Refinement), Volume 5 Waste, Chapter 3, Table 3.0: https://www.ipcc-nggip.iges.or.jp/public/2019rf/pdf/5_Volume5/19R_V5_3_Ch03_SWDS.pdf
- Local RIPS references and page/table locators are recorded row-by-row in the CSV audit files.

## Required review before publication

- Check the five large differences between status-index and local-service definitions before interpreting cross-city managed-waste rankings.
- Normalize the rounded composition flagged in the data (`kab_pemalang`) before use in a model that does not already normalize shares.
- Treat all composition data as waste-at-generation/collection unless a source explicitly says landfill gate. IPCC asks for this distinction before landfill-emission calculations.

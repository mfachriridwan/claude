# RIPS waste-fraction hierarchy (10 / 36 / 56)

## Output

- **Level I:** 10 broad material groups.
- **Level II:** 36 harmonised fractions.
- **Level III:** 56 detailed refinements.
- Coverage: 21 cities/regencies and 42 domestic/non-domestic stream records.

## Interpretation

The RIPS documents generally report broader aggregate categories. Level I preserves those measured aggregates as closely as possible. Level II and Level III values are modelled allocations and are labelled as estimates. They are suitable as priors or scenario inputs, not substitutes for local sorting measurements. Percentages use wet mass. Each stream is normalised to 100% to remove source rounding differences; `source_sum_pct` and `normalization_factor` preserve the audit trail.

The provenance of every assumption has been re-audited in `ASSUMPTION_SOURCE_AUDIT.md`. Several allocations are modelling priors rather than literature-derived ratios; in particular A01, A03, A05, A09, and a number of Level III equal-share splits. These should be calibrated or included in sensitivity/Dirichlet uncertainty analysis before research conclusions are reported.

Level III is a catalogue of **56 detailed refinements of selected Level II parents**. It is not an exhaustive terminal partition of all waste, so its rows need not sum to 100%. Use `detail_coverage_pct_total` to see how much of the stream is represented.

The exact 56-item reconstruction is provisional. The source states the number 56, but its printed Table 2 contains ambiguous index notation and typographical inconsistencies, and it does not explicitly list the six Food refinements used in this implementation. See `ASSUMPTION_SOURCE_AUDIT.md` before treating the Level III list as a literal reproduction of the source.

## Reconciliation of the published count

The prose states 36 Level II fractions, while the printed Table 2 lists 37 if aluminium wrapping foil is counted separately. This implementation merges aluminium foil into metal packaging, producing the stated 36 Level II groups. It also corrects obvious numbering typos for condoms and WEEE while retaining the category meanings.

## Main mapping rules

| ID | Rule | Parameter |
|---|---|---|
| A01 | **Organic lumped in food:** If RIPS combines organics, allocate 85% to Food and 15% to Gardening; add leaf and wood fractions to Gardening. | 0.85 / 0.15; editable prior |
| A02 | **Paper versus board:** Split RIPS kertas/kardus into Paper and Board. | 0.55 / 0.45; editable prior |
| A03 | **Miscellaneous residual:** Allocate RIPS lain-lain 80% to Miscellaneous combustibles and 20% to Inert. | 0.80 / 0.20; editable prior |
| A04 | **Food Level II:** Vegetable versus animal-derived food. | 0.80 / 0.20; literature-informed prior |
| A05 | **Gardening Level II:** Dead animal/excrement versus garden waste. | 0.02 / 0.98; editable prior |
| A06 | **Plastic Level II:** Base plastic: 35% packaging, 5% non-packaging, 60% film; all reported styrofoam is assigned to packaging plastic/PS. | 0.35 / 0.05 / 0.60; literature-informed plus source-aware |
| A07 | **Metal Level II:** Packaging (including foil) versus non-packaging metal. | 0.71 / 0.29; literature-informed prior |
| A08 | **Glass Level II:** Packaging, table/kitchenware, other/special. | 0.91 / 0.045 / 0.045; literature-informed prior |
| A09 | **Special Level II:** Reported WEEE is mapped directly; reported B3 is split 20% batteries and 80% other HHW. | B3 0.20 / 0.80; editable prior |
| A10 | **Level III meaning:** The 56 Level III entries are detailed refinements of selected Level II parents, not an exhaustive terminal partition; use the coverage field. | n/a; taxonomy rule |

## Validation

For every city and stream: Level I sums to 100%; Level II sums to its Level I parent; and mass values equal percentage × stream mass / 100. See `validation_audit.csv`.

## Sources

- Edjabou et al. (2015), *Municipal solid waste composition: Sampling methodology, statistical analyses, and case study evaluation*. https://doi.org/10.1016/j.wasman.2014.11.009
- Primary manuscript and Table 2: https://backend.orbit.dtu.dk/ws/files/119653490/Manuscript.pdf
- EU packaging material identification: https://eur-lex.europa.eu/eli/dec/1997/129/oj/eng
- EU WEEE Directive: https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex%3A02012L0019-20240408
- City-specific RIPS sources are recorded in each output row.

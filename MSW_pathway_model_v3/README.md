# MSW Pathway Model v3 (2025 baseline)

Screening LCA + TEA of six municipal solid waste management options for 21 Indonesian cities/regencies, updated with the October 2026 research package. The update adds a 2025 tonnage baseline, a Level I/II/III taxonomy and material parameters with auditable provenance.

## Run order
```bash
pip install numpy pandas matplotlib openpyxl      # reportlab, pillow and nbclient only for the PDFs/notebook
python build_database_v3.py     # sources/ -> data/*.csv
python msw_pathway_model_v3.py  # data/ -> outputs/ (about 15 s)
python validate_v3.py           # 49 checks -> outputs/validation_report.md
python docs/make_methodology_pdf.py && python docs/make_manuscript_pdf.py
```
In Colab, upload and unzip this folder, `cd` into it, and run `MSW_pathway_model_v3.ipynb`.

## Contents
| Path | What |
|---|---|
| `data/input_kota_2025_updated.csv` | Clean 21-location input with the 2025 baseline, provenance and quality flags |
| `data/table2_model_fraction_parameters.csv`, `data/fraction_parameters_L1_L2_L3.csv`, `data/taxonomy_crosswalk.csv` | Material parameters (Level I/II/III) and taxonomy crosswalk; blank = unknown |
| `data/table5_qmanaged_2025.csv` | Managed waste: status-index and local series kept apart |
| `data/assumption_register.csv`, `data/model_constants.csv` | Every number the model uses |
| `msw_pathway_model_v3.py`, `MSW_pathway_model_v3.ipynb` | Model (no hard-coded parameters) and executed notebook |
| `validate_v3.py`, `outputs/validation_report.md` | Validation suite and its report |
| `docs/MSW_Methodology_v3_EN.pdf` | Methodology v3 |
| `docs/MSW_Manuscript_v3_EN.pdf` | Draft journal article (authors still need to confirm the author list and declarations) |
| `CHANGELOG_AND_SOURCE_DECISIONS.md` | Changes, source decisions, data gaps, remaining assumptions |
| `sources/` | The update package files used (SHA-256 checked against its manifest) |

Status vocabulary used everywhere: local measurement, literature default, parent-category proxy, foreign benchmark, assumption/prior, scenario, gap.

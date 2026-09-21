# AGENTS.md

## Cursor Cloud specific instructions

This repository contains standalone educational scripts in **Python** (1 file) and **R** (6 files). There is no build system, no test framework, no CI/CD, and no package manifests.

### Scripts overview

| Script | Language | Purpose | Key libraries |
|---|---|---|---|
| `nlp_large_document.py` | Python | NLP pipeline for large documents | nltk, spacy, scikit-learn, matplotlib, wordcloud |
| `nlp_large_document.R` | R | NLP pipeline for large documents | tidytext, tm, quanteda, ggplot2, topicmodels |
| `spring_pendulum_viewer.R` | R | Interactive 3D plotly HTML viewer | deSolve, plotly, htmlwidgets |
| `spring_pendulum_3d.R` | R | Static 3D trajectory + analysis | deSolve, rgl |
| `spring_pendulum_animate.R` | R | 3D animation + GIF export | deSolve, rgl, gifski |
| `spring_pendulum_3d_animated.R` | R | Simplified 3D animation | deSolve, rgl |
| `spring_pendulum_3d_simple_animation.R` | R | Clean 3D animation variant | deSolve, rgl |

### Running scripts

- **Python**: `python3 nlp_large_document.py` (prints ready message; uncomment pipeline call with a file path to process a document)
- **R (plotly-based)**: `Rscript spring_pendulum_viewer.R` (generates interactive HTML files; note: hardcoded output path `/Users/scarpiy/Desktop` must be changed)
- **R (rgl-based)**: `xvfb-run --auto-servernum Rscript spring_pendulum_3d.R` (requires `xvfb-run` in headless environments for `rgl` 3D rendering)

### Gotchas

- **rgl requires a display**: Scripts using `rgl` (all `spring_pendulum_*.R` except `viewer`) need X11. In headless Cloud Agent VMs, prefix with `xvfb-run --auto-servernum`.
- **Hardcoded paths**: `spring_pendulum_animate.R` and `spring_pendulum_viewer.R` contain hardcoded output paths to `/Users/scarpiy/Desktop`. Redirect output or change the path when running.
- **No tests**: There are no automated tests in this repository.
- **No linter config**: There are no linting configurations. Use standard `pylint`/`flake8` for Python and `lintr` for R if needed.
- **matplotlib in headless mode**: Use `matplotlib.use('Agg')` before importing pyplot, or set `MPLBACKEND=Agg` environment variable when running Python scripts without a display.

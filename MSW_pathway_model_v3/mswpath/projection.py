"""
Projection of the six options to a future year (default 2045) against the existing condition of 2025.

Changes applied for year t (data/projection_2045_parameters.csv):
  grid      EF(t) = EF(2025) x max(0, 1 - delta (t - 2025)), delta = 1/35 (linear path to a net-zero grid in 2060);
            applied through the grid multiplier efg_k, so it changes electricity credits and burdens and the fossil CED
            of grid electricity
  costs     real escalation (1 + e)^(t - 2025): electricity value p_el (2%/yr), plant O&M o_wte, o_rdf, o_ad (1%/yr),
            landfill and open-dump cost c_sl, c_od (2%/yr); CAPEX constant in real terms
  tonnage   Q(t) = Q(2025) (1 + g)^(t - 2025) with each location's growth rate; composition held constant
Everything else (composition, landfill gas, coal displaced in kilns, diesel) is unchanged, so differences isolate the
three drivers. The same random numbers are used in both years, so changes are paired draw by draw.
"""
from pathlib import Path
import numpy as np
import pandas as pd
from .core import MSWModel

COMPONENTS = ("grid", "costs", "tonnage")


def read_parameters(data_dir):
    t = pd.read_csv(Path(data_dir) / "projection_2045_parameters.csv").set_index("key").value
    return {k: (float(v) if k != "tonnage_growth" else v) for k, v in t.items()}


def grid_factor(pr, year):
    return max(0.0, 1.0 - pr["delta_grid"] * (year - pr["base_year"]))


def scale_raw(raw, years):
    """Grow tonnage by (1+g)^years in every column the model reads (single projection preserved)."""
    r = raw.copy()
    f = (1 + r.growth_rate_used.astype(float)) ** years
    for c in ("Q2025_tpd", "Q2025_alt_tpd", "M_dom_tpd_2025", "M_nd_tpd_2025", "tonnage_source_value_tpd"):
        r[c] = r[c].astype(float) * f
    return r, f


def apply_year(M, raw, year, components=COMPONENTS, pr=None, data_dir=None):
    """Turn a 2025 model (possibly with local parameter values) into `year` in place, then load the cities.
    Local values are read as 2025 values and escalated like the defaults. Returns info."""
    pr = pr or read_parameters(data_dir)
    t = year - int(pr["base_year"]); info = dict(year=year, years=t)
    cols = ["central", "low", "high"]
    if "grid" in components:
        g = grid_factor(pr, year); M.REG.loc["efg_k", cols] = M.REG.loc["efg_k", cols].astype(float) * g; info["grid_factor"] = g
    if "costs" in components:
        for keys, e in ((("p_el",), pr["esc_electricity"]), (("o_wte", "o_rdf", "o_ad"), pr["esc_labour"]),
                        (("c_sl", "c_od"), pr["esc_landfill"])):
            for k in keys:
                M.REG.loc[k, cols] = M.REG.loc[k, cols].astype(float) * (1 + e) ** t
        info.update(f_el=(1 + pr["esc_electricity"]) ** t, f_lab=(1 + pr["esc_labour"]) ** t, f_lf=(1 + pr["esc_landfill"]) ** t)
    r = raw
    if "tonnage" in components and t:
        r, f = scale_raw(raw, t); info["tonnage_factor_median"] = float(np.median(f))
    M.load_cities(r)
    M.check_single_projection()
    return info


def model_for_year(data_dir, raw, year, components=COMPONENTS, seed=None, pr=None):
    """An MSWModel whose parameters and tonnage describe `year`. Returns (model, info)."""
    M = MSWModel(data_dir, seed=seed)
    info = apply_year(M, raw, year, components, pr=pr, data_dir=data_dir)
    return M, info

"""
Break-even (threshold) analysis: how good would one uncertain input have to be for an option to match the sanitary
landfill on carbon-inclusive cost?

For a location, all inputs are held at their central values and one input x is swept over a grid. The gap
    gap(x) = CIC_k(x) - CIC_SL(x),   CIC = C + p G / 1000   (USD per t MSW, p in USD per t CO2e)
is computed for every grid value in one vectorised model call, and the x where the gap changes sign is reported.
'never' means the option stays dearer than SL over the whole grid; 'always' means it is cheaper at every grid value.
Facilities do not yet exist, so these thresholds are targets that tenders, pilots or offtake contracts must reach.
"""
import numpy as np
import pandas as pd
from .core import PW

# input, grid low, grid high, direction of improvement (+1 = higher is better for the option)
SWEEPS = {
    "S5": [("y_pen_mech", 0.2, 1.2, +1), ("pre_ofmsw", 0.0, 30.0, -1), ("p_rdf", 0.0, 6.0, +1), ("o_rdf", 0.0, 40.0, -1),
           ("cap", 0.1, 0.95, -1)],
    "S3": [("y_pen_mech", 0.2, 1.2, +1), ("pre_ofmsw", 0.0, 30.0, -1), ("o_ad", 0.0, 60.0, -1), ("cap", 0.1, 0.95, -1)],
    "S2": [("p_rdf", 0.0, 6.0, +1), ("o_rdf", 0.0, 40.0, -1), ("cap", 0.1, 0.95, -1)],
    "S1": [("p_el", 0.03, 0.25, +1), ("K_wte", 40e6, 250e6, -1), ("o_wte", 5.0, 50.0, -1)],
}


def _cross(xg, gap):
    if (gap < 0).all(): return "always"
    if (gap > 0).all(): return "never"
    k = np.where(np.diff(np.sign(gap)) != 0)[0][0]
    x0, x1, g0, g1 = xg[k], xg[k + 1], gap[k], gap[k + 1]
    return float(x0 - g0 * (x1 - x0) / (g1 - g0))


def break_even(M, i, option, key, lo, hi, pc, n=401, scenario=None):
    sc = scenario or {}
    c, P, s, Q, kw = M.setup(i, n, True, **sc)
    xg = np.linspace(lo, hi, n)
    P[key] = xg.copy()
    R = M.model(s, P, c, Q, **kw)
    j, jsl = PW.index(option), PW.index("SL")
    cic = R["C"] + pc * R["G"] / 1e3
    gap = cic[:, j] - cic[:, jsl]
    feasible = bool(R["ok"][n // 2, j])
    central = float(M.REG.central[key]) if key in M.REG.index else float(M.SP.loc[key, "central"])
    return dict(central_value=central, break_even=_cross(xg, gap), feasible_central=feasible,
                gap_at_central=float(np.interp(central, xg, gap)))


def threshold_table(M, options=("S5", "S3", "S2", "S1"), carbon_values=(25, 50, 100), locations=None):
    rows = []
    idx = range(len(M.H)) if locations is None else [list(M.H.index).index(l) for l in locations]
    for i in idx:
        c = M.H.iloc[i]
        for opt in options:
            for key, lo, hi, d in SWEEPS[opt]:
                for pc in carbon_values:
                    r = break_even(M, i, opt, key, lo, hi, pc)
                    rows.append(dict(city=c.city, option=opt, input=key, carbon_value=pc, better_if="higher" if d > 0 else "lower",
                                     grid=f"{lo:g} to {hi:g}", **r))
    return pd.DataFrame(rows)

"""
Value of information: which uncertain inputs are worth measuring before choosing an option?

For a location and a fixed carbon value p, the net benefit of option k in Monte Carlo draw i is
NB_ik = -(C_ik + p G_ik / 1000) (USD per t MSW). The decision maker picks the option with the highest expected NB.

EVPI   = E_i[max_k NB_ik] - max_k E_i[NB_ik]
EVPPI(x) = E_x[max_k E[NB_k | x]] - max_k E[NB_k]  (the value of learning input x alone)

E[NB_k | x] is estimated by regressing NB_k on a cubic polynomial of x (the regression approach of Strong and Oakley
2013, with a polynomial instead of a GAM). The fit has a small positive bias, so the EVPPI of a random variable unrelated
to the model is computed as a noise floor and subtracted. Options that fail their gates in more than half
of the draws are removed from the choice set. Results are in USD per tonne and, multiplied by the 2025 tonnage, in
USD per year: the most a city should pay per year of decision for perfect knowledge of that input.
"""
import numpy as np
import pandas as pd
from .core import PW


def _evppi(x, NB, deg=3):
    z = (x - x.mean()) / (x.std() + 1e-12)
    X = np.vander(z, deg + 1)                                       # cubic polynomial with intercept
    beta, *_ = np.linalg.lstsq(X, NB, rcond=None)
    fit = X @ beta                                                  # E[NB_k | x] for every draw and option
    return float(fit.max(1).mean() - NB.mean(0).max())


def voi_location(P, s, R, FR, pc=50.0, deg=3, min_feasible=0.5, seed=0):
    keep = R["ok"].mean(0) >= min_feasible
    NB = -(R["C"] + pc * R["G"] / 1e3)[:, keep]
    evpi = float(NB.max(1).mean() - NB.mean(0).max())
    inputs = {k: v for k, v in P.items() if not k.startswith("_") and k not in ("w", "pc") and np.ndim(v) == 1
              and np.ptp(v) > 0}
    inputs["moisture (bulk, as received)"] = R["X"]["moist"]
    for j, f in enumerate(FR):
        if np.ptp(s[:, j]) > 0: inputs[f"share_{f}"] = s[:, j]
    rng = np.random.default_rng(seed)
    floor = np.mean([_evppi(rng.random(len(NB)), NB, deg) for _ in range(20)])
    rows = [dict(input=k, evppi=_evppi(np.asarray(v, float), NB, deg)) for k, v in inputs.items()]
    df = pd.DataFrame(rows)
    df["evppi_net"] = (df.evppi - floor).clip(lower=0)
    best = np.array(PW)[keep][NB.mean(0).argmax()]
    return df.sort_values("evppi", ascending=False), dict(evpi=evpi, floor=floor, best_expected=best,
                                                           choice_set=",".join(np.array(PW)[keep]))


# Groups of inputs that one measurement campaign would resolve together.
GROUPS = {
    "Waste characterisation (moisture, composition)": ["moisture (bulk, as received)", "share_food", "share_garden",
                                                        "share_paper", "share_plastic", "share_wood", "share_textile",
                                                        "share_other", "gamma", "yard"],
    "Landfill gas performance (collection, DOC, F, oxidation)": ["cap", "doc_k", "F", "ox", "mcf_od"],
    "RDF line and off-take (O&M, CAPEX, price, coal substitution, sorting)": ["o_rdf", "K_rdf", "p_rdf", "psi", "tau_k",
                                                                             "e_rdf", "q_dry", "omega", "c_truck", "tort"],
    "AD on mixed waste (yield, penalty, capture, costs, pre-treatment)": ["y_ch4", "y_pen_mech", "kappa", "vs_ts", "K_ad",
                                                                         "o_ad", "pre_ofmsw", "pre", "fug_ad", "eta_chp", "par_ad"],
    "WtE performance and cost": ["eta_wte", "K_wte", "o_wte", "h_k", "h_plastic_k", "n2o_wte", "ash"],
    "Landfill cost": ["c_sl", "e_sl"],
    "Finance (discount rate, lifetime, scale exponent)": ["r", "n", "b"],
    "Grid and electricity price": ["efg_k", "p_el"],
}


def _evppi_group(Xg, NB):
    Z = (Xg - Xg.mean(0)) / (Xg.std(0) + 1e-12)
    X = np.column_stack([np.ones(len(Z)), Z, Z ** 2])                # additive quadratic regression
    beta, *_ = np.linalg.lstsq(X, NB, rcond=None)
    fit = X @ beta
    return float(fit.max(1).mean() - NB.mean(0).max())


def voi_groups(P, s, R, FR, pc=50.0, min_feasible=0.5, seed=0):
    keep = R["ok"].mean(0) >= min_feasible
    NB = -(R["C"] + pc * R["G"] / 1e3)[:, keep]
    evpi = float(NB.max(1).mean() - NB.mean(0).max())
    pool = {k: np.asarray(v, float) for k, v in P.items() if not k.startswith("_") and k not in ("w", "pc")
            and np.ndim(v) == 1 and np.ptp(v) > 0}
    pool["moisture (bulk, as received)"] = R["X"]["moist"]
    for j, f in enumerate(FR):
        if np.ptp(s[:, j]) > 0: pool[f"share_{f}"] = s[:, j]
    rng = np.random.default_rng(seed)
    rows = []
    for g, keys in GROUPS.items():
        ks = [k for k in keys if k in pool]
        if not ks: continue
        Xg = np.column_stack([pool[k] for k in ks])
        noise = np.mean([_evppi_group(rng.random(Xg.shape), NB) for _ in range(5)])
        val = _evppi_group(Xg, NB)
        rows.append(dict(group=g, n_inputs=len(ks), evppi=val, noise_floor=noise, evppi_net=max(val - noise, 0.0)))
    df = pd.DataFrame(rows)
    df["share_of_evpi"] = df.evppi_net / evpi if evpi > 1e-9 else 0.0
    return df.sort_values("evppi_net", ascending=False), evpi

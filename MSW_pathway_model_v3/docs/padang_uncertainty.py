"""Uncertainty and sensitivity analysis for one location (default Kota Padang): Monte Carlo, split of sources,
probability of being best by carbon value, Spearman, Sobol, one-at-a-time tornado and scenarios."""
import numpy as np
import pandas as pd
from mswpath import MSWModel, read_input, PW, SCENARIOS
from mswpath.core import tri
from mswpath.discovery import sobol_indices


def run(data_dir, city_id="kota_padang", N=4000, n_sobol=1024):
    M = MSWModel(data_dir)
    raw = read_input(data_dir / "input_kota_2025_updated.csv")
    i_full = int(np.where(raw.city_id == city_id)[0][0])
    one = raw.iloc[[i_full]].reset_index(drop=True)
    M.load_cities(one)
    # same random stream as the full 21-location run: run_mc seeds each location with [SEED, i]
    SEED = M.SEED
    out = {}

    def mc(name, vary=("param", "comp"), keep=False):
        M.rng = np.random.default_rng([SEED, i_full])
        sc = SCENARIOS[name]
        c, P, s, Q, kw = M.setup(0, N, False, vary=vary, **sc)
        if np.isnan(Q).any(): return None
        R = M.model(s, P, c, Q, **kw)
        pick, front = M.decide(R, P["pc"])
        return dict(P=P, s=s, R=R, pick=pick, front=front, Q=Q)

    base = mc("market")
    out["base"] = base
    R, P = base["R"], base["P"]
    q = lambda a: np.percentile(a, [5, 50, 95])
    rows = []
    for j, k in enumerate(PW):
        for ind in ("G", "C", "CED", "LU"):
            p5, p50, p95 = q(R[ind][:, j]); rows.append(dict(option=k, indicator=ind, p5=p5, p50=p50, p95=p95))
        for bg in ("OD", "SL"):
            d = M.delta(R, bg, "G")[:, j]; p5, p50, p95 = q(d)
            rows.append(dict(option=k, indicator=f"dG_{bg}", p5=p5, p50=p50, p95=p95))
        rows.append(dict(option=k, indicator="p_feasible", p50=R["ok"][:, j].mean()))
        rows.append(dict(option=k, indicator="p_front", p50=base["front"][:, j].mean()))
        rows.append(dict(option=k, indicator="p_best", p50=(base["pick"] == j).mean()))
    out["dist"] = pd.DataFrame(rows)
    # probability of being best by carbon-value bin
    bins = np.arange(0, 101, 10)
    pcb = pd.cut(P["pc"], bins, include_lowest=True)
    out["p_by_pc"] = pd.crosstab(pcb, pd.Categorical(np.array(PW)[base["pick"]], categories=PW), normalize="index")
    # social-cost difference S5 - SL along the carbon value (central parameters)
    c0, P0, s0, Q0, _ = M.setup(0, 1, True)
    R0 = M.model(s0, P0, c0, Q0)
    pcs = np.linspace(0, 100, 101)
    out["sc_curve"] = pd.DataFrame({k: R0["C"][0, j] + pcs * R0["G"][0, j] / 1e3 for j, k in enumerate(PW)}, index=pcs)
    out["feasible_central"] = dict(zip(PW, R0["ok"][0]))
    # composition spread (Dirichlet + harmonisation priors)
    out["comp"] = pd.DataFrame(np.percentile(base["s"], [5, 50, 95], axis=0).T, index=M.FR, columns=["p5", "p50", "p95"])
    out["comp"]["central"] = s0[0]
    out["alpha0"] = M.H.iloc[0].alpha0
    out["moist"] = q(R["X"]["moist"]); out["lhv"] = q(R["X"]["lhv"]); out["p_lhv_gate"] = (R["X"]["lhv"] >= M.GATE["lhv_min"]).mean()
    # split of uncertainty sources
    sp = []
    for lab, vary in (("composition only", ("comp",)), ("parameters only", ("param",)), ("both", ("param", "comp"))):
        r_ = mc("market", vary)
        for j, k in enumerate(PW):
            for ind in ("G", "C"):
                p5, _, p95 = q(r_["R"][ind][:, j]); sp.append(dict(source=lab, option=k, indicator=ind, w90=p95 - p5))
    out["split"] = pd.DataFrame(sp).pivot_table(index=["indicator", "option"], columns="source", values="w90")
    # Spearman for this location
    M.rng = np.random.default_rng([SEED, i_full])
    out["spearman"] = M.spearman({city_id: (base["P"], base["s"], base["R"])})
    # Sobol
    out["sobol"] = sobol_indices(M, 0, N=n_sobol)
    # one-at-a-time tornado (each register parameter at its low and high value; wetness at its low and high bound)
    def central_outputs(fix=None, wet=None):
        c, P, s, Q, kw = M.setup(0, 1, True, fix=fix)
        if wet is not None:
            P["w"] = tri(np.full((1, 1), wet), M.W_LO, M.W, M.W_HI)
        R = M.model(s, P, c, Q)
        sc = R["C"][0] + 50 * R["G"][0] / 1e3
        return dict(gap_S5_SL=sc[5] - sc[0], G_SL=R["G"][0, 0], G_S1=R["G"][0, 1], G_S5=R["G"][0, 5], C_S5=R["C"][0, 5],
                    C_S1=R["C"][0, 1])
    b0 = central_outputs()
    tor = []
    for k, rr in M.REG.iterrows():
        if rr.high <= rr.low or k in ("alpha_hi", "alpha_lo"): continue
        lo, hi = central_outputs({k: rr.low}), central_outputs({k: rr.high})
        for o in b0:
            tor.append(dict(input=k, meaning=rr.meaning, low=rr.low, high=rr.high, output=o, base=b0[o],
                            at_low=lo[o], at_high=hi[o], swing=abs(hi[o] - lo[o])))
    lo, hi = central_outputs(wet=0.0), central_outputs(wet=1.0)
    for o in b0:
        tor.append(dict(input="wetness", meaning="All fraction moistures at their low / high bound", low=0, high=1,
                        output=o, base=b0[o], at_low=lo[o], at_high=hi[o], swing=abs(hi[o] - lo[o])))
    out["tornado"] = pd.DataFrame(tor)
    out["tornado_base"] = b0
    # scenarios (central best by carbon value + Monte Carlo probability of being best)
    sr = []
    for name, sc in SCENARIOS.items():
        r_ = mc(name)
        if r_ is None:
            sr.append(dict(scenario=name, note="no value for this location")); continue
        c, Pc, s, Qc, kw = M.setup(0, 1, True, **sc)
        Rc = M.model(s, Pc, c, Qc, **kw)
        row = dict(scenario=name, Q=float(Qc[0]))
        for pc in (0, 25, 50, 100):
            row[f"best_{pc}"] = PW[M.decide(Rc, np.array([float(pc)]))[0][0]]
        pb = np.bincount(r_["pick"], minlength=6) / len(r_["pick"])
        row.update({f"p_{k}": pb[j] for j, k in enumerate(PW)})
        sr.append(row)
    out["scen"] = pd.DataFrame(sr)
    return out

"""
Scenario discovery: under what city and waste-system conditions does each option become preferable?

The model is run over a space of conditions that a planner can observe or choose (carbon value, tonnage, distance to a
cement kiln, grid, WtE tariff, source separation of food, composition, moisture, landfill gas collection, landfill
cost), with all other parameters sampled from the register. For every draw the best feasible option (lowest carbon-inclusive
cost) is recorded. Interpretable rules are then extracted with a classification tree (CART) and PRIM boxes.

Composition is sampled from a Dirichlet distribution centred on the mean of the 21 harmonised RIPS compositions with a
concentration fitted to their between-city spread (method of moments). It therefore spans the compositions observed in
the study, not every possible composition.
"""
from types import SimpleNamespace
import numpy as np
import pandas as pd
from .core import PW, NAMES, tri

FEATURES = {
    "carbon_value": "Carbon value (USD/t CO2e)",
    "Q_tpd": "Waste to the facility (t/day)",
    "kiln_km": "Road distance to cement kiln (km)",
    "grid_ef": "Grid emission factor (kg CO2/kWh)",
    "tariff": "Perpres 109/2025 WtE tariff available (0/1)",
    "food_separated": "Food waste separated at source (0/1)",
    "food_share": "Food share (wet)",
    "plastic_share": "Plastic share (wet)",
    "paper_garden_share": "Paper + garden + wood share (wet)",
    "moisture": "Bulk moisture as received",
    "LHV": "LHV as received (MJ/kg)",
    "gas_capture": "Landfill gas collection efficiency",
    "landfill_cost": "Sanitary landfill cost at 500 t/d (USD/t)",
}


def dirichlet_fit(S):
    """Mean and method-of-moments concentration of a set of compositions (rows sum to 1)."""
    m = S.mean(0); v = S.var(0, ddof=1)
    ok = (m > 1e-4) & (v > 0)
    a0 = np.median(m[ok] * (1 - m[ok]) / v[ok] - 1)
    return m, max(a0, 2.0)


def sample_conditions(M, N=40000, seed=7, q_range=(50, 3000), km_range=(10, 400)):
    """Draw N city/system conditions and evaluate the six options. Returns (features DataFrame, best option)."""
    rng = np.random.default_rng(seed)
    M.rng = rng
    P = M.draw(N, central=False)
    P = M.draw_lcia(P)
    P = M.draw_extra(P)
    m, a0 = dirichlet_fit(M.SB)
    s = rng.dirichlet(np.maximum(m * a0, 1e-6), N)
    Q = np.exp(rng.uniform(np.log(q_range[0]), np.log(q_range[1]), N))
    road = rng.uniform(*km_range, N)
    grids = np.array(list(M.EF_GRID.values()))
    ef = grids[rng.integers(0, len(grids), N)]
    tariff = rng.random(N) < 0.5
    foodsep = rng.random(N) < 0.5
    P["pre"] = np.where(foodsep, 0.0, P["pre"])
    if "y_pen_mech" in P:                      # source-separated food: no mechanical-separation penalty
        P["y_pen_mech"] = np.where(foodsep, 1.0, P["y_pen_mech"]); P["pre_ofmsw"] = np.where(foodsep, 0.0, P["pre_ofmsw"])
    c = SimpleNamespace(ef_grid=ef, d_line=road / P["tort"])
    policy = np.where(tariff, "perpres109", "market")
    R = M.model(s, P, c, Q, policy=policy)
    pick, front = M.decide(R, P["pc"])
    FR = M.FR
    X = pd.DataFrame({
        "carbon_value": P["pc"], "Q_tpd": Q, "kiln_km": road, "grid_ef": ef,
        "tariff": tariff.astype(int), "food_separated": foodsep.astype(int),
        "food_share": s[:, FR.index("food")], "plastic_share": s[:, FR.index("plastic")],
        "paper_garden_share": s[:, [FR.index(f) for f in ("paper", "garden", "wood")]].sum(1),
        "moisture": R["X"]["moist"], "LHV": R["X"]["lhv"],
        "gas_capture": P["cap"], "landfill_cost": P["c_sl"],
    })
    best = pd.Series(np.array(PW)[pick], name="best")
    meta = dict(N=N, seed=seed, dirichlet_alpha0=a0, comp_mean=dict(zip(FR, m.round(4))),
                q_range=q_range, km_range=km_range)
    return X, best, R, meta


def cart_rules(X, y, max_depth=4, min_leaf=0.02, seed=0):
    """Fit a shallow classification tree; return the tree, its text rules, accuracy and a leaf table."""
    from sklearn.tree import DecisionTreeClassifier, export_text
    from sklearn.model_selection import train_test_split
    Xtr, Xte, ytr, yte = train_test_split(X, y, test_size=0.3, random_state=seed, stratify=y)
    t = DecisionTreeClassifier(max_depth=max_depth, min_samples_leaf=max(int(min_leaf * len(Xtr)), 1),
                               random_state=seed).fit(Xtr, ytr)
    acc_tr, acc_te = t.score(Xtr, ytr), t.score(Xte, yte)
    base = y.value_counts(normalize=True).max()
    text = export_text(t, feature_names=list(X.columns), decimals=2, show_weights=False)
    leaves = leaf_table(t, X, y)
    imp = pd.Series(t.feature_importances_, index=X.columns).sort_values(ascending=False)
    return dict(tree=t, text=text, acc_train=acc_tr, acc_test=acc_te, baseline_acc=base, leaves=leaves, importance=imp)


def leaf_table(t, X, y):
    """One row per leaf: the conjunction of conditions, its share of draws and the option mix inside it."""
    tr = t.tree_; feats = X.columns
    paths = []

    def walk(node, conds):
        if tr.children_left[node] == -1:
            paths.append((node, conds)); return
        f, thr = feats[tr.feature[node]], tr.threshold[node]
        walk(tr.children_left[node], conds + [(f, "<=", thr)])
        walk(tr.children_right[node], conds + [(f, ">", thr)])
    walk(0, [])
    leaf_of = t.apply(X)
    rows = []
    for node, conds in paths:
        mask = leaf_of == node
        mix = y[mask].value_counts(normalize=True)
        rows.append(dict(rule=" AND ".join(simplify(conds)), share_of_draws=mask.mean(), predicted=mix.idxmax(),
                         purity=mix.max(), **{f"p_{k}": mix.get(k, 0.0) for k in PW}))
    return pd.DataFrame(rows).sort_values(["predicted", "share_of_draws"], ascending=[True, False])


def simplify(conds):
    """Merge repeated thresholds on the same feature into one interval and print binary features as yes/no."""
    lo, hi = {}, {}
    for f, op, v in conds:
        if op == "<=": hi[f] = min(hi.get(f, np.inf), v)
        else: lo[f] = max(lo.get(f, -np.inf), v)
    out = []
    for f in dict.fromkeys([c[0] for c in conds]):
        if f in ("tariff", "food_separated"):
            out.append(f"{f} = {'yes' if f in lo else 'no'}"); continue
        a, b = lo.get(f), hi.get(f)
        if a is not None and b is not None: out.append(f"{a:.3g} < {f} <= {b:.3g}")
        elif a is not None: out.append(f"{f} > {a:.3g}")
        else: out.append(f"{f} <= {b:.3g}")
    return out


def prim_box(X, target, alpha=0.05, min_mass=0.05, min_coverage=0.5):
    """Patient Rule Induction Method (peeling only). Returns the box with the highest density that keeps at least
    `min_coverage` of the target cases, and the whole peeling trajectory."""
    keep = np.ones(len(X), bool); box = {c: [X[c].min(), X[c].max()] for c in X.columns}
    T = target.astype(bool).to_numpy(); nT = T.sum()
    traj = []
    while keep.mean() > min_mass:
        best = None
        for c in X.columns:
            v = X[c].to_numpy()
            vals = v[keep]
            if np.unique(vals).size <= 2:          # binary feature: try fixing each level
                cands = [(c, "eq", u) for u in np.unique(vals)]
            else:
                cands = [(c, "lo", np.quantile(vals, alpha)), (c, "hi", np.quantile(vals, 1 - alpha))]
            for cc, kind, thr in cands:
                m2 = keep & ((v == thr) if kind == "eq" else (v >= thr) if kind == "lo" else (v <= thr))
                if m2.sum() == keep.sum() or m2.sum() < 20: continue
                dens = T[m2].mean()
                if best is None or dens > best[0]: best = (dens, cc, kind, thr, m2)
        if best is None: break
        dens, c, kind, thr, keep = best
        if kind == "eq": box[c] = [thr, thr]
        elif kind == "lo": box[c][0] = thr
        else: box[c][1] = thr
        traj.append(dict(mass=keep.mean(), coverage=T[keep].sum() / max(nT, 1), density=dens,
                         box={k: list(v) for k, v in box.items()}))
    tr = pd.DataFrame(traj)
    if tr.empty:
        return None, tr
    ok = tr[tr.coverage >= min_coverage]
    pick = ok.loc[ok.density.idxmax()] if len(ok) else tr.iloc[0]
    full = {c: [X[c].min(), X[c].max()] for c in X.columns}
    restricted = {c: v for c, v in pick.box.items() if (v[0] > full[c][0] + 1e-12) or (v[1] < full[c][1] - 1e-12)}
    return dict(coverage=pick.coverage, density=pick.density, mass=pick.mass, box=restricted,
                base_rate=T.mean()), tr


def condition_map(X, y, fx, fy, bins=(12, 12), logx=False, logy=False, min_n=30):
    """Most frequent best option and its frequency on a 2-D grid of two conditions."""
    ex = np.geomspace(X[fx].min(), X[fx].max(), bins[0] + 1) if logx else np.linspace(X[fx].min(), X[fx].max(), bins[0] + 1)
    ey = np.geomspace(X[fy].min(), X[fy].max(), bins[1] + 1) if logy else np.linspace(X[fy].min(), X[fy].max(), bins[1] + 1)
    ix = np.clip(np.digitize(X[fx], ex) - 1, 0, bins[0] - 1); iy = np.clip(np.digitize(X[fy], ey) - 1, 0, bins[1] - 1)
    mode = np.full(bins[::-1], -1); freq = np.full(bins[::-1], np.nan)
    codes = pd.Categorical(y, categories=PW).codes
    for a in range(bins[0]):
        for b in range(bins[1]):
            sel = (ix == a) & (iy == b)
            if sel.sum() >= min_n:
                cnt = np.bincount(codes[sel], minlength=len(PW))
                mode[b, a] = cnt.argmax(); freq[b, a] = cnt.max() / cnt.sum()
    return ex, ey, mode, freq


def sobol_indices(M, i, N=512, pc=50.0, seed=11):
    """Sobol first-order and total indices of register and realism parameters (plus the common wetness draw) for one
    location, composition fixed at its central value. Outputs: G and C of every option and the carbon-inclusive-cost
    gap S5 - SL at carbon value pc."""
    from SALib.sample import sobol as sobol_sample
    from SALib.analyze import sobol as sobol_analyze
    REG = M.REG
    SPm = M.SP[(M.SP.use == "main case") & (M.SP.high > M.SP.low)] if M.SP is not None else None
    extra = list(SPm.index) if SPm is not None else []
    names = [k for k, v in REG.iterrows() if v.high > v.low and k not in ("alpha_hi", "alpha_lo")] + extra + ["wetness"]
    prob = dict(num_vars=len(names), names=names, bounds=[[0, 1]] * len(names))
    U = sobol_sample.sample(prob, N, calc_second_order=False, seed=seed)
    n = len(U)
    M.rng = np.random.default_rng(seed)
    P = M.draw(n, central=True)
    P = M.draw_lcia(P)
    P = M.draw_extra(P)
    for j, k in enumerate(names[:-1]):
        v = REG.loc[k] if k in REG.index else SPm.loc[k]; P[k] = tri(U[:, j], v.low, v.central, v.high)
    P["w"] = tri(U[:, [-1]], M.W_LO, M.W, M.W_HI)
    c = M.H.iloc[i]
    s = np.repeat(M.SB[i][None, :], n, 0)
    if c.woody:
        mv = s[:, M.iW] * P["yard"]; s[:, M.iG] += mv; s[:, M.iW] -= mv
    elif c.lumped:
        mv = s[:, M.iF] * P["gamma"]; s[:, M.iF] -= mv; s[:, M.iG] += mv
    Q = np.full(n, c.Q2025)
    R = M.model(s, P, c, Q)
    outs = {f"G_{k}": R["G"][:, j] for j, k in enumerate(PW)}
    outs.update({f"C_{k}": R["C"][:, j] for j, k in enumerate(PW)})
    sc = R["C"] + pc * R["G"] / 1e3
    outs["CICgap_S5_SL"] = sc[:, 5] - sc[:, 0]
    rows = []
    for nm, y in outs.items():
        r = sobol_analyze.analyze(prob, y, calc_second_order=False, print_to_console=False, seed=seed)
        for k, s1, st in zip(names, r["S1"], r["ST"]):
            rows.append(dict(city=c.city, output=nm, input=k, S1=s1, ST=st))
    return pd.DataFrame(rows)


COL = {"SL": "#8c8c8c", "S1": "#d62728", "S2": "#ff7f0e", "S3": "#2ca02c", "S4": "#9467bd", "S5": "#1f77b4"}


def plot_condition_maps(X, best, path=None, close=True):
    """Six condition maps (most frequent best option on 2-D grids), separated by policy context."""
    import matplotlib.pyplot as plt
    from matplotlib.colors import ListedColormap
    from matplotlib.patches import Patch
    cmap = ListedColormap([COL[k] for k in PW])
    MIX = (X.food_separated == 0) & (X.tariff == 0)
    panels = [("carbon_value", "Q_tpd", False, True, "mixed waste, no tariff", MIX),
              ("carbon_value", "kiln_km", False, False, "mixed waste, no tariff", MIX),
              ("LHV", "Q_tpd", False, True, "mixed waste, Perpres 109 tariff", (X.food_separated == 0) & (X.tariff == 1)),
              ("carbon_value", "gas_capture", False, False, "mixed waste, no tariff", MIX),
              ("carbon_value", "Q_tpd", False, True, "food separated at source, no tariff", (X.food_separated == 1) & (X.tariff == 0)),
              ("carbon_value", "moisture", False, False, "mixed waste, no tariff", MIX)]
    fig, axs = plt.subplots(3, 2, figsize=(12, 13.5))
    for ax, (fx, fy, lx, ly, sub, sel) in zip(axs.flat, panels):
        ex, ey, mode, freq = condition_map(X[sel], best[sel], fx, fy, logx=lx, logy=ly, bins=(10, 10))
        ax.pcolormesh(ex, ey, np.ma.masked_less(mode, 0), cmap=cmap, vmin=-0.5, vmax=5.5, alpha=0.45)
        for a in range(mode.shape[1]):
            for b in range(mode.shape[0]):
                if mode[b, a] >= 0:
                    xc = np.sqrt(ex[a] * ex[a + 1]) if lx else (ex[a] + ex[a + 1]) / 2
                    yc = np.sqrt(ey[b] * ey[b + 1]) if ly else (ey[b] + ey[b + 1]) / 2
                    ax.text(xc, yc, f"{100 * freq[b, a]:.0f}", ha="center", va="center", fontsize=6)
        if ly: ax.set_yscale("log")
        if lx: ax.set_xscale("log")
        ax.set_xlabel(FEATURES[fx]); ax.set_ylabel(FEATURES[fy])
        ax.set_title(f"Most frequent best option: {sub}\n(numbers = % of draws in the cell)", fontsize=9)
    fig.legend(handles=[Patch(color=COL[k], alpha=0.6, label=NAMES[k]) for k in PW], loc="lower center", ncol=6, frameon=False)
    fig.tight_layout(rect=(0, 0.04, 1, 1))
    if path: fig.savefig(path, dpi=170)
    if close: plt.close(fig)
    return fig

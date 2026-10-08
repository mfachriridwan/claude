"""
Explicit, step-by-step recalculation of the central results for one location (default: Kota Padang).

Every equation of mswpath.core.MSWModel.model is written out again with named intermediate quantities, so that the
worked example can print them. At the end each result is compared with the model output; a mismatch stops the script.
"""
import numpy as np
import pandas as pd
from mswpath import MSWModel, read_input, PW
from mswpath.core import MAP, RIPS_CATS


def calc(data_dir, city_id="kota_padang"):
    M = MSWModel(data_dir)
    raw = read_input(data_dir / "input_kota_2025_updated.csv")
    i = int(np.where(raw.city_id == city_id)[0][0])
    M.load_cities(raw)
    c, P, s, Q, kw = M.setup(i, 1, True)
    R = M.model(s, P, c, Q)
    v = lambda k: float(P[k][0])
    r = raw.iloc[i]
    FR = M.FR
    out = dict(M=M, raw=raw, i=i, row=r, c=c, P=P, R=R, Q=float(Q[0]))

    # ---------------------------------------------------------------- 1. inputs
    comp = pd.DataFrame({"dom_pct": [r.get(f"dom_{k}") for k in RIPS_CATS],
                         "nd_pct": [r.get(f"nd_{k}") for k in RIPS_CATS]}, index=RIPS_CATS)
    out["rips"] = comp
    md, mn = float(r.M_dom_tpd_2025), float(r.M_nd_tpd_2025)
    out["tonnage"] = dict(Qt=float(r.tonnage_source_value_tpd), t=r.tonnage_source_year, g=float(r.growth_rate_used),
                          expo=int(r.projection_exponent_years), factor=float(r.tonnage_projection_factor),
                          Md_t=float(r.M_dom_tpd), Mn_t=float(r.M_nd_tpd), Md=md, Mn=mn, Q2025=float(r.Q2025_tpd))

    def stream(pre):
        x = np.array([sum(float(r[f"{pre}_{k}"]) for k in MAP[f] if pd.notna(r.get(f"{pre}_{k}"))) for f in FR])
        return x, x.sum()
    xd, td = stream("dom"); xn, tn = stream("nd")
    sd, sn = xd / td, xn / tn
    s_comb = (md * sd + mn * sn) / (md + mn)
    assert np.allclose(s_comb, M.SB[i]) and np.allclose(s_comb, s[0]), "composition mismatch"
    out["frac"] = pd.DataFrame({"dom_pct_mapped": xd, "nd_pct_mapped": xn, "s_dom": sd, "s_nd": sn, "s": s_comb}, index=FR)
    out["norm"] = dict(dom_sum=td, nd_sum=tn, f_dom=100 / td, f_nd=100 / tn, w_dom=md / (md + mn), w_nd=mn / (md + mn))

    # ---------------------------------------------------------------- 2. characterisation
    w = P["w"][0]; dm = 1 - w
    docf = M.DOCF0.copy(); docf[M.iR] = v("docf_rubber")
    hk = np.where(M.APPLY_HK, v("h_k"), 1.0); hk[M.iP] = v("h_plastic_k")
    h = M.HDRY * hk
    LAM, F, dock = M.LAM, v("F"), v("doc_k")
    sj = s_comb
    ch4_j = 1e3 * sj * dm * M.DOC * docf * dock * F * 16 / 12          # MCF = 1
    fos_j = 1e3 * sj * dm * M.CARB * M.PHI * 44 / 12
    lhv_j = sj * dm * h - LAM * sj * w
    tab = pd.DataFrame({"s": sj, "w": w, "dry": sj * dm, "DOC": M.DOC, "DOCf": docf, "C": M.CARB, "phi": M.PHI,
                        "h": h, "CH4_MCF1": ch4_j, "CO2_fossil": fos_j, "LHV_contrib": lhv_j}, index=FR)
    moist = (sj * w).sum(); lhv = max((sj * dm * h).sum() - LAM * moist, 0)
    assert abs(lhv - R["X"]["lhv"][0]) < 1e-9
    out["char"] = tab
    out["bulk"] = dict(moisture=moist, dry=(sj * dm).sum(), LHV=lhv, L0=ch4_j.sum(), fossil=fos_j.sum(),
                       HHV_dry=(lhv + LAM * moist) / (1 - moist) + LAM * 9 * 0.06)

    # ---------------------------------------------------------------- 3. helpers (same equations as the model)
    g27, g273 = M.GWP["GWP100"]
    efg = c.ef_grid * v("efg_k"); dkm = c.d_line * v("tort")
    rr, nn, bb = v("r"), v("n"), v("b")
    crf = rr * (1 + rr) ** nn / ((1 + rr) ** nn - 1)
    Qd = out["Q"]
    capex = lambda K, qref, q: K * (max(q, 1e-9) / M.AVAIL / qref) ** bb * crf / (max(q, 1e-9) * 365)
    sl_unit = lambda t: v("c_sl") * (max(Qd * t, 5.0) / M.QREF["sl"]) ** (-v("e_sl"))
    pef = 3.6 / v("pef_eta") * (1 + v("up_fuel")) * v("efg_k")
    mjd = (1 + v("up_diesel")) / v("ef_diesel")

    def landfill(m, inert=0.0):
        t = m.sum() + inert
        ch4 = 1e3 * (m * dm * M.DOC * docf).sum() * dock * v("mcf_sl") * F * 16 / 12
        capt = ch4 * v("cap"); emit = (ch4 - capt) * (1 - v("ox"))
        return dict(t=t, ch4=ch4, capt=capt, emit=emit, G_ch4=emit * g27, G_anc=t * v("anc_sl"),
                    G=emit * g27 + t * v("anc_sl"), unit=sl_unit(t), C=t * sl_unit(t),
                    CED=t * v("anc_sl") * mjd,
                    LU=(m.sum() / (v("rho_sl") * v("h_sl")) + inert / (v("rho_inert") * v("h_sl"))) * v("f_gross_sl"))

    def rdf_line(m):
        rj = m * np.minimum(M.TAU * v("tau_k"), 1.0); rej = m - rj
        m_in = rj.sum(); water = (rj * w).sum(); dry = m_in - water
        m_out = min(m_in, dry / (1 - v("omega")))
        e = ((rj * dm * h).sum() - LAM * (m_out - dry)) * v("rdf_ncv_k")   # rdf_ncv_k = 1 in the main case
        e_net = e - (m_in - m_out) * v("q_dry")
        m_del = m_out * e_net / e
        fos = 1e3 * (rj * dm * M.CARB * M.PHI).sum() * 44 / 12
        parts = dict(fossil=fos, electricity=v("e_rdf") * efg, diesel=v("anc_rdf"), transport=m_del * dkm * v("ef_truck"),
                     coal_credit=-v("psi") * e_net * v("ef_coal"))
        cost = dict(capex=capex(v("K_rdf"), M.QREF["rdf"], Qd), om=v("o_rdf"), transport=m_del * dkm * v("c_truck"),
                    revenue=-e_net * v("p_rdf"))
        ced = (v("e_rdf") * pef + v("anc_rdf") * mjd + m_del * dkm * v("ef_truck") * mjd - v("psi") * e_net * 1e3 * (1 + v("up_coal")))
        return dict(r=rj, rej=rej, m_in=m_in, water=water, dry=dry, m_out=m_out, e=e, e_net=e_net, m_del=m_del,
                    ncv=e / m_out, parts=parts, G=sum(parts.values()), cost=cost, C=sum(cost.values()), CED=ced,
                    LU=v("fp_rdf") / (M.AVAIL * 365 * nn))

    def ad(a):
        V = a * 1e3 * dm[M.iF] * v("vs_ts") * v("y_ch4") * v("y_pen_mech")   # yield penalty for food sorted from mixed MSW
        el = V * (1 - v("fug_ad")) * M.LHV_CH4 / 3.6 * v("eta_chp") * (1 - v("par_ad"))
        parts = dict(fugitive=V * M.RHO_CH4 * v("fug_ad") * g27, diesel=a * v("anc_ad"), electricity_credit=-el * efg)
        cost = dict(capex=a * capex(v("K_ad"), M.QREF["ad"], Qd * a), om=a * v("o_ad"), pretreatment=a * v("pre_ofmsw"), revenue=-el * v("p_el"))
        return dict(a=a, V=V, el=el, parts=parts, G=sum(parts.values()), cost=cost, C=sum(cost.values()),
                    CED=a * v("anc_ad") * mjd - el * pef, LU=a * v("fp_ad") / (M.AVAIL * 365 * nn))

    # ---------------------------------------------------------------- 4. options
    res = {}
    od_ch4 = 1e3 * (sj * dm * M.DOC * docf).sum() * dock * v("mcf_od") * F * 16 / 12
    res["OD"] = dict(ch4=od_ch4, G=od_ch4 * g27 + v("anc_od"), C=v("c_od"),
                     parts=dict(landfill_CH4=od_ch4 * g27, diesel=v("anc_od")))
    sl = landfill(sj)
    res["SL"] = dict(lf=sl, G=sl["G"], C=sl["C"], parts=dict(landfill_CH4=sl["G_ch4"], diesel=sl["G_anc"]),
                     cost=dict(landfill=sl["C"]), CED=sl["CED"], LU=sl["LU"])
    # S1
    fos = fos_j.sum(); E = lhv * 1e3 / 3.6 * v("eta_wte"); ash = landfill(0 * sj, v("ash"))
    p1 = dict(fossil=fos, N2O=v("n2o_wte") * g273, auxiliary=v("anc_wte"), electricity_credit=-E * efg, ash_landfill=ash["G"])
    c1 = dict(capex=capex(v("K_wte"), M.QREF["wte"], Qd), om=v("o_wte"), revenue=-E * v("p_el"), ash_landfill=ash["C"])
    res["S1"] = dict(E=E, parts=p1, cost=c1, G=sum(p1.values()), C=sum(c1.values()),
                     CED=v("anc_wte") * mjd - E * pef + ash["CED"], LU=v("fp_wte") / (M.AVAIL * 365 * nn) + ash["LU"])
    # S2
    L2 = rdf_line(sj); rj2 = landfill(L2["rej"])
    p2 = dict(L2["parts"], rejects_landfill=rj2["G"])
    c2 = dict(L2["cost"], rejects_landfill=rj2["C"])
    res["S2"] = dict(line=L2, lf=rj2, parts=p2, cost=c2, G=sum(p2.values()), C=sum(c2.values()),
                     CED=L2["CED"] + rj2["CED"], LU=L2["LU"] + rj2["LU"])
    # S3
    a = sj[M.iF] * v("kappa"); food = np.zeros_like(sj); food[M.iF] = a
    A3 = ad(a); lf3 = landfill(sj - food, a * v("dig"))
    p3 = dict(A3["parts"], residue_landfill=lf3["G"], front_end_sorting=v("pre") * (v("e_rdf") * efg + v("anc_rdf")))
    c3 = dict(A3["cost"], residue_landfill=lf3["C"], front_end_sorting=v("pre") * (capex(v("K_rdf"), M.QREF["rdf"], Qd) + v("o_rdf")))
    res["S3"] = dict(ad=A3, lf=lf3, parts=p3, cost=c3, G=sum(p3.values()), C=sum(c3.values()),
                     CED=A3["CED"] + lf3["CED"] + v("pre") * (v("e_rdf") * pef + v("anc_rdf") * mjd),
                     LU=A3["LU"] + lf3["LU"] + v("pre") * v("fp_rdf") / (M.AVAIL * 365 * nn))
    # S4
    phb = sl["capt"] / v("r_phb"); tpa = phb * Qd * 365 / 1e3
    phb_cost = v("c_phb") * (max(tpa, 1.0) / 500.0) ** (-v("e_phb"))
    p4 = dict(landfill_CH4=sl["G_ch4"], diesel=sl["G_anc"], PHB_chemicals=phb * v("ef_phb"), PP_credit=-phb * v("sub_pp") * v("ef_pp"))
    c4 = dict(landfill=sl["C"], PHB_production=phb * phb_cost, PHB_revenue=-phb * v("p_phb"))
    res["S4"] = dict(phb=phb, tpa=tpa, phb_cost=phb_cost, parts=p4, cost=c4, G=sum(p4.values()), C=sum(c4.values()),
                     CED=sl["CED"] + phb * (v("ef_phb") / v("ef_natgas") - v("sub_pp") * v("ced_pp")), LU=sl["LU"])
    # S5
    L5 = rdf_line(sj - food); lf5 = landfill(L5["rej"], a * v("dig"))
    p5 = dict({f"AD_{k}": x for k, x in A3["parts"].items()}, **{f"RDF_{k}": x for k, x in L5["parts"].items()},
              rejects_landfill=lf5["G"])
    c5 = dict({f"AD_{k}": x for k, x in A3["cost"].items()}, **{f"RDF_{k}": x for k, x in L5["cost"].items()},
              rejects_landfill=lf5["C"])
    res["S5"] = dict(line=L5, lf=lf5, parts=p5, cost=c5, G=sum(p5.values()), C=sum(c5.values()),
                     CED=A3["CED"] + L5["CED"] + lf5["CED"], LU=A3["LU"] + L5["LU"] + lf5["LU"])

    # ---------------------------------------------------------------- 5. verification against the model
    for j, k in enumerate(PW):
        assert abs(res[k]["G"] - R["G"][0, j]) < 1e-6, (k, res[k]["G"], R["G"][0, j])
        assert abs(res[k]["C"] - R["C"][0, j]) < 1e-6, (k, res[k]["C"], R["C"][0, j])
        assert abs(res[k]["CED"] - R["CED"][0, j]) < 1e-6, (k, "CED")
        assert abs(res[k]["LU"] - R["LU"][0, j]) < 1e-9, (k, "LU")
    assert abs(res["OD"]["G"] - R["G_od"][0]) < 1e-6 and abs(res["OD"]["C"] - R["C_od"][0]) < 1e-9
    out["res"] = res
    out["ctx"] = dict(efg=efg, ef_grid=c.ef_grid, grid=c.grid, kiln=c.kiln, road=dkm, crf=crf, pef=pef, mjd=mjd,
                      gwp_ch4=g27, gwp_n2o=g273)
    out["P"] = {k: v(k) for k in P if not k.startswith("_") and k not in ("w",)}
    return out

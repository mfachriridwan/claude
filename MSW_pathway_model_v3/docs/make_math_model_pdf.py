"""Build docs/MSW_Mathematical_Model_EN.pdf: every equation of the model, from data harmonisation to the 2045
projection, written as implemented in mswpath (core, discovery, voi, thresholds, projection). Values are read from data/."""
import pandas as pd
from reportlab.lib.pagesizes import A4
from reportlab.lib.units import cm
from reportlab.platypus import SimpleDocTemplate, Spacer, PageBreak
from doccommon import *

REG = pd.read_csv(DATA / "assumption_register.csv")
K = pd.read_csv(DATA / "model_constants.csv").set_index("key")
SP = pd.read_csv(DATA / "scenario_parameters.csv")
LC = pd.read_csv(DATA / "lcia_factors.csv")
T2 = pd.read_csv(DATA / "table2_model_fraction_parameters.csv")
PRJ = pd.read_csv(DATA / "projection_2045_parameters.csv")
GRID = pd.read_csv(DATA / "grid_emission_factors.csv")
story = []; A = story.append
_n = [0]


def E(tex, fs=12.5):
    _n[0] += 1
    A(eqrow(tex, f"mm{_n[0]:03d}", str(_n[0]), fs=fs))


def H1(t): A(P(t, "h1"))
def H2(t): A(P(t, "h2"))
def T(t): A(P(t))


A(P("Mathematical Model of the MSW Recovery-Pathway Screening", "title"))
A(P("All equations used in the study, as implemented in the open-source package mswpath v3.2: data harmonisation, "
    "waste characterisation, life-cycle inventory and costs of six options, decision analysis, uncertainty, sensitivity, "
    "scenario discovery, value of information, break-even targets and the projection to 2045", "subtitle"))
A(P("Muhammad Fachri Ridwan &mdash; Supervisor: Prof. Anthony Halog &mdash; The University of Queensland &mdash; "
    "October 2026", "subtitle"))
A(Spacer(1, 6))
A(boxed([P("<b>How to read this document</b>", "box")] + bullets([
    "Equations are numbered in the order of the computation. Each section names the module and function that implements it, "
    "so every equation can be traced to code (mswpath/core.py unless stated otherwise).",
    "Everything is expressed per tonne of mixed MSW, wet weight, as received at the facility gate in 2025 (functional unit). "
    "Masses m<sub>j</sub> are tonnes of fraction j per tonne of MSW.",
    "In the Monte Carlo analysis every parameter is a vector of draws; all equations hold draw by draw.",
    "Numerical values are in the tables of Section 16, generated from data/*.csv, which also hold source and evidence status."], "box")))

# ------------------------------------------------------------------------------------------------ 1
H1("1 Notation and system")
T("Indices: j &isin; {food, garden, paper, wood, textile, rubber, plastic, metal, glass, other} = the ten model fractions; "
  "k &isin; {SL, S1, ..., S5} = options; i = Monte Carlo draw; p = carbon value (USD/t CO<sub>2</sub>e). Options: SL sanitary "
  "landfill with gas collection and flare; S1 waste-to-energy (grate incineration); S2 refuse-derived fuel (RDF) "
  "co-processed in the nearest cement kiln; S3 anaerobic digestion (AD) of food separated from mixed waste; S4 PHB "
  "bioplastic from landfill gas; S5 integrated RDF + AD. Baselines: open dump (OD) and SL. Every option manages the whole "
  "tonne; residues go to a residue landfill (sanitary unless stated). For each option the model returns G<sub>k</sub> "
  "(kg CO<sub>2</sub>e/t), C<sub>k</sub> (USD/t), fossil cumulative energy demand CED<sub>k</sub> (MJ/t) and land take "
  "LU<sub>k</sub> (m<super>2</super>/t), with feasibility flags.")

# ------------------------------------------------------------------------------------------------ 2
H1("2 Data harmonisation to the 2025 baseline (MSWModel.harmonise, tonnage; build_database_v3.py)")
H2("2.1 Tonnage, projected once")
E(r"Q_{2025}=Q_t\,(1+g)^{\,2025-t},\qquad M^{d}_{2025}=M^{d}_t\,(1+g)^{\,2025-t},\qquad M^{n}_{2025}=M^{n}_t\,(1+g)^{\,2025-t}")
T("Q<sub>t</sub> is the reported tonnage (t/day) of year t, g the RIPS growth rate (or the default g<sub>0</sub> of the "
  "register), d and n the domestic and non-domestic streams. The exponent is zero when the tonnage already refers to 2025; "
  "the model asserts Q used = Q<sub>2025</sub> of the database (single-projection guard).")
H2("2.2 Composition")
E(r"s^{(\ell)}_j=\frac{x^{(\ell)}_j}{\sum_{j'} x^{(\ell)}_{j'}},\quad f^{(\ell)}=\frac{100}{\sum_{j'} x^{(\ell)}_{j'}},\qquad "
  r"s_j=\frac{M^{d}_{2025}\,s^{(d)}_j+M^{n}_{2025}\,s^{(n)}_j}{M^{d}_{2025}+M^{n}_{2025}}")
T("x<sup>(&ell;)</sup><sub>j</sub> is the wet-mass percentage of stream &ell; after mapping the RIPS categories to the model "
  "fractions (sum of the categories mapped to j); blank categories are absent (zero share), never imputed in the main case. "
  "Harmonisation rules (sampled priors): if organics are lumped, a share &gamma; of food is moved to garden; if wood "
  "&ge; 10% with no leaf category, a share y of wood is moved to garden:")
E(r"\mathrm{L:}\ s_{garden}\leftarrow s_{garden}+\gamma\,s_{food},\ s_{food}\leftarrow(1-\gamma)\,s_{food};\qquad "
  r"\mathrm{W:}\ s_{garden}\leftarrow s_{garden}+y\,s_{wood},\ s_{wood}\leftarrow(1-y)\,s_{wood}")
T("Scenario <i>glass_imputed</i>, only where glass is not reported, with an imputed share u<sub>g</sub>:")
E(r"s_j\leftarrow s_j\,(1-u_g)\ (j\neq glass),\qquad s_{glass}\leftarrow s_{glass}+u_g")

# ------------------------------------------------------------------------------------------------ 3
H1("3 Waste characterisation (MSWModel.model)")
T("Moisture as received w<sub>j</sub>, dry matter (1 &minus; w<sub>j</sub>), dry lower heating value h<sub>j</sub> "
  "(legacy values multiplied by the calibration factor k<sub>h</sub>, plastic by k<sub>h,P</sub>), latent heat &lambda;:")
E(r"M=\sum_j s_j w_j,\qquad H=\max\left[0,\ \sum_j s_j\,(1-w_j)\,k_{h,j}h_j-\lambda M\right]\quad(\mathrm{MJ/kg})")
T("Fossil CO<sub>2</sub> released when a mass vector m is burned (C<sub>j</sub> dry carbon, &phi;<sub>j</sub> fossil share):")
E(r"E_{fos}(m)=10^3\,\frac{44}{12}\sum_j m_j\,(1-w_j)\,C_j\,\varphi_j\qquad(\mathrm{kg\,CO_2/t})")
T("Moisture enters once: DOC and C are on a dry basis and multiply dry mass (DOC<sub>wet</sub> = DOC<sub>dry</sub>(1&minus;w)). "
  "In the Monte Carlo all fraction moistures move together through one common draw u<sub>w</sub>: "
  "w<sub>j</sub> = F<sup>&minus;1</sup><sub>tri</sub>(u<sub>w</sub>; w<sub>j,lo</sub>, w<sub>j</sub>, w<sub>j,hi</sub>).")

# ------------------------------------------------------------------------------------------------ 4
H1("4 Landfill module (open dump and sanitary landfill)")
T("For a mass vector m sent to a landfill plus inert mass m<sub>in</sub> (ash, digestate):")
E(r"\mathrm{CH_4}(m)=10^3\,k_D\,\mathrm{MCF}\,F\,\frac{16}{12}\sum_j m_j\,(1-w_j)\,\mathrm{DOC}_j\,\mathrm{DOCf}_j\qquad(\mathrm{kg\,CH_4/t})")
E(r"\mathrm{CH_4^{capt}}=\eta\,\mathrm{CH_4}\,[SL],\qquad \mathrm{CH_4^{emit}}=(\mathrm{CH_4}-\mathrm{CH_4^{capt}})\,(1-OX\,[SL])")
E(r"G_{LF}=\mathrm{CH_4^{emit}}\,GWP_{CH_4}+a_{LF}\,(t),\qquad t=\sum_j m_j+m_{in}")
E(r"C_{LF}=t\,c_u,\qquad c_u=c_{SL}\left(\frac{\max(Q\,t,\,5)}{Q_{ref,SL}}\right)^{-e_{SL}}\ (SL),\qquad c_u=c_{OD}\ (OD)")
T("MCF = MCF<sub>SL</sub> (managed anaerobic) or MCF<sub>OD</sub>; [SL] = 1 for the sanitary landfill and 0 for the open dump "
  "(no collection, no cover oxidation); k<sub>D</sub> is a multiplier on DOC&middot;DOCf; DOCf of rubber is the gap "
  "parameter DOCf<sub>R</sub>. The landfill baseline values are G<sub>SL</sub> = G<sub>LF</sub>(s), G<sub>OD</sub> = "
  "G<sub>LF</sub>(s) with OD settings; L<sub>0</sub> = CH<sub>4</sub>(s) with MCF = 1 is reported for transparency.")

# ------------------------------------------------------------------------------------------------ 5
H1("5 Techno-economic building blocks")
E(r"CRF=\frac{r\,(1+r)^n}{(1+r)^n-1},\qquad c^{cap}(K_{ref},q_{ref},q)=\frac{CRF\,K_{ref}\,(q/A/q_{ref})^{b}}{365\,q}\quad(\mathrm{USD/t})")
T("q = throughput (t/day) of the unit, A = availability (nameplate = throughput/A), b = capacity exponent, K<sub>ref</sub> the "
  "CAPEX at reference capacity q<sub>ref</sub>. Electricity is credited at the grid factor EF = EF<sub>grid</sub> k<sub>EF</sub> "
  "and valued at p<sub>el</sub> (or the Perpres 109/2025 tariff in that scenario where Q &ge; 1,000 t/day).")

# ------------------------------------------------------------------------------------------------ 6
H1("6 The six options")
H2("6.1 SL sanitary landfill with flare")
E(r"G_{SL}=G_{LF}(s),\qquad C_{SL}=C_{LF}(s)")
H2("6.2 S1 waste-to-energy")
E(r"E_{el}=\frac{10^3}{3.6}\,H\,\eta_W\quad(\mathrm{kWh/t}),\qquad G_1=E_{fos}(s)+n_{N_2O}\,GWP_{N_2O}+a_W-E_{el}\,EF+G_{LF}(0,\alpha)")
E(r"C_1=c^{cap}(K_W,q_{W},Q)+o_W-E_{el}\,p_{el}+C_{LF}(0,\alpha)")
T("&alpha; = bottom ash and APC residue (t/t) landfilled as inert mass.")
H2("6.3 S2 RDF to cement kiln")
E(r"r_j=s_j\,\min(\tau_j k_\tau,1),\quad m_{in}=\sum_j r_j,\quad d=\sum_j r_j(1-w_j),\quad m_{out}=\min\left(m_{in},\frac{d}{1-\omega}\right)")
E(r"e=\left[\sum_j r_j(1-w_j)k_{h,j}h_j-\lambda\,(m_{out}-d)\right]k_{NCV},\quad e_{net}=e-(m_{in}-m_{out})\,q_{dry},\quad m_{del}=m_{out}\frac{e_{net}}{e}")
E(r"\mathrm{NCV}_{RDF}=e/m_{out},\qquad G_2=E_{fos}(r)+e_R\,EF+a_R+m_{del}\,D\,ef_{tr}-\psi\,e_{net}\,EF_{coal}+G_{LF}(s-r)")
E(r"C_2=c^{cap}(K_R,q_R,Q)+o_R+m_{del}\,D\,c_{tr}-e_{net}\,p_{RDF}+C_{LF}(s-r)")
T("&tau;<sub>j</sub> transfer coefficients to RDF, &omega; target moisture, q<sub>dry</sub> dryer heat per t of water, "
  "&psi; GJ coal displaced per GJ RDF, D = road distance to the nearest kiln = great-circle distance &times; tortuosity, "
  "k<sub>NCV</sub> = 1 (0.78 in the stress test).")
H2("6.4 S3 anaerobic digestion of food separated from mixed waste")
E(r"a=\kappa\,s_{food},\quad V=10^3\,a\,(1-w_{food})\,\frac{VS}{TS}\,y_{CH_4}\,k_{mech}\ (\mathrm{Nm^3/t}),\quad "
  r"E_{AD}=V(1-f_{AD})\frac{LHV_{CH_4}}{3.6}\eta_{CHP}(1-\pi_{AD})")
E(r"G_3=V\rho_{CH_4}f_{AD}GWP_{CH_4}+a\,a_A-E_{AD}EF+G_{LF}(s-a\,e_{food},\,\delta a)+\pi\,(e_R EF+a_R)")
E(r"C_3=a\left[c^{cap}(K_A,q_A,Qa)+o_A+c_{pre}\right]-E_{AD}\,p_{el}+C_{LF}(s-a\,e_{food},\,\delta a)+\pi\left[c^{cap}(K_R,q_R,Q)+o_R\right]")
T("&kappa; capture of food into the digester, k<sub>mech</sub> relative methane yield of mechanically separated food, "
  "c<sub>pre</sub> extra pre-treatment (USD/t feed), f<sub>AD</sub> fugitive methane share, &pi;<sub>AD</sub> own electricity "
  "use, &delta; digestate landfilled per t feed, &pi; front-end separation charged as a share of the RDF line; e<sub>food</sub> "
  "is the unit vector of the food fraction.")
H2("6.5 S4 PHB from landfill gas")
E(r"P_{PHB}=\frac{\mathrm{CH_4^{capt}}(s)}{R_{PHB}}\ (\mathrm{kg/t}),\quad T_{PHB}=\frac{P_{PHB}\,Q\,365}{10^3},\quad "
  r"c_{PHB}(T)=c_{500}\left(\frac{\max(T,1)}{500}\right)^{-e_{PHB}}")
E(r"G_4=G_{SL}+P_{PHB}\,(ef_{PHB}-\sigma\,ef_{PP}),\qquad C_4=C_{SL}+P_{PHB}\,(c_{PHB}-p_{PHB})")
H2("6.6 S5 integrated RDF + AD")
E(r"G_5=G_{AD}(a)+G_{RDF}(s-a\,e_{food})+G_{LF}(\mathrm{rej},\,\delta a),\qquad C_5=C_{AD}(a)+C_{RDF}(s-a\,e_{food})+C_{LF}(\mathrm{rej},\,\delta a)")
T("The whole tonne passes the RDF line, which also separates the food (no separate front-end charge); "
  "rej = (s &minus; a e<sub>food</sub>) &minus; r. Mass balance a + m<sub>in</sub> + &Sigma; rej = 1 is asserted.")

# ------------------------------------------------------------------------------------------------ 7
H1("7 Fossil cumulative energy demand and land take")
E(r"\mathrm{PEF}=\frac{3.6}{\eta_{pp}}\,(1+u_{fuel})\,k_{EF}\ (\mathrm{MJ/kWh}),\qquad \mathrm{MJ}_{d}=\frac{1+u_{diesel}}{ef_{diesel}}\ (\mathrm{MJ\ per\ kg\,CO_2e})")
E(r"\mathrm{CED}_1=a_W\,\mathrm{MJ}_d-E_{el}\,\mathrm{PEF}+\mathrm{CED}_{LF},\quad "
  r"\mathrm{CED}_2=e_R\mathrm{PEF}+a_R\mathrm{MJ}_d+m_{del}D\,ef_{tr}\mathrm{MJ}_d-10^3\psi e_{net}(1+u_{coal})+\mathrm{CED}_{LF}")
E(r"\mathrm{CED}_{AD}=a\,a_A\,\mathrm{MJ}_d-E_{AD}\,\mathrm{PEF},\qquad \mathrm{CED}_4=\mathrm{CED}_{SL}+P_{PHB}\left(\frac{ef_{PHB}}{ef_{gas}}-\sigma\,\mathrm{CED}_{PP}\right),\qquad \mathrm{CED}_{LF}=t\,a_{LF}\,\mathrm{MJ}_d")
E(r"LU_{SL}=\left(\frac{\sum_j m_j}{\rho_{SL}H_{SL}}+\frac{m_{in}}{\rho_{in}H_{SL}}\right)f_{gross},\quad LU_{OD}=\frac{\sum_j m_j+m_{in}}{\rho_{OD}H_{OD}},\quad LU_{plant}=\frac{FP}{A\,365\,n}")
T("Biogenic energy is not counted. Plant land LU<sub>plant</sub> spreads the footprint FP over the lifetime throughput; "
  "each option's land take is the sum of its plants and landfills.")

# ------------------------------------------------------------------------------------------------ 8
H1("8 Feasibility gates")
gate = lambda k: f"{K.loc[k, 'value']:g}"
T(f"G1: H &ge; {gate('GATE_lhv_min')} MJ/kg and G2: Q &ge; {gate('GATE_q_wte')} t/day for WtE; G3: Q &ge; {gate('GATE_q_psel')} "
  f"t/day for the Perpres 109/2025 tariff; G4: NCV<sub>RDF</sub> &ge; {gate('GATE_ncv_rdf')} MJ/kg and G5: D &le; "
  f"{gate('GATE_d_max')} km for RDF and RDF + AD; G6: T<sub>PHB</sub> &ge; {gate('GATE_phb_min')} t/yr. An option failing a "
  "gate is infeasible in that draw.")

# ------------------------------------------------------------------------------------------------ 9
H1("9 Decision analysis (MSWModel.delta, decide, run_mc)")
E(r"\Delta G_k=G_0-G_k,\qquad \Delta C_k=C_k-C_0,\qquad MAC_k=10^3\,\frac{C_k-C_0}{G_0-G_k}\quad(0\in\{OD,SL\})")
E(r"CIC_k(p)=C_k+\frac{p\,G_k}{10^3},\qquad k^*_i(p)=\arg\min_{k\,\in\,\mathcal{F}_i}\ CIC_{ik}(p)")
E(r"k\ \mathrm{Pareto\ efficient}\ \Leftrightarrow\ \mathrm{no}\ k':\ G_{k'}\leq G_k,\ C_{k'}\leq C_k,\ (G_{k'},C_{k'})\neq(G_k,C_k)")
E(r"\hat P_k(p)=\frac{1}{N}\sum_{i=1}^{N}\mathbf{1}\left[k^*_i(p)=k\right],\qquad SE=\sqrt{\frac{\hat P_k(1-\hat P_k)}{N}}")
T("&#x2131;<sub>i</sub> is the set of options feasible in draw i. CIC is the carbon-inclusive cost: it values greenhouse gases "
  "only. The carbon value is fixed at p = 0, 2, 25, 50 and 100 USD/t CO<sub>2</sub>e (policy choice; 2 = UU 7/2021 carbon "
  "tax). For two options with straight CIC lines the crossing carbon value is p* = 10<super>3</super>(C<sub>b</sub>&minus;"
  "C<sub>a</sub>)/(G<sub>a</sub>&minus;G<sub>b</sub>).")

# ------------------------------------------------------------------------------------------------ 10
H1("10 Uncertainty propagation")
E(r"x=F^{-1}_{tri}(u;a,c,b)=\left\{a+\sqrt{u(b-a)(c-a)}\ \ \mathrm{if}\ u<\frac{c-a}{b-a};\quad b-\sqrt{(1-u)(b-a)(b-c)}\ \ \mathrm{otherwise}\right\},\quad u\sim U(0,1)")
E(r"\mathbf{S}\sim\mathrm{Dir}(\alpha_0\,\mathbf{s}+\epsilon),\qquad S_j=\frac{\Gamma_j}{\sum_{j'}\Gamma_{j'}},\quad \Gamma_j\sim\mathrm{Gamma}(\alpha_0 s_j+\epsilon,\,1)")
T("Every register, CED/land and realism parameter is triangular (low, central, high). Composition is Dirichlet around the "
  "harmonised shares with heuristic concentration &alpha;<sub>0</sub> (80, or 40 for flagged data); &epsilon; = 0 except in "
  "the pseudocount scenario (0.5). Each location uses its own random stream, seeded identically in every scenario "
  "(common random numbers), so differences between scenarios are paired. Scenarios replace parameter values (e.g. "
  "k<sub>mech</sub> = 1, c<sub>pre</sub> = 0) or the moisture vector (IPCC values; upper bounds) without changing the equations.")

# ------------------------------------------------------------------------------------------------ 11
H1("11 Sensitivity analysis")
E(r"\rho_S(X,Y)=\mathrm{corr}\left(\mathrm{rank}(X),\,\mathrm{rank}(Y)\right)\qquad(\mathrm{mean}\ |\rho_S|\ \mathrm{over\ locations})")
E(r"S_{1,x}=\frac{\mathrm{Var}_x\left[\mathrm{E}(Y|x)\right]}{\mathrm{Var}(Y)},\qquad S_{T,x}=\frac{\mathrm{E}_{x_{\sim}}\left[\mathrm{Var}(Y|x_{\sim})\right]}{\mathrm{Var}(Y)}")
T("Sobol indices (discovery.sobol_indices) are estimated with Saltelli sampling and the SALib estimators (Saltelli et al. "
  "2010 for S<sub>1</sub>, Jansen for S<sub>T</sub>), N = 512 base samples, composition fixed at each location. Outputs include "
  "the carbon-inclusive cost gap Y = CIC<sub>S5</sub>(50) &minus; CIC<sub>SL</sub>(50).")

# ------------------------------------------------------------------------------------------------ 12
H1("12 Scenario discovery (mswpath/discovery.py)")
E(r"\hat\alpha_0=\mathrm{median}_j\left[\frac{\bar s_j(1-\bar s_j)}{\mathrm{Var}(s_j)}-1\right]\qquad(\mathrm{method\ of\ moments\ over\ the\ 21\ compositions})")
E(r"\mathrm{Gini}(node)=1-\sum_k \pi_k^2,\qquad \mathrm{split}=\arg\max\left[\mathrm{Gini}(parent)-\sum_{c}\frac{n_c}{n}\,\mathrm{Gini}(c)\right]")
E(r"\mathrm{coverage}=\frac{\#\{i\in B:\ k^*_i=k\}}{\#\{i:\ k^*_i=k\}},\quad \mathrm{density}=\frac{\#\{i\in B:\ k^*_i=k\}}{\#\{i\in B\}},\quad \mathrm{mass}=\frac{\#\{i\in B\}}{N}")
T("40,000 condition sets are sampled (carbon value, tonnage, kiln distance, grid, tariff, source separation, composition, "
  "moisture, gas collection, landfill cost); the best option of each is explained by a CART tree (depth 4, Gini impurity) "
  "and PRIM boxes B, peeled by removing an &alpha; = 5% quantile of one condition (or fixing a binary condition) at a time to "
  "maximise density while coverage stays above a threshold.")

# ------------------------------------------------------------------------------------------------ 13
H1("13 Value of information (mswpath/voi.py)")
E(r"NB_{ik}=-CIC_{ik}(p),\qquad EVPI=\frac{1}{N}\sum_i\max_k NB_{ik}-\max_k\frac{1}{N}\sum_i NB_{ik}")
E(r"EVPPI(X)=\frac{1}{N}\sum_i\max_k\hat g_k(X_i)-\max_k\frac{1}{N}\sum_i NB_{ik},\qquad \hat g_k(X)\approx\mathrm{E}\left[NB_k\,|\,X\right]")
T("&#x011D;<sub>k</sub> is a least-squares regression of NB<sub>k</sub> on X (cubic polynomial for one input; additive "
  "quadratic for a group of inputs resolved by one measurement campaign), after Strong et al. (2014). The EVPPI of a random "
  "input unrelated to the model is subtracted as a bias floor. Options feasible in fewer than half of the draws are removed. "
  "Value per year = EVPPI &times; 365 Q<sub>2025</sub>.")

# ------------------------------------------------------------------------------------------------ 14
H1("14 Break-even targets (mswpath/thresholds.py)")
E(r"\Delta(x)=CIC_k(p;x)-CIC_{SL}(p;x),\qquad x^*:\ \Delta(x^*)=0,\quad x^*=x_a-\Delta(x_a)\frac{x_b-x_a}{\Delta(x_b)-\Delta(x_a)}")
T("One input x is swept over a grid with all others central; x* is found by linear interpolation between the grid points "
  "where &Delta; changes sign; 'never' / 'always' when &Delta; keeps one sign over the whole range.")

# ------------------------------------------------------------------------------------------------ 15
H1("15 Projection to 2045 (mswpath/projection.py)")
gf = 1 - 20 / 35
E(r"EF_{grid}(t)=EF_{grid}(2025)\,\max\left[0,\;1-\delta\,(t-2025)\right],\qquad \delta=\frac{1}{35}\ \mathrm{yr^{-1}}\ \Rightarrow\ EF_{grid}(2045)=%.3f\,EF_{grid}(2025)" % gf)
E(r"x(t)=x(2025)\,(1+e_x)^{\,t-2025}:\quad p_{el}\ (e=0.02),\quad o_W,o_R,o_A\ (e=0.01),\quad c_{SL},c_{OD}\ (e=0.02)")
E(r"Q(t)=Q(2025)\,(1+g)^{\,t-2025},\qquad \Delta Y_{k}=Y_k(2045)-Y_k(2025),\qquad \Delta Y_k=\sum_{d}\Delta Y_k^{(d)}+I_k")
T("All equations of Sections 3&ndash;9 are re-evaluated with these inputs; CAPEX is constant in real terms and composition, "
  "landfill gas and kiln coal are unchanged. &Delta;Y<sup>(d)</sup> is the change with driver d (grid, costs, tonnage) applied "
  "alone and I<sub>k</sub> the interaction. Local parameter values of the one-city tool are read as 2025 values and escalated "
  "in the same way.")

# ------------------------------------------------------------------------------------------------ 16
A(PageBreak())
H1("16 Symbols and values")
H2("16.1 Constants (data/model_constants.csv)")
rows = [["Key", "Value", "Unit", "Meaning", "Source"]] + [[k, f"{r.value:g}", r.unit, r.meaning, r.source] for k, r in K.iterrows()]
A(table(rows, [2.6, 1.4, 1.8, 6.4, 4.8]))
H2("16.2 Grid emission factors (data/grid_emission_factors.csv)")
A(table([["Grid", "EF 2025 (kg CO2/kWh)", "EF 2045 (projection)"]] +
        [[r.grid_region, f"{r.ef_kgCO2_per_kWh:.3f}", f"{r.ef_kgCO2_per_kWh * gf:.3f}"] for r in GRID.itertuples()], [4, 4, 4]))
H2("16.3 Fraction properties (data/table2_model_fraction_parameters.csv)")
rows = [["Fraction", "w as received (range)", "w IPCC", "DOC dry", "DOCf", "C dry", "fossil &phi;", "LHV dry MJ/kg", "&tau; to RDF"]]
for r in T2.itertuples():
    rows.append([r.fraction, f"{r.moisture_as_received:.2f} ({r.moisture_as_received_low:.2f}-{r.moisture_as_received_high:.2f})",
                 f"{r.moisture_ipcc_default:.2f}", f"{r.DOC_dry:.2f}", f"{r.DOCf_model:.2f}" if pd.notna(r.DOCf_model) else "gap",
                 f"{r.carbon_dry:.2f}", f"{r.fossil_carbon_share:.2f}", f"{r.LHV_dry:.1f}", f"{r.tau:.2f}"])
A(table(rows, [1.8, 2.8, 1.4, 1.4, 1.3, 1.3, 1.5, 2.0, 1.5]))
H2("16.4 Register parameters (data/assumption_register.csv)")
rows = [["Key", "Central (low&ndash;high)", "Unit", "Meaning", "St."]]
for r in REG.itertuples():
    rows.append([r.key, num(r.central) + (f" ({num(r.low)}&ndash;{num(r.high)})" if r.low != r.high else ""), r.unit, r.meaning, r.status])
A(table(rows, [1.8, 3.4, 2.1, 9.0, 0.7]))
H2("16.5 Realism, scenario and stress parameters (data/scenario_parameters.csv)")
rows = [["Key", "Central (low&ndash;high)", "Unit", "Meaning", "Use"]]
for r in SP.itertuples():
    rows.append([r.key, f"{r.central:g}" + (f" ({r.low:g}&ndash;{r.high:g})" if r.low != r.high else ""), r.unit, r.meaning, r.use])
A(table(rows, [2.2, 2.8, 1.6, 8.6, 1.8]))
H2("16.6 CED and land-use factors (data/lcia_factors.csv)")
rows = [["Key", "Central (low&ndash;high)", "Unit", "Meaning", "St."]]
for r in LC.itertuples():
    rows.append([r.key, f"{r.central:g}" + (f" ({r.low:g}&ndash;{r.high:g})" if r.low != r.high else ""), r.unit, r.meaning, r.status])
A(table(rows, [2.0, 3.0, 2.2, 9.1, 0.7]))
H2("16.7 Projection parameters (data/projection_2045_parameters.csv)")
rows = [["Key", "Value", "Unit", "Meaning"]] + [[r.key, str(r.value), r.unit, r.meaning] for r in PRJ.itertuples()]
A(table(rows, [3.0, 2.4, 1.6, 10.0]))
H2("16.8 Symbol map: equations to data keys")
smap = [("k<sub>D</sub>, F, MCF<sub>SL</sub>, MCF<sub>OD</sub>, &eta;, OX", "doc_k, F, mcf_sl, mcf_od, cap, ox"),
        ("a<sub>LF</sub>, c<sub>SL</sub>, e<sub>SL</sub>, c<sub>OD</sub>", "anc_sl / anc_od, c_sl, e_sl, c_od"),
        ("k<sub>h</sub>, k<sub>h,P</sub>, DOCf<sub>R</sub>, k<sub>EF</sub>", "h_k, h_plastic_k, docf_rubber, efg_k"),
        ("&eta;<sub>W</sub>, n<sub>N2O</sub>, a<sub>W</sub>, &alpha;, K<sub>W</sub>, o<sub>W</sub>", "eta_wte, n2o_wte, anc_wte, ash, K_wte, o_wte"),
        ("k<sub>&tau;</sub>, &omega;, q<sub>dry</sub>, e<sub>R</sub>, &psi;, EF<sub>coal</sub>, ef<sub>tr</sub>, a<sub>R</sub>, K<sub>R</sub>, o<sub>R</sub>, p<sub>RDF</sub>, c<sub>tr</sub>",
         "tau_k, omega, q_dry, e_rdf, psi, ef_coal, ef_truck, anc_rdf, K_rdf, o_rdf, p_rdf, c_truck"),
        ("&kappa;, VS/TS, y<sub>CH4</sub>, k<sub>mech</sub>, c<sub>pre</sub>, f<sub>AD</sub>, &eta;<sub>CHP</sub>, &pi;<sub>AD</sub>, &delta;, &pi;, K<sub>A</sub>, o<sub>A</sub>",
         "kappa, vs_ts, y_ch4, y_pen_mech, pre_ofmsw, fug_ad, eta_chp, par_ad, dig, pre, K_ad, o_ad"),
        ("R<sub>PHB</sub>, c<sub>500</sub>, e<sub>PHB</sub>, p<sub>PHB</sub>, ef<sub>PHB</sub>, ef<sub>PP</sub>, &sigma;", "r_phb, c_phb, e_phb, p_phb, ef_phb, ef_pp, sub_pp"),
        ("r, n, b, A, q<sub>ref</sub>", "r, n, b, AVAIL, QREF_*"),
        ("&eta;<sub>pp</sub>, u<sub>fuel</sub>, u<sub>diesel</sub>, ef<sub>diesel</sub>, u<sub>coal</sub>, CED<sub>PP</sub>, &rho;, H, f<sub>gross</sub>, FP",
         "pef_eta, up_fuel, up_diesel, ef_diesel, up_coal, ced_pp, rho_*, h_*, f_gross_sl, fp_*"),
        ("&gamma;, y, g<sub>0</sub>, &alpha;<sub>0</sub>", "gamma, yard, g_default, alpha_hi / alpha_lo")]
A(table([["Symbol", "Data key"]] + [[a, b] for a, b in smap], [8.0, 9.0]))

A(P("References", "h1"))
for r in ["IPCC (2006). Guidelines for National Greenhouse Gas Inventories, Vol. 5 Waste; IPCC (2019) Refinement; IPCC (2021) AR6 WG1 Table 7.15.",
          "Saltelli, A., et al. (2010). Variance based sensitivity analysis of model output. Comput. Phys. Commun. 181, 259-270.",
          "Breiman, L., Friedman, J.H., Olshen, R.A., Stone, C.J. (1984). Classification and Regression Trees. Wadsworth.",
          "Friedman, J.H., Fisher, N.I. (1999). Bump hunting in high-dimensional data. Statistics and Computing 9, 123-143.",
          "Strong, M., Oakley, J.E., Brennan, A. (2014). Estimating multiparameter partial expected value of perfect information from a probabilistic sensitivity analysis sample: a nonparametric regression approach. Med. Decis. Making 34, 311-326.",
          "Frischknecht, R., et al. (2015). Cumulative energy demand in LCA: the energy harvested approach. Int. J. LCA 20, 957-969.",
          "Full source list of every parameter: data/assumption_register.csv, data/secondary_data_verification.csv and MSW_Methodology_v3_EN.pdf."]:
    A(P(r, "ref"))

doc = SimpleDocTemplate(str(DOCS / "MSW_Mathematical_Model_EN.pdf"), pagesize=A4, leftMargin=2 * cm, rightMargin=2 * cm,
                        topMargin=1.8 * cm, bottomMargin=1.6 * cm, title="Mathematical model of the MSW pathway screening",
                        author="Muhammad Fachri Ridwan; Anthony Halog")
deco = page_deco("Mathematical model of the MSW recovery-pathway screening (mswpath v3.2)")
doc.build(story, onFirstPage=deco, onLaterPages=deco)
print("wrote", DOCS / "MSW_Mathematical_Model_EN.pdf", "with", _n[0], "equations")

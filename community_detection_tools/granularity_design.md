# Community Granularity in the Path A Era Networks

**Design, rationale, and the claims it rests on**

Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
20 October 2026 – sixth draft

---

## Status of the claims in this document

| Marker | Meaning |
|---|---|
| **[E]** | Empirical finding, traceable to a file named in Appendix B |
| **[M]** | Mathematical property of the objective function |
| **[L]** | Claim supported by the literature cited |
| **[D]** | Design decision, which the findings constrain but do not determine |
| **[P]** | Figure computed from the $m/P$ proxy; replaced by measured values when the current sweep completes |

---

## 1. The problem

The caffeine literature grows from 518 citing articles in era 11 (1971–77) to 9,080 in
era 17 (2013–19), a 17.5-fold increase, and from 3,889 to 113,008 within-era citations
**[E]**. The design compares research communities across these eras and takes the
community as its unit of analysis.

Communities are produced by an algorithm whose resolution parameter sets their number and
size, so the units of comparison are products of a measurement procedure. Any quantity
that depends on community size then carries whatever trend the procedure imposes. Under a
single resolution across eras that trend tracks the growth of the field (§6), and it sits
inside descent, singleton share, and composition measures where a reader cannot see it.

The design controls that trend by construction. Granularity is defined on the citing
population, a scaling rule declares how it should move with the size of the literature,
an anchor sets its level, and each era's resolution is derived from those two declared
parameters. Results are reported under the primary rule and under alternatives, so a
reader sees which conclusions depend on the convention and which survive it.

---

## 2. Why resolution is a design choice on these networks

### 2.1 The networks

Seven-year birth eras; each work placed by its own publication year; an arc kept only when
both endpoints fall inside the window. Arcs are deduplicated. Weak components below four
vertices are dropped before detection and restored afterward as communities of their own,
so every partition covers every vertex of the `.net` file in its vertex order. Communities
are detected with the Leiden algorithm, a heuristic optimizer of modularity with a
resolution parameter $\gamma$.

Era 17 holds 76,070 vertices: 9,080 cite something inside the era, 4,185 are both cited
and citing, and 66,990 are cited-only with mean in-degree 1.59. The citing articles are
joined to one another by 10,794 arcs, mean degree 2.38 among themselves **[E]**. A
cited-only vertex of in-degree 1 gains $1 - \gamma K^{\mathrm{out}}_c / m$ by joining its
citer's community, which is positive whenever $\gamma K^{\mathrm{out}}_c < m$ **[M]**; the
sweep records that quantity at every $\gamma$ (Appendix B).

### 2.2 The resolution diagnostics

CHAMP (Convex Hull of Admissible Modularity Partitions) represents each partition as a line
$Q(\gamma) = (A - \gamma P)/m$ and keeps the partitions on the upper envelope; a partition
that holds the envelope over a wide interval of $\gamma$ is a candidate for a preferred
resolution. Two sweeps were run on eras 11–17: grid 1 on $[0.01, 1.0]$ with 40 points and
grid 2 on $[0.1, 6.0]$ with 60 points, five Leiden starts per point.

The elbow criterion selects a resolution that tracks the grid floor **[E]**: 1.7–3.5× the
floor on grid 1 and 2–4× on grid 2, because $P$ falls fastest at the left edge of any
range and the largest change is always the first one.

The widest dominance interval, recomputed on the correct envelope (Appendix A), does not
converge within the swept range **[E]**:

| Era | Interval | Width | $K$ | Arcs cut | Censored |
|---|---|---|---|---|---|
| 11 | $[3.78,\ 5.31]$ | 1.53 | 123 | 22.4% | – |
| 12 | $[4.46,\ 5.14]$ | 0.68 | 147 | 22.2% | – |
| 13 | $[5.07,\ 6.00]$ | 0.93 | 245 | 18.3% | ceiling |
| 14 | $[5.16,\ 6.00]$ | 0.84 | 495 | 20.1% | ceiling |
| 15 | $[2.37,\ 3.29]$ | 0.93 | 422 | 10.4% | – |
| 16 | $[4.68,\ 6.00]$ | 1.32 | 643 | 13.3% | ceiling |
| 17 | $[3.37,\ 4.52]$ | 1.15 | 795 | 14.1% | – |

Three of seven are pinned to the ceiling, and in $\log\gamma$ the ranking changes again.
Era 17's envelope holds 24 partitions whose intervals range from 0.04 to 1.15 with no one
of them standing apart **[E]**.

The structural reason is visible in §2.1: most vertices are cited-only, their assignments
are fixed by a single citer, and the citing articles that could distinguish communities
are joined at mean degree 2.38. The envelope changes gradually because the network offers
no scale at which it changes sharply.

**[D]** Community resolution is therefore a design choice on these networks, and the
design states it as one. The two criteria tested do not identify a preferred resolution;
whether the literature has a characteristic scale under some other representation is not
a question this analysis needs answered.

---

## 3. The approach

### 3.1 Granularity

For a partition $C = \{c_1, \dots, c_K\}$, let $w_c$ be the number of citing articles in
community $c$ and $W = \sum_c w_c$. Then

$$k_{\mathrm{eff}}(C) = \frac{W^2}{\sum_c w_c^2}
\qquad\qquad
g(C) = \frac{W}{k_{\mathrm{eff}}} = \frac{\sum_c w_c^2}{W}.$$

$g$ is **granularity**, the number of citing articles in the community of a randomly
chosen citing article; $k_{\mathrm{eff}}$ is the **effective number of communities**, the
inverse-Simpson count. The arithmetic mean $W/K$ will not serve, because community sizes
on these networks are extremely unequal: on the grid 2 partitions the median community is
a triad while a random vertex sits in a community of thousands, and $K/k_{\mathrm{eff}}$
runs from 5 in era 11 to 110 in era 17 **[E]**.

### 3.2 The scaling rule and the anchor

**[D]** Writing $k_{\mathrm{eff}} \propto W^{\alpha}$: $\alpha = 0$ holds the effective
number of communities constant across eras; $\alpha = 1$ holds effective community size
constant; intermediate values let both grow. The anchor sets the level, as $g$ at a
reference era. The pipeline takes $\alpha$ and the anchor as inputs and derives one
$\gamma$ per era:

```
target k_eff(e)  =  k_ref · (W_e / W_ref)^α,     k_ref = W_ref / g_anchor
γ_e              =  the γ at which era e attains that k_eff
```

The sweeps supply each era's calibration curve $\gamma \to k_{\mathrm{eff}}$; the
admissible set, the elbow, and `gamma_star` play no part.

### 3.3 The specifications

**[D] Primary: $\alpha = 1$, anchored at $g = 120$ citing articles per effective community
at era 14.** Sensitivity: $\alpha = 0$ and $\alpha = 0.5$ at the primary anchor, and anchors
of roughly 90 and 160 at $\alpha = 1$. Five constructions, each carried through descent,
the tie weightings, and every model. The primary specification is declared here, before
any model is fit, and the grounds for it (§4, claims 4 and 5) concern the comparability of
the units and not any outcome. Reporting how a result varies with the specification is
the point of the design; choosing a specification to produce a result is excluded by
declaring it in advance.

---

## 4. The claims the approach rests on

The approach is defensible if the following hold. Each is stated with its support, from
the literature or from the evidence in hand, and the one that is an empirical claim about
these partitions is stated with the evidence that will establish it.

**Claim 1. The unit is the research front, and direct citation represents it. [L]**
Price (1965) distinguished the research front, the recent work that cites and is cited
within a few years of publication, from the archive it draws on. A within-era
direct-citation community is a research front organized by its shared foundations: the
citing articles are the front, the cited-only works are the base that holds them
together. Descent and integration are relations of lineage, and a direct citation is the
lineage relation itself. Bibliographic coupling (Kessler 1963) measures similarity of
foundations and co-citation (Small 1973) measures how later work groups earlier work;
each is a different construct. Coupling therefore enters as a robustness representation,
not as a candidate for the primary unit.

**Claim 2. Granularity is measured on the citing population. [L, D]** Follows from claim
1: the front is the citing articles, so the size of a front is counted in citing articles.
Cited-only works remain in the network, where they determine the boundaries, and
contribute nothing to the count. Counting them would make granularity depend on how many
references each community's members happened to make inside the window.

**Claim 3. The partitions measure the construct. [E, pending]** This is the empirical
claim, and three kinds of evidence establish it. *Reliability*: partitions at a fixed
$\gamma$ agree across Leiden seeds on the citing articles, measured by adjusted mutual
information; produced by the probe of §7. *Convergent validity*: communities detected on
bibliographic coupling among the same citing articles, calibrated to the same granularity,
agree with the direct-citation communities well above chance; coupling measures a
different construct so perfect agreement is not expected, and agreement near chance would
mean the direct-citation partition reflects the sparsity of citing-to-citing ties rather
than the organization of the front. *Internal coherence*: within-community journal
concentration exceeds a permutation null that reassigns citing articles across communities
of the same sizes. The first comes from the current run; the second and third from a
second run on existing inputs (Appendix D). If convergent validity fails, coupling becomes the
primary representation and direct citation the check, and the calibration of §3 applies
to the coupling network unchanged.

**Claim 4. The specialty scale is about a hundred active contributors, and the anchor is
set there. [L]** Price's invisible colleges, Crane's (1972) specialty networks, and Kuhn's
(1970) estimate of scientific communities of perhaps a hundred members converge on that
order. The unit here is articles; a seven-year window of a specialty publishing fifteen to
twenty papers a year holds about a hundred citing articles. An anchor of 100–120 citing
articles per effective community sets the partition at the scale of the specialty as that
literature has described it, which is the unit the questions of descent and integration
are asked of. Era 14 is the midpoint of the analytic window, so the calibration reaches
equally far in both directions, and it is where $W$ jumps 2.6× from era 13 and then grows
1.1× to era 15: the field's consolidation to modern scale, which is where a
specialty-scale anchor belongs. The alternative anchors bracket that scale.

**Claim 5. Holding effective community size constant is the convention under which
communities are comparable across eras. [E, D]** Three quantities can be compared across
specifications **[P]**:

Granularity $g$:

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 | range |
|---|---|---|---|---|---|---|---|---|
| 0.00 | 14.5 | 24.2 | 42.6 | 119.5 | 127.6 | 163.4 | 251.7 | 17.4× |
| 0.50 | 37.4 | 54.2 | 69.4 | 119.5 | 121.3 | 131.3 | 173.7 | 4.6× |
| 1.00 | 98.6 | 117.7 | 116.8 | 119.5 | 117.1 | 119.1 | 116.5 | 1.2× |

Concentration $K/k_{\mathrm{eff}}$:

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 | range |
|---|---|---|---|---|---|---|---|---|
| 0.00 | 2.7 | 2.9 | 4.3 | 11.4 | 10.6 | 13.2 | 18.7 | 6.9× |
| 0.50 | 5.3 | 5.1 | 6.1 | 11.4 | 10.2 | 11.0 | 13.9 | 2.7× |
| 1.00 | 11.6 | 10.8 | 9.3 | 11.4 | 9.9 | 10.3 | 10.0 | 1.2× |

Citing articles per detected community $W/K$:

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 | range |
|---|---|---|---|---|---|---|---|---|
| 0.00 | 5.29 | 8.38 | 9.86 | 10.53 | 12.08 | 12.40 | 13.47 | 2.55× |
| 0.50 | 7.00 | 10.55 | 11.38 | 10.53 | 11.83 | 11.90 | 12.51 | 1.79× |
| 1.00 | 8.49 | 10.94 | 12.61 | 10.53 | 11.77 | 11.60 | 11.61 | 1.49× |

The calibration fixes $g$ at every $\alpha$, and $W/K = g / (K/k_{\mathrm{eff}})$, so the
one measure free to vary is concentration. It is stable at $\alpha = 1$ (1.2×) and drifts
nearly sevenfold at $\alpha = 0$, where holding the effective count constant leaves the
partitions progressively more unequal as the field grows. $\alpha = 1$ is the convention
under which the shape of the partition, and not only its level, is held across eras. The
primary specification is re-examined on measured counts and the full size distributions
(largest-community share, size Gini, counts above size thresholds) when the sweep
completes.

**Claim 6. The scaling rule is a convention, and the design does not estimate it. [E, D]**
Holding $\gamma$ fixed produces whatever exponent the method and the graph yield: fitted
$\alpha$ between 0.08 and 0.38 across $\gamma \in [0.1, 6]$ **[E, P]**, below the
$\alpha \approx 0.59$ that modularity's resolution limit alone would impose. The settings
of $\alpha$ name positions about how a field specializes, by adding areas of
characteristic size or by growing a stable number of them, and that is what makes the
sensitivity set interpretable. Because $\alpha$ is declared rather than estimated, the
construction cannot adjudicate between those positions, and the paper does not claim to.

**Claim 7. Eligibility is set per outcome, not by one minimum. [D]** $k_{\mathrm{eff}}$ is
a diversity equivalent, not a count of analytical units; the number of rows available to a
community-level model is the number of communities above a size floor, and the floor
depends on the outcome. Descent requires a minimum of citing articles and of qualifying
cross-era relationships; composition requires enough articles to estimate a topical or
journal distribution; integration requires enough observed ties to distinguish
integration from an isolated connection. The sweep reports the counts above 1, 3, 5, and
10 citing articles at every $\gamma$ so each threshold can be set from measurement.

---

## 5. Provisional calibration

Anchored at $g = 120$ for era 14, from the $m/P$ proxy **[P]**:

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 |
|---|---|---|---|---|---|---|---|
| 0.00 | K=98, kₑ=35.6 @γ2.7 | K=107, kₑ=37.1 @γ2.5 | K=165, kₑ=38.2 @γ1.9 | K=403, kₑ=35.5 @γ2.4 | K=383, kₑ=36.3 @γ1.9 | K=475, kₑ=36.1 @γ1.4 | K=674, kₑ=36.1 @γ1.7 |
| 0.50 | K=74, kₑ=13.8 @γ1.0 | K=85, kₑ=16.6 @γ1.1 | K=143, kₑ=23.4 @γ1.1 | K=403, kₑ=35.5 @γ2.4 | K=391, kₑ=38.1 @γ2.1 | K=495, kₑ=44.9 @γ1.8 | K=726, kₑ=52.3 @γ2.7 |
| 1.00 | K=61, kₑ=5.3 @γ0.4 | K=82, kₑ=7.6 @γ0.7 | K=129, kₑ=13.9 @γ0.7 | K=403, kₑ=35.5 @γ2.4 | K=393, kₑ=39.5 @γ2.3 | K=508, kₑ=49.5 @γ2.1 | K=782, kₑ=77.9 @γ3.8 |

Every required $\gamma$ falls between 0.4 and 3.8, inside the swept range. $K$ is the
detected count, excluding restored components. Target against achieved at $\alpha = 1$:

| | e11 | e12 | e13 | e14 | e15 | e16 | e17 |
|---|---|---|---|---|---|---|---|
| target $k_{\mathrm{eff}}$ | 4.3 | 7.5 | 13.6 | 35.5 | 38.8 | 49.4 | 76.0 |
| achieved | 5.3 | 7.6 | 13.9 | 35.5 | 39.5 | 49.5 | 77.9 |

Six of seven are within 2 percent; era 11 misses by 23 percent at the coarse end of the
grid, and the tolerance rule of §7 decides whether that is calibrated or outside the
attainable window. The feasible anchor window at $\alpha = 1$ on the present grid is
roughly 70 to 170: era 17's floor is $g \approx 67$ at $\gamma = 6$, and era 11 reaches
$g \approx 170$ at $\gamma = 0.1$.

---

## 6. Why not a fixed resolution

Common practice fixes $\gamma = 1$. At $\gamma = 1$ granularity runs 37, 65, 86, 253, 197,
218, and 359 citing articles across eras 11 to 17, a 9.6-fold change that tracks the
17.5-fold growth of the field **[E, P]**. Several quantities the design measures at the
community level depend on community size, so a fixed resolution leaves an era trend
inside them.

*Descent.* If descent requires at least one qualifying cross-era relationship, larger
communities have more opportunities to satisfy it; under independent article-level
opportunities the probability in a community of $s$ articles is $1 - (1-p)^s$, increasing
and concave in $s$. Measured descent would rise with community size while the
article-level probability was unchanged. (Article-level events are not independent, so
this is the direction of the effect, not its size; the criterion implemented in
`PathA_TieLayer.R` determines the exact relationship.)

*Singleton share.* Falls as effective size rises, so a fixed resolution makes it decline
across the window by construction.

*Composition.* Topical and journal diversity per community rise with the number of
articles observed, so an apparent increase in interdisciplinarity would be partly an
increase in community size.

These are open item C6 in the specification, and under a fixed $\gamma$ the drift runs in
the direction of the design's expectations. Under this design the trend is removed from
the primary specification by construction, and the $\alpha = 0$ specification, whose
granularity ranges 17.4×, shows the magnitude of the size dependence in each outcome.
That is what the sensitivity specifications are for: a result that holds across them is
not a product of community size, and a result that appears only under one is reported as
depending on the convention.

---

## 7. The procedure

Each step is one function in `community_detection_tools`, and the files it writes are the
record a reader can audit (Appendix B).

**Profile.** Each era network is read once and its composition recorded, including the
citing population $W_e$, the restored-component count, and the cited-only vertices with
in-degree two or more.

**Sweep.** Leiden at sixty resolutions on $[0.1, 6.0]$, five starts each. At every
resolution the partition is expanded over the full vertex set and its citer-weighted
$k_{\mathrm{eff}}$ and $g$ measured directly, with the detected and restored counts, the
counts above size thresholds, the largest-community share, the size Gini, the $m/P$ proxy
beside the measured value, the exact admissible set with its intervals, and
$\max_c \gamma K^{\mathrm{out}}_c / m$. The column $\gamma \to k_{\mathrm{eff}}$ is the
calibration curve.

**Probe.** On at least eras 11, 13, and 17, single Leiden starts across ten seeds at five
resolutions: monotonicity of $k_{\mathrm{eff}}$ in $\gamma$, its coefficient of variation
across seeds, and adjusted mutual information between seeds on the citing articles. The
run-to-run variation sets the calibration tolerance; the AMI is the reliability evidence
of claim 3. **[D]** If AMI is high at the working resolutions the direct-citation
partitions carry the construct and calibration proceeds; if it is low, coupling is the
basis for detection, and the rest of the procedure is unchanged.

**Calibrate.** For each era, the target from the scaling rule, and $\gamma_e$ the grid
point whose measured $k_{\mathrm{eff}}$ lies nearest it in log ratio. **[D]** Among grid
points within tolerance, the nearest is taken and the smaller $\gamma$ on a tie; the span
of acceptable points is reported, and a selection at the edge of the grid is flagged.

**Write.** Leiden re-run at $\gamma_e$ with a fixed seed; the partition written tagged by
specification (`era17_alpha1.00_g120_e14.clu`) with a sidecar recording $\gamma_e$, the
seed, the target, the achieved $k_{\mathrm{eff}}$ on the partition written, the detected
and restored counts, and the size summaries. The calibration table holds one row per era
per specification.

**Recompute.** The cross-era citation relationships are built from the dated edge list and
the era assignments, independently of detection, so the 464,691 distance-one backward ties
are fixed across specifications **[E]**; their aggregation to community endpoints is
recomputed per specification, and the models fit to each. The backward-tie findings
(55.7% of backward ties at distance one, concentration falling with distance, 31.7% of
distance-one ties on works cited exactly once) stand as reported **[E]**.

---

## 8. The contribution

Community-level comparison across a literature that grows seventeen-fold has a confound
built into it: any quantity that depends on community size carries an era trend that a
single resolution parameter leaves inside it. The resolution parameter looks like a
technical setting, so the trend usually goes unexamined.

This design brings the choice into the open and controls it. The unit is the research
front, represented by direct citation and measured in citing articles. A scaling rule
declares how granularity moves with the size of the literature and an anchor, set at the
specialty scale, fixes its level; each era's resolution is derived from the two, and the
pipeline records the target, the achieved value, and the shape of every partition it
writes. The primary specification holds effective community size constant, which is the
convention under which community composition and relationships are comparable; it is
declared before any model is fit, and the alternative specifications show which
conclusions depend on it. The claims the design rests on are stated with their support,
and the one that is an empirical claim about these partitions is stated with the evidence
that establishes it.

---

## Appendix A: the admissible-set computation

A partition $i$ is admissible when some $\gamma$ in the swept range exists at which no
other partition scores higher:

$$\{\gamma \in [\gamma_{\min}, \gamma_{\max}] : A_i - \gamma P_i \geq A_j - \gamma P_j \ \ \forall j\} \neq \emptyset .$$

Each competitor constrains $\gamma$ to a half-line, since the inequality is
$(A_i - A_j) \geq \gamma (P_i - P_j)$: a competitor with smaller $P$ bounds $\gamma$ from
above, one with larger $P$ from below, and equal $P$ reduces to a comparison of
intercepts. Intersecting the half-lines with the swept range leaves an interval, and the
partition is admissible exactly when it is non-empty. The implementation is $O(n^2)$ in
grid points and exact, and the interval is the dominance width reported in §2.2.

The former implementation tested a single $\gamma$ per competitor and also computed the
crossing point with the wrong sign, so a fallback comparison at partition $i$'s own
$\gamma$ usually decided. Both pass unnoticed on a network with a clear elbow, where the
partition found at each $\gamma$ is optimal near it; on these networks, where one
partition is recovered across a span of $\gamma$, they fail constantly. On grid 2 the
former code flagged 13 of 24 envelope partitions for era 17 and moved the reported elbow
from 0.4 to 1.0 for era 11. The design uses neither the admissible set nor the elbow; the
corrected computation is reported because §2.2 reports the envelope, and a test against a
brute-force envelope now accompanies it.

---

## Appendix B: measurement provenance

| Quantity | Source |
|---|---|
| Era vertex, arc, component, citing, restored-component, and in-degree counts | `era_profiles.tsv` |
| Per-$\gamma$ $A$, $P$, detected $K$, full $K$, admissible set and intervals, $k_{\mathrm{eff}}$ (citer-weighted and proxy), $g$, size summaries, $\max_c \gamma K^{\mathrm{out}}_c/m$ | `era<NN>_sweep.tsv` |
| Probe: per-run granularity, per-pair AMI and ARI, per-$\gamma$ summary | `era<NN>_probe_{runs,agreement,summary}.tsv` |
| Calibration: target, sweep-achieved, written-achieved, tolerance flags, per era and specification | `specifications/<spec>/calibration_<spec>.tsv` |
| Partitions and their $\gamma$, seed, target, achieved, counts | `specifications/<spec>/era<NN>_<spec>.clu`, `_meta.tsv` |
| Vertex-weighted size distributions of §3.1 | `era*_local_gamma.clu`, grid 2 |
| Corpus, node-list, and backward-tie figures | `PathA_Revised_Eras.R`, `PathA_BackwardTies.R` |

Grid 1: `pajek_files/champ_sweep_7yr/gamma_0.01_1.0_n40/`.
Grid 2: `pajek_files/champ_sweep_7yr/gamma_0.1_6.0_n60/`.

Checks the current run closes: the era 17 arc total against the in-degree and
citing-arc figures (66,990 × 1.59 + 10,794 ≈ 117,300 against 113,008 in §1); the $\gamma$
of each `era*_local_gamma.clu`; the restored-component count per era; the proxy-to-measured
ratio at every $\gamma$.

---

## Appendix C: revision note

The sixth draft reorganizes the document around the approach and the claims it rests on
(§3, §4), each claim with its support from the literature or the evidence in hand, and
states construct validity as the empirical claim it is, with the evidence that
establishes it. The fixed-resolution contrast (§6) is kept because fixing $\gamma = 1$ is
common practice. The provisional calibration (§5) and the procedure (§7) are unchanged in
substance from the fifth draft. Findings carried from earlier drafts: the three-way
separation of what the resolution diagnostics show (first draft's claim that the
literature has no characteristic scale withdrawn); the effective count treated as a
diversity equivalent rather than a count of units; the primary specification moved from
$\alpha = 0.5$ to $\alpha = 1$; the arithmetic dependence among the three comparability
measures; the anchor as a declared parameter with its own sensitivity dimension; the
correction of the §2.1 condition to $\gamma K^{\mathrm{out}}_c < m$; the two defects in the
admissible-set code.

---

## Appendix D: what remains to be done

Each item either implements a step of §7 or checks a claim of §4.

**Implement the procedure**

- [ ] Sweep eras 11–17 on grid 2 with the revised pipeline (eras 14–17 running; 11–13 next). Replaces every **[P]** figure with measured values: claim 5's tables, claim 6's exponents, §5, §6.
- [ ] Run the probe on eras 11, 13, 17. Sets the calibration tolerance from run-to-run variation; supplies the reliability evidence for claim 3.
- [ ] Set the primary anchor and the two alternatives within the measured feasible window (claim 4; working values 120, 90, 160).
- [ ] Calibrate and write the five specifications; confirm every era is within tolerance or flagged at a bound (§5, era 11).
- [ ] Set the per-outcome eligibility floors from the size-threshold counts (claim 7).
- [ ] Recompute descent, tie weightings, and models per specification.

**Check the claims**

- [ ] Claim 3, convergent validity: build the within-era bibliographic-coupling networks from the dated edge list, calibrate them by §7, and compute AMI against the direct-citation partitions per era.
- [ ] Claim 3, internal coherence: within-community journal concentration against a permutation null.
- [ ] Claim 5: re-examine $\alpha = 1$ on measured counts and the full size distributions (largest share, Gini, threshold counts).
- [ ] §6: read the descent criterion in `PathA_TieLayer.R`, state its relationship to community size, and state what it credits as persistence: a front that carries its foundations forward while replacing its articles is the case the criterion must decide.
- [ ] §2.1: confirm $\max_c \gamma K^{\mathrm{out}}_c / m < 1$ across the working range from the sweep table.
- [ ] Appendix B: reconcile the era 17 arc total.

---

## References

Crane, D. (1972). *Invisible Colleges: Diffusion of Knowledge in Scientific Communities.*
University of Chicago Press.

Kessler, M. M. (1963). Bibliographic coupling between scientific papers. *American
Documentation* 14(1): 10–25.

Kuhn, T. S. (1970). *The Structure of Scientific Revolutions*, 2nd ed., Postscript.
University of Chicago Press.

Price, D. J. de Solla (1963). *Little Science, Big Science.* Columbia University Press.

Price, D. J. de Solla (1965). Networks of scientific papers. *Science* 149(3683): 510–515.

Small, H. (1973). Co-citation in the scientific literature: A new measure of the
relationship between two documents. *Journal of the American Society for Information
Science* 24(4): 265–269.

Vinh, N. X., Epps, J., & Bailey, J. (2010). Information theoretic measures for clusterings
comparison. *Journal of Machine Learning Research* 11: 2837–2854.

Weir, W. H., Emmons, S., Gibson, R., Taylor, D., & Mucha, P. J. (2017). Post-processing
partitions to identify domains of modularity optimization. *Algorithms* 10(3): 93.

# Community Granularity in the Path A Era Networks

**Design, rationale, and the claims it rests on**

Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
20 October 2026 – seventh draft

---

## Status of the claims in this document

| Marker | Meaning |
|---|---|
| **[E]** | Empirical finding, traceable to a file named in Appendix B |
| **[M]** | Mathematical property of the objective function |
| **[L]** | Claim supported by the literature cited |
| **[D]** | Design decision, which the findings constrain but do not determine |

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
and citing, and 66,990 are cited-only with mean in-degree 1.39, of which 11,090 are cited
two or more times. The citing articles are joined to one another by 19,761 arcs, mean
degree 4.35 among themselves, and the remaining 93,247 arcs land on cited-only works
**[E]**. A cited-only vertex of in-degree 1 gains $1 - \gamma K^{\mathrm{out}}_c / m$ by
joining its citer's community, which is positive whenever $\gamma K^{\mathrm{out}}_c < m$
**[M]**; measured at every $\gamma$ of the sweep, $\max_c \gamma K^{\mathrm{out}}_c / m$
never exceeds 0.34 **[E]**, so the condition holds throughout.

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
are joined at mean degree 4.35. The envelope changes gradually because the network offers
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
of the same sizes. Reliability is established: on eras 11, 13, and 17, with ten single-start Leiden runs at
each of five resolutions, AMI across seeds on the citing articles averages 0.73–0.85 and
never falls below 0.67; mean $k_{\mathrm{eff}}$ is monotone in $\gamma$ in every era; and
the coefficient of variation of $k_{\mathrm{eff}}$ across seeds is 0.03–0.08 at the
working resolutions **[E]**. The second and third kinds of evidence come from a second run
on existing inputs (Appendix D). If convergent validity fails, coupling becomes the
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
communities are comparable across eras. [E, D]** On the measured calibration curves,
anchored at $g = 120$ in era 14:

Granularity $g$:

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 | range |
|---|---|---|---|---|---|---|---|---|
| 0.00 | 14.4 | 25.1 | 45.6 | 123.1 | 131.8 | 185.9 | 254.6 | 17.7× |
| 0.50 | 48.6 | 57.1 | 74.4 | 123.1 | 125.2 | 142.0 | 171.2 | 3.5× |
| 1.00 | 106.7 | 114.8 | 114.1 | 123.1 | 119.6 | 121.0 | 116.2 | 1.15× |

Concentration $K/k_{\mathrm{eff}}$:

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 | range |
|---|---|---|---|---|---|---|---|---|
| 0.00 | 2.6 | 2.8 | 4.4 | 10.8 | 10.0 | 14.9 | 18.6 | 7.1× |
| 0.50 | 6.0 | 5.4 | 6.1 | 10.8 | 9.6 | 11.5 | 13.4 | 2.5× |
| 1.00 | 11.1 | 8.8 | 8.6 | 10.8 | 9.5 | 10.3 | 9.6 | 1.3× |

Size Gini over communities with at least one citing article:

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 |
|---|---|---|---|---|---|---|---|
| 0.00 | 0.61 | 0.69 | 0.75 | 0.81 | 0.83 | 0.84 | 0.85 |
| 0.50 | 0.73 | 0.75 | 0.80 | 0.81 | 0.82 | 0.83 | 0.83 |
| 1.00 | 0.78 | 0.80 | 0.82 | 0.81 | 0.82 | 0.82 | 0.81 |

The calibration fixes $g$ at every $\alpha$, and $W/K = g / (K/k_{\mathrm{eff}})$, so
concentration and the shape of the size distribution are the measures free to vary. Both
are stable under $\alpha = 1$: concentration within 1.3× and the Gini within 0.78–0.82
across the window. Under $\alpha = 0$, holding the effective count constant lets the
partitions run from nearly equal in era 11 to highly concentrated in era 17, a sevenfold
drift in concentration and a Gini rising from 0.61 to 0.85. $\alpha = 1$ is the
convention under which the shape of the partition, and not only its level, is held across
eras. The largest-community share falls from 0.40 to 0.05 under $\alpha = 1$; that is the
effective count itself (era 11 holds about five effective communities at the specialty
scale), not a change of shape.

**Claim 6. The scaling rule is a convention, and the design does not estimate it. [E, D]**
Holding $\gamma$ fixed produces whatever exponent the method and the graph yield: fitted
$\alpha$ between $-0.03$ and $0.26$ across $\gamma \in [0.1, 6]$ **[E]**, far below the
$\alpha \approx 0.59$ that modularity's resolution limit alone would impose. At any fixed
resolution nearly all of the field's growth goes into community size. The settings
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

## 5. The calibrated specifications

From the measured calibration curves, anchored at $g = 120$ in era 14, tolerance 0.10
**[E]**. $K$ is the detected count, excluding restored components; $n_{\geq 10}$ the
number of communities holding ten or more citing articles.

| Spec | Era | target $k_{\mathrm{eff}}$ | achieved | $\gamma$ | $K$ | $g$ | $n_{\geq 10}$ | met |
|---|---|---|---|---|---|---|---|---|
| α = 1 | 11 | 4.32 | 4.86 | 0.10 | 54 | 106.7 | 5 | at floor, +12.5% |
| | 12 | 7.48 | 7.81 | 0.40 | 69 | 114.8 | 9 | yes |
| | 13 | 13.56 | 14.25 | 0.60 | 122 | 114.1 | 15 | yes |
| | 14 | 35.37 | 34.47 | 1.70 | 372 | 123.1 | 53 | yes |
| | 15 | 38.55 | 38.69 | 1.50 | 366 | 119.6 | 60 | yes |
| | 16 | 49.09 | 48.67 | 1.70 | 499 | 121.0 | 79 | yes |
| | 17 | 75.67 | 78.11 | 3.40 | 753 | 116.2 | 142 | yes |
| α = 0.5 | 11–17 | | | 0.5, 0.9, 0.8, 1.7, 1.3, 1.3, 2.4 | 64–712 | 49–171 | 11–113 | 6 of 7 (era 11, −14%) |
| α = 0 | 11–17 | 35.4 | | 1.9, 1.9, 1.4, 1.7, 1.2, 1.1, 1.3 | 94–662 | 14–255 | 17–80 | 6 of 7 (era 16, −10%) |
| α = 1, $g$ = 90 | 11–17 | | | 0.2, 0.7, 0.7, 2.4, 2.2, 2.4, 4.0 | 55–785 | 84–94 | 7–169 | 7 of 7 |
| α = 1, $g$ = 160 | 11–17 | | | 0.1, 0.3, 0.4, 1.2, 0.9, 1.2, 2.5 | 54–712 | 107–162 | 5–121 | 4 of 7 |

Every required $\gamma$ except era 11's lies inside the grid. Era 11's coarsest partition
on this grid is $g = 107$ at $\gamma = 0.1$, which sets the measured ceiling of the
feasible anchor window at $\alpha = 1$; the floor, from era 17 at $\gamma = 6$, is
$g = 61$. The anchor of 90 calibrates all seven eras; 160 lies above era 11's and era 12's
reach. Extending the grid floor to 0.02 for eras 11–13, which costs about a minute of
computation, extends era 11's curve below $k_{\mathrm{eff}} = 4.9$ and is the next sweep;
until then the coarser alternative is the attainable one, about 105.

The counts in the last column are the size of the early literature: at the specialty
scale, eras 11 and 12 hold five and nine communities with ten or more citing articles. The
per-outcome eligibility floors of claim 7 determine which outcomes those eras can support;
no choice of anchor changes what was published in 1971–84.

The partition written at each $\gamma$ reproduces the sweep's partition exactly, because
the sweep seeds its five starts 0–4 and the writer uses seed 0 with five starts; the
written and sweep-achieved values agree to the decimal.

---

## 6. Why not a fixed resolution

Common practice fixes $\gamma = 1$. At $\gamma = 1$ granularity runs 22, 51, 69, 183, 146,
194, and 303 citing articles across eras 11 to 17, a 13.9-fold change that tracks the
17.5-fold growth of the field **[E]**. Several quantities the design measures at the
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
run-to-run variation sets the calibration tolerance (CV 0.03–0.08 at the working
resolutions, so 0.10 sits above the noise); the AMI is the reliability evidence of
claim 3. **[D]** If AMI is high at the working resolutions the direct-citation
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

Closed by the current run: the era 17 arc total reconciles exactly (66,990 cited-only
works at mean in-degree 1.392 absorb 93,247 arcs, and 19,761 run among citing articles,
for 113,008); the restored-component count per era is 44, 77, 89, 243, 269, 250, and 346;
the $m/P$ proxy runs 0.52–0.93 of the measured citer-weighted count, rising with era and
with $\gamma$, so no figure derived from it is retained.

---

## Appendix C: revision note

The seventh draft replaces every proxy figure with the measured value from the completed
sweeps and probe: the composition of era 17 (§2.1), the reliability evidence of claim 3,
the comparability tables of claim 5, the exponents of claim 6, the calibrated
specifications (§5), the fixed-resolution contrast (§6), and the Appendix B
reconciliation. The **[P]** marker is retired.

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

- [x] Sweep eras 11–17 on grid 2 with the revised pipeline.
- [x] Run the probe on eras 11, 13, 17. Tolerance 0.10 justified; reliability established.
- [ ] Re-sweep eras 11–13 with the grid floor at 0.02, so era 11 reaches the anchor and the coarser alternative becomes attainable (§5).
- [ ] Fix the coarser alternative anchor from the extended curves (160 if era 11 reaches it; otherwise the attainable ceiling).
- [x] Calibrate and write the primary specification (6 of 7 within tolerance; era 11 at the floor).
- [ ] Calibrate and write the four alternative specifications.
- [ ] Set the per-outcome eligibility floors from the size-threshold counts (claim 7).
- [ ] Recompute descent, tie weightings, and models per specification.

**Check the claims**

- [ ] Claim 3, convergent validity: build the within-era bibliographic-coupling networks from the dated edge list, calibrate them by §7, and compute AMI against the direct-citation partitions per era.
- [ ] Claim 3, internal coherence: within-community journal concentration against a permutation null.
- [x] Claim 5: re-examined on measured counts and the full size distributions; holds.
- [ ] §6: read the descent criterion in `PathA_TieLayer.R`, state its relationship to community size, and state what it credits as persistence: a front that carries its foundations forward while replacing its articles is the case the criterion must decide.
- [x] §2.1: $\max_c \gamma K^{\mathrm{out}}_c / m \leq 0.34$ across the whole grid.
- [x] Appendix B: era 17 arc total reconciled.

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

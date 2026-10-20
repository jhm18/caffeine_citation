# Community Granularity in the Path A Era Networks

**Design, rationale, and validation requirements**

Jonathan H. Morgan, Ph.D. & Sarah Delmar, Ph.D.
20 October 2026 – fourth draft

---

## Status of the claims in this document

The claims below are of four kinds, marked throughout so that each can be accepted or
disputed separately.

| Marker | Meaning |
|---|---|
| **[E]** | Empirical finding, traceable to a file named in Appendix B |
| **[M]** | Mathematical property of the objective function, not an observation |
| **[I]** | Interpretation, open to dispute |
| **[D]** | Design decision, which the findings constrain but do not determine |
| **[P]** | Provisional: rests on the $m/P$ proxy, which has not been validated (§5.5, §9.1) |

No citer-weighted granularity has yet been measured on any partition. The granularity
figures in §6 and §7 are proxy estimates; those in §5.1 are measured, but on all vertices
rather than on citing articles.

---

## 1. The research problem

The caffeine literature grows from 518 citing articles in era 11 (1971–77) to 9,080 in
era 17 (2013–19), a 17.5-fold increase, and from 3,889 to 113,008 within-era citations
**[E]**. The design compares research communities across these periods and takes the
community as its unit of analysis.

The communities, however, are produced by an algorithm whose resolution parameter
influences their number and size. The inferential problem is the one that arises whenever
the units of comparison are themselves products of a measurement procedure: how do we
distinguish historical change in the organization of caffeine research from change
introduced by the way we partition its citation networks?

The original plan answered this by selecting one pooled $\gamma^{\*}$ for all eras, on the
evidence that the eras share an organizational scale. That selection could not be made
(§3, §4). In its place, granularity is defined and measured explicitly, a declared scaling
rule states how it should change as the literature grows, and results are reported under
several such rules (§5–§7).

---

## 2. What the networks are

Seven-year birth eras. Each work is placed by its own publication year, and an arc is kept
only when both endpoints fall inside the window. Arcs are deduplicated; weak components
below four vertices are dropped before detection and restored afterward as communities of
their own, so every partition covers every vertex of the `.net` file in its vertex order.
Communities are detected with the Leiden algorithm, a heuristic optimizer of modularity
with a resolution parameter $\gamma$: larger $\gamma$ penalizes within-community arc mass
more heavily and yields more, smaller communities.

The composition matters for everything downstream. Era 17 holds 76,070 vertices, of which
9,080 cite something inside the era and 4,185 are both cited and citing. The remaining
66,990 are cited-only, with mean in-degree 1.59. The articles that cite one another
directly are joined by 10,794 arcs, giving them mean degree 2.38 among themselves **[E]**.

Those figures do not yet reconcile with the arc total. At mean in-degree 1.59 the
cited-only vertices absorb roughly 106,500 arcs, and with the 10,794 arcs among citing
articles the sum is about 117,300, above the 113,008 within-era citations reported in §1.
Deduplication and component-dropping can only lower the count, so either the two figures
come from different pipeline stages or the in-degree mean is nearer 1.53. The
reconciliation is recorded as an open check in Appendix B.

Under the modularity objective, a cited-only vertex with in-degree 1 and out-degree 0
gains $1 - \gamma K^{\mathrm{out}}_c / m$ by joining its citer's community $c$ rather than
standing alone, where $K^{\mathrm{out}}_c$ is the community's total out-degree and $m$ the
number of arcs **[M]**. The gain is positive whenever
$\gamma K^{\mathrm{out}}_c < m$. That condition is not automatic: at $\gamma = 6$ it fails
for any community holding more than a sixth of the out-arc mass, and the
largest-community share has not been measured. Where it holds, such a vertex sits with its
citer at the optimum and its arc is internal under that partition. This is a property of
the objective, not an observation about the partitions Leiden returned; neither the
condition nor the placement has been checked against the membership files, and Leiden is a
heuristic rather than an exact optimizer.

---

## 3. What the resolution diagnostics show

### 3.1 What CHAMP does

CHAMP (Convex Hull of Admissible Modularity Partitions) evaluates how candidate partitions
perform as the resolution parameter changes. Each partition's modularity is a linear
function of $\gamma$: with $A$ the within-community arc weight and $P$ the same quantity
expected under the null model, $Q(\gamma) = (A - \gamma P)/m$. A coarse partition has high
$A$ and high $P$, so its line starts high and falls steeply; a fine partition starts lower
and falls gently.

The upper envelope of those lines identifies, at each $\gamma$, the best partition
available. Partitions that appear somewhere on the envelope form the *admissible set*. A
partition that remains optimal over a substantial interval of $\gamma$ could indicate a
stable resolution; one that is optimal only over a sliver is a passing artifact of where
the grid points fell. The question is whether such intervals provide a defensible basis
for selecting a common resolution across eras.

Two sweeps were run on eras 11–17:

| Grid | Range | Points | Runs per $\gamma$ | Wall time |
|---|---|---|---|---|
| 1 | $[0.01,\ 1.0]$ | 40 | 5 | 48 min |
| 2 | $[0.1,\ 6.0]$ | 60 | 5 | 56 min |

### 3.2 The elbow criterion returns a function of the grid floor **[E]**

The elbow criterion takes the admissible pair with the largest change in $P$ and returns
the shallower side. On these networks $P$ falls fastest at the left edge of the range, so
the largest change is the first one. The elbows below are recomputed on the correct
envelope (§3.5, Appendix A), not read from the `gamma_star` column.

| Grid | Floor | Elbow $\gamma$, eras 11–17 | Ratio to floor |
|---|---|---|---|
| 1 | 0.01 | 0.0354 in five of seven; for era 17 the first two envelope partitions cross at 0.0171 | 1.7–3.5× |
| 2 | 0.10 | 0.20–0.40 in all seven | 2–4× |

For era 17 on grid 1, any floor below 0.0171 returns the same partition at a different
nominal $\gamma$. The selected resolution changes with the grid floor rather than
converging to a stable value.

### 3.3 The dominance-width criterion does not converge within the swept range **[E]**

Recomputed on the correctly derived envelope (Appendix A), the widest dominance interval
on grid 2:

| Era | Interval | Width | $K$ | Arcs cut | Censored |
|---|---|---|---|---|---|
| 11 | $[3.78,\ 5.31]$ | 1.53 | 123 | 22.4% | – |
| 12 | $[4.46,\ 5.14]$ | 0.68 | 147 | 22.2% | – |
| 13 | $[5.07,\ 6.00]$ | 0.93 | 245 | 18.3% | ceiling |
| 14 | $[5.16,\ 6.00]$ | 0.84 | 495 | 20.1% | ceiling |
| 15 | $[2.37,\ 3.29]$ | 0.93 | 422 | 10.4% | – |
| 16 | $[4.68,\ 6.00]$ | 1.32 | 643 | 13.3% | ceiling |
| 17 | $[3.37,\ 4.52]$ | 1.15 | 795 | 14.1% | – |

Three of seven are pinned to the ceiling, so raising it would move those three and may
reorder the rest. The censoring moved from the floor of grid 1 to the ceiling of grid 2.

Width is measured here in linear $\gamma$, which treats an interval of width 1 near
$\gamma = 5$ as equal to one near $\gamma = 1$, although the second is a much larger
proportional change in resolution. Measured in $\log\gamma$, era 15's interval (0.33) is
nearly as wide as era 11's (0.34) and wider than era 17's (0.29). The ranking of eras
therefore depends on the scale of measurement as well as on the ceiling.

### 3.4 The envelope changes gradually **[E]**

Era 17's envelope on grid 2 holds 24 partitions. $A/m$ declines monotonically from 0.981
to 0.835 and $K$ climbs monotonically from 585 to 852. No interval holds against its
neighbors by a margin unlike the others: widths range from 0.04 to 1.15, and the largest is
merely the largest of two dozen comparable intervals.

The raw sweep, which includes partitions the envelope excludes, shows run-to-run
variation: $K = 558$ at $\gamma = 0.2$ against 585 at $\gamma = 0.1$, and 821 at
$\gamma = 5.5$ against 822 at $\gamma = 5.1$. A pronounced dominance interval could
provide evidence of a preferred partition under this modularity objective; these envelopes
do not exhibit one **[I]**.

### 3.5 A defect in the admissible-set computation **[E]**

The `dominant` column written by `champ_tools.jl` does not match the envelope computed
directly from the per-$\gamma$ coefficients: on grid 2, era 17 flags 13 of 24 envelope
partitions, era 16 flags 9 of 22, and era 13 flags 12 of 20 while also flagging two that
are not on the envelope. The cause and the correct test are set out in Appendix A.

Two consequences matter here. The envelope figures reported above are recomputations
rather than readings of the `dominant` column. And because the elbow is computed over the
flagged set, the reported $\gamma^{\*}$ is wrong for two eras on grid 2: era 11 reports 1.0
against a true-envelope elbow of 0.4, and era 14 reports 0.5 against 0.4. The design set
out below does not use the admissible set, so the defect does not propagate into it.

---

## 4. Why the tested criteria do not identify a preferred resolution

Three propositions must be kept apart.

1. **The elbow and dominance-width criteria do not yield stable selections on these
   networks.** Supported by §3.2 and §3.3 **[E]**.
2. **These citation networks do not exhibit a clearly preferred partition under the
   tested modularity objective.** Plausible, consistent with §3.4, but conditional on that
   objective and on the two criteria examined **[I]**.
3. **The caffeine literature has no characteristic organizational scale.** Not
   established. Demonstrating it would require evidence from representations other than
   this one.

A structural explanation for the first two is available. The networks contain many
cited-only vertices whose assignments are strongly constrained by their citation
relationships; those vertices contribute little independent information about community
boundaries, while the citing articles that could distinguish communities are sparsely
connected to one another, at mean degree 2.38 **[E, I]**.

A second explanation is conjecture: caffeine is not a discipline but a substance studied
across pharmacology, nutrition, sport science, oncology, psychiatry, sleep research, and
toxicology, and a topic literature spanning that many domains may be organized at several
scales at once **[I]**. This is worth stating because it is testable – on a
bibliographic-coupling representation, for instance – and it is not tested here.

Bibliographic coupling among citing articles would give roughly an order of magnitude more
density on exactly the vertices that carry the structure. If that representation yields
stable communities, the conclusion will be that the present citation-network
representation does not reveal a preferred scale, not that none exists.

**[D]** We therefore treat community resolution as a design choice rather than as a
parameter these sweeps can identify.

---

## 5. Measuring community granularity

### 5.1 Why the arithmetic mean will not serve

Communities on these networks vary enormously in size, so dividing the number of articles
by the number of detected communities misrepresents the community a typical article
belongs to.

Suppose a network holds 100 citing articles in two communities, of 90 and 10. The
arithmetic mean community size is 50. But a randomly chosen article belongs to the larger
community with probability 0.9, so the size of the community it belongs to has expectation

$$g = 0.9(90) + 0.1(10) = 82,$$

and the corresponding effective number of communities is $100/82 \approx 1.22$. Two
communities were detected; their inequality makes them count as about one and a quarter.

That is not a contrived case here. Measured on the grid 2 partitions, with sizes in
vertices:

| Era | $n$ | $K$ | mean $n/K$ | $k_{\mathrm{eff}}$ | effective size | ratio | median |
|---|---|---|---|---|---|---|---|
| 11 | 2,629 | 118 | 22.3 | 23.5 | 111.8 | 5.0× | 5 |
| 12 | 4,268 | 143 | 29.8 | 7.3 | 584.5 | 19.6× | 3 |
| 13 | 10,479 | 199 | 52.7 | 4.7 | 2,250.4 | 42.7× | 4 |
| 14 | 27,380 | 562 | 48.7 | 14.2 | 1,930.2 | 39.6× | 4 |
| 15 | 32,848 | 564 | 58.2 | 8.4 | 3,917.9 | 67.3× | 4 |
| 16 | 45,508 | 657 | 69.3 | 13.4 | 3,397.0 | 49.0× | 5 |
| 17 | 76,070 | 904 | 84.1 | 8.2 | 9,258.6 | 110.0× | 5 |

The median community is a triad while a random vertex sits in a community of thousands
**[E]**.

Two things about this table must be stated plainly. First, $K$ here is the community count
in the `.clu` file, which includes the weak components below four vertices that were
dropped before detection and restored afterward (§2). It is therefore larger than the
$K$ reported by the sweeps: era 17 shows 904 here against an envelope maximum of 852 in
§3.4, and era 11 shows 118 against the 74 that §7.1 reports at $\gamma = 1.0$. The
restored components are by definition of size three or less, so the median of 3–5 may
describe them rather than the detected communities; the count of restored components per
era is needed before the median can be interpreted. Second, the `.clu` files were written
at per-era values of $\gamma$ that this draft does not record, and the values appear to be
the per-era $\gamma^{\*}$ that §3.5 shows to be defective for two eras. Both the $\gamma$
and the two counts will be tabulated in the next draft; the comparability measures in §7.2
depend on which $K$ is meant.

### 5.2 Definition

For a partition $C = \{c_1, \dots, c_K\}$ of an era network, let $w_c$ be the number of
citing articles in community $c$ and $W = \sum_c w_c$. Then

$$k_{\mathrm{eff}}(C) = \frac{W^2}{\sum_c w_c^2}
\qquad\qquad
g(C) = \frac{W}{k_{\mathrm{eff}}} = \frac{\sum_c w_c^2}{W}$$

$g$ is **granularity**: the number of citing articles in the community of a randomly
chosen citing article, measured in citing articles. $k_{\mathrm{eff}}$ is the
corresponding **effective number of communities**, the inverse-Simpson count, or Hill
number of order 2.

### 5.3 What the effective count is not

$k_{\mathrm{eff}}$ is a diversity equivalent. It summarizes how concentrated the citing
articles are across communities. It is not the number of independent analytical units
available to a model, and it should not be read as one. An era with 100 detected
communities and $k_{\mathrm{eff}} = 10$ does not contain ten observations; conversely, ten
effective communities do not guarantee enough information to estimate anything.

The number of rows available to a community-level model is the number of detected
communities, and the relevant diagnostic is therefore a count of communities above a size
floor rather than $k_{\mathrm{eff}}$. The construction will report the number holding at
least 1, 3, 5, and 10 citing articles.

A further distinction matters: the number of *available* observations is not the number of
*informative* ones. A community of three citing articles is technically an observation,
but three articles provide little information for estimating a topical distribution, and a
community adequate for one outcome may be inadequate for another. **[D]** Eligibility
rules should therefore be defined per outcome rather than by a single universal minimum:

- **Descent** requires a minimum number of citing articles and of qualifying cross-era
  relationships.
- **Composition** requires enough articles to estimate a topical or journal distribution
  with usable precision.
- **Integration** requires enough observed ties to distinguish substantive integration
  from an isolated connection.

Each threshold should follow from the measurement properties of its outcome.

### 5.4 Why granularity is measured on the citing population

Granularity counts articles that make at least one citation inside the era. These define
the **active citing population** whose community organization the design compares over
time.

Cited-only articles remain in the network, because they carry its citation structure. They
are excluded from the granularity measure because including them would make granularity
depend on how many citation targets each community happens to accumulate, which, under a
fixed seven-year tiling, is largely a function of how many references its members made
inside the window.

This is not a claim that cited-only works are unimportant. Foundational papers are
frequently cited while emitting no within-era citations at all; the backward-tie analysis
identifies several by name. The distinction is between articles that actively contribute
within-era citation relationships and articles represented only as citation targets, and
it follows from the estimand rather than from any judgment about intellectual significance.

It introduces an asymmetry the reader should see plainly: **detection uses all vertices,
while granularity is calibrated on citing articles only.** The vertices that help
determine community boundaries are therefore not the vertices used to calibrate community
size. A heavily cited foundational article referenced by pharmacologists, neuroscientists,
and nutrition researchers may affect whether those groups appear separate or joined, while
contributing nothing to the granularity measure.

The §2 argument narrows where that influence can lie. A cited-only vertex with in-degree 1
follows its citer and carries no independent information; only cited-only vertices with
in-degree 2 or more can join citers that would otherwise be apart. Their number and
in-degree distribution are readable from the `.net` files now, and they are the measurable
part of the construct-validity question.

**That question remains**: whether partitions detected on the full citation network but
calibrated on citing articles are an appropriate representation of research communities.
That depends on the role cited-only works play in determining community boundaries, which
has not been measured. We will examine whether the resulting partitions differ materially
from communities detected on alternative representations, particularly bibliographic
coupling among citing articles.

### 5.5 A proxy, and its limits **[E]**

The directed penalty term satisfies

$$P/m = \sum_c \frac{K^{\mathrm{out}}_c K^{\mathrm{in}}_c}{m^2} \approx \sum_c s_c^2
\qquad\Longrightarrow\qquad k_{\mathrm{eff}} \approx m/P$$

where $s_c$ is community $c$'s share of arc mass, so an effective community count is
readable at every $\gamma$ from the sweep table with no membership vector. The
approximation assumes each community's share of out-arcs matches its share of in-arcs,
which the cited-only population makes unlikely.

This proxy is degree-weighted. It has been checked against the `.clu` files on two eras,
but the `.clu` figures are the vertex-weighted counts of §5.1, not the citer-weighted
counts the design calibrates on. Against those, the proxy runs 18 percent low for era 17
(6.7 against 8.2) and 41 percent low for era 11 (13.8 against 23.5). Its relation to the
citer-weighted quantity is unknown in both sign and size, and the two measured ratios are
far enough apart that the correction cannot be assumed constant across eras. A correction
that changes with era changes the fitted scaling exponent, not only the level. Every figure
in §6 and §7 derives from the proxy.

Before the proxy is used for calibration, we must establish that the relationship between
resolution and citer-weighted effective community count is sufficiently stable and
monotonic to be inverted, and that partitions satisfying a granularity target agree with
one another in membership. §9.1 specifies those measurements.

---

## 6. Calibrating granularity across eras

### 6.1 The scaling rule

**[D]** Rather than impose one resolution across eras, we specify how the effective number
of communities should change as the literature grows. Writing
$k_{\mathrm{eff}} \propto W^{\alpha}$:

- $\alpha = 0$ holds the effective number of communities approximately constant;
- $\alpha = 1$ holds effective community size approximately constant;
- intermediate values let both number and size increase.

The pipeline takes $\alpha$ and an anchor as declared inputs and derives one $\gamma$ per
era:

```
target k_eff(e)  =  k_ref · (W_e / W_ref)^α
γ_e              =  the γ at which era e attains that k_eff
```

The sweeps of §3 supply the calibration curves. The admissible set, the elbow criterion,
and `gamma_star` play no part.

### 6.2 These are measurement conventions

$\alpha$ is not identified by any analysis reported here. Holding $\gamma$ fixed produces
whatever exponent the method and the graph yield:

| $\gamma$ | fitted $\alpha$ | $k_{\mathrm{eff}}$ era 11 | era 17 |
|---|---|---|---|
| 0.1 | 0.078 | 3.0 | 2.8 |
| 0.5 | 0.381 | 6.0 | 15.4 |
| 1.0 | 0.230 | 13.8 | 25.3 |
| 2.0 | 0.160 | 23.0 | 37.6 |
| 3.0 | 0.101 | 39.3 | 56.3 |
| 6.0 | 0.242 | 59.8 | 135.0 |

**[E, P]** On these networks, holding $\gamma$ fixed produces fitted scaling exponents
between 0.08 and 0.38. A common resolution therefore implicitly determines how effective
community counts change with the size of the literature, although the resulting exponent
depends on the resolution chosen. The value it imposes is not a property of the literature
alone. Modularity's resolution limit, under which the minimum detectable community degree
grows as $\sqrt{m}$, would give $k_{\mathrm{eff}} \propto m^{0.5}$; because arcs grow 29×
across the window while citing articles grow 17.5×, that is $\alpha \approx 0.59$ in $W$.
The realized values fall below it because the giant component does not divide **[I]**.

At the working anchor (§6.3), every value of $\alpha$ in $[0,1]$ is attainable within the
grid already swept **[E, P]**. Attainability depends on the anchor as well as on
$\alpha$; §6.3 and §10 record where it fails.

The settings of $\alpha$ correspond to nameable positions about how a field specializes,
by adding areas of characteristic size or by growing a stable number of them, and saying
so is what makes the sensitivity set interpretable rather than arbitrary. But because
$\alpha$ is specified rather than estimated, **this construction cannot adjudicate between
those positions.** An analysis that appeared to do so would be assuming the pattern it
claimed to find. The settings are treated as sensitivity specifications throughout.

### 6.3 The anchor is a second declared parameter

$\alpha$ fixes how granularity changes across eras; the anchor fixes its level. At
$\alpha = 1$ the anchor determines the approximate effective community size in every era,
so it is not a minor technical parameter. An anchor of 90 would produce a substantially
finer partition than one of 120, and an anchor of 160 a coarser one. Those alternatives
could yield different conclusions about integration and descent with $\alpha$ held fixed.

Sensitivity over $\alpha$ alone is therefore incomplete. **[D]** The sensitivity design is
two-dimensional, with the two questions separated:

- *How should granularity change across eras?* – controlled by $\alpha$.
- *At what level should granularity be set?* – controlled by the anchor.

A full factorial of three scaling rules by three anchors is nine constructions carried
through descent, the tie weightings, and every model. A reduced design answers both
questions at five: all three $\alpha$ at the primary anchor, plus one finer and one coarser
anchor at the primary $\alpha$.

The working anchor is $g = 120$ citing articles per effective community at era 14. It has
no substantive justification at present, and era 14 is an awkward place to anchor: $W$
jumps 2.6× from era 13 and only 1.1× to era 15, so the anchor sits at the one
discontinuity in the series. The alternative anchors are constrained by the swept range.
At $\alpha = 1$, era 17's attainable floor within the grid is $g \approx 67$ (§10), so a
finer anchor of 60 cannot be reached; and a coarser anchor of 180 requires
$k_{\mathrm{eff}} \approx 2.9$ for era 11, which is at the grid floor ($\gamma = 0.1$
yields 3.0). The feasible window at $\alpha = 1$ is therefore roughly 70 to 170 on the
present sweep, and the anchor values, primary and alternative, should be set within it only
after the citer-weighted calibration curves have been measured. §9.1 specifies that
measurement.

---

## 7. Primary and sensitivity specifications

### 7.1 Provisional calibration results

**[P] All figures in this section derive from the $m/P$ proxy and are provisional.**
Anchored at $g = 120$ for era 14. Each cell gives the detected community count $K$ (the
sweep count, excluding restored components), the effective count $k_{\mathrm{eff}}$, and
the $\gamma$ producing them.

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 |
|---|---|---|---|---|---|---|---|
| 0.00 | K=98, kₑ=35.6 @γ2.7 | K=107, kₑ=37.1 @γ2.5 | K=165, kₑ=38.2 @γ1.9 | K=403, kₑ=35.5 @γ2.4 | K=383, kₑ=36.3 @γ1.9 | K=475, kₑ=36.1 @γ1.4 | K=674, kₑ=36.1 @γ1.7 |
| 0.25 | K=94, kₑ=21.2 @γ1.9 | K=103, kₑ=25.3 @γ1.7 | K=158, kₑ=29.1 @γ1.4 | K=403, kₑ=35.5 @γ2.4 | K=383, kₑ=36.3 @γ1.9 | K=497, kₑ=40.4 @γ1.6 | K=710, kₑ=45.9 @γ2.3 |
| 0.50 | K=74, kₑ=13.8 @γ1.0 | K=85, kₑ=16.6 @γ1.1 | K=143, kₑ=23.4 @γ1.1 | K=403, kₑ=35.5 @γ2.4 | K=391, kₑ=38.1 @γ2.1 | K=495, kₑ=44.9 @γ1.8 | K=726, kₑ=52.3 @γ2.7 |
| 0.75 | K=70, kₑ=9.0 @γ0.6 | K=78, kₑ=11.4 @γ0.8 | K=134, kₑ=17.4 @γ0.8 | K=403, kₑ=35.5 @γ2.4 | K=391, kₑ=38.1 @γ2.1 | K=501, kₑ=46.3 @γ1.9 | K=753, kₑ=66.1 @γ3.4 |
| 1.00 | K=61, kₑ=5.3 @γ0.4 | K=82, kₑ=7.6 @γ0.7 | K=129, kₑ=13.9 @γ0.7 | K=403, kₑ=35.5 @γ2.4 | K=393, kₑ=39.5 @γ2.3 | K=508, kₑ=49.5 @γ2.1 | K=782, kₑ=77.9 @γ3.8 |

Every required $\gamma$ falls between 0.4 and 3.8, and the sweep community count never
falls below 61. That floor says nothing about how many communities clear a size threshold
(§5.3).

The table reports what was achieved, not what was targeted, and the two differ. At
$\alpha = 1$ the targets are $35.5 \cdot W_e / W_{14}$:

| | e11 | e12 | e13 | e14 | e15 | e16 | e17 |
|---|---|---|---|---|---|---|---|
| target $k_{\mathrm{eff}}$ | 4.3 | 7.5 | 13.6 | 35.5 | 38.8 | 49.4 | 76.0 |
| achieved | 5.3 | 7.6 | 13.9 | 35.5 | 39.5 | 49.5 | 77.9 |
| miss | +23% | +1% | +2% | – | +2% | 0% | +2% |

Era 11 misses by nearly a quarter, as it does at $\alpha = 0.75$ (7.3 targeted, 9.0
achieved) and $\alpha = 0.5$ (12.4 against 13.8); the largest miss elsewhere is era 13 at
$\alpha = 0$ (35.5 against 38.2). Whether these count as successful calibration is the
tolerance question of §9.1, and the next draft will report target and achieved for every
cell.

### 7.2 What the comparability measures do and do not show

Three quantities can be compared across eras. Granularity $g = W/k_{\mathrm{eff}}$:

| $\alpha$ | e11 | e12 | e13 | e14 | e15 | e16 | e17 | range |
|---|---|---|---|---|---|---|---|---|
| 0.00 | 14.5 | 24.2 | 42.6 | 119.5 | 127.6 | 163.4 | 251.7 | 17.4× |
| 0.50 | 37.4 | 54.2 | 69.4 | 119.5 | 121.3 | 131.3 | 173.7 | 4.6× |
| 1.00 | 98.6 | 117.7 | 116.8 | 119.5 | 117.1 | 119.1 | 116.5 | 1.2× |

Concentration $K/k_{\mathrm{eff}}$, a measure of how unequal the partition is:

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

**These three are not independent evidence.** By definition
$g = W/k_{\mathrm{eff}}$, and therefore

$$\frac{W}{K} = \frac{g}{K/k_{\mathrm{eff}}}.$$

The calibration sets $k_{\mathrm{eff}}$ in every specification, so $g$ is fixed by
construction at every $\alpha$, and the third table follows arithmetically from the first
two: at era 11, $98.6 / 11.6 = 8.50$, which is the 8.49 shown. Exactly **one** of the three
measures is free to vary independently of the calibration, and that is concentration. The
1.2× range in $g$ at $\alpha = 1$ is partly a property of the construction and partly the
era 11 calibration miss of §7.1; it is not a finding.

What the evidence supports is therefore narrower than three agreeing tables suggest.
Nothing in the calibration constrains $K/k_{\mathrm{eff}}$, and that ratio varies by a
factor of 1.2 at $\alpha = 1$ against 6.9 at $\alpha = 0$ **[E, P]**. Under $\alpha = 0$,
holding the effective count constant leaves the partitions progressively more unequal as
the field grows, from a ratio of 2.7 in era 11 to 18.7 in era 17, and leaves a monotone era
trend in $W/K$.

Concentration is nonetheless a single moment of the size distribution. Two partitions can
share a value of $K/k_{\mathrm{eff}}$ while differing in shape: one with a single very
large community among many small ones, another with several moderately large ones. The
largest-community share, the size Gini, and the counts of communities above size thresholds
are needed to assess comparability of shape, and none has been measured.

### 7.3 Specification

**[D] Primary: $\alpha = 1$, provisionally.** The case for it is the stability of
concentration, the one comparability measure the calibration leaves free (§7.2). Effective
community size is stable at $\alpha = 1$ by construction and is no evidence; mean
citing-article count per detected community follows from the other two. That one measure,
on proxy counts, is thin, and the conclusion will be tested against the full
community-size distributions, on citer-weighted counts, before the primary specification
is finalized.

**[D] Sensitivity: $\alpha = 0$ and $\alpha = 0.5$**, at the primary anchor, plus one
finer and one coarser anchor at $\alpha = 1$ (§6.3).

**What the primary specification constrains.** Holding effective community size
approximately constant does more than make community size uninformative. It constrains how
the growth of a research specialty can appear in the detected partitions: growth is
represented as additional communities rather than as an increase in the size of an existing
one. A large specialty that would appear as one community at a coarser resolution may be
divided into several here, which can change the number of apparent specialties, the number
of relationships among them, the measured rate of descent, and the apparent integration of
previously distinct areas.

The primary specification is therefore appropriate for comparing community composition and
relationships at a relatively stable effective size. It cannot independently establish how
the number or size of research specialties changed.

**What the sensitivity specifications test.** They are not all alike. At $\alpha = 0$,
granularity ranges 17.4× across eras, which is wider than the 9.6× range at a fixed
$\gamma = 1$ (§8.1). The $\alpha = 0$ construction therefore reintroduces, in stronger
form, the size dependence that §8.1 argues a fixed resolution leaves inside community-level
measures. A divergence between $\alpha = 0$ and $\alpha = 1$ results is predicted by that
argument and cannot by itself be read as evidence about the literature. Its value is to
show the magnitude of size dependence in each outcome, which is what the §8.1 argument
asserts and does not quantify. The $\alpha = 0.5$ construction and the alternative anchors
are the ones that test whether conclusions survive moderate changes in the convention,
and they are part of the inference rather than a robustness formality.

### 7.4 On selecting a specification after the fact

Reporting how a result varies with $\alpha$ is legitimate; choosing $\alpha$ to maximize an
effect is not, and the distinction must be visible in how the paper is written. "Stable
across the three specifications examined" is a reportable finding. The primary
specification is declared here, in advance of any model fitting, and the reasons given in
§7.2 and §7.3 concern the comparability of the units rather than any outcome.

---

## 8. Consequences for inference

### 8.1 Why not simply fix $\gamma = 1$

At $\gamma = 1$, granularity runs 37, 65, 86, 253, 197, 218, and 359 citing articles
across eras 11 to 17, a 9.6-fold change that tracks the 17.5-fold growth of the field
**[E, P]**. Several quantities the design measures at the community level depend on
community size, so a fixed resolution leaves an era trend inside them.

*Illustrative size effect.* If descent requires at least one qualifying cross-era
relationship, larger communities have more opportunities to satisfy that criterion. Under
independent article-level opportunities, the probability of at least one qualifying
relationship in a community of $s$ articles is $1 - (1-p)^s$, which is increasing and
concave in $s$. Measured descent could therefore rise with community size while the
article-level probability is unchanged.

Two qualifications. Article-level events in a citation network are not independent, so the
expression is a motivating example rather than a model. And the actual relationship between
community size and measured descent depends on the criterion implemented in
`PathA_TieLayer.R`, which remains to be verified: articles are assigned to a single birth
era, so a criterion requiring member works in both periods cannot be the one in use. The
implemented measure may involve a minimum number of backward citations, a normalized tie
weight, or another condition, and those would produce different relationships with size.

**Single-article community share.** Falls as effective size rises, so a fixed resolution
makes it decline across the window by construction.

**Composition measures.** Topical and journal diversity per community rise with the number
of articles observed, so an apparent increase in interdisciplinarity would be partly an
increase in community size.

The first two are the quantities recorded as open item C6 in the specification, and the
drift runs in the direction of the design's expectations. Everything said here of a fixed
$\gamma$ applies with more force to $\alpha = 0$ (§7.3).

### 8.2 What the design delivers, and what it does not

It makes the dependence visible. Under a single $\gamma$ a reader cannot tell whether a
community-level trend reflects the literature or the partition. Under several
specifications, results that persist are less dependent on the choice of granularity, and
results that appear under some specifications require further investigation, to determine
whether they reflect scale-dependent scientific organization or properties of the
measurement procedure. Sensitivity analysis locates that question; it does not settle it,
and a specification-dependent result is not thereby an artifact.

It also records what is being held constant. A referee will ask what makes the eras
comparable. Under a fixed resolution the answer is a null-model weighting, which is not a
substantive invariant. Under this design the answer is the distribution of citing articles
across communities, at a declared level and under a declared scaling rule.

What it does not deliver should be equally clear. It controls one dimension of
comparability. It does not make topical composition, internal connectivity, or the
statistical properties of community-level observations comparable across eras, and §7.3
notes that it constrains how specialization can appear.

---

## 9. Implementation and validation

### 9.1 Validation required before any partition is written

The construction inverts a $\gamma \to k_{\mathrm{eff}}$ curve. Nothing reported here
establishes that the curve supports inversion: Leiden is stochastic and can change
partitions discontinuously in $\gamma$. Five things must be measured first, on at least
eras 11, 13, and 17.

1. **Monotonicity.** Is citer-weighted $k_{\mathrm{eff}}$ approximately monotone in
   $\gamma$ over the working range?
2. **Run-to-run variation.** How much does citer-weighted $k_{\mathrm{eff}}$ move across
   repeated Leiden runs at fixed $\gamma$ under different seeds? If that variation is
   comparable to the calibration tolerance, the procedure cannot distinguish successful
   calibration from stochastic variation.
3. **Partition stability.** Among partitions that satisfy the granularity tolerance, how
   similar are their citing-article memberships? Two runs may both yield
   $k_{\mathrm{eff}} = 35$ while assigning hundreds of citing articles differently, and a
   calibration matching on a scalar would accept both while the downstream analysis of
   descent and integration differed. Agreement should be measured with adjusted mutual
   information, which corrects for agreement expected by chance and suits partitions with
   different cluster counts.
4. **Selection among near-equivalent partitions.** When several $\gamma$ give
   indistinguishable effective counts, which partition is taken, and on what rule?
5. **Tolerance.** What relative deviation from the target counts as successful calibration,
   justified by (2) and (3) rather than chosen? The era 11 misses in §7.1 are the first
   cases it must adjudicate.

The proxy-to-measured correction of §5.5 is measured at the same time, against
citer-weighted counts, since the probe computes them directly. The §2 condition
$\max_c \gamma K^{\mathrm{out}}_c / m < 1$ is checked over the working range in the same
pass.

**[D] Requirement (3) is a decision point, not only a check.** If membership agreement
across seeds at a fixed $\gamma$ is poor, the partitions are not a stable foundation under
any $\alpha$, the anchor question is moot, and bibliographic coupling moves from a
validation exercise to a prerequisite. The probe is inexpensive and bears directly on
whether the rest of this design is worth building, so it should be run before anything
else.

### 9.2 Code changes

- **A granularity function** returning citer-weighted $k_{\mathrm{eff}}$ and $g$ from a
  membership vector joined to within-era out-degrees, together with the counts of
  communities holding at least 1, 3, 5, and 10 citing articles, the largest-community
  share, a size Gini, the singleton share, and the number of restored components, reported
  separately from the detected count.
- **A probe function** implementing §9.1, including adjusted mutual information across
  seeds.
- **A calibration function** inverting the curve per era, reporting target and achieved
  granularity separately, whether the tolerance was met, and whether the search pressed
  against a $\gamma$ bound.
- **Partition output at a specified $\gamma$**, tagged by specification so the
  constructions coexist, with the $\gamma$ recorded in the file.
- **A corrected admissible-set computation** in `champ_tools.jl` (Appendix A). It is off
  the critical path, since the design does not use the admissible set, but the envelope is
  reported.

### 9.3 What changes per specification, and what does not

The underlying cross-era citation relationships do not change. They are constructed from
the dated citation edge list and the era assignments, independently of community detection,
so the 464,691 distance-one backward ties are fixed across all specifications **[E]**.

What changes is their aggregation. Each specification produces a different partition, so
the endpoints of community-to-community relationships differ, and community-level descent
measures and tie weightings must be recomputed for each.

Everything upstream of detection is also unchanged: the unified node list with preserved
node ids, the dated edge list, the era maps, and the whole-corpus network. The cross-era
share recorded per work as `pct_out_cross_era` remains a covariate, and the backward-tie
findings – 55.7% of backward ties at distance one, concentration falling rather than rising
with distance, 31.7% of distance-one ties landing on works cited exactly once – stand as
reported **[E]**.

---

## 10. Limitations and unresolved questions

**The proxy.** All of §6 and §7 rests on $m/P$, which has been compared only with
vertex-weighted counts, where it runs 18 to 41 percent low on two eras. Its relation to
citer-weighted $k_{\mathrm{eff}}$ is unmeasured, and because the two measured ratios
differ, the correction may change with era and so change the fitted $\alpha$ as well as the
level. The specific $\gamma$ values and the comparisons in §7.2 must be recomputed on
citer-weighted counts.

**Two community counts are in use.** The sweep $K$ excludes restored components; the
`.clu` $K$ includes them (§5.1). Until both are tabulated, the §5.1 medians and the §7.2
concentration figures cannot be compared across sections.

**The arc totals do not reconcile** (§2).

**The primary specification rests on one independent measure** (§7.2) and on the proxy. It
is not settled.

**Era 17's attainable floor is censored.** The analysis of attainable granularity rests on
$g_{\min} \approx 67$ being era 17's floor within the swept range, not in principle. It
rules out the finer anchor of 60 on the present sweep (§6.3).

**Equal granularity is one way to improve comparability, but it is not sufficient**, and it
is not universally necessary: a model could account explicitly for community size, or use
an estimand insensitive to it. That escape is narrower here than in general, because under
$\alpha = 0$ mean community size is a deterministic function of era and the two cannot be
separated at the era level.

**The anchor has no substantive justification yet**, and it sits at the discontinuity
between eras 13 and 14 (§6.3).

**The descent criterion needs verification** (§8.1) before the size-dependence argument can
be stated precisely.

**Construct validity is open.** The partitions are communities of all participating works,
calibrated on the citing population (§5.4). Whether that is the construct the models should
take as their unit is a question for discussion.

**The density problem is managed, not solved.** Everything here is downstream of mean
degree near 3 with most of the vertex set cited once or not at all. Bibliographic coupling
among citing articles is the same evidence at roughly an order of magnitude more density,
and is the open design question recorded in §14.7 of the specification. If coupling yields
stable communities, §4's second proposition weakens considerably.

---

## 11. Questions for discussion

Three questions are about the scientific constructs rather than the implementation, and the
design should not be settled without answers to them.

**What is the appropriate scientific unit?** Should a community represent a group of
articles that cite one another, a group that shares intellectual foundations, or a research
specialty inferred from both? The answer bears directly on whether direct citation or
bibliographic coupling is the better representation.

**Should the analysis hold community size approximately constant?** The $\alpha = 1$
specification improves comparability on the measures reported, and it also constrains how
specialization can appear in the partitions (§7.3). Is that an appropriate measurement
convention for studying the descent and integration of research communities?

**What constitutes meaningful community persistence?** If two successive communities share
intellectual foundations but contain different articles, should they count as the same
continuing research specialty? This connects the granularity design to the descent
criterion and may matter most for interpreting the historical development of caffeine
research.

---

## 12. The contribution

The citation networks do not identify a preferred community resolution under the criteria
examined. Rather than impose a common resolution parameter across eras, we calibrate
community detection to explicit assumptions about how granularity changes as the literature
grows. The primary specification holds effective community size approximately constant,
while alternative specifications allow community size and number to change. Comparing
results across these specifications reveals which conclusions about research-community
organization and persistence depend on the chosen granularity.

The design controls one dimension of comparability: the distribution of citing articles
across communities. It does not establish that communities are comparable in composition,
connectivity, or statistical behavior. The question it puts to a reader is not whether a
particular $\alpha$ is correct, but whether the substantive conclusions survive reasonable
alternatives.

---

## Appendix A: the defect in the admissible-set computation

A partition $i$ is admissible when some $\gamma$ in the swept range exists at which no
other partition scores higher. The implementation in `champ_tools.jl` tests this by
examining, for each competitor $j$, a single $\gamma$ midway between the pair's crossing
point and partition $i$'s own $\gamma$, and marking $i$ inadmissible if $j$ scores higher
there. A competitor may score higher at that probe point while $i$ is still optimal
elsewhere in the range, so the test rejects admissible partitions.

The correct test is a one-dimensional feasibility question. Partition $i$ is admissible iff

$$\{\gamma \in [\gamma_{\min}, \gamma_{\max}] : A_i - \gamma P_i \geq A_j - \gamma P_j \ \ \forall j\} \neq \emptyset .$$

Each competitor constrains $\gamma$ to a half-line, since
$A_i - \gamma P_i \geq A_j - \gamma P_j$ is equivalent to
$(A_i - A_j) \geq \gamma (P_i - P_j)$. A competitor with *smaller* $P$ than $i$ makes
$P_i - P_j$ positive and so bounds $\gamma$ from above: $i$ is the coarser, steeper line
and can beat $j$ only at small $\gamma$. A competitor with *larger* $P$ bounds $\gamma$
from below, and equal $P$ reduces to a comparison of intercepts. Intersecting those
half-lines with the swept range leaves an interval, and the partition is admissible exactly
when that interval is non-empty. This is $O(n^2)$ for $n$ grid points and exact.

Measured effect on grid 2: era 17 flags 13 of 24 envelope partitions, era 16 flags 9 of 22,
and era 13 flags 12 of 20 while also flagging two that are not on the envelope. Because the
elbow criterion is computed over the flagged set, the reported $\gamma^{\*}$ is wrong for
era 11 (1.0 against 0.4) and era 14 (0.5 against 0.4). This is open item C7 in the
specification, in the opposite direction to what C7 records: on these grids the count is
low, not high.

---

## Appendix B: measurement provenance

| Quantity | Source |
|---|---|
| Era vertex, arc, component, citing, and both-degree counts | `era_profiles.tsv`, both grids |
| Per-$\gamma$ $A$, $P$, $K$ (detected communities only) | `era*_sweep.tsv`, grids 1 and 2 |
| Envelopes, elbow, and dominance widths | recomputed from $A$, $P$ as the upper envelope of $Q_i(\gamma) = (A_i - \gamma P_i)/m$ |
| Community size distributions, vertex-weighted $k_{\mathrm{eff}}$, $K$ including restored components | `era*_local_gamma.clu`, grid 2; $\gamma$ per era not yet recorded |
| $\alpha$ fits, attainable windows, calibration tables | derived from the above via the $m/P$ proxy |
| Corpus, node-list, and backward-tie figures | `PathA_Revised_Eras.R`, `PathA_BackwardTies.R` |

Grid 1: `pajek_files/champ_sweep_7yr/gamma_0.01_1.0_n40/`.
Grid 2: `pajek_files/champ_sweep_7yr/gamma_0.1_6.0_n60/`.

Open checks: the era 17 arc total against the in-degree and citing-arc figures (§2); the
$\gamma$ at which each `.clu` file was written; the number of restored components per era.

---

## Appendix C: revision note

For readers of the earlier drafts, the substantive changes are these.

The first draft asserted that the literature has no characteristic organizational scale.
That claim is withdrawn; §4 now separates the three propositions and marks the third as
unestablished.

The first draft treated the effective community count as a number of analytical units,
summing it across eras and declaring eras below ten too thin to model. Both are withdrawn
(§5.3).

The primary specification changed from $\alpha = 0.5$ to $\alpha = 1$ as a consequence of
that correction, since the $\alpha = 0.5$ choice rested on the withdrawn feasibility
argument.

The second draft presented three comparability measures as agreeing evidence for
$\alpha = 1$. Two of the three are arithmetically determined by the others (§7.2), so the
independent evidence is narrower.

Partition stability across Leiden seeds has been added as a validation requirement and a
decision point (§9.1), and the anchor has been promoted to a declared parameter with its own
sensitivity dimension (§6.3).

The fourth draft corrects the condition in the §2 argument (it is
$\gamma K^{\mathrm{out}}_c < m$, which is not automatic), reverses the direction of the
bounds in Appendix A, restates the resolution-limit exponent in $W$ rather than $m$, and
withdraws the claim that the proxy has been validated against citer-weighted counts. It
distinguishes the two community counts in use, reports target against achieved calibration
for the primary specification, records the feasible anchor window, and reframes
$\alpha = 0$ as a demonstration of size dependence rather than an independent test.

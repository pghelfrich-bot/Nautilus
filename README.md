# Statistics & Figure Producer

An interactive R Shiny teaching tool for early-career researchers in biology,
medicine, physiology, and ecology. Students upload a dataset, get plain-language
guidance on which test fits, run it with the assumptions checked for them, and
export publication-quality figures. It combines the tabular ease of PAST, the
analytical breadth of Minitab, and the graphical control of Origin.

## Built for students

Every analysis is wrapped in guardrails so beginners don't misuse a test:

- **Data health check** on upload — flags missing values, small samples,
  outliers, and unbalanced groups before any test is run.
- **Choose a Test** tab — answer two plain questions and get a specific
  recommendation, *or* point it at your own columns and it checks normality,
  variances, and sample sizes and names the right test for your data.
- **Automatic assumption checks** on every comparison, with amber/green
  advisories ("at least one group departs from normal — Mann-Whitney is safer").
- **"Auto" test mode** that picks the correct parametric or non-parametric test
  from the data's actual properties.
- **Plain-language interpretation** of every p-value and, crucially,
  **effect sizes with a magnitude label** (Cohen's d, Hedges g, eta²/epsilon²,
  rank-biserial r, Cramér's V) — because significance is not the same as size.
- Two worked demo datasets (a clinical/experiment set and an ecology community
  matrix) so students can explore with zero setup.
- **Paste-in data** — drop tab/comma/semicolon-separated data straight from
  Excel; the delimiter is auto-detected.
- **Transform / Standardize** tab that inspects the selected columns and
  *recommends* an appropriate transformation (z-score for mismatched scales,
  log/sqrt for skew, Hellinger/relative for community counts) before applying
  z-score, log(x+1), square-root, range 0-1, Hellinger, relative, or
  presence-absence. Results become new columns available everywhere.
- **Compare Methods** tab to put analyses side by side: PCA vs PCoA vs NMDS on
  one matrix (3-panel figure plus a Procrustes correlation and NMDS stress
  quantifying how much method choice matters), parametric vs non-parametric
  group tests (p-values and effect sizes together), and Pearson vs Spearman
  correlation.

## Analyses

**Explore** — descriptive statistics (per group), skewness/kurtosis, Shapiro-Wilk
normality, histograms/density, and Q-Q plots.

**Compare groups** — two-group (Welch / Student t, Mann-Whitney, paired),
3+ groups (one-way / Welch ANOVA, Kruskal-Wallis) with Tukey/Dunn post-hoc, and
two-way ANOVA / ANCOVA with interaction plots.

**Categorical & proportions** — contingency tables (chi-squared with automatic
Fisher fallback, McNemar for paired), 2×2 risk & odds (odds ratio, relative
risk, risk difference with 95% CIs), and diagnostic accuracy / ROC (AUC,
sensitivity, specificity, PPV, NPV at the Youden-optimal cutoff).

**Regression** — linear/multiple regression with residual diagnostics, logistic
regression (odds ratios, probability curve), and Poisson GLM with an
over-dispersion warning.

**Correlation** — Pearson / Spearman / Kendall matrices and pairwise partial
correlation controlling for a chosen covariate.

**Survival** — Kaplan-Meier curves, log-rank test, and Cox proportional-hazards
regression.

**Ecology** — diversity indices (richness, Shannon, Simpson, inverse Simpson,
Pielou evenness), community comparison (PERMANOVA, ANOSIM, Mantel), and
ordination (PCA, PCoA, CA, DCA, NMDS, RDA).

**Bioinformatics** — a **clustered heatmap** (per-feature or per-sample
z-scoring, sample dendrogram, group annotation bar, diverging colorblind-safe
scale), a **differential-expression volcano plot** (per-feature t-test or
Mann-Whitney with Benjamini-Hochberg correction, fold-change/significance
thresholds, labelled hits, downloadable results table), and a **sample
clustering dendrogram** (choice of distance and linkage, tips coloured by
group).

**Export** — one place to choose the file format for *every* Download-figure
button: PNG or TIFF (raster, pick the dpi) or PDF/SVG (vector, razor-sharp and
editable in Illustrator/Inkscape for manuscripts).

**Power** — power / sample-size solving for t-tests, ANOVA, two proportions, and
correlation, with a power-vs-n curve and effect-size conventions.

## Figure standards

All figures pass through a single `theme_publication()` and export at 300 dpi:

- Wong et al. (2011) colorblind-safe palette (blue `#0072B2`, orange `#D55E00`,
  extended as groups grow).
- Georgia serif typography throughout.
- Inward tick marks on all four sides, black 1.2 pt spines, white background,
  no gridlines.
- Y-axes locked to labelled tick boundaries (≤ 3% breathing room only when data
  would collide with a spine).
- Bold-letter panel labels in the top-left corner, no panel titles, no footer
  notes, no N values in legends.
- Boxplots with jittered points, a diamond mean marker, suppressed outlier
  markers, and significance brackets drawn directly on the canvas.

## Running

```r
install.packages(c("shiny", "DT", "ggplot2", "dplyr", "tidyr", "scales",
                   "moments", "car", "FSA", "ppcor", "vegan", "survival",
                   "pwr", "ggsignif", "patchwork", "ggdendro", "ggrepel",
                   "svglite", "readxl"))
shiny::runApp("app.R")
```

Core operation needs only `shiny`, `DT`, `ggplot2`, `dplyr`, `tidyr`, `scales`.
Every other package unlocks a specific feature (ordination, survival, power,
Dunn post-hoc, partial correlation, multi-panel comparison figures, Excel
import, significance brackets) and is checked at runtime — a missing package
degrades that one feature with an on-screen note rather than crashing the app.

This app has been executed and tested end-to-end on R 4.3.3: every module's
outputs and figures were driven with both demo datasets via `shiny::testServer`
and confirmed to run without error, and the app boots as a live server. To run
it yourself, install the packages above (on Debian/Ubuntu, precompiled binaries
from the Posit Public Package Manager avoid slow compilation) and launch. For
the Georgia typeface, ensure it is installed on your system; otherwise the
theme falls back to the default serif.

## Package citations

Run `citation("<package>")` in R for the canonical, version-specific reference.
Methods packages: `vegan` (diversity, ordination, PERMANOVA/ANOSIM/Mantel),
`survival` (Kaplan-Meier, Cox), `car` (Type-II ANOVA, diagnostics), `FSA`
(Dunn post-hoc), `ppcor` (partial correlation), `moments` (distribution shape),
`pwr` (power analysis), `ggplot2` and `ggsignif` (figures).

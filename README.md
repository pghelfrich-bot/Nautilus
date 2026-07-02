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
ordination (PCA, CA, DCA, NMDS, RDA).

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
                   "moments", "car", "coin", "FSA", "ppcor", "vegan",
                   "FactoMineR", "ade4", "survival", "pwr", "ggsignif", "readxl"))
shiny::runApp("app.R")
```

Core operation needs only `shiny`, `DT`, `ggplot2`, `dplyr`, `tidyr`, `scales`.
Every other package unlocks a specific feature (ordination, survival, power,
Dunn post-hoc, partial correlation, Excel import, significance brackets) and is
checked at runtime — a missing package degrades that one feature with an
on-screen note rather than crashing the app.

## Package citations

Run `citation("<package>")` in R for the canonical, version-specific reference.
Methods packages: `vegan` (diversity, ordination, PERMANOVA/ANOSIM/Mantel),
`survival` (Kaplan-Meier, Cox), `car` (Type-II ANOVA, diagnostics), `FSA`
(Dunn post-hoc), `ppcor` (partial correlation), `moments` (distribution shape),
`pwr` (power analysis), `ggplot2` and `ggsignif` (figures).

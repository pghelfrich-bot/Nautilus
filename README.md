# Statistics & Figure Producer

An interactive R Shiny application for exploratory statistics, guided test
selection, multivariate ordination, and publication-quality figure generation.
It pairs the tabular ease of PAST, the analytical breadth of Minitab, and the
graphical control of Origin in a single browser-based workflow.

## Capabilities

**1. Statistics calculator**

- Data upload (CSV / TSV / Excel) with automatic variable typing.
- Descriptive statistics: mean, SD, SE, median, IQR, range, skewness,
  kurtosis, coefficient of variation, per-group or pooled.
- Normality assessment: Shapiro-Wilk and Kolmogorov-Smirnov with a plain
  verdict.
- **Test Guide** that maps response/predictor structure, design, and
  distribution to the appropriate test with the backing R function named.
- Group comparisons: Welch t-test, Mann-Whitney U, one-way ANOVA, Welch
  ANOVA, Kruskal-Wallis, with Tukey HSD / Dunn / pairwise post-hoc and
  BH-adjusted p-values.
- Correlation: Pearson, Spearman, Kendall, and pairwise partial correlation
  controlling for a chosen covariate.
- Ordination: PCA, Correspondence Analysis, Detrended CA, NMDS, and
  Redundancy Analysis.

**2. Figure producer**

All figures are generated through a single `theme_publication()` and export at
300 dpi. The theme enforces:

- Wong et al. (2011) colorblind-safe palette (blue `#0072B2`, orange
  `#D55E00`, extended as more groups appear).
- Georgia serif typography throughout.
- Inward tick marks on all four sides, black 1.2 pt spines, white background,
  no gridlines.
- Y-axes locked to labelled tick boundaries (≤ 3% breathing room only when
  data would collide with a spine).
- Bold letter panel labels in the top-left corner, no panel titles, no footer
  notes, no N values in legends.
- Boxplots with jittered points, a diamond mean marker, and suppressed
  outlier markers; significance brackets drawn directly on the canvas.

## Running

```r
install.packages(c("shiny", "DT", "ggplot2", "dplyr", "tidyr", "scales",
                   "moments", "car", "coin", "FSA", "ppcor", "vegan",
                   "FactoMineR", "ade4", "ggsignif", "readxl"))
shiny::runApp("app.R")
```

Core operation requires only `shiny`, `DT`, `ggplot2`, `dplyr`, `tidyr`, and
`scales`. The remaining packages unlock skewness/kurtosis, Dunn post-hoc,
partial correlation, ordination, Excel import, and significance brackets; each
is checked at runtime and its feature degrades gracefully if absent.

Click **Load demonstration dataset** on the Data tab to explore without a
file.

## Package citations

Run `citation("<package>")` in R for the canonical, version-specific reference.
Key methods packages: `vegan` (NMDS, CA, DCA, RDA), `FactoMineR` and `ade4`
(multivariate exploration), `car` and `coin` (inference), `FSA` (Dunn
post-hoc), `ppcor` (partial correlation), `moments` (distributional shape),
`ggplot2` and `ggsignif` (figures).

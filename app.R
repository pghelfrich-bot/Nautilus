# Statistics & Figure Producer
# Interactive teaching platform for summary statistics, guided test selection,
# assumption checking, effect sizes, biological and ecological analyses, and
# publication-quality figures. Designed for early-career researchers across
# medical, physiological, and ecological disciplines.
#
# Package citations (cite the version reported by citation("<pkg>")):
#   shiny        - Chang W et al. Web Application Framework for R.
#   DT           - Xie Y et al. A Wrapper of the JavaScript Library DataTables.
#   ggplot2      - Wickham H. Elegant Graphics for Data Analysis. Springer, 2016.
#   dplyr, tidyr - Wickham H et al. Grammar of data manipulation / tidy data.
#   moments      - Komsta L, Novomestky F. Moments, cumulants, skewness, kurtosis.
#   car          - Fox J, Weisberg S. An R Companion to Applied Regression, 3rd ed.
#   coin         - Hothorn T et al. Conditional inference procedures.
#   FSA          - Ogle DH et al. Fisheries Stock Analysis (Dunn post-hoc).
#   ppcor        - Kim S. Partial and semi-partial correlation.
#   vegan        - Oksanen J et al. Community Ecology Package.
#   FactoMineR   - Le S, Josse J, Husson F. Multivariate exploratory analysis.
#   ade4         - Dray S, Dufour AB. Implementing the duality diagram.
#   survival     - Therneau TM. A Package for Survival Analysis in R.
#   pwr          - Champely S. Basic Functions for Power Analysis.
#   ggsignif     - Ahlmann-Eltze C, Patil I. Significance brackets for ggplot2.
#   patchwork    - Pedersen TL. The Composer of Plots (multi-panel figures).
#   ggdendro     - de Vries A, Ripley BD. Dendrograms as ggplot2 data.
#   ggrepel      - Slowikowski K. Repulsive text labels for ggplot2.
#   svglite      - Wickham H et al. An SVG graphics device.
#   drc          - Ritz C et al. Dose-Response Analysis Using R. PLoS ONE 2015.
#   scales       - Wickham H, Seidel D. Scale functions for visualization.
#   readxl       - Wickham H, Bryan J. Read Excel files.

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

has_pkg <- function(p) requireNamespace(p, quietly = TRUE)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---------------------------------------------------------------------------
# Graphical standards
# ---------------------------------------------------------------------------

# Wong et al. (2011) Nature Methods 8:441 colorblind-safe palette.
wong_palette <- c("#0072B2", "#D55E00", "#E69F00", "#009E73",
                  "#F0E442", "#CC79A7", "#56B4E9", "#000000")

wong_values <- function(n) {
  if (n <= length(wong_palette)) wong_palette[seq_len(n)]
  else colorRampPalette(wong_palette)(n)
}

# Distinct, high-contrast plotting shapes for a second grouping factor (e.g. site).
pub_shapes <- c(16, 17, 15, 18, 8, 4, 3, 7, 10, 12)
pub_shape_scale <- function(name = NULL)
  scale_shape_manual(values = rep(pub_shapes, length.out = 64), name = name)

# ggplot linewidth is expressed in millimetres; convert from points.
pt_to_mm <- function(pt) pt / 2.834645669

PUB_FONT <- "Georgia"
SPINE_MM <- pt_to_mm(1.2)

theme_publication <- function(base_size = 13, base_family = PUB_FONT) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text              = element_text(family = base_family, colour = "black"),
      panel.grid        = element_blank(),
      panel.background  = element_rect(fill = "white", colour = NA),
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.border      = element_rect(fill = NA, colour = "black", linewidth = SPINE_MM),
      axis.line         = element_blank(),
      axis.ticks        = element_line(colour = "black", linewidth = SPINE_MM),
      axis.ticks.length = unit(-4, "pt"),           # negative draws ticks inward
      axis.text         = element_text(colour = "black"),
      axis.text.x       = element_text(margin = margin(t = 6)),
      axis.text.y       = element_text(margin = margin(r = 6)),
      axis.title        = element_text(colour = "black"),
      legend.key        = element_blank(),
      legend.background = element_blank(),
      legend.title      = element_text(face = "plain"),
      plot.title        = element_blank(),
      strip.background  = element_blank(),
      strip.text        = element_blank(),
      complete = TRUE
    )
}

# Discrete groups take the exact Wong colours in order (blue, orange, amber,
# green, ...); a ramp only supplies extras beyond the eight base hues.
pub_discrete <- c(unname(wong_palette), colorRampPalette(wong_palette)(64))
pub_colour <- function(name = NULL) {
  list(scale_colour_manual(values = pub_discrete, na.value = "grey40", name = name),
       scale_fill_manual(values = pub_discrete, na.value = "grey40", name = name))
}

# Unlabelled ticks on the top and right spines so all four sides carry ticks.
four_side_x <- function(...) scale_x_continuous(..., sec.axis = dup_axis(name = NULL, labels = NULL))

# Lock a continuous axis to its labelled tick boundaries. A small symmetric
# pad (default 3%) is permitted only when marks would otherwise sit on a spine.
locked_axis <- function(axis = c("y", "x"), values, n = 6, pad = 0, sec = TRUE) {
  axis <- match.arg(axis)
  brks <- scales::extended_breaks(n = n)(values[is.finite(values)])
  brks <- brks[is.finite(brks)]
  lims <- range(brks)
  ex <- expansion(mult = pad)
  s <- if (axis == "y") scale_y_continuous else scale_x_continuous
  if (sec) s(breaks = brks, limits = lims, expand = ex,
             sec.axis = dup_axis(name = NULL, labels = NULL))
  else s(breaks = brks, limits = lims, expand = ex)
}

# Bold uppercase panel label anchored to the top-left; pair with labs(tag = "A").
theme_panel_tag <- function(family = PUB_FONT, size = 16) {
  theme(plot.tag = element_text(face = "bold", family = family, size = size),
        plot.tag.position = c(0.02, 0.98))
}

# ---------------------------------------------------------------------------
# Student-facing helpers: notes, plain-language interpretation, effect sizes
# ---------------------------------------------------------------------------

# Colour-coded advisory box. sev: info (blue), ok (green), warn (amber), bad (red).
note <- function(text, sev = c("info", "ok", "warn", "bad")) {
  sev <- match.arg(sev)
  col <- c(info = "#0072B2", ok = "#009E73", warn = "#E69F00", bad = "#D55E00")[[sev]]
  div(style = sprintf(paste0("border-left:5px solid %s; background:%s14;",
                             "padding:8px 12px; margin:7px 0; border-radius:3px;"),
                      col, col),
      HTML(text))
}

fmt_p <- function(p) ifelse(is.na(p), "NA",
                     ifelse(p < 0.001, "&lt; 0.001", formatC(p, format = "f", digits = 3)))
fmt_n <- function(x, d = 3) formatC(x, format = "f", digits = d)

interp_p <- function(p, alpha = 0.05) {
  if (is.na(p)) return(list(txt = "The p-value could not be computed for this data.", sev = "warn"))
  if (p < alpha)
    list(txt = sprintf(paste("<b>p = %s</b> is below your %.2f threshold, so this is a",
                             "<b>statistically significant</b> result: a difference/association",
                             "this large is unlikely to arise from sampling noise alone."),
                       fmt_p(p), alpha), sev = "ok")
  else
    list(txt = sprintf(paste("<b>p = %s</b> is above your %.2f threshold, so this is",
                             "<b>not statistically significant</b>: there is not enough evidence",
                             "to distinguish the pattern from chance. This is not proof of 'no effect' —",
                             "a larger sample might still detect one."),
                       fmt_p(p), alpha), sev = "info")
}

mag_label <- function(x, cuts, labs) labs[findInterval(abs(x), cuts) + 1L]

d_magnitude   <- function(d) mag_label(d, c(0.2, 0.5, 0.8), c("negligible", "small", "medium", "large"))
eta_magnitude <- function(e) mag_label(e, c(0.01, 0.06, 0.14), c("negligible", "small", "medium", "large"))
r_magnitude   <- function(r) mag_label(r, c(0.1, 0.3, 0.5), c("negligible", "small", "moderate", "strong"))
v_magnitude   <- function(v) mag_label(v, c(0.1, 0.3, 0.5), c("negligible", "small", "moderate", "strong"))

cohens_d <- function(x, y) {
  nx <- length(x); ny <- length(y)
  sp <- sqrt(((nx - 1) * stats::var(x) + (ny - 1) * stats::var(y)) / (nx + ny - 2))
  if (sp == 0) return(NA_real_)
  (mean(x) - mean(y)) / sp
}
hedges_g <- function(d, n_total) d * (1 - 3 / (4 * n_total - 9))

eta_sq_aov <- function(fit) {
  a <- summary(fit)[[1]]
  ss <- a[["Sum Sq"]]
  ss[1] / sum(ss)
}
epsilon_sq_kw <- function(H, n) H * (n + 1) / (n^2 - 1)
rank_biserial <- function(W, n1, n2) 1 - 2 * W / (n1 * n2)  # W = U statistic of group 1
cramers_v <- function(chisq, n, r, c) sqrt(as.numeric(chisq) / (n * (min(r, c) - 1)))

p_to_stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                          ifelse(p < 0.05, "*", "ns")))

# Shapiro-Wilk wrapper that respects its 3..5000 sample-size window.
safe_shapiro <- function(x) {
  x <- stats::na.omit(x)
  if (length(x) < 3 || length(x) > 5000) return(NA_real_)
  tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
}

classify_variable <- function(x) {
  if (is.numeric(x)) {
    u <- length(unique(stats::na.omit(x)))
    if (u <= 2) "binary"
    else if (u <= 8 && all(x == round(x), na.rm = TRUE)) "discrete/count"
    else "continuous"
  } else if (length(unique(stats::na.omit(x))) <= 2) "binary" else "categorical"
}

summary_table <- function(df, vars) {
  num <- vars[vapply(df[vars], is.numeric, logical(1))]
  if (!length(num)) return(NULL)
  out <- lapply(num, function(v) {
    x <- stats::na.omit(df[[v]])
    if (!length(x)) return(NULL)
    data.frame(
      Variable = v, N = length(x), Mean = mean(x), SD = stats::sd(x),
      SE = stats::sd(x) / sqrt(length(x)), Median = stats::median(x),
      IQR = stats::IQR(x), Min = min(x), Max = max(x),
      Skewness = if (has_pkg("moments")) moments::skewness(x) else NA_real_,
      Kurtosis = if (has_pkg("moments")) moments::kurtosis(x) else NA_real_,
      CV = stats::sd(x) / mean(x), check.names = FALSE)
  })
  out <- do.call(rbind, out)
  nc <- setdiff(names(out), c("Variable", "N"))
  out[nc] <- lapply(out[nc], function(z) round(z, 4))
  out
}

# Standardization / transformation of a numeric matrix.
apply_transform <- function(m, method) {
  m <- as.matrix(m)
  out <- switch(method,
    z    = scale(m),
    log1p = log1p(m),
    sqrt = sqrt(m),
    range = apply(m, 2, function(x) { r <- range(x, na.rm = TRUE)
              if (diff(r) == 0) x * 0 else (x - r[1]) / diff(r) }),
    hellinger = if (has_pkg("vegan")) as.matrix(vegan::decostand(m, "hellinger")) else sqrt(m / rowSums(m)),
    total = m / rowSums(m),
    pa   = (m > 0) * 1)
  as.data.frame(out)
}

# Inspect a numeric block and recommend an appropriate transformation.
transform_reco <- function(m) {
  m <- m[stats::complete.cases(m), , drop = FALSE]
  neg <- any(m < 0)
  ints <- mean(vapply(m, function(x) all(x == round(x), na.rm = TRUE), logical(1)))
  zeros <- mean(as.matrix(m) == 0, na.rm = TRUE)
  sds <- vapply(m, stats::sd, numeric(1)); sds <- sds[sds > 0]
  scale_gap <- if (length(sds) > 1) max(sds) / min(sds) else 1
  skew <- if (has_pkg("moments"))
    mean(abs(vapply(m, function(x) moments::skewness(stats::na.omit(x)), numeric(1))), na.rm = TRUE) else NA
  community <- !neg && ints > 0.7 && zeros > 0.2 && ncol(m) >= 3
  msgs <- list()
  if (community)
    msgs <- c(msgs, list(list(txt = "These look like <b>community / abundance counts</b> (non-negative, many zeros). Before PCA/RDA use the <b>Hellinger</b> transform; for NMDS/PERMANOVA a Bray-Curtis distance already handles scale, so raw or <b>total</b> (relative) values are fine.", sev = "info")))
  if (!is.na(skew) && skew > 1 && !neg)
    msgs <- c(msgs, list(list(txt = sprintf("Mean absolute skewness is %.1f (right-skewed). A <b>log (x+1)</b> or <b>square-root</b> transform will pull in the tail and stabilise variance.", skew), sev = "warn")))
  if (scale_gap > 5 && !community)
    msgs <- c(msgs, list(list(txt = sprintf("Your variables differ ~%.0f-fold in spread. For PCA, clustering, or distance-based methods, <b>z-score</b> them so large-range variables don't dominate.", scale_gap), sev = "warn")))
  if (!length(msgs))
    msgs <- list(list(txt = "No transformation looks necessary: variables are on comparable scales and not strongly skewed. Standardize only if a specific method requires it.", sev = "ok"))
  msgs
}

# Diverging colorblind-safe fill for heatmaps / z-scores (blue low, orange high).
heat_fill <- function(limit, name = "z-score")
  scale_fill_gradient2(low = "#0072B2", mid = "white", high = "#D55E00",
                       midpoint = 0, limits = c(-limit, limit), name = name,
                       oob = scales::squish)

# Per-feature differential expression between two groups.
de_table <- function(X, group, method = c("t", "wilcox")) {
  method <- match.arg(method); lv <- levels(group)
  g1 <- group == lv[1]; g2 <- group == lv[2]
  rows <- lapply(colnames(X), function(f) {
    x <- X[g1, f]; y <- X[g2, f]; x <- x[is.finite(x)]; y <- y[is.finite(y)]
    if (length(x) < 2 || length(y) < 2) return(NULL)
    p <- tryCatch(if (method == "t") stats::t.test(x, y)$p.value
                  else stats::wilcox.test(x, y, exact = FALSE)$p.value, error = function(e) NA_real_)
    m1 <- mean(x); m2 <- mean(y)
    data.frame(Feature = f, mean_1 = m1, mean_2 = m2,
               log2FC = if (m1 > 0 && m2 > 0) log2(m2 / m1) else NA_real_,
               meanDiff = m2 - m1, p = p)
  })
  d <- do.call(rbind, rows); d$p_adj <- stats::p.adjust(d$p, "BH"); d
}

# Clustered heatmap with an optional sample dendrogram and group annotation bar.
heatmap_figure <- function(X, group = NULL, scale = c("feature", "sample", "none"),
                           cluster_samples = TRUE, cluster_features = TRUE,
                           show_values = FALSE, row_dendro = TRUE,
                           font = PUB_FONT, base_size = 13, tags = TRUE) {
  scale <- match.arg(scale)
  X <- as.matrix(X); storage.mode(X) <- "double"
  Xs <- switch(scale, feature = scale(X), sample = t(scale(t(X))), none = X)
  Xs[!is.finite(Xs)] <- 0
  hc_s <- if (cluster_samples && nrow(Xs) > 2) stats::hclust(stats::dist(Xs)) else NULL
  hc_f <- if (cluster_features && ncol(Xs) > 2) stats::hclust(stats::dist(t(Xs))) else NULL
  s_ord <- if (!is.null(hc_s)) hc_s$order else seq_len(nrow(Xs))
  f_ord <- if (!is.null(hc_f)) hc_f$order else seq_len(ncol(Xs))
  snames <- rownames(X) %||% as.character(seq_len(nrow(X)))
  fnames <- colnames(X)
  H <- Xs[s_ord, f_ord, drop = FALSE]
  n <- length(s_ord); p <- length(f_ord); lim <- max(abs(H), 1e-9)
  df <- data.frame(xi = rep(seq_len(n), times = p), yi = rep(seq_len(p), each = n),
                   value = as.vector(H))
  show_s <- n <= 45; show_f <- p <= 60
  hm <- ggplot(df, aes(xi, yi, fill = value)) +
    geom_tile(colour = "grey85", linewidth = pt_to_mm(0.2)) +
    heat_fill(lim) +
    scale_x_continuous(expand = c(0, 0), breaks = if (show_s) seq_len(n) else NULL,
                       labels = if (show_s) snames[s_ord] else NULL,
                       sec.axis = dup_axis(name = NULL, labels = NULL)) +
    scale_y_continuous(expand = c(0, 0), breaks = if (show_f) seq_len(p) else NULL,
                       labels = if (show_f) fnames[f_ord] else NULL,
                       sec.axis = dup_axis(name = NULL, labels = NULL)) +
    labs(x = NULL, y = NULL, tag = if (tags) "A" else NULL) +
    theme_publication(base_size = base_size, base_family = font) +
    (if (tags) theme_panel_tag(font) else theme(plot.tag = element_blank())) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
          axis.text.y = element_text(size = 8))
  if (show_values) hm <- hm + geom_text(aes(label = sprintf("%.1f", value)),
                                        family = font, size = 2.4)
  if (!has_pkg("patchwork") || !has_pkg("ggdendro") || is.null(hc_s)) return(hm)
  seg <- ggdendro::dendro_data(hc_s, type = "rectangle")$segments
  top <- ggplot(seg) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend), colour = "black", linewidth = pt_to_mm(0.8)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0.5, n + 0.5)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.03))) +
    labs(x = NULL, y = NULL) + theme_void()
  # Optional feature (row) dendrogram on the left, aligned to heatmap rows.
  want_left <- row_dendro && !is.null(hc_f) && show_f
  left <- NULL
  if (want_left) {
    segf <- ggdendro::dendro_data(hc_f, type = "rectangle")$segments
    left <- ggplot(segf) +
      geom_segment(aes(x = y, y = x, xend = yend, yend = xend), colour = "black", linewidth = pt_to_mm(0.8)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0.5, p + 0.5)) +
      scale_x_reverse(expand = expansion(mult = c(0.03, 0))) +
      labs(x = NULL, y = NULL) + theme_void()
  }
  abar <- NULL
  if (!is.null(group)) {
    ann <- data.frame(xi = seq_len(n), grp = factor(group[s_ord]))
    abar <- ggplot(ann, aes(xi, 1, fill = grp)) +
      geom_tile() + pub_colour("Group") +
      scale_x_continuous(expand = c(0, 0), limits = c(0.5, n + 0.5)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(x = NULL, y = NULL) + theme_void() +
      theme(legend.position = "right", legend.text = element_text(family = font),
            legend.title = element_text(family = font))
  }
  lw <- 0.14  # left dendrogram column width fraction
  if (want_left) {
    if (!is.null(abar))
      patchwork::wrap_plots(top, abar, left, hm, design = "#A\n#B\nCD",
        widths = c(lw, 1 - lw), heights = c(0.16, 0.05, 0.79))
    else
      patchwork::wrap_plots(top, left, hm, design = "#A\nBC",
        widths = c(lw, 1 - lw), heights = c(0.18, 0.82))
  } else if (!is.null(abar)) {
    patchwork::wrap_plots(top, abar, hm, ncol = 1, heights = c(0.16, 0.05, 0.79))
  } else {
    patchwork::wrap_plots(top, hm, ncol = 1, heights = c(0.18, 0.82))
  }
}

# Four-parameter log-logistic dose-response fit (drc LL.4), optionally per group.
# Returns the model, a prediction grid, and a per-curve parameter/EC50 table.
fit_dose <- function(dose, resp, grp = NULL, logx = TRUE, npts = 200) {
  df <- data.frame(dose = dose, resp = resp)
  if (!is.null(grp)) df$grp <- factor(grp)
  df <- df[stats::complete.cases(df), ]
  m <- if (is.null(grp)) drc::drm(resp ~ dose, data = df, fct = drc::LL.4())
       else drc::drm(resp ~ dose, curveid = grp, data = df, fct = drc::LL.4())
  co <- coef(m)
  ed <- drc::ED(m, 50, interval = "delta", display = FALSE)
  pos <- df$dose[df$dose > 0]
  xs <- if (logx && length(pos)) exp(seq(log(min(pos)), log(max(pos)), length.out = npts))
        else seq(min(df$dose), max(df$dose), length.out = npts)
  if (is.null(grp)) {
    pred <- data.frame(dose = xs, fit = predict(m, newdata = data.frame(dose = xs)), grp = "All")
    partab <- data.frame(Group = "All", EC50 = co[["e:(Intercept)"]],
      EC50_low = ed[1, 3], EC50_high = ed[1, 4], Hill = -co[["b:(Intercept)"]],
      Lower = co[["c:(Intercept)"]], Upper = co[["d:(Intercept)"]])
  } else {
    lv <- levels(df$grp); rn <- rownames(ed)
    pred <- do.call(rbind, lapply(lv, function(g) data.frame(dose = xs,
      fit = predict(m, newdata = data.frame(dose = xs, grp = factor(g, levels = lv))), grp = g)))
    partab <- do.call(rbind, lapply(lv, function(g) {
      er <- ed[grep(paste0("e:", g, ":50"), rn, fixed = TRUE)[1], ]
      data.frame(Group = g, EC50 = co[[paste0("e:", g)]], EC50_low = er[3], EC50_high = er[4],
        Hill = -co[[paste0("b:", g)]], Lower = co[[paste0("c:", g)]], Upper = co[[paste0("d:", g)]]) }))
  }
  list(model = m, pred = pred, partab = partab, data = df) }

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

sidebar_help <- function(...) helpText(style = "font-size:12px;", ...)

ui <- navbarPage(
  title = "Statistics & Figure Producer",
  id = "nav",
  header = tags$head(tags$style(HTML("
    body, .form-control, .selectize-input, .shiny-input-container { font-family: Georgia, serif; }
    .well { background:#fafafa; }
    h4 { border-bottom:2px solid #0072B2; padding-bottom:4px; margin-top:4px; }
    .helptip { color:#0072B2; font-weight:bold; cursor:help; }
    .navbar-brand { font-weight:bold; }
  "))),

  # --- Data & health ------------------------------------------------------
  tabPanel("Data",
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput("file", "Upload CSV / TSV / Excel",
                  accept = c(".csv", ".txt", ".tsv", ".xls", ".xlsx")),
        checkboxInput("header", "First row is a header", TRUE),
        radioButtons("sep", "Delimiter", c(Comma = ",", Tab = "\t", Semicolon = ";"), inline = TRUE),
        tags$hr(),
        strong("...or paste data"),
        textAreaInput("paste_text", NULL, rows = 5,
                      placeholder = "Paste tab-, comma-, or semicolon-separated data with a header row (e.g. straight from Excel)."),
        actionButton("paste_go", "Use pasted data", class = "btn-success btn-block"),
        tags$hr(),
        strong("No data yet? Try a worked example:"),
        actionButton("demo_exp", "Experiment / clinical demo", class = "btn-primary btn-block"),
        actionButton("demo_comm", "Ecology community demo", class = "btn-primary btn-block"),
        sidebar_help("Numeric columns feed statistics and figures; text columns and ",
                     "columns with few distinct values act as grouping variables.")
      ),
      mainPanel(width = 9,
        h4("Data health check"),
        uiOutput("health_notes"),
        DTOutput("health_tbl"),
        tags$hr(),
        h4("Preview"),
        DTOutput("preview")
      )
    )
  ),

  # --- Explore ------------------------------------------------------------
  navbarMenu("Explore",
    tabPanel("Summary Statistics",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("sum_vars_ui"), uiOutput("sum_group_ui")),
        mainPanel(width = 9,
          h4("Descriptive statistics"), DTOutput("sum_tbl"),
          uiOutput("sum_notes"))
      )
    ),
    tabPanel("Distribution & Normality",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("dist_var", "Variable", choices = NULL),
          uiOutput("dist_group_ui"),
          radioButtons("dist_type", "Figure", c("Histogram + density" = "hist",
                                                "Q-Q (normal check)" = "qq")),
          downloadButton("dl_dist", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Normality assessment"), uiOutput("norm_notes"), DTOutput("norm_tbl"),
          tags$hr(), plotOutput("dist_plot", height = "460px"))
      )
    ),
    tabPanel("Transform / Standardize",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("tr_vars_ui"),
          selectInput("tr_method", "Transformation",
            c("Z-score (center & scale)" = "z", "Log (x + 1)" = "log1p",
              "Square root" = "sqrt", "Range 0-1" = "range",
              "Hellinger (community)" = "hellinger", "Total / relative" = "total",
              "Presence-absence" = "pa")),
          radioButtons("tr_out", "Write result as",
                       c("New columns (keep originals)" = "add", "Replace originals" = "replace"), inline = FALSE),
          actionButton("tr_apply", "Apply transformation", class = "btn-primary btn-block"),
          sidebar_help("Transformed columns become available to every other tab.")),
        mainPanel(width = 9,
          h4("Recommendation for your data"), uiOutput("tr_reco"),
          h4("Preview"), DTOutput("tr_preview"))
      )
    ),
    tabPanel("PCA Explorer",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("pca_vars_ui"),
          selectInput("pca_group", "Colour by (optional)", choices = c("None" = "")),
          selectInput("pca_shape", "Shape by (optional, e.g. site)", choices = c("None" = "")),
          checkboxInput("pca_scale", "Scale variables (recommended)", TRUE),
          radioButtons("pca_fig", "Figure", c("Biplot" = "biplot", "Scree plot" = "scree")),
          sliderInput("pca_arrows", "Loading arrows to show", min = 0, max = 20, value = 8),
          downloadButton("dl_pca", "Download figure")),
        mainPanel(width = 9,
          uiOutput("pca_notes"), plotOutput("pca_plot", height = "520px"),
          tags$hr(), h4("Variable loadings"), DTOutput("pca_loadings"))
      )
    )
  ),

  # --- Choose a test ------------------------------------------------------
  tabPanel("Choose a Test",
    sidebarLayout(
      sidebarPanel(width = 4,
        strong("A. Describe your question"),
        selectInput("guide_response", "Outcome you measured",
                    c("A number (continuous)" = "cont", "A count" = "count",
                      "A category / yes-no" = "cat", "Time until an event" = "time",
                      "A whole community/matrix" = "matrix")),
        selectInput("guide_predictor", "What you are comparing / relating it to",
                    c("Nothing — one sample" = "none",
                      "2 groups" = "g2", "3+ groups" = "g3",
                      "Another number" = "num", "Several predictors" = "multi")),
        radioButtons("guide_paired", "Are measurements paired or repeated?",
                     c("No, independent" = "ind", "Yes, paired/repeated" = "paired"), inline = TRUE),
        tags$hr(),
        strong("B. Or let the data decide"),
        selectInput("guide_y", "Numeric outcome column", choices = NULL),
        selectInput("guide_g", "Grouping column", choices = NULL),
        sidebar_help("Section B checks your actual data's normality, variances and ",
                     "sample sizes and recommends a specific test.")),
      mainPanel(width = 8,
        h4("Recommended approach"), uiOutput("guide_out"),
        tags$hr(),
        h4("Live check on your data"), uiOutput("guide_live"))
    )
  ),

  # --- Compare groups -----------------------------------------------------
  navbarMenu("Compare Groups",
    tabPanel("Two Groups",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("gc_response", "Response (numeric)", choices = NULL),
          selectInput("gc_group", "Grouping factor (2 levels)", choices = NULL),
          radioButtons("gc_design", "Design", c("Independent" = "ind", "Paired" = "paired"), inline = TRUE),
          selectInput("gc_test", "Test",
            c("Auto (recommend for me)" = "auto",
              "Welch t-test" = "t", "Student t-test" = "tstud",
              "Mann-Whitney U" = "mw")),
          checkboxInput("gc_signif", "Significance bracket on figure", TRUE),
          downloadButton("dl_box", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Assumption check"), uiOutput("gc_assump"),
          h4("Result"), uiOutput("gc_interp"), verbatimTextOutput("gc_result"),
          tags$hr(), plotOutput("gc_plot", height = "500px"))
      )
    ),
    tabPanel("3+ Groups",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("av_response", "Response (numeric)", choices = NULL),
          selectInput("av_group", "Grouping factor", choices = NULL),
          selectInput("av_test", "Test",
            c("Auto (recommend for me)" = "auto", "One-way ANOVA" = "anova",
              "Welch ANOVA" = "welch", "Kruskal-Wallis" = "kw")),
          checkboxInput("av_posthoc", "Pairwise post-hoc", TRUE),
          checkboxInput("av_signif", "Significance brackets on figure", TRUE),
          downloadButton("dl_avbox", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Assumption check"), uiOutput("av_assump"),
          h4("Result"), uiOutput("av_interp"), verbatimTextOutput("av_result"),
          h4("Post-hoc pairwise"), DTOutput("av_posthoc_tbl"),
          tags$hr(), plotOutput("av_plot", height = "520px"))
      )
    ),
    tabPanel("Two-Way / ANCOVA",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("tw_response", "Response (numeric)", choices = NULL),
          selectInput("tw_f1", "Factor 1", choices = NULL),
          selectInput("tw_f2", "Factor 2 / covariate", choices = NULL),
          checkboxInput("tw_interaction", "Include interaction", TRUE),
          downloadButton("dl_tw", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Model (Type II tests)"), uiOutput("tw_interp"), verbatimTextOutput("tw_result"),
          tags$hr(), plotOutput("tw_plot", height = "500px"))
      )
    )
  ),

  # --- Categorical & proportions -----------------------------------------
  navbarMenu("Categorical",
    tabPanel("Contingency Tables",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("ct_row", "Row variable", choices = NULL),
          selectInput("ct_col", "Column variable", choices = NULL),
          radioButtons("ct_design", "Design",
                       c("Independent" = "ind", "Paired (McNemar)" = "paired"), inline = TRUE),
          downloadButton("dl_ct", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Observed counts"), DTOutput("ct_obs"),
          h4("Result"), uiOutput("ct_interp"), verbatimTextOutput("ct_result"),
          tags$hr(), plotOutput("ct_plot", height = "460px"))
      )
    ),
    tabPanel("Risk & Odds (2x2)",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("rr_exp", "Exposure / treatment (2 levels)", choices = NULL),
          selectInput("rr_out", "Outcome (2 levels)", choices = NULL),
          sidebar_help("The first level of each variable is treated as the reference. ",
                       "Effect sizes report the second level relative to the first.")),
        mainPanel(width = 9,
          h4("2x2 table"), DTOutput("rr_tbl"),
          h4("Association measures"), uiOutput("rr_interp"), DTOutput("rr_measures"))
      )
    ),
    tabPanel("Diagnostic / ROC",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("dx_score", "Predictor / test score (numeric)", choices = NULL),
          selectInput("dx_truth", "True status (2 levels)", choices = NULL),
          sidebar_help("Higher scores are assumed to indicate the positive (second) ",
                       "level; the tool auto-corrects orientation if needed."),
          downloadButton("dl_roc", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Discrimination"), uiOutput("dx_interp"),
          h4("At the optimal (Youden) cutoff"), DTOutput("dx_tbl"),
          tags$hr(), plotOutput("dx_plot", height = "460px"))
      )
    )
  ),

  # --- Regression ---------------------------------------------------------
  navbarMenu("Regression",
    tabPanel("Dose-Response",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("dr_dose", "Dose / concentration", choices = NULL),
          selectInput("dr_resp", "Response", choices = NULL),
          selectInput("dr_group", "Separate curves by (optional)", choices = c("None" = "")),
          checkboxInput("dr_logx", "Log-scale dose axis", TRUE),
          downloadButton("dl_dr", "Download figure")),
        mainPanel(width = 9,
          h4("Dose-response (4-parameter logistic)"), uiOutput("dr_notes"),
          plotOutput("dr_plot", height = "500px"),
          tags$hr(), h4("EC50 and curve parameters"), DTOutput("dr_tbl"))
      )
    ),
    tabPanel("Linear",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("reg_y", "Response (numeric)", choices = NULL),
          uiOutput("reg_x_ui"),
          downloadButton("dl_reg", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Model"), uiOutput("reg_interp"), verbatimTextOutput("reg_result"),
          h4("Coefficients"), DTOutput("reg_coef"),
          tags$hr(), plotOutput("reg_plot", height = "460px"))
      )
    ),
    tabPanel("Logistic",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("logit_y", "Binary outcome (2 levels)", choices = NULL),
          uiOutput("logit_x_ui"),
          downloadButton("dl_logit", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Model"), uiOutput("logit_interp"), verbatimTextOutput("logit_result"),
          h4("Odds ratios"), DTOutput("logit_or"),
          tags$hr(), plotOutput("logit_plot", height = "460px"))
      )
    ),
    tabPanel("Count (Poisson)",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("pois_y", "Count response", choices = NULL),
          uiOutput("pois_x_ui")),
        mainPanel(width = 9,
          h4("Model"), uiOutput("pois_interp"), verbatimTextOutput("pois_result"),
          h4("Rate ratios"), DTOutput("pois_rr"))
      )
    )
  ),

  # --- Correlation --------------------------------------------------------
  tabPanel("Correlation",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("cor_vars_ui"),
        selectInput("cor_method", "Method",
                    c("Pearson (linear)" = "pearson", "Spearman (rank)" = "spearman",
                      "Kendall (rank)" = "kendall")),
        selectInput("cor_partial", "Partial: control for", choices = c("None" = "")),
        downloadButton("dl_cor", "Download figure (300 dpi)")),
      mainPanel(width = 9,
        uiOutput("cor_notes"),
        h4("Correlation matrix"), DTOutput("cor_tbl"),
        h4("p-values"), DTOutput("cor_p_tbl"),
        tags$hr(), plotOutput("cor_plot", height = "500px"))
    )
  ),

  # --- Survival -----------------------------------------------------------
  tabPanel("Survival",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("surv_time", "Time-to-event (numeric)", choices = NULL),
        selectInput("surv_status", "Event indicator (1 = event, 0 = censored)", choices = NULL),
        selectInput("surv_group", "Grouping factor (optional)", choices = c("None" = "")),
        downloadButton("dl_surv", "Download figure (300 dpi)")),
      mainPanel(width = 9,
        h4("Kaplan-Meier + tests"), uiOutput("surv_interp"), verbatimTextOutput("surv_result"),
        tags$hr(), plotOutput("surv_plot", height = "500px"))
    )
  ),

  # --- Ecology ------------------------------------------------------------
  navbarMenu("Ecology",
    tabPanel("Diversity Indices",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("div_vars_ui"),
          selectInput("div_group", "Compare groups (optional)", choices = c("None" = "")),
          selectInput("div_index", "Index for figure",
                      c("Shannon H'" = "Shannon", "Simpson 1-D" = "Simpson",
                        "Richness S" = "Richness", "Pielou evenness J" = "Evenness")),
          downloadButton("dl_div", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Per-sample diversity"), uiOutput("div_notes"), DTOutput("div_tbl"),
          tags$hr(), plotOutput("div_plot", height = "500px"))
      )
    ),
    tabPanel("Community Comparison",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("beta_vars_ui"),
          selectInput("beta_group", "Grouping factor", choices = NULL),
          selectInput("beta_test", "Test",
                      c("PERMANOVA (adonis2)" = "permanova", "ANOSIM" = "anosim")),
          selectInput("beta_dist", "Distance", c("bray", "jaccard", "euclidean", "gower")),
          uiOutput("mantel_ui")),
        mainPanel(width = 9,
          h4("Multivariate group test"), uiOutput("beta_interp"), verbatimTextOutput("beta_result"),
          tags$hr(), h4("Mantel test (matrix correlation)"), verbatimTextOutput("mantel_result"))
      )
    ),
    tabPanel("Ordination",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput("ord_method", "Method",
            c("PCA" = "pca", "PCoA (metric MDS)" = "pcoa", "Correspondence Analysis" = "ca",
              "Detrended CA" = "dca", "NMDS" = "nmds", "Redundancy Analysis" = "rda")),
          uiOutput("ord_vars_ui"), uiOutput("ord_group_ui"),
          selectInput("ord_shape", "Shape by (optional, e.g. site)", choices = c("None" = "")),
          uiOutput("ord_constrain_ui"),
          selectInput("nmds_dist", "NMDS distance", c("bray", "euclidean", "jaccard", "gower")),
          checkboxInput("ord_scale", "Scale variables (PCA)", TRUE),
          downloadButton("dl_ord", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Ordination summary"), uiOutput("ord_notes"), verbatimTextOutput("ord_summary"),
          tags$hr(), plotOutput("ord_plot", height = "540px"))
      )
    )
  ),

  # --- Bioinformatics -----------------------------------------------------
  navbarMenu("Bioinformatics",
    tabPanel("Heatmap",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("hm_vars_ui"),
          selectInput("hm_group", "Annotation / group (optional)", choices = c("None" = "")),
          selectInput("hm_scale", "Scaling",
            c("Z-score each feature (recommended)" = "feature",
              "Z-score each sample" = "sample", "None (raw values)" = "none")),
          checkboxInput("hm_cluster_s", "Cluster samples (columns)", TRUE),
          checkboxInput("hm_cluster_f", "Cluster features (rows)", TRUE),
          checkboxInput("hm_row_dendro", "Show feature dendrogram (left)", TRUE),
          checkboxInput("hm_values", "Print values in cells", FALSE),
          downloadButton("dl_hm", "Download figure")),
        mainPanel(width = 9,
          uiOutput("hm_notes"), plotOutput("hm_plot", height = "600px"))
      )
    ),
    tabPanel("Differential Expression",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("de_vars_ui"),
          selectInput("de_group", "Group (2 levels)", choices = NULL),
          selectInput("de_method", "Per-feature test", c("t-test" = "t", "Mann-Whitney" = "wilcox")),
          numericInput("de_alpha", "Adjusted p threshold", 0.05, min = 0.001, max = 0.5, step = 0.01),
          numericInput("de_fc", "Fold-change threshold (log2)", 1, min = 0, step = 0.25),
          numericInput("de_label", "Label top N features", 8, min = 0, max = 40),
          radioButtons("de_plottype", "Plot type", c("Volcano" = "volcano", "MA plot" = "ma")),
          downloadButton("dl_volcano", "Download figure"),
          tags$hr(),
          actionButton("de_to_heatmap", "Send significant features to Heatmap →",
                       class = "btn-success btn-block")),
        mainPanel(width = 9,
          h4("Differential expression"), uiOutput("de_notes"), plotOutput("de_plot", height = "520px"),
          tags$hr(), h4("Results (BH-adjusted)"), DTOutput("de_tbl"),
          downloadButton("dl_de_csv", "Download results (CSV)"))
      )
    ),
    tabPanel("Sample Clustering",
      sidebarLayout(
        sidebarPanel(width = 3,
          uiOutput("cl_vars_ui"),
          selectInput("cl_group", "Colour leaves by (optional)", choices = c("None" = "")),
          selectInput("cl_dist", "Distance", c("euclidean", "manhattan", "maximum")),
          selectInput("cl_link", "Linkage", c("complete", "average", "ward.D2", "single")),
          downloadButton("dl_cl", "Download figure")),
        mainPanel(width = 9,
          uiOutput("cl_notes"), plotOutput("cl_plot", height = "520px"))
      )
    )
  ),

  # --- Compare methods ----------------------------------------------------
  tabPanel("Compare Methods",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("cmp_type", "What to compare",
          c("Ordinations: PCA vs PCoA vs NMDS" = "ord",
            "Group test: parametric vs non-parametric" = "grp",
            "Correlation: Pearson vs Spearman" = "cor")),
        conditionalPanel("input.cmp_type == 'ord'",
          uiOutput("cmp_ord_vars_ui"),
          selectInput("cmp_ord_group", "Grouping (colour)", choices = c("None" = "")),
          selectInput("cmp_ord_shape", "Shape by (optional, e.g. site)", choices = c("None" = "")),
          selectInput("cmp_dist", "Distance (PCoA / NMDS)", c("bray", "euclidean", "jaccard", "gower"))),
        conditionalPanel("input.cmp_type == 'grp'",
          selectInput("cmp_y", "Response (numeric)", choices = NULL),
          selectInput("cmp_g", "Grouping factor", choices = NULL)),
        conditionalPanel("input.cmp_type == 'cor'",
          selectInput("cmp_x1", "Variable 1", choices = NULL),
          selectInput("cmp_x2", "Variable 2", choices = NULL)),
        downloadButton("dl_cmp", "Download figure (300 dpi)")),
      mainPanel(width = 9,
        h4("How the methods compare"), uiOutput("cmp_notes"),
        DTOutput("cmp_tbl"),
        tags$hr(), plotOutput("cmp_plot", height = "520px"))
    )
  ),

  # --- Power --------------------------------------------------------------
  tabPanel("Power",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("pwr_test", "Design",
                    c("Two-group t-test" = "t", "One-way ANOVA" = "anova",
                      "Two proportions" = "prop", "Correlation" = "cor")),
        numericInput("pwr_effect", "Expected effect size", 0.5, step = 0.05),
        numericInput("pwr_n", "Sample size per group (blank to solve)", NA),
        numericInput("pwr_power", "Power (blank to solve)", 0.8, min = 0, max = 1, step = 0.05),
        numericInput("pwr_alpha", "Alpha", 0.05, min = 0.001, max = 0.2, step = 0.01),
        numericInput("pwr_k", "Groups (ANOVA only)", 3, min = 2)),
      mainPanel(width = 9,
        h4("Power / sample size"), uiOutput("pwr_interp"), verbatimTextOutput("pwr_result"),
        tags$hr(), plotOutput("pwr_plot", height = "420px"))
    )
  ),

  # --- Export & style -----------------------------------------------------
  tabPanel("Export & Style",
    fluidRow(
      column(4,
        h4("Appearance"),
        selectInput("fig_font", "Font",
          c("Sans-serif (Arial / Helvetica)" = "sans", "Serif (Times)" = "serif",
            "Helvetica" = "Helvetica", "Palatino" = "Palatino",
            "Times New Roman" = "Times New Roman", "Georgia" = "Georgia"),
          selected = "sans"),
        sliderInput("fig_fontsize", "Base text size", min = 8, max = 20, value = 13, step = 1),
        checkboxInput("fig_tags", "Show panel labels (A, B, ...)", TRUE),
        note("Changes preview live on every figure in the app.", "info")),
      column(4,
        h4("Export"),
        selectInput("fig_format", "File format",
          c("PNG (raster)" = "png", "PDF (vector, best for journals)" = "pdf",
            "SVG (vector, editable)" = "svg", "TIFF (raster, journals)" = "tiff")),
        numericInput("fig_dpi", "Resolution (dpi, raster only)", 300, min = 72, max = 1200, step = 50),
        radioButtons("fig_size", "Figure size",
          c("Auto (per figure)" = "auto", "Single column (3.5 in)" = "single",
            "Double column (7 in)" = "double", "Custom" = "custom")),
        conditionalPanel("input.fig_size == 'custom'",
          numericInput("fig_w", "Width (in)", 6.5, min = 1, max = 20, step = 0.5),
          numericInput("fig_h", "Height (in)", 5, min = 1, max = 20, step = 0.5))),
      column(4,
        h4("Frictionless publication figures"),
        note("1. Load or paste data, then open the analysis tab you need.", "ok"),
        note("2. Every figure already follows publication conventions: Wong colorblind-safe palette, clean spines with inward ticks, tick-locked axes, lettered panels.", "ok"),
        note("3. Set the font, size, format, and dimensions here once — they apply to every <b>Download figure</b> button. File names reflect the analysis so nothing is overwritten.", "ok"),
        note("Vector <b>PDF</b>/<b>SVG</b> stay sharp at any size and open in Illustrator/Inkscape. <b>Single/Double column</b> presets match common journal widths while preserving each figure's aspect ratio.", "info"),
        note("Sans-serif and Serif always render on any system. Named fonts (Helvetica, Palatino, ...) need that font installed, otherwise the graphics device substitutes the closest match.", "warn"))
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(data = NULL)

  # Format-aware figure download. Honours the global Export settings so any
  # figure can be saved as raster (PNG/TIFF) or vector (PDF/SVG) at a chosen dpi.
  dl_handler <- function(stem, plotFun, width = 6.5, height = 5) downloadHandler(
    filename = function() {
      fmt <- tolower(input$fig_format %||% "png")
      paste0(if (is.function(stem)) stem() else stem, ".", fmt)
    },
    content = function(file) {
      fmt <- tolower(input$fig_format %||% "png")
      w0 <- if (is.function(width)) width() else width
      h0 <- if (is.function(height)) height() else height
      dims <- switch(input$fig_size %||% "auto",
        single = c(3.5, h0 * 3.5 / w0),
        double = c(7,   h0 * 7   / w0),
        custom = c(input$fig_w %||% w0, input$fig_h %||% h0),
        c(w0, h0))
      ggplot2::ggsave(file, plotFun(), width = dims[1], height = dims[2], units = "in",
                      dpi = as.numeric(input$fig_dpi %||% 300), bg = "white", device = fmt)
    })

  # Live figure styling controlled from the Export tab; every plot reads these
  # so changes redraw immediately.
  pfont  <- reactive(input$fig_font %||% "Georgia")
  psize  <- reactive(as.numeric(input$fig_fontsize %||% 13))
  ptags  <- reactive(isTRUE(input$fig_tags %||% TRUE))
  thm    <- reactive(theme_publication(base_size = psize(), base_family = pfont()))
  tagthm <- reactive(if (ptags()) theme_panel_tag(pfont()) else theme(plot.tag = element_blank()))

  observeEvent(input$file, {
    req(input$file)
    ext <- tolower(tools::file_ext(input$file$name))
    df <- tryCatch({
      if (ext %in% c("xls", "xlsx") && has_pkg("readxl"))
        as.data.frame(readxl::read_excel(input$file$datapath))
      else
        utils::read.table(input$file$datapath, header = input$header, sep = input$sep,
                          stringsAsFactors = FALSE, check.names = TRUE, fill = TRUE, quote = "\"")
    }, error = function(e) { showNotification(paste("Read error:", conditionMessage(e)),
                                              type = "error"); NULL })
    rv$data <- df
  })

  observeEvent(input$paste_go, {
    req(nzchar(input$paste_text %||% ""))
    txt <- input$paste_text
    sep <- if (grepl("\t", txt)) "\t" else if (grepl(";", txt)) ";" else ","
    df <- tryCatch(utils::read.table(text = txt, header = TRUE, sep = sep,
                                     stringsAsFactors = FALSE, check.names = TRUE, fill = TRUE),
                   error = function(e) { showNotification(paste("Parse error:", conditionMessage(e)),
                                                          type = "error"); NULL })
    if (!is.null(df) && ncol(df) >= 1) { rv$data <- df
      showNotification(sprintf("Loaded %d rows x %d columns from pasted text.", nrow(df), ncol(df)), type = "message") }
  })

  observeEvent(input$demo_exp, {
    set.seed(1)
    n <- 40
    conc <- 10^stats::runif(2 * n, -2, 2)                 # 0.01 to 100 units
    ec50 <- rep(c(1, 5), each = n)                        # potency differs by group
    viab <- 100 / (1 + (conc / ec50)^1.3) + stats::rnorm(2 * n, 0, 5)
    rv$data <- data.frame(
      Group         = rep(c("Control", "Treatment"), each = n),
      Sex           = sample(c("F", "M"), 2 * n, TRUE),
      Site          = sample(c("North", "South", "East"), 2 * n, TRUE),
      Response      = c(rnorm(n, 10, 2), rnorm(n, 12.5, 2.2)),
      Biomarker     = c(rlnorm(n, 1.4, 0.4), rlnorm(n, 1.7, 0.4)),
      Dose          = rep(c(0, 5, 10, 20), length.out = 2 * n),
      Concentration = round(conc, 3),
      Viability     = round(viab, 1),
      Improved      = rbinom(2 * n, 1, rep(c(0.35, 0.65), each = n)),
      SurvTime      = round(c(rexp(n, 0.10), rexp(n, 0.06)), 1),
      Event         = rbinom(2 * n, 1, 0.75),
      Counts        = rpois(2 * n, rep(c(3, 6), each = n))
    )
  })

  observeEvent(input$demo_comm, {
    set.seed(7)
    n <- 30
    grp <- rep(c("Disturbed", "Reference"), each = n / 2)
    rv$data <- data.frame(
      Site = paste0("S", seq_len(n)),
      Habitat = grp,
      pH = rnorm(n, 7, 0.4), Depth = runif(n, 1, 15),
      Sp_A = c(rpois(n / 2, 8), rpois(n / 2, 2)),
      Sp_B = c(rpois(n / 2, 1), rpois(n / 2, 7)),
      Sp_C = rpois(n, 4), Sp_D = rpois(n, 3),
      Sp_E = c(rpois(n / 2, 0), rpois(n / 2, 5)),
      Sp_F = rpois(n, 2), Sp_G = c(rpois(n / 2, 6), rpois(n / 2, 1)),
      Sp_H = rpois(n, 1)
    )
  })

  numeric_vars <- reactive({ df <- rv$data; req(df); names(df)[vapply(df, is.numeric, logical(1))] })
  factor_vars  <- reactive({ df <- rv$data; req(df)
    names(df)[vapply(df, function(x) !is.numeric(x) || length(unique(stats::na.omit(x))) <= 12, logical(1))] })
  binary_vars  <- reactive({ df <- rv$data; req(df)
    names(df)[vapply(df, function(x) length(unique(stats::na.omit(x))) == 2, logical(1))] })

  # Keep all selectors synced to the current dataset.
  observe({
    nv <- numeric_vars(); fv <- factor_vars(); bv <- binary_vars()
    upd <- function(id, ch, sel = NULL) updateSelectInput(session, id, choices = ch, selected = sel)
    upd("dist_var", nv, nv[1])
    upd("guide_y", nv, nv[1]); upd("guide_g", fv, fv[1])
    upd("gc_response", nv, nv[1]); upd("gc_group", intersect(fv, bv), intersect(fv, bv)[1])
    upd("av_response", nv, nv[1]); upd("av_group", fv, fv[1])
    upd("tw_response", nv, nv[1]); upd("tw_f1", fv, fv[1]); upd("tw_f2", c(fv, nv), fv[2] %||% fv[1])
    upd("ct_row", fv, fv[1]); upd("ct_col", fv, fv[2] %||% fv[1])
    upd("rr_exp", bv, bv[1]); upd("rr_out", bv, bv[2] %||% bv[1])
    upd("dx_score", nv, nv[1]); upd("dx_truth", bv, bv[1])
    upd("reg_y", nv, nv[1]); upd("logit_y", bv, bv[1]); upd("pois_y", nv, nv[1])
    upd("dr_dose", nv, nv[1]); upd("dr_resp", nv, nv[2] %||% nv[1]); upd("dr_group", c("None" = "", fv))
    upd("cor_partial", c("None" = "", nv))
    upd("surv_time", nv, nv[1]); upd("surv_status", c(bv, nv), bv[1] %||% nv[1])
    upd("surv_group", c("None" = "", fv))
    upd("div_group", c("None" = "", fv)); upd("beta_group", fv, fv[1])
    upd("ord_group", c("None" = "", fv))
    upd("cmp_ord_group", c("None" = "", fv)); upd("cmp_ord_shape", c("None" = "", fv))
    upd("ord_shape", c("None" = "", fv)); upd("pca_shape", c("None" = "", fv))
    upd("cmp_y", nv, nv[1]); upd("cmp_g", fv, fv[1])
    upd("cmp_x1", nv, nv[1]); upd("cmp_x2", nv, nv[2] %||% nv[1])
    upd("hm_group", c("None" = "", fv)); upd("de_group", fv, fv[1])
    upd("cl_group", c("None" = "", fv))
    upd("pca_group", c("None" = "", fv))
  })

  # --- Data health --------------------------------------------------------
  output$preview <- renderDT({ req(rv$data)
    datatable(head(rv$data, 50), options = list(scrollX = TRUE, pageLength = 8)) })

  output$health_tbl <- renderDT({ df <- rv$data; req(df)
    tab <- data.frame(
      Variable = names(df),
      Type = vapply(df, classify_variable, character(1)),
      N = vapply(df, function(x) sum(!is.na(x)), integer(1)),
      Missing = vapply(df, function(x) sprintf("%d (%.0f%%)", sum(is.na(x)),
                        100 * mean(is.na(x))), character(1)),
      Distinct = vapply(df, function(x) length(unique(stats::na.omit(x))), integer(1)),
      Outliers = vapply(df, function(x) {
        if (!is.numeric(x)) return("-")
        x <- stats::na.omit(x); q <- stats::quantile(x, c(.25, .75))
        io <- 1.5 * (q[2] - q[1])
        as.character(sum(x < q[1] - io | x > q[2] + io))
      }, character(1)),
      row.names = NULL, check.names = FALSE)
    datatable(tab, options = list(pageLength = 15), rownames = FALSE) })

  output$health_notes <- renderUI({
    df <- rv$data
    if (is.null(df)) return(note("Upload a file or load a demo dataset to begin. The table below then flags missing values, small samples, outliers and unbalanced groups.", "info"))
    msgs <- list()
    tot_miss <- mean(is.na(df))
    if (tot_miss > 0.1)
      msgs <- c(msgs, list(note(sprintf("About %.0f%% of cells are missing. Tests drop incomplete rows, which can shrink your sample — check whether the gaps are random.", 100 * tot_miss), "warn")))
    small <- names(df)[vapply(df, function(x) sum(!is.na(x)) < 15, logical(1))]
    if (length(small))
      msgs <- c(msgs, list(note(sprintf("Small samples (n &lt; 15) in: <b>%s</b>. Prefer non-parametric tests and interpret p-values cautiously.", paste(small, collapse = ", ")), "warn")))
    if (!length(msgs))
      msgs <- list(note("No major issues detected. Review the table for per-column detail, then head to <b>Choose a Test</b>.", "ok"))
    tagList(msgs)
  })

  # --- Summary ------------------------------------------------------------
  output$sum_vars_ui  <- renderUI(checkboxGroupInput("sum_vars", "Variables",
                                    choices = numeric_vars(), selected = numeric_vars()))
  output$sum_group_ui <- renderUI(selectInput("sum_group", "Group by (optional)",
                                    choices = c("None" = "", factor_vars())))
  output$dist_group_ui <- renderUI(selectInput("dist_group", "Group by (optional)",
                                    choices = c("None" = "", factor_vars())))

  output$sum_tbl <- renderDT({ req(input$sum_vars); df <- rv$data
    if (!is.null(input$sum_group) && nzchar(input$sum_group)) {
      parts <- lapply(split(df, df[[input$sum_group]]), function(s) {
        t <- summary_table(s, input$sum_vars)
        if (!is.null(t)) cbind(Group = s[[input$sum_group]][1], t) else NULL })
      tab <- do.call(rbind, parts)
    } else tab <- summary_table(df, input$sum_vars)
    datatable(tab, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE) })

  output$sum_notes <- renderUI(note("Mean vs. median far apart, or a large CV (SD/mean), signals skew — lean on the median/IQR and rank-based tests for those variables.", "info"))

  norm_data <- reactive({ req(input$sum_vars %||% input$dist_var)
    vars <- input$sum_vars %||% input$dist_var
    df <- rv$data
    num <- vars[vapply(df[vars], is.numeric, logical(1))]
    do.call(rbind, lapply(num, function(v) {
      x <- stats::na.omit(df[[v]])
      sh <- safe_shapiro(x)
      data.frame(Variable = v, N = length(x),
                 Shapiro_p = if (is.na(sh)) NA else signif(sh, 4),
                 Skewness = if (has_pkg("moments")) round(moments::skewness(x), 3) else NA,
                 Verdict = if (is.na(sh)) "n out of Shapiro range"
                           else if (sh >= 0.05) "consistent with normal" else "departs from normal",
                 check.names = FALSE) })) })

  output$norm_tbl <- renderDT(datatable(norm_data(), options = list(scrollX = TRUE), rownames = FALSE))
  output$norm_notes <- renderUI({
    nd <- norm_data(); req(nd)
    bad <- nd$Variable[!is.na(nd$Shapiro_p) & nd$Shapiro_p < 0.05]
    if (length(bad)) note(sprintf("These depart from normality: <b>%s</b>. For group comparisons on them, prefer Mann-Whitney or Kruskal-Wallis, or transform (e.g. log) first.", paste(bad, collapse = ", ")), "warn")
    else note("Shapiro-Wilk does not flag departures from normality here, so parametric tests (t-test, ANOVA) are reasonable. Always eyeball the Q-Q plot too.", "ok")
  })

  dist_plot <- reactive({ req(input$dist_var); df <- rv$data; v <- input$dist_var
    grp <- if (!is.null(input$dist_group) && nzchar(input$dist_group)) input$dist_group else NULL
    d <- df[!is.na(df[[v]]), , drop = FALSE]
    if (!is.null(grp)) d[[grp]] <- factor(d[[grp]])
    if (input$dist_type == "qq") {
      p <- ggplot(d, aes(sample = .data[[v]],
                         colour = if (!is.null(grp)) .data[[grp]] else NULL)) +
        stat_qq(size = 1.8, alpha = 0.8) + stat_qq_line(colour = "black", linewidth = pt_to_mm(1.2)) +
        labs(x = "Theoretical quantiles", y = "Sample quantiles", tag = "A") +
        pub_colour() + thm() + tagthm()
      if (is.null(grp)) p <- p + guides(colour = "none")
      return(p)
    }
    sx <- if (!is.null(grp)) split(d[[v]], d[[grp]]) else list(d[[v]])
    dens_max <- max(vapply(sx, function(z) if (length(z) > 1) max(stats::density(z)$y) else 0, numeric(1)))
    hist_max <- max(graphics::hist(d[[v]], breaks = 30, plot = FALSE)$density)
    p <- ggplot(d, aes(x = .data[[v]])) +
      geom_histogram(aes(y = after_stat(density), fill = if (!is.null(grp)) .data[[grp]] else NULL),
                     bins = 30, colour = "black", linewidth = pt_to_mm(0.6),
                     position = "identity", alpha = 0.5) +
      geom_density(aes(colour = if (!is.null(grp)) .data[[grp]] else NULL), linewidth = pt_to_mm(1.2)) +
      labs(x = v, y = "Density", tag = "A") + pub_colour() + four_side_x() +
      locked_axis("y", c(0, max(dens_max, hist_max)), n = 5, pad = 0.03) +
      thm() + tagthm()
    if (is.null(grp)) p <- p + guides(fill = "none", colour = "none")
    p })
  output$dist_plot <- renderPlot(dist_plot())
  output$dl_dist <- dl_handler(function() paste0("distribution_", input$dist_var), dist_plot)

  # --- Transform / standardize -------------------------------------------
  output$tr_vars_ui <- renderUI(checkboxGroupInput("tr_vars", "Columns to transform",
                                  choices = numeric_vars(), selected = numeric_vars()))
  tr_matrix <- reactive({ req(input$tr_vars)
    m <- rv$data[, input$tr_vars, drop = FALSE]; m[vapply(m, is.numeric, logical(1))] })
  output$tr_reco <- renderUI({ m <- tr_matrix(); req(ncol(m) >= 1)
    neg <- any(m < 0, na.rm = TRUE)
    warn <- if (input$tr_method %in% c("log1p", "sqrt", "hellinger", "total", "pa") && neg)
      note("Some selected values are negative, which is invalid for log/sqrt/community transforms. Z-score or range 0-1 instead.", "bad") else NULL
    tagList(lapply(transform_reco(m), function(x) note(x$txt, x$sev)), warn) })
  output$tr_preview <- renderDT({ m <- tr_matrix(); req(ncol(m) >= 1)
    tr <- tryCatch(apply_transform(m, input$tr_method), error = function(e) NULL)
    if (is.null(tr)) return(datatable(data.frame(Message = "Transformation not valid for these columns.")))
    names(tr) <- paste0(names(m), "_", input$tr_method)
    datatable(round(head(tr, 12), 4), options = list(scrollX = TRUE, dom = "t")) })
  observeEvent(input$tr_apply, { req(input$tr_vars)
    m <- tr_matrix(); tr <- tryCatch(apply_transform(m, input$tr_method), error = function(e) NULL)
    if (is.null(tr)) { showNotification("Transformation invalid for these columns.", type = "error"); return() }
    df <- rv$data
    if (input$tr_out == "replace") { df[input$tr_vars] <- tr }
    else { newnames <- paste0(input$tr_vars, "_", input$tr_method); df[newnames] <- tr }
    rv$data <- df
    showNotification(sprintf("Applied %s transformation to %d column(s).", input$tr_method, ncol(m)), type = "message") })

  # --- PCA Explorer -------------------------------------------------------
  output$pca_vars_ui <- renderUI(checkboxGroupInput("pca_vars", "Variables",
                                   choices = numeric_vars(), selected = numeric_vars()))
  pca_model <- reactive({ req(input$pca_vars, rv$data)
    vars <- intersect(input$pca_vars, names(rv$data)); if (length(vars) < 2) return(NULL)
    df <- rv$data; m <- df[, vars, drop = FALSE]; keep <- stats::complete.cases(m); m <- m[keep, , drop = FALSE]
    m <- m[, vapply(m, function(z) stats::sd(z) > 0, logical(1)), drop = FALSE]
    if (ncol(m) < 2) return(NULL)
    grp <- if (nzchar(input$pca_group %||% "")) factor(df[[input$pca_group]][keep]) else NULL
    shp <- if (nzchar(input$pca_shape %||% "")) factor(df[[input$pca_shape]][keep]) else NULL
    pc <- stats::prcomp(m, scale. = isTRUE(input$pca_scale))
    list(pc = pc, ve = pc$sdev^2 / sum(pc$sdev^2) * 100, grp = grp, shp = shp) })
  output$pca_notes <- renderUI({ mo <- pca_model()
    if (is.null(mo)) return(note("Select at least 2 numeric variables with variation.", "warn"))
    note(sprintf("PC1 and PC2 capture <b>%.1f%%</b> of the total variation. In the biplot, points are samples and arrows are variables: arrows pointing the same way are positively correlated, opposite ways negatively, and longer arrows load more strongly. The scree plot shows how many components are worth keeping (look for the 'elbow').", mo$ve[1] + mo$ve[2]), "info") })
  pca_plot <- reactive({ mo <- pca_model(); req(mo)
    if (input$pca_fig == "scree") {
      k <- min(10, length(mo$ve))
      d <- data.frame(idx = seq_len(k), PC = paste0("PC", seq_len(k)),
                      ve = mo$ve[seq_len(k)], cum = cumsum(mo$ve)[seq_len(k)])
      ggplot(d, aes(idx, ve)) +
        geom_col(fill = "#0072B2", colour = "black", linewidth = pt_to_mm(0.6), width = 0.7) +
        geom_line(aes(y = cum), colour = "#D55E00", linewidth = pt_to_mm(1.4)) +
        geom_point(aes(y = cum), colour = "#D55E00", size = 2.4) +
        scale_x_continuous(breaks = d$idx, labels = d$PC, expand = expansion(add = 0.6),
                           sec.axis = dup_axis(name = NULL, labels = NULL)) +
        labs(x = NULL, y = "Variance explained (%)", tag = "A") +
        locked_axis("y", c(0, 100), n = 6) + thm() + tagthm()
    } else {
      sc <- as.data.frame(mo$pc$x[, 1:2]); names(sc) <- c("PC1", "PC2")
      sc$grp <- mo$grp %||% factor("All")
      has_shape <- !is.null(mo$shp); sc$shp <- if (has_shape) mo$shp else factor("All")
      ld <- as.data.frame(mo$pc$rotation[, 1:2]); names(ld) <- c("PC1", "PC2"); ld$var <- rownames(ld)
      mult <- 0.8 * min(max(abs(sc$PC1)) / max(abs(ld$PC1)), max(abs(sc$PC2)) / max(abs(ld$PC2)))
      ld$x <- ld$PC1 * mult; ld$y <- ld$PC2 * mult
      ld <- head(ld[order(-(ld$PC1^2 + ld$PC2^2)), ], input$pca_arrows)
      p <- ggplot(sc, aes(PC1, PC2)) +
        geom_hline(yintercept = 0, colour = "grey80", linewidth = pt_to_mm(0.5)) +
        geom_vline(xintercept = 0, colour = "grey80", linewidth = pt_to_mm(0.5)) +
        geom_point(aes(colour = grp, shape = shp), size = 2.4, alpha = 0.85) +
        pub_colour(input$pca_group) + pub_shape_scale(input$pca_shape)
      xr <- range(sc$PC1); yr <- range(sc$PC2)
      if (nrow(ld) > 0 && input$pca_arrows > 0) {
        p <- p + geom_segment(data = ld, aes(x = 0, y = 0, xend = x, yend = y),
                   arrow = grid::arrow(length = grid::unit(6, "pt")), colour = "black", linewidth = pt_to_mm(0.9))
        if (has_pkg("ggrepel")) p <- p + ggrepel::geom_text_repel(data = ld, aes(x = x, y = y, label = var),
                   family = pfont(), size = 3.4, colour = "black", max.overlaps = 50)
        xr <- range(c(sc$PC1, ld$x)); yr <- range(c(sc$PC2, ld$y))
      }
      p <- p + labs(x = sprintf("PC1 (%.1f%%)", mo$ve[1]), y = sprintf("PC2 (%.1f%%)", mo$ve[2]),
                    tag = "A", colour = input$pca_group) +
        locked_axis("x", xr, n = 6, pad = 0.05) + locked_axis("y", yr, n = 6, pad = 0.05) +
        thm() + tagthm()
      if (is.null(mo$grp)) p <- p + guides(colour = "none")
      if (!has_shape) p <- p + guides(shape = "none")
      p
    } })
  output$pca_plot <- renderPlot(pca_plot())
  output$pca_loadings <- renderDT({ mo <- pca_model(); req(mo)
    k <- min(5, ncol(mo$pc$rotation))
    ld <- round(as.data.frame(mo$pc$rotation[, seq_len(k), drop = FALSE]), 3)
    ld <- cbind(Variable = rownames(ld), ld)
    datatable(ld, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE) })
  output$dl_pca <- dl_handler(function() paste0("pca_", input$pca_fig), pca_plot, width = 6.5, height = 5.5)

  # --- Choose a test ------------------------------------------------------
  output$guide_out <- renderUI({
    resp <- input$guide_response; pred <- input$guide_predictor; paired <- input$guide_paired == "paired"
    rec <- switch(resp,
      matrix = c("PCA / RDA for continuous gradients (vegan::rda, FactoMineR::PCA).",
                 "NMDS on Bray-Curtis for community structure (vegan::metaMDS).",
                 "PERMANOVA / ANOSIM to test group separation (vegan::adonis2, vegan::anosim)."),
      cat = c(if (paired) "McNemar's test for paired proportions (stats::mcnemar.test)."
              else "Chi-squared test of independence (stats::chisq.test).",
              "Fisher's exact test when any expected count &lt; 5 (stats::fisher.test).",
              "For a 2x2, report the odds ratio / relative risk (see Risk &amp; Odds)."),
      time = c("Kaplan-Meier curves with a log-rank test across groups (survival::survdiff).",
               "Cox proportional-hazards regression for covariates (survival::coxph)."),
      count = c("Poisson GLM; switch to quasi-Poisson / negative binomial if over-dispersed (stats::glm)."),
      cont = switch(pred,
        none = "One-sample t-test if roughly normal, else Wilcoxon signed-rank (stats::t.test / wilcox.test).",
        g2 = if (paired) "Paired t-test, or Wilcoxon signed-rank if non-normal (paired = TRUE)."
             else "Welch's t-test, or Mann-Whitney U if non-normal (stats::t.test / wilcox.test).",
        g3 = "One-way ANOVA + Tukey HSD, or Kruskal-Wallis + Dunn if non-normal.",
        num = "Pearson correlation / linear regression, or Spearman if non-linear or non-normal.",
        multi = "Multiple regression or ANCOVA (stats::lm, car::Anova)."))
    tagList(lapply(rec, function(r) note(r, "info")))
  })

  output$guide_live <- renderUI({
    req(input$guide_y, input$guide_g)
    df <- rv$data; d <- df[stats::complete.cases(df[c(input$guide_y, input$guide_g)]), ]
    y <- d[[input$guide_y]]; g <- factor(d[[input$guide_g]])
    if (!is.numeric(y)) return(note("Pick a numeric outcome column in section B.", "warn"))
    k <- nlevels(g)
    if (k < 2) return(note("The grouping column needs at least 2 levels.", "warn"))
    pvals <- tapply(y, g, safe_shapiro)
    non_normal <- any(pvals < 0.05, na.rm = TRUE)
    var_p <- tryCatch(stats::bartlett.test(y, g)$p.value, error = function(e) NA)
    unequal_var <- !is.na(var_p) && var_p < 0.05
    small <- min(table(g)) < 15
    msgs <- list()
    msgs <- c(msgs, list(note(sprintf("Groups: <b>%d</b> (%s); smallest n = %d.",
              k, paste(levels(g), collapse = ", "), min(table(g))), "info")))
    msgs <- c(msgs, list(note(sprintf("Within-group normality (Shapiro): %s.",
              if (non_normal) "at least one group departs from normal" else "no departures flagged"),
              if (non_normal) "warn" else "ok")))
    if (!is.na(var_p))
      msgs <- c(msgs, list(note(sprintf("Equal variances (Bartlett p = %s): %s.", fmt_p(var_p),
                if (unequal_var) "variances differ — use Welch" else "reasonable"),
                if (unequal_var) "warn" else "ok")))
    rec <- if (k == 2) {
      if (non_normal || small) "Recommendation: <b>Mann-Whitney U</b> (non-normal or small)."
      else if (unequal_var) "Recommendation: <b>Welch's t-test</b> (unequal variances)."
      else "Recommendation: <b>Student's t-test</b> (assumptions met)."
    } else {
      if (non_normal || small) "Recommendation: <b>Kruskal-Wallis</b> + Dunn post-hoc."
      else if (unequal_var) "Recommendation: <b>Welch ANOVA</b> + Games-Howell."
      else "Recommendation: <b>one-way ANOVA</b> + Tukey HSD."
    }
    msgs <- c(msgs, list(note(rec, "ok")))
    tagList(msgs)
  })

  # --- Two-group comparison ----------------------------------------------
  gc_data <- reactive({ req(input$gc_response, input$gc_group)
    d <- rv$data[, c(input$gc_response, input$gc_group)]; names(d) <- c("y", "g")
    d <- d[stats::complete.cases(d), ]; d$g <- factor(d$g); d })

  gc_choice <- reactive({ d <- gc_data(); req(nlevels(d$g) == 2)
    if (input$gc_test != "auto") return(input$gc_test)
    nn <- any(tapply(d$y, d$g, safe_shapiro) < 0.05, na.rm = TRUE) || min(table(d$g)) < 15
    if (nn) "mw" else {
      vp <- tryCatch(stats::var.test(y ~ g, d)$p.value, error = function(e) NA)
      if (!is.na(vp) && vp < 0.05) "t" else "tstud" } })

  output$gc_assump <- renderUI({ d <- gc_data()
    if (nlevels(d$g) != 2) return(note(sprintf("This test needs exactly 2 groups; '%s' has %d. Use the 3+ Groups tab.",
                                                input$gc_group, nlevels(d$g)), "bad"))
    sh <- tapply(d$y, d$g, safe_shapiro); nn <- any(sh < 0.05, na.rm = TRUE)
    msgs <- list(note(sprintf("n = %s per group.", paste(as.integer(table(d$g)), collapse = " / ")),
                      if (min(table(d$g)) < 15) "warn" else "ok"),
                 note(sprintf("Normality: %s.", if (nn) "at least one group departs — non-parametric is safer" else "no departures flagged"),
                      if (nn) "warn" else "ok"))
    if (input$gc_test == "auto")
      msgs <- c(msgs, list(note(sprintf("Auto-selected: <b>%s</b>.",
                c(t = "Welch t-test", tstud = "Student t-test", mw = "Mann-Whitney U")[[gc_choice()]]), "info")))
    tagList(msgs) })

  gc_fit <- reactive({ d <- gc_data(); req(nlevels(d$g) == 2)
    test <- gc_choice(); paired <- input$gc_design == "paired"
    switch(test,
      t     = stats::t.test(y ~ g, d, var.equal = FALSE, paired = paired),
      tstud = stats::t.test(y ~ g, d, var.equal = TRUE, paired = paired),
      mw    = stats::wilcox.test(y ~ g, d, paired = paired, conf.int = TRUE, exact = FALSE)) })

  output$gc_result <- renderPrint(print(gc_fit()))

  output$gc_interp <- renderUI({ d <- gc_data(); req(nlevels(d$g) == 2)
    fit <- gc_fit(); ip <- interp_p(fit$p.value)
    lv <- levels(d$g); x <- d$y[d$g == lv[1]]; y <- d$y[d$g == lv[2]]
    es <- if (gc_choice() == "mw") {
      W <- unname(fit$statistic); rb <- rank_biserial(W, length(x), length(y))
      sprintf("Rank-biserial correlation r = %.2f (%s effect).", rb, r_magnitude(rb))
    } else {
      dd <- cohens_d(x, y); g <- hedges_g(dd, length(x) + length(y))
      sprintf("Cohen's d = %.2f (Hedges g = %.2f; %s effect). Group means: %s = %.2f, %s = %.2f.",
              dd, g, d_magnitude(dd), lv[1], mean(x), lv[2], mean(y))
    }
    tagList(note(ip$txt, ip$sev), note(es, "info")) })

  gc_plot <- reactive({ d <- gc_data(); req(nrow(d) > 0)
    # Reserve labelled headroom for the significance bracket so it isn't clipped.
    yvals <- d$y
    if (isTRUE(input$gc_signif) && nlevels(d$g) == 2)
      yvals <- c(yvals, max(d$y) + 0.16 * diff(range(d$y)))
    p <- ggplot(d, aes(g, y, fill = g)) +
      geom_boxplot(outlier.shape = NA, width = 0.55, colour = "black", linewidth = pt_to_mm(1.0), alpha = 0.35) +
      geom_jitter(aes(colour = g), width = 0.12, height = 0, size = 1.8, alpha = 0.8, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.4, fill = "white",
                   colour = "black", stroke = pt_to_mm(1.0)) +
      labs(x = input$gc_group, y = input$gc_response, tag = "A") + pub_colour() +
      locked_axis("y", yvals, n = 6, pad = 0.03) + guides(fill = "none") +
      thm() + tagthm()
    if (isTRUE(input$gc_signif) && nlevels(d$g) == 2 && has_pkg("ggsignif")) {
      pv <- tryCatch(gc_fit()$p.value, error = function(e) NA)
      if (!is.na(pv)) p <- p + ggsignif::geom_signif(comparisons = list(levels(d$g)),
        annotations = sprintf("p = %s  %s", ifelse(pv < 0.001, "<0.001", formatC(pv, format = "f", digits = 3)),
                              p_to_stars(pv)), tip_length = 0.01, family = pfont(), textsize = 4)
    }
    p })
  output$gc_plot <- renderPlot(gc_plot())
  output$dl_box <- dl_handler(function() paste0("comparison_", input$gc_response), gc_plot)

  # --- 3+ group comparison ------------------------------------------------
  av_data <- reactive({ req(input$av_response, input$av_group)
    d <- rv$data[, c(input$av_response, input$av_group)]; names(d) <- c("y", "g")
    d <- d[stats::complete.cases(d), ]; d$g <- factor(d$g); d })

  av_choice <- reactive({ d <- av_data(); req(nlevels(d$g) >= 2)
    if (input$av_test != "auto") return(input$av_test)
    nn <- any(tapply(d$y, d$g, safe_shapiro) < 0.05, na.rm = TRUE) || min(table(d$g)) < 15
    if (nn) "kw" else {
      vp <- tryCatch(stats::bartlett.test(y ~ g, d)$p.value, error = function(e) NA)
      if (!is.na(vp) && vp < 0.05) "welch" else "anova" } })

  output$av_assump <- renderUI({ d <- av_data()
    if (nlevels(d$g) < 2) return(note("Need at least 2 groups.", "bad"))
    nn <- any(tapply(d$y, d$g, safe_shapiro) < 0.05, na.rm = TRUE)
    msgs <- list(note(sprintf("%d groups; smallest n = %d.", nlevels(d$g), min(table(d$g))),
                      if (min(table(d$g)) < 15) "warn" else "ok"),
                 note(sprintf("Normality: %s.", if (nn) "a group departs — Kruskal-Wallis is safer" else "no departures flagged"),
                      if (nn) "warn" else "ok"))
    if (input$av_test == "auto")
      msgs <- c(msgs, list(note(sprintf("Auto-selected: <b>%s</b>.",
                c(anova = "one-way ANOVA", welch = "Welch ANOVA", kw = "Kruskal-Wallis")[[av_choice()]]), "info")))
    tagList(msgs) })

  av_fit <- reactive({ d <- av_data(); req(nlevels(d$g) >= 2)
    switch(av_choice(),
      anova = stats::aov(y ~ g, d),
      welch = stats::oneway.test(y ~ g, d, var.equal = FALSE),
      kw    = stats::kruskal.test(y ~ g, d)) })

  output$av_result <- renderPrint({ f <- av_fit()
    if (inherits(f, "aov")) print(summary(f)) else print(f) })

  output$av_interp <- renderUI({ d <- av_data(); f <- av_fit()
    pv <- if (inherits(f, "aov")) summary(f)[[1]][["Pr(>F)"]][1] else f$p.value
    ip <- interp_p(pv)
    es <- if (av_choice() == "kw") {
      H <- unname(f$statistic); e2 <- epsilon_sq_kw(H, nrow(d))
      sprintf("Epsilon-squared = %.3f (%s effect).", e2, eta_magnitude(e2))
    } else if (av_choice() == "anova") {
      e2 <- eta_sq_aov(f); sprintf("Eta-squared = %.3f (%s effect): the grouping explains %.0f%% of the variance.",
                                   e2, eta_magnitude(e2), 100 * e2)
    } else "Welch ANOVA relaxes the equal-variance assumption; report group means and CIs."
    extra <- if (pv < 0.05) note("Significant overall — inspect the post-hoc table to see which specific pairs differ.", "info") else NULL
    tagList(note(ip$txt, ip$sev), note(es, "info"), extra) })

  av_posthoc <- reactive({ if (!isTRUE(input$av_posthoc)) return(NULL)
    d <- av_data(); req(nlevels(d$g) >= 2)
    res <- switch(av_choice(),
      anova = , welch = { ph <- stats::TukeyHSD(stats::aov(y ~ g, d))$g
        data.frame(Comparison = rownames(ph), Difference = round(ph[, "diff"], 4),
                   p_adj = signif(ph[, "p adj"], 4), row.names = NULL) },
      kw = { if (has_pkg("FSA")) { dt <- FSA::dunnTest(y ~ g, d, method = "bh")$res
          data.frame(Comparison = dt$Comparison, Z = round(dt$Z, 3), p_adj = signif(dt$P.adj, 4), row.names = NULL)
        } else { pw <- stats::pairwise.wilcox.test(d$y, d$g, p.adjust.method = "BH")
          lo <- as.data.frame(as.table(pw$p.value)); lo <- lo[!is.na(lo$Freq), ]
          data.frame(Comparison = paste(lo$Var1, "-", lo$Var2), p_adj = signif(lo$Freq, 4), row.names = NULL) } })
    res$Signif <- p_to_stars(res$p_adj); res })
  output$av_posthoc_tbl <- renderDT({ ph <- av_posthoc(); if (is.null(ph)) return(NULL)
    datatable(ph, options = list(pageLength = 10), rownames = FALSE) })

  av_plot <- reactive({ d <- av_data(); req(nrow(d) > 0)
    ph_sig <- if (isTRUE(input$av_signif) && has_pkg("ggsignif")) av_posthoc() else NULL
    nsig <- if (!is.null(ph_sig) && "p_adj" %in% names(ph_sig)) sum(ph_sig$p_adj < 0.05) else 0
    # Stack significance brackets in labelled headroom above the data.
    yvals <- if (nsig > 0) c(d$y, max(d$y) + (0.08 + 0.09 * nsig) * diff(range(d$y))) else d$y
    p <- ggplot(d, aes(g, y, fill = g)) +
      geom_boxplot(outlier.shape = NA, width = 0.6, colour = "black", linewidth = pt_to_mm(1.0), alpha = 0.35) +
      geom_jitter(aes(colour = g), width = 0.12, height = 0, size = 1.7, alpha = 0.8, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.2, fill = "white",
                   colour = "black", stroke = pt_to_mm(1.0)) +
      labs(x = input$av_group, y = input$av_response, tag = "A") + pub_colour() +
      locked_axis("y", yvals, n = 6, pad = 0.03) + guides(fill = "none") +
      thm() + tagthm()
    if (nsig > 0) {
      ph <- ph_sig
      if (!is.null(ph) && "p_adj" %in% names(ph)) {
        sig <- ph[ph$p_adj < 0.05, , drop = FALSE]
        if (nrow(sig)) { comps <- strsplit(sig$Comparison, " ?- ?")
          p <- p + ggsignif::geom_signif(comparisons = comps,
            annotations = p_to_stars(sig$p_adj), step_increase = 0.08,
            tip_length = 0.01, family = pfont(), textsize = 5) }
      }
    }
    p })
  output$av_plot <- renderPlot(av_plot())
  output$dl_avbox <- dl_handler(function() paste0("anova_", input$av_response), av_plot)

  # --- Two-way / ANCOVA ---------------------------------------------------
  tw_data <- reactive({ req(input$tw_response, input$tw_f1, input$tw_f2)
    d <- rv$data[, c(input$tw_response, input$tw_f1, input$tw_f2)]
    names(d) <- c("y", "f1", "f2"); d <- d[stats::complete.cases(d), ]
    d$f1 <- factor(d$f1)
    if (length(unique(d$f2)) <= 12 && !is.numeric(d$f2)) d$f2 <- factor(d$f2)
    d })
  tw_fit <- reactive({ d <- tw_data()
    form <- if (isTRUE(input$tw_interaction)) y ~ f1 * f2 else y ~ f1 + f2
    stats::lm(form, d) })
  output$tw_result <- renderPrint({ fit <- tw_fit()
    if (has_pkg("car")) print(car::Anova(fit, type = 2)) else print(stats::anova(fit)) })
  output$tw_interp <- renderUI({ fit <- tw_fit()
    a <- if (has_pkg("car")) car::Anova(fit, type = 2) else stats::anova(fit)
    pcol <- grep("Pr", names(a)); if (!length(pcol)) return(NULL)
    ps <- a[[pcol[1]]]; rn <- rownames(a)
    msgs <- lapply(seq_along(ps), function(i) {
      if (rn[i] %in% c("Residuals", "(Intercept)") || is.na(ps[i])) return(NULL)
      note(sprintf("<b>%s</b>: %s", rn[i], interp_p(ps[i])$txt), if (ps[i] < 0.05) "ok" else "info")
    })
    tagList(Filter(Negate(is.null), msgs),
            note("A significant interaction means one factor's effect depends on the other — interpret main effects with care.", "info")) })
  tw_plot <- reactive({ d <- tw_data()
    summ <- d %>% group_by(f1, f2) %>%
      summarise(m = mean(y), se = stats::sd(y) / sqrt(dplyr::n()), .groups = "drop")
    if (is.numeric(d$f2)) {
      p <- ggplot(d, aes(f2, y, colour = f1)) + geom_point(size = 1.8, alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE, linewidth = pt_to_mm(1.4)) +
        labs(x = input$tw_f2, y = input$tw_response, colour = input$tw_f1, tag = "A")
    } else {
      p <- ggplot(summ, aes(f1, m, colour = f2, group = f2)) +
        geom_line(linewidth = pt_to_mm(1.4)) +
        geom_errorbar(aes(ymin = m - se, ymax = m + se), width = 0.1, linewidth = pt_to_mm(1.0)) +
        geom_point(size = 2.6) +
        labs(x = input$tw_f1, y = paste("Mean", input$tw_response), colour = input$tw_f2, tag = "A")
    }
    p + pub_colour() + thm() + tagthm() })
  output$tw_plot <- renderPlot(tw_plot())
  output$dl_tw <- dl_handler("twoway", tw_plot)

  # --- Contingency --------------------------------------------------------
  ct_table <- reactive({ req(input$ct_row, input$ct_col)
    d <- rv$data[, c(input$ct_row, input$ct_col)]; d <- d[stats::complete.cases(d), ]
    table(d[[1]], d[[2]]) })
  output$ct_obs <- renderDT(datatable(as.data.frame.matrix(ct_table()), options = list(dom = "t")))
  ct_fit <- reactive({ tb <- ct_table()
    if (input$ct_design == "paired") { req(nrow(tb) == ncol(tb)); return(stats::mcnemar.test(tb)) }
    ex <- tryCatch(stats::chisq.test(tb)$expected, error = function(e) matrix(Inf, 1, 1))
    if (any(ex < 5)) stats::fisher.test(tb) else stats::chisq.test(tb) })
  output$ct_result <- renderPrint(print(ct_fit()))
  output$ct_interp <- renderUI({ tb <- ct_table(); fit <- ct_fit(); ip <- interp_p(fit$p.value)
    extra <- NULL
    if (grepl("Chi", fit$method)) { v <- cramers_v(fit$statistic, sum(tb), nrow(tb), ncol(tb))
      extra <- note(sprintf("Cramér's V = %.2f (%s association).", v, v_magnitude(v)), "info") }
    else if (grepl("Fisher", fit$method))
      extra <- note("Expected counts were small, so Fisher's exact test was used automatically.", "info")
    tagList(note(ip$txt, ip$sev), extra) })
  ct_plot <- reactive({ tb <- ct_table()
    dd <- as.data.frame(tb); names(dd) <- c("Row", "Col", "n")
    ggplot(dd, aes(Row, n, fill = Col)) +
      geom_col(position = "dodge", colour = "black", linewidth = pt_to_mm(0.6)) +
      labs(x = input$ct_row, y = "Count", fill = input$ct_col, tag = "A") + pub_colour() +
      locked_axis("y", c(0, dd$n), n = 6) + thm() + tagthm() })
  output$ct_plot <- renderPlot(ct_plot())
  output$dl_ct <- dl_handler("contingency", ct_plot)

  # --- Risk & odds --------------------------------------------------------
  rr_table <- reactive({ req(input$rr_exp, input$rr_out)
    d <- rv$data[, c(input$rr_exp, input$rr_out)]; d <- d[stats::complete.cases(d), ]
    table(Exposure = factor(d[[1]]), Outcome = factor(d[[2]])) })
  output$rr_tbl <- renderDT(datatable(as.data.frame.matrix(rr_table()), options = list(dom = "t")))
  output$rr_measures <- renderDT({ tb <- rr_table(); req(all(dim(tb) == 2))
    # Rows = exposure (ref, exposed); cols = outcome (ref, positive).
    a <- tb[2, 2]; b <- tb[2, 1]; c <- tb[1, 2]; d <- tb[1, 1]
    or <- (a * d) / (b * c); se_or <- sqrt(1/a + 1/b + 1/c + 1/d)
    r1 <- a / (a + b); r0 <- c / (c + d); rr <- r1 / r0
    se_rr <- sqrt((1 - r1) / a + (1 - r0) / c); rd <- r1 - r0
    z <- stats::qnorm(0.975)
    tab <- data.frame(
      Measure = c("Odds ratio", "Relative risk", "Risk difference"),
      Estimate = round(c(or, rr, rd), 3),
      CI_low = round(c(exp(log(or) - z * se_or), exp(log(rr) - z * se_rr), rd - z * sqrt(r1*(1-r1)/(a+b) + r0*(1-r0)/(c+d))), 3),
      CI_high = round(c(exp(log(or) + z * se_or), exp(log(rr) + z * se_rr), rd + z * sqrt(r1*(1-r1)/(a+b) + r0*(1-r0)/(c+d))), 3))
    datatable(tab, options = list(dom = "t"), rownames = FALSE) })
  output$rr_interp <- renderUI({ tb <- rr_table()
    if (!all(dim(tb) == 2)) return(note("Both variables must have exactly 2 levels for a 2x2 analysis.", "bad"))
    a <- tb[2, 2]; b <- tb[2, 1]; c <- tb[1, 2]; d <- tb[1, 1]
    or <- (a * d) / (b * c); rr <- (a / (a + b)) / (c / (c + d))
    dir <- if (or > 1) "increased" else "decreased"
    tagList(
      note(sprintf("The exposed group has <b>%s odds</b> of the outcome (OR = %.2f). An OR/RR whose 95%% CI excludes 1 is statistically significant.", dir, or), "info"),
      note(sprintf("Relative risk = %.2f: the outcome is %.1f× as likely under exposure. Use RR for cohort/experimental designs, OR for case-control.", rr, rr), "info"),
      note("Odds ratio and relative risk diverge when the outcome is common (&gt; ~10%).", "warn")) })

  # --- Diagnostic / ROC ---------------------------------------------------
  dx_data <- reactive({ req(input$dx_score, input$dx_truth)
    d <- rv$data[, c(input$dx_score, input$dx_truth)]; names(d) <- c("score", "truth")
    d <- d[stats::complete.cases(d), ]; req(length(unique(d$truth)) == 2)
    d$pos <- as.integer(factor(d$truth)) - 1L  # second level = positive
    if (mean(d$score[d$pos == 1]) < mean(d$score[d$pos == 0])) d$score <- -d$score
    d })
  roc_points <- reactive({ d <- dx_data()
    thr <- sort(unique(d$score), decreasing = TRUE); P <- sum(d$pos == 1); N <- sum(d$pos == 0)
    tpr <- vapply(thr, function(t) sum(d$score >= t & d$pos == 1) / P, numeric(1))
    fpr <- vapply(thr, function(t) sum(d$score >= t & d$pos == 0) / N, numeric(1))
    data.frame(thr = c(Inf, thr), tpr = c(0, tpr), fpr = c(0, fpr)) })
  dx_auc <- reactive({ d <- dx_data(); r <- rank(d$score)
    n1 <- sum(d$pos == 1); n0 <- sum(d$pos == 0)
    (sum(r[d$pos == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) })
  output$dx_interp <- renderUI({ auc <- dx_auc()
    lab <- if (auc >= 0.9) "excellent" else if (auc >= 0.8) "good" else if (auc >= 0.7) "fair"
           else if (auc >= 0.6) "poor" else "no better than chance"
    note(sprintf("<b>AUC = %.3f</b> (%s discrimination): the chance a random positive case scores higher than a random negative one. 0.5 = coin flip, 1.0 = perfect.", auc, lab), "info") })
  output$dx_tbl <- renderDT({ rp <- roc_points(); d <- dx_data()
    j <- which.max(rp$tpr - rp$fpr); cut <- rp$thr[j]
    pred <- d$score >= cut
    tp <- sum(pred & d$pos == 1); fp <- sum(pred & d$pos == 0)
    fn <- sum(!pred & d$pos == 1); tn <- sum(!pred & d$pos == 0)
    tab <- data.frame(Metric = c("Sensitivity", "Specificity", "PPV", "NPV", "Accuracy"),
      Value = round(c(tp/(tp+fn), tn/(tn+fp), tp/(tp+fp), tn/(tn+fn), (tp+tn)/nrow(d)), 3))
    datatable(tab, options = list(dom = "t"), rownames = FALSE) })
  dx_plot <- reactive({ rp <- roc_points(); auc <- dx_auc()
    ggplot(rp, aes(fpr, tpr)) +
      geom_abline(slope = 1, intercept = 0, colour = "grey70", linewidth = pt_to_mm(1.0)) +
      geom_path(colour = "#0072B2", linewidth = pt_to_mm(1.6)) +
      annotate("text", x = 0.62, y = 0.12, label = sprintf("AUC = %.3f", auc),
               family = pfont(), size = 5, hjust = 0) +
      labs(x = "False positive rate (1 - specificity)", y = "True positive rate (sensitivity)", tag = "A") +
      locked_axis("x", c(0, 1), n = 5) + locked_axis("y", c(0, 1), n = 5) +
      coord_fixed() + thm() + tagthm() })
  output$dx_plot <- renderPlot(dx_plot())
  output$dl_roc <- dl_handler("roc", dx_plot, 6, 6)

  # --- Linear regression --------------------------------------------------
  output$reg_x_ui <- renderUI(checkboxGroupInput("reg_x", "Predictors",
                                choices = setdiff(names(rv$data), input$reg_y),
                                selected = numeric_vars()[numeric_vars() != input$reg_y][1]))
  reg_fit <- reactive({ req(input$reg_y, input$reg_x)
    d <- rv$data[, c(input$reg_y, input$reg_x)]; d <- d[stats::complete.cases(d), ]
    stats::lm(stats::reformulate(input$reg_x, input$reg_y), d) })
  output$reg_result <- renderPrint(print(summary(reg_fit())))
  output$reg_coef <- renderDT({ fit <- reg_fit(); s <- summary(fit)$coefficients
    ci <- stats::confint(fit)
    tab <- data.frame(Term = rownames(s), Estimate = round(s[, 1], 4),
      CI_low = round(ci[, 1], 4), CI_high = round(ci[, 2], 4), p = signif(s[, 4], 4))
    datatable(tab, options = list(dom = "t"), rownames = FALSE) })
  output$reg_interp <- renderUI({ fit <- reg_fit(); s <- summary(fit)
    r2 <- s$r.squared; fstat <- s$fstatistic
    pv <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    ip <- interp_p(pv)
    tagList(note(sprintf("The model explains <b>%.1f%%</b> of the variance in %s (adjusted R² = %.3f).",
                         100 * r2, input$reg_y, s$adj.r.squared), "info"),
            note(sprintf("Overall model: %s", ip$txt), ip$sev),
            note("Check the residual plot: a funnel shape suggests non-constant variance; curvature suggests a non-linear relationship.", "info")) })
  reg_plot <- reactive({ fit <- reg_fit()
    if (length(input$reg_x) == 1 && is.numeric(rv$data[[input$reg_x]])) {
      d <- fit$model; names(d)[1:2] <- c("y", "x")
      ggplot(d, aes(x, y)) + geom_point(size = 2, alpha = 0.75, colour = "#0072B2") +
        geom_smooth(method = "lm", colour = "#D55E00", fill = "#D55E00", alpha = 0.15, linewidth = pt_to_mm(1.4)) +
        labs(x = input$reg_x, y = input$reg_y, tag = "A") +
        locked_axis("x", d$x, n = 6, pad = 0.03) + locked_axis("y", d$y, n = 6, pad = 0.03) +
        thm() + tagthm()
    } else {
      d <- data.frame(fitted = stats::fitted(fit), resid = stats::resid(fit))
      ggplot(d, aes(fitted, resid)) + geom_hline(yintercept = 0, colour = "grey70", linewidth = pt_to_mm(1.0)) +
        geom_point(size = 2, alpha = 0.75, colour = "#0072B2") +
        labs(x = "Fitted values", y = "Residuals", tag = "A") +
        locked_axis("x", d$fitted, n = 6, pad = 0.03) + locked_axis("y", d$resid, n = 6, pad = 0.03) +
        thm() + tagthm()
    } })
  output$reg_plot <- renderPlot(reg_plot())
  output$dl_reg <- dl_handler("regression", reg_plot)

  # --- Logistic regression ------------------------------------------------
  output$logit_x_ui <- renderUI(checkboxGroupInput("logit_x", "Predictors",
                                  choices = setdiff(names(rv$data), input$logit_y),
                                  selected = numeric_vars()[numeric_vars() != input$logit_y][1]))
  logit_fit <- reactive({ req(input$logit_y, input$logit_x)
    d <- rv$data[, c(input$logit_y, input$logit_x)]; d <- d[stats::complete.cases(d), ]
    d[[input$logit_y]] <- as.integer(factor(d[[input$logit_y]])) - 1L
    stats::glm(stats::reformulate(input$logit_x, input$logit_y), d, family = stats::binomial()) })
  output$logit_result <- renderPrint(print(summary(logit_fit())))
  output$logit_or <- renderDT({ fit <- logit_fit(); s <- summary(fit)$coefficients
    ci <- suppressMessages(stats::confint(fit))
    tab <- data.frame(Term = rownames(s), OR = round(exp(s[, 1]), 3),
      CI_low = round(exp(ci[, 1]), 3), CI_high = round(exp(ci[, 2]), 3), p = signif(s[, 4], 4))
    datatable(tab, options = list(dom = "t"), rownames = FALSE) })
  output$logit_interp <- renderUI(tagList(
    note("Odds ratios &gt; 1 raise the odds of the outcome per one-unit increase in the predictor; &lt; 1 lower them. A CI excluding 1 is significant.", "info"),
    note("The second level of your outcome is modelled as the 'positive' event.", "info")))
  logit_plot <- reactive({ fit <- logit_fit()
    req(length(input$logit_x) == 1 && is.numeric(rv$data[[input$logit_x]]))
    d <- fit$model; names(d)[1:2] <- c("y", "x")
    grid <- data.frame(x = seq(min(d$x), max(d$x), length.out = 200))
    grid$p <- stats::predict(fit, newdata = stats::setNames(grid, input$logit_x), type = "response")
    ggplot() + geom_point(data = d, aes(x, y), size = 1.8, alpha = 0.5, colour = "#0072B2") +
      geom_line(data = grid, aes(x, p), colour = "#D55E00", linewidth = pt_to_mm(1.6)) +
      labs(x = input$logit_x, y = paste("P(", input$logit_y, ")"), tag = "A") +
      locked_axis("x", d$x, n = 6, pad = 0.03) + locked_axis("y", c(0, 1), n = 5) +
      thm() + tagthm() })
  output$logit_plot <- renderPlot({ tryCatch(logit_plot(), error = function(e)
    ggplot() + annotate("text", 0, 0, label = "Probability curve shown for a single numeric predictor.",
                        family = pfont(), size = 5) + theme_void()) })
  output$dl_logit <- dl_handler("logistic", logit_plot)

  # --- Poisson GLM --------------------------------------------------------
  output$pois_x_ui <- renderUI(checkboxGroupInput("pois_x", "Predictors",
                                 choices = setdiff(names(rv$data), input$pois_y),
                                 selected = factor_vars()[factor_vars() != input$pois_y][1]))
  pois_fit <- reactive({ req(input$pois_y, input$pois_x)
    d <- rv$data[, c(input$pois_y, input$pois_x)]; d <- d[stats::complete.cases(d), ]
    stats::glm(stats::reformulate(input$pois_x, input$pois_y), d, family = stats::poisson()) })
  output$pois_result <- renderPrint(print(summary(pois_fit())))
  output$pois_rr <- renderDT({ fit <- pois_fit(); s <- summary(fit)$coefficients
    tab <- data.frame(Term = rownames(s), RateRatio = round(exp(s[, 1]), 3), p = signif(s[, 4], 4))
    datatable(tab, options = list(dom = "t"), rownames = FALSE) })
  output$pois_interp <- renderUI({ fit <- pois_fit()
    disp <- sum(stats::resid(fit, type = "pearson")^2) / fit$df.residual
    od <- disp > 1.5
    tagList(note("Rate ratios &gt; 1 multiply the expected count per one-unit increase in the predictor.", "info"),
            note(sprintf("Dispersion = %.2f. %s", disp,
                 if (od) "This is over-dispersed (&gt; ~1.5): refit with quasi-Poisson or a negative-binomial model or your p-values will be too optimistic."
                 else "Close to 1, so the Poisson assumption is reasonable."), if (od) "warn" else "ok")) })

  # --- Dose-response ------------------------------------------------------
  dr_fit <- reactive({ req(input$dr_dose, input$dr_resp, has_pkg("drc"))
    df <- rv$data; req(input$dr_dose %in% names(df), input$dr_resp %in% names(df))
    grp <- if (nzchar(input$dr_group %||% "")) df[[input$dr_group]] else NULL
    tryCatch(fit_dose(df[[input$dr_dose]], df[[input$dr_resp]], grp, logx = isTRUE(input$dr_logx)),
             error = function(e) list(error = conditionMessage(e))) })
  output$dr_notes <- renderUI({
    if (!has_pkg("drc")) return(note("The <b>drc</b> package is required for dose-response fitting. install.packages('drc').", "warn"))
    f <- dr_fit(); req(f)
    if (!is.null(f$error)) return(note(sprintf("The curve could not be fit: %s. Dose-response fitting needs a range of doses spanning low to saturating response, with replication.", f$error), "bad"))
    note("The <b>EC50</b> (also called ED50 or IC50) is the dose producing a half-maximal response — the standard potency summary. The <b>Hill slope</b> measures how steeply response changes with dose; <b>Lower/Upper</b> are the fitted response plateaus. Non-overlapping EC50 confidence intervals between groups indicate a real difference in potency.", "info") })
  dr_plot <- reactive({ f <- dr_fit(); req(f); is.null(f$error) || return(NULL)
    d <- f$data; d$grp <- if ("grp" %in% names(d)) d$grp else factor("All")
    pred <- f$pred; pred$grp <- factor(pred$grp)
    dd <- if (isTRUE(input$dr_logx)) d[d$dose > 0, ] else d
    p <- ggplot(dd, aes(dose, resp, colour = grp)) +
      geom_point(size = 2, alpha = 0.8) +
      geom_line(data = pred, aes(dose, fit, colour = grp), linewidth = pt_to_mm(1.6)) +
      pub_colour(input$dr_group) +
      labs(x = input$dr_dose, y = input$dr_resp, tag = "A", colour = input$dr_group) +
      locked_axis("y", c(dd$resp, pred$fit), n = 6, pad = 0.03) +
      thm() + tagthm()
    if (isTRUE(input$dr_logx))
      p <- p + scale_x_log10(sec.axis = dup_axis(name = NULL, labels = NULL))
    else p <- p + locked_axis("x", dd$dose, n = 6, pad = 0.03)
    if (nlevels(factor(d$grp)) < 2) p <- p + guides(colour = "none")
    p })
  output$dr_plot <- renderPlot({ f <- dr_fit()
    if (is.null(f) || !is.null(f$error))
      return(ggplot() + annotate("text", 0, 0, label = "Choose a dose and response; a fittable sigmoid is needed.", family = pfont(), size = 5) + theme_void())
    dr_plot() })
  output$dr_tbl <- renderDT({ f <- dr_fit(); req(f); is.null(f$error) || return(NULL)
    t <- f$partab; t[-1] <- lapply(t[-1], function(z) signif(as.numeric(z), 4))
    names(t) <- c("Group", "EC50", "EC50 lower", "EC50 upper", "Hill slope", "Lower plateau", "Upper plateau")
    datatable(t, options = list(dom = "t", scrollX = TRUE), rownames = FALSE) })
  output$dl_dr <- dl_handler("dose_response", dr_plot, width = 6.5, height = 5)

  # --- Correlation --------------------------------------------------------
  output$cor_vars_ui <- renderUI(checkboxGroupInput("cor_vars", "Variables",
                                   choices = numeric_vars(), selected = head(numeric_vars(), 6)))
  cor_input <- reactive({ req(input$cor_vars); if (length(input$cor_vars) < 2) return(NULL)
    m <- rv$data[, input$cor_vars, drop = FALSE]; m[stats::complete.cases(m), , drop = FALSE] })
  cor_result <- reactive({ m <- cor_input(); req(m)
    if (!is.null(input$cor_partial) && nzchar(input$cor_partial) && has_pkg("ppcor") &&
        !(input$cor_partial %in% names(m))) {
      vars <- input$cor_vars; k <- length(vars)
      base <- rv$data[, c(vars, input$cor_partial), drop = FALSE]; base <- base[stats::complete.cases(base), ]
      r <- matrix(1, k, k, dimnames = list(vars, vars)); pmat <- matrix(0, k, k, dimnames = list(vars, vars))
      for (i in seq_len(k - 1)) for (j in (i + 1):k) {
        pc <- tryCatch(ppcor::pcor.test(base[[vars[i]]], base[[vars[j]]], base[[input$cor_partial]],
                       method = input$cor_method), error = function(e) NULL)
        if (!is.null(pc)) { r[i, j] <- r[j, i] <- pc$estimate; pmat[i, j] <- pmat[j, i] <- pc$p.value } }
      list(r = r, p = pmat, partial = TRUE)
    } else {
      r <- stats::cor(m, method = input$cor_method)
      pmat <- outer(seq_len(ncol(m)), seq_len(ncol(m)), Vectorize(function(i, j)
        if (i == j) 0 else tryCatch(stats::cor.test(m[[i]], m[[j]], method = input$cor_method)$p.value,
                                    error = function(e) NA_real_)))
      dimnames(pmat) <- dimnames(r); list(r = r, p = pmat, partial = FALSE) } })
  output$cor_tbl <- renderDT({ cr <- cor_result(); req(cr); datatable(round(cr$r, 3), options = list(scrollX = TRUE)) })
  output$cor_p_tbl <- renderDT({ cr <- cor_result(); req(cr); datatable(signif(cr$p, 3), options = list(scrollX = TRUE)) })
  output$cor_notes <- renderUI(note(sprintf("%s correlation shown. Values near ±1 are strong, near 0 weak. Correlation is not causation, and %s only captures %s relationships.",
      c(pearson = "Pearson", spearman = "Spearman", kendall = "Kendall")[[input$cor_method %||% "pearson"]],
      input$cor_method %||% "pearson",
      if ((input$cor_method %||% "pearson") == "pearson") "linear" else "monotonic"), "info"))
  cor_plot <- reactive({ cr <- cor_result(); req(cr)
    long <- as.data.frame(as.table(cr$r)); names(long) <- c("V1", "V2", "r"); long$lab <- sprintf("%.2f", long$r)
    ggplot(long, aes(V1, V2, fill = r)) + geom_tile(colour = "black", linewidth = pt_to_mm(0.6)) +
      geom_text(aes(label = lab), family = pfont(), size = 3.6) +
      scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#0072B2", midpoint = 0, limits = c(-1, 1), name = "r") +
      labs(x = NULL, y = NULL, tag = "A") + coord_fixed() +
      thm() + tagthm() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) })
  output$cor_plot <- renderPlot(cor_plot())
  output$dl_cor <- dl_handler("correlation", cor_plot, 6.5, 6)

  # --- Survival -----------------------------------------------------------
  surv_ok <- reactive(has_pkg("survival"))
  surv_data <- reactive({ req(input$surv_time, input$surv_status)
    cols <- c(input$surv_time, input$surv_status,
              if (nzchar(input$surv_group %||% "")) input$surv_group else NULL)
    d <- rv$data[, cols, drop = FALSE]; d <- d[stats::complete.cases(d), ]
    names(d)[1:2] <- c("time", "status")
    d$status <- as.integer(factor(d$status)) - 1L
    if (ncol(d) == 3) names(d)[3] <- "grp" else d$grp <- factor("All")
    d$grp <- factor(d$grp); d })
  surv_fit_obj <- reactive({ req(surv_ok()); d <- surv_data()
    survival::survfit(survival::Surv(time, status) ~ grp, data = d) })
  output$surv_result <- renderPrint({ if (!surv_ok()) { cat("Install the 'survival' package to enable this module."); return() }
    d <- surv_data(); print(surv_fit_obj())
    if (nlevels(d$grp) > 1) { cat("\n--- Log-rank test ---\n")
      print(survival::survdiff(survival::Surv(time, status) ~ grp, d))
      cat("\n--- Cox proportional hazards ---\n")
      print(summary(survival::coxph(survival::Surv(time, status) ~ grp, d))) } })
  output$surv_interp <- renderUI({ if (!surv_ok()) return(note("The <b>survival</b> package is required for Kaplan-Meier, log-rank, and Cox models. Install it with install.packages('survival').", "warn"))
    d <- surv_data()
    if (nlevels(d$grp) < 2) return(note("Add a grouping factor to compare survival curves with a log-rank test and hazard ratios.", "info"))
    lr <- survival::survdiff(survival::Surv(time, status) ~ grp, d)
    pv <- stats::pchisq(lr$chisq, length(lr$n) - 1, lower.tail = FALSE); ip <- interp_p(pv)
    tagList(note(sprintf("Log-rank test: %s", ip$txt), ip$sev),
            note("The Cox hazard ratio (HR) below compares instantaneous event risk between groups: HR &gt; 1 = higher risk. It assumes hazards stay proportional over time.", "info")) })
  surv_plot <- reactive({ req(surv_ok()); sf <- surv_fit_obj()
    st <- if (is.null(sf$strata)) rep("All", length(sf$time)) else rep(sub(".*=", "", names(sf$strata)), sf$strata)
    df <- data.frame(time = sf$time, surv = sf$surv, grp = st)
    starts <- data.frame(time = 0, surv = 1, grp = unique(st))
    df <- rbind(starts, df)
    ggplot(df, aes(time, surv, colour = grp)) + geom_step(linewidth = pt_to_mm(1.6)) +
      labs(x = "Time", y = "Survival probability", colour = input$surv_group, tag = "A") + pub_colour() +
      locked_axis("x", c(0, df$time), n = 6) + locked_axis("y", c(0, 1), n = 5) +
      thm() + tagthm() +
      (if (nlevels(surv_data()$grp) < 2) guides(colour = "none") else NULL) })
  output$surv_plot <- renderPlot({ if (!surv_ok()) return(NULL); surv_plot() })
  output$dl_surv <- dl_handler("survival", surv_plot)

  # --- Diversity ----------------------------------------------------------
  output$div_vars_ui <- renderUI(checkboxGroupInput("div_vars", "Species / abundance columns",
                                   choices = numeric_vars(), selected = grep("^Sp", numeric_vars(), value = TRUE) %||% numeric_vars()))
  div_table <- reactive({ req(input$div_vars); if (length(input$div_vars) < 2) return(NULL)
    if (!has_pkg("vegan")) return(NULL)
    m <- rv$data[, input$div_vars, drop = FALSE]; keep <- stats::complete.cases(m); m <- m[keep, ]
    S <- vegan::specnumber(m); H <- vegan::diversity(m, "shannon")
    tab <- data.frame(Sample = seq_len(nrow(m)),
      Richness = S, Shannon = round(H, 3), Simpson = round(vegan::diversity(m, "simpson"), 3),
      InvSimpson = round(vegan::diversity(m, "invsimpson"), 3), Evenness = round(H / log(S), 3))
    if (nzchar(input$div_group %||% "")) tab$Group <- rv$data[[input$div_group]][keep]
    tab })
  output$div_tbl <- renderDT({ t <- div_table()
    if (is.null(t)) return(datatable(data.frame(Message = "Select 2+ species columns; requires the 'vegan' package.")))
    datatable(t, options = list(scrollX = TRUE, pageLength = 12), rownames = FALSE) })
  output$div_notes <- renderUI({ if (!has_pkg("vegan")) return(note("Install the <b>vegan</b> package for diversity indices, ordination and community tests.", "warn"))
    note("Richness counts species; Shannon and Simpson also weight relative abundance; evenness (Pielou J) is 1 when all species are equally common. Compare indices across habitats below.", "info") })
  div_plot <- reactive({ t <- div_table(); req(t, "Group" %in% names(t))
    t$Group <- factor(t$Group); idx <- input$div_index
    ggplot(t, aes(Group, .data[[idx]], fill = Group)) +
      geom_boxplot(outlier.shape = NA, width = 0.55, colour = "black", linewidth = pt_to_mm(1.0), alpha = 0.35) +
      geom_jitter(aes(colour = Group), width = 0.12, height = 0, size = 1.9, alpha = 0.8, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.2, fill = "white", colour = "black", stroke = pt_to_mm(1.0)) +
      labs(x = input$div_group, y = idx, tag = "A") + pub_colour() +
      locked_axis("y", t[[idx]], n = 6, pad = 0.03) + guides(fill = "none") +
      thm() + tagthm() })
  output$div_plot <- renderPlot({ t <- div_table()
    if (is.null(t) || !"Group" %in% names(t))
      return(ggplot() + annotate("text", 0, 0, label = "Choose a grouping factor to compare diversity across groups.",
                                 family = pfont(), size = 5) + theme_void())
    div_plot() })
  output$dl_div <- dl_handler(function() paste0("diversity_", input$div_index), div_plot)

  # --- Community comparison ----------------------------------------------
  output$beta_vars_ui <- renderUI(checkboxGroupInput("beta_vars", "Species / abundance columns",
                                    choices = numeric_vars(), selected = grep("^Sp", numeric_vars(), value = TRUE) %||% numeric_vars()))
  output$mantel_ui <- renderUI(checkboxGroupInput("mantel_env", "Environmental columns (Mantel)",
                                 choices = numeric_vars(), selected = setdiff(grep("^Sp", numeric_vars(), value = TRUE, invert = TRUE), input$beta_group)))
  beta_bits <- reactive({ req(input$beta_vars, input$beta_group); has_pkg("vegan") || return(NULL)
    if (length(input$beta_vars) < 2) return(NULL)
    cols <- c(input$beta_vars, input$beta_group)
    d <- rv$data[stats::complete.cases(rv$data[cols]), cols, drop = FALSE]
    list(m = d[, input$beta_vars, drop = FALSE], g = factor(d[[input$beta_group]]), rows = rownames(d)) })
  output$beta_result <- renderPrint({ b <- beta_bits()
    if (is.null(b)) { cat("Select 2+ species columns and a grouping factor; requires 'vegan'."); return() }
    dm <- vegan::vegdist(b$m, method = input$beta_dist)
    if (input$beta_test == "permanova") print(vegan::adonis2(dm ~ b$g))
    else print(vegan::anosim(dm, b$g)) })
  output$beta_interp <- renderUI({ b <- beta_bits(); if (is.null(b)) return(note("PERMANOVA / ANOSIM test whether whole-community composition differs between groups. Requires the <b>vegan</b> package and 2+ species columns.", "warn"))
    dm <- vegan::vegdist(b$m, method = input$beta_dist)
    pv <- if (input$beta_test == "permanova") vegan::adonis2(dm ~ b$g)$`Pr(>F)`[1] else vegan::anosim(dm, b$g)$signif
    ip <- interp_p(pv)
    tagList(note(sprintf("%s on %s distances: %s", toupper(input$beta_test), input$beta_dist, ip$txt), ip$sev),
            note("Significant separation can also arise from differences in within-group dispersion — check with vegan::betadisper.", "info")) })
  output$mantel_result <- renderPrint({ b <- beta_bits()
    if (is.null(b) || is.null(input$mantel_env) || length(input$mantel_env) < 1) { cat("Select environmental columns to correlate community and environmental distance matrices."); return() }
    env <- rv$data[as.integer(b$rows), input$mantel_env, drop = FALSE]
    ok <- stats::complete.cases(env)
    print(vegan::mantel(vegan::vegdist(b$m[ok, , drop = FALSE], input$beta_dist),
                        stats::dist(scale(env[ok, , drop = FALSE])))) })

  # --- Ordination ---------------------------------------------------------
  output$ord_vars_ui <- renderUI(checkboxGroupInput("ord_vars", "Matrix variables",
                                   choices = numeric_vars(), selected = numeric_vars()))
  output$ord_group_ui <- renderUI(selectInput("ord_group", "Grouping (colour)", choices = c("None" = "", factor_vars())))
  output$ord_constrain_ui <- renderUI(if (input$ord_method == "rda")
    checkboxGroupInput("ord_constrain", "Constraining variables (RDA)", choices = numeric_vars()))
  output$ord_notes <- renderUI(note("Ordination compresses many variables into 2 axes so you can see structure. PCA/RDA suit continuous data; CA/DCA/NMDS suit species-abundance tables. Check the variance explained (or NMDS stress &lt; 0.2).", "info"))

  ord_model <- reactive({ req(input$ord_vars); if (length(input$ord_vars) < 2) return(NULL)
    df <- rv$data; m <- df[, input$ord_vars, drop = FALSE]; keep <- stats::complete.cases(m); m <- m[keep, , drop = FALSE]
    grp <- if (nzchar(input$ord_group %||% "")) factor(df[[input$ord_group]][keep]) else NULL
    shp <- if (nzchar(input$ord_shape %||% "")) factor(df[[input$ord_shape]][keep]) else NULL
    method <- input$ord_method
    if (method != "pca" && !has_pkg("vegan")) return(list(error = "The 'vegan' package is required for this method."))
    res <- tryCatch(switch(method,
      pca = { pc <- stats::prcomp(m, scale. = isTRUE(input$ord_scale))
        ve <- (pc$sdev^2 / sum(pc$sdev^2))[1:2] * 100
        list(scores = as.data.frame(pc$x[, 1:2]), axes = c("PC1", "PC2"), ve = ve, obj = pc) },
      pcoa = { D <- vegan::vegdist(m, method = input$nmds_dist)
        pc <- stats::cmdscale(D, k = 2, eig = TRUE); pos <- pc$eig[pc$eig > 0]
        ve <- pc$eig[1:2] / sum(pos) * 100
        list(scores = as.data.frame(pc$points), axes = c("PCoA1", "PCoA2"), ve = ve, obj = pc) },
      ca = { ca <- vegan::cca(m); sc <- vegan::scores(ca, display = "sites", choices = 1:2)
        ve <- ca$CA$eig[1:2] / sum(ca$CA$eig) * 100
        list(scores = as.data.frame(sc), axes = c("CA1", "CA2"), ve = ve, obj = ca) },
      dca = { dc <- vegan::decorana(m); sc <- vegan::scores(dc, display = "sites", choices = 1:2)
        list(scores = as.data.frame(sc), axes = c("DCA1", "DCA2"), ve = c(NA, NA), obj = dc) },
      nmds = { nm <- vegan::metaMDS(m, distance = input$nmds_dist, trace = 0, autotransform = FALSE)
        list(scores = as.data.frame(vegan::scores(nm, display = "sites")), axes = c("NMDS1", "NMDS2"),
             ve = c(NA, NA), stress = nm$stress, obj = nm) },
      rda = { req(input$ord_constrain); env <- df[keep, input$ord_constrain, drop = FALSE]
        rd <- vegan::rda(m ~ ., data = env); sc <- as.data.frame(vegan::scores(rd, display = "sites", choices = 1:2))
        ve <- summary(rd)$cont$importance[2, 1:2] * 100
        list(scores = sc, axes = c("RDA1", "RDA2"), ve = ve, obj = rd) }),
      error = function(e) list(error = conditionMessage(e)))
    if (!is.null(res$error)) return(res)
    names(res$scores)[1:2] <- c("Dim1", "Dim2"); res$group <- grp; res$shape <- shp; res })

  output$ord_summary <- renderPrint({ res <- ord_model(); req(res)
    if (!is.null(res$error)) { cat("Error:", res$error, "\n"); return(invisible()) }
    cat("Method:", toupper(input$ord_method), "\n")
    if (!any(is.na(res$ve))) cat(sprintf("%s: %.1f%%   %s: %.1f%%\n", res$axes[1], res$ve[1], res$axes[2], res$ve[2]))
    if (!is.null(res$stress)) cat(sprintf("NMDS stress: %.4f %s\n", res$stress,
                                          if (res$stress < 0.2) "(acceptable)" else "(high — interpret cautiously)"))
    cat("\n")
    if (input$ord_method == "pcoa") {
      eig <- res$obj$eig; pos <- eig[eig > 0]
      cat("Principal coordinates analysis (classical MDS)\n")
      cat("Positive eigenvalues:", length(pos), "of", length(eig), "\n")
      cat("Variance explained by first 5 axes:\n")
      print(round(head(pos / sum(pos) * 100, 5), 2))
      if (!is.null(res$obj$GOF)) cat("Goodness of fit:", round(res$obj$GOF[1], 3), "\n")
    } else print(res$obj) })

  ord_plot <- reactive({ res <- ord_model(); req(res); is.null(res$error) || return(NULL)
    d <- res$scores; d$grp <- if (!is.null(res$group)) res$group else factor("All")
    has_shape <- !is.null(res$shape); d$shp <- if (has_shape) res$shape else factor("All")
    xlab <- if (!is.na(res$ve[1])) sprintf("%s (%.1f%%)", res$axes[1], res$ve[1]) else res$axes[1]
    ylab <- if (!is.na(res$ve[2])) sprintf("%s (%.1f%%)", res$axes[2], res$ve[2]) else res$axes[2]
    p <- ggplot(d, aes(Dim1, Dim2, colour = grp, fill = grp)) +
      geom_hline(yintercept = 0, colour = "grey75", linewidth = pt_to_mm(0.5)) +
      geom_vline(xintercept = 0, colour = "grey75", linewidth = pt_to_mm(0.5)) +
      geom_point(aes(shape = shp), size = 2.6, alpha = 0.85) +
      pub_shape_scale(input$ord_shape) +
      labs(x = xlab, y = ylab, tag = "A", colour = input$ord_group, fill = input$ord_group) + pub_colour() +
      locked_axis("x", d$Dim1, n = 6, pad = 0.03) + locked_axis("y", d$Dim2, n = 6, pad = 0.03) +
      thm() + tagthm()
    if (!has_shape) p <- p + guides(shape = "none")
    if (nlevels(d$grp) > 1) p <- p + stat_ellipse(type = "norm", linewidth = pt_to_mm(1.0), show.legend = FALSE)
    else p <- p + guides(colour = "none", fill = "none")
    p })
  output$ord_plot <- renderPlot(ord_plot())
  output$dl_ord <- dl_handler(function() paste0("ordination_", input$ord_method), ord_plot, 6.5, 6)

  # --- Bioinformatics: heatmap -------------------------------------------
  pending_hm <- reactiveVal(NULL)   # feature set handed over from the volcano
  output$hm_vars_ui <- renderUI({
    sel <- intersect(pending_hm() %||% character(0), numeric_vars())
    if (!length(sel)) sel <- grep("^Sp", numeric_vars(), value = TRUE) %||% numeric_vars()
    checkboxGroupInput("hm_vars", "Features (numeric columns)", choices = numeric_vars(), selected = sel)
  })
  hm_input <- reactive({ req(input$hm_vars, rv$data)
    vars <- intersect(input$hm_vars, names(rv$data)); if (length(vars) < 2) return(NULL)
    df <- rv$data; m <- df[, vars, drop = FALSE]; keep <- stats::complete.cases(m)
    grp <- if (nzchar(input$hm_group %||% "")) df[[input$hm_group]][keep] else NULL
    list(m = m[keep, , drop = FALSE], grp = grp) })
  hm_plot <- reactive({ h <- hm_input(); req(h)
    heatmap_figure(h$m, group = h$grp, scale = input$hm_scale,
                   cluster_samples = isTRUE(input$hm_cluster_s),
                   cluster_features = isTRUE(input$hm_cluster_f),
                   show_values = isTRUE(input$hm_values),
                   row_dendro = isTRUE(input$hm_row_dendro),
                   font = pfont(), base_size = psize(), tags = ptags()) })
  output$hm_plot <- renderPlot({ if (is.null(hm_input()))
      return(ggplot() + annotate("text", 0, 0, label = "Select at least 2 feature columns.", family = pfont(), size = 5) + theme_void())
    hm_plot() })
  output$hm_notes <- renderUI(note("Rows are features, columns are samples. Z-scoring each feature (default) puts every variable on a common scale so colour reflects relative high/low, not absolute magnitude. Clustering groups similar samples/features together; the top dendrogram shows how samples relate. Orange = above average, blue = below.", "info"))
  output$dl_hm <- dl_handler("heatmap", hm_plot, width = 8, height = 6.5)

  # --- Bioinformatics: differential expression ---------------------------
  output$de_vars_ui <- renderUI(checkboxGroupInput("de_vars", "Features to test",
                                  choices = numeric_vars(),
                                  selected = grep("^Sp", numeric_vars(), value = TRUE) %||% numeric_vars()))
  de_data <- reactive({ req(input$de_vars, input$de_group, rv$data)
    vars <- intersect(input$de_vars, names(rv$data))
    req(length(vars) >= 1, input$de_group %in% names(rv$data))
    df <- rv$data; cols <- c(vars, input$de_group)
    d <- df[stats::complete.cases(df[cols]), cols, drop = FALSE]
    g <- factor(d[[input$de_group]]); req(nlevels(g) == 2)
    list(X = as.matrix(d[, vars, drop = FALSE]), g = g, lv = levels(g)) })
  de_res <- reactive({ dd <- de_data(); req(dd)
    r <- de_table(dd$X, dd$g, method = input$de_method)
    r$effect <- if (all(is.finite(r$log2FC))) r$log2FC else r$meanDiff
    r$effect_name <- if (all(is.finite(r$log2FC))) "log2 fold change" else "mean difference"
    fc <- input$de_fc %||% 1; al <- input$de_alpha %||% 0.05
    r$Change <- ifelse(r$p_adj < al & r$effect >= fc, "Up",
                ifelse(r$p_adj < al & r$effect <= -fc, "Down", "n.s."))
    # Average expression for the MA plot (mean log intensity when positive).
    r$A <- if (all(r$mean_1 > 0 & r$mean_2 > 0)) 0.5 * (log2(r$mean_1) + log2(r$mean_2))
           else 0.5 * (r$mean_1 + r$mean_2)
    r })
  output$de_tbl <- renderDT({ r <- de_res(); req(r)
    tab <- r[order(r$p_adj), c("Feature", "mean_1", "mean_2", "log2FC", "meanDiff", "p", "p_adj", "Change")]
    tab[2:7] <- lapply(tab[2:7], signif, 4)
    datatable(tab, options = list(pageLength = 12, scrollX = TRUE), rownames = FALSE) })
  output$de_notes <- renderUI({ dd <- de_data(); r <- de_res(); req(dd, r)
    nsig <- sum(r$Change != "n.s.", na.rm = TRUE)
    tagList(
      note(sprintf("Each feature is compared between <b>%s</b> and <b>%s</b> with a %s and Benjamini-Hochberg correction for testing many features at once.",
                   dd$lv[1], dd$lv[2], if (input$de_method == "t") "t-test" else "Mann-Whitney test"), "info"),
      note(sprintf("<b>%d</b> feature(s) pass both thresholds (adjusted p &lt; %.2g and |%s| ≥ %.2g): orange = higher in %s, blue = higher in %s.",
                   nsig, input$de_alpha %||% 0.05, r$effect_name[1], input$de_fc %||% 1, dd$lv[2], dd$lv[1]),
           if (nsig > 0) "ok" else "warn")) })
  de_plot <- reactive({ r <- de_res(); req(r)
    r <- r[is.finite(r$effect) & is.finite(r$p_adj), ]
    r$logp <- -log10(pmax(r$p_adj, .Machine$double.xmin))
    fc <- input$de_fc %||% 1; al <- input$de_alpha %||% 0.05
    cols <- c(Up = "#D55E00", Down = "#0072B2", "n.s." = "grey70")
    if ((input$de_plottype %||% "volcano") == "ma") {
      r <- r[is.finite(r$A), ]
      xr <- range(r$A) + c(-1, 1) * 0.05 * diff(range(r$A))
      yr <- range(c(r$effect, -fc, fc)); yr <- yr + c(-1, 1) * 0.10 * diff(yr)
      p <- ggplot(r, aes(A, effect, colour = Change)) +
        geom_hline(yintercept = 0, colour = "grey70", linewidth = pt_to_mm(0.8)) +
        geom_hline(yintercept = c(-fc, fc), linetype = "dashed", colour = "grey60", linewidth = pt_to_mm(0.8)) +
        geom_point(size = 2, alpha = 0.8) + scale_colour_manual(values = cols, name = NULL) +
        labs(x = "Average expression (A)", y = sprintf("%s (M)", r$effect_name[1]), tag = "A") +
        locked_axis("x", xr, n = 6) + locked_axis("y", yr, n = 6) + thm() + tagthm()
      lab_aes <- aes(A, effect, label = Feature)
    } else {
      xr <- range(c(r$effect, -fc, fc)); xr <- xr + c(-1, 1) * 0.10 * diff(xr)
      yr <- c(0, max(c(r$logp, -log10(al))) * 1.15)
      p <- ggplot(r, aes(effect, logp, colour = Change)) +
        geom_vline(xintercept = c(-fc, fc), linetype = "dashed", colour = "grey60", linewidth = pt_to_mm(0.8)) +
        geom_hline(yintercept = -log10(al), linetype = "dashed", colour = "grey60", linewidth = pt_to_mm(0.8)) +
        geom_point(size = 2, alpha = 0.8) + scale_colour_manual(values = cols, name = NULL) +
        labs(x = r$effect_name[1], y = expression(-log[10]~"adjusted p"), tag = "A") +
        locked_axis("x", xr, n = 6) + locked_axis("y", yr, n = 6) + thm() + tagthm()
      lab_aes <- aes(effect, logp, label = Feature)
    }
    nlab <- input$de_label %||% 0
    if (nlab > 0 && has_pkg("ggrepel")) { top <- r[r$Change != "n.s.", ]
      top <- head(top[order(top$p_adj), ], nlab)
      if (nrow(top)) p <- p + ggrepel::geom_text_repel(data = top, mapping = lab_aes,
        family = pfont(), size = 3.4, colour = "black", max.overlaps = 50, min.segment.length = 0) }
    p })
  output$de_plot <- renderPlot(de_plot())
  output$dl_volcano <- dl_handler(function() paste0("de_", input$de_plottype %||% "volcano"),
                                  de_plot, width = 6.5, height = 5.5)
  output$dl_de_csv <- downloadHandler(function() "differential_expression.csv",
    function(f) utils::write.csv(de_res()[order(de_res()$p_adj), ], f, row.names = FALSE))

  # Frictionless handoff: push the significant features into the Heatmap tab.
  observeEvent(input$de_to_heatmap, {
    r <- tryCatch(de_res(), error = function(e) NULL); req(r)
    sig <- r$Feature[r$Change != "n.s." & !is.na(r$Change)]
    if (!length(sig)) { showNotification("No significant features to send yet.", type = "warning"); return() }
    pending_hm(sig)
    if (nzchar(input$de_group %||% "")) updateSelectInput(session, "hm_group", selected = input$de_group)
    updateNavbarPage(session, "nav", selected = "Heatmap")
    showNotification(sprintf("Sent %d significant feature(s) to the Heatmap tab.", length(sig)), type = "message")
  })

  # --- Bioinformatics: sample clustering ---------------------------------
  output$cl_vars_ui <- renderUI(checkboxGroupInput("cl_vars", "Features (numeric columns)",
                                  choices = numeric_vars(),
                                  selected = grep("^Sp", numeric_vars(), value = TRUE) %||% numeric_vars()))
  cl_data <- reactive({ req(input$cl_vars, rv$data)
    vars <- intersect(input$cl_vars, names(rv$data)); if (length(vars) < 2) return(NULL)
    df <- rv$data; m <- df[, vars, drop = FALSE]; keep <- stats::complete.cases(m); m <- m[keep, , drop = FALSE]
    grp <- if (nzchar(input$cl_group %||% "")) factor(df[[input$cl_group]][keep]) else NULL
    list(m = scale(as.matrix(m)), grp = grp,
         labels = (rownames(df)[keep]) %||% as.character(which(keep))) })
  cl_plot <- reactive({ cd <- cl_data(); req(cd); req(has_pkg("ggdendro"))
    hc <- stats::hclust(stats::dist(cd$m, method = input$cl_dist), method = input$cl_link)
    dd <- ggdendro::dendro_data(hc, type = "rectangle")
    lab <- dd$labels; lab$grp <- if (!is.null(cd$grp)) cd$grp[hc$order] else factor("All")
    p <- ggplot() +
      geom_segment(data = dd$segments, aes(x = x, y = y, xend = xend, yend = yend),
                   colour = "black", linewidth = pt_to_mm(0.8)) +
      geom_point(data = lab, aes(x = x, y = 0, colour = grp), size = 2.6) +
      labs(x = NULL, y = "Height", tag = "A", colour = input$cl_group) + pub_colour() +
      scale_x_continuous(breaks = NULL, expand = expansion(mult = 0.02)) +
      locked_axis("y", c(0, max(dd$segments$y) * 1.05), n = 6) +
      thm() + tagthm() +
      theme(axis.ticks.x = element_blank())
    if (is.null(cd$grp)) p <- p + guides(colour = "none")
    p })
  output$cl_plot <- renderPlot({ if (is.null(cl_data()))
      return(ggplot() + annotate("text", 0, 0, label = "Select at least 2 feature columns.", family = pfont(), size = 5) + theme_void())
    cl_plot() })
  output$cl_notes <- renderUI(note("Samples are standardized, then grouped by similarity: samples joined lower in the tree are more alike. Colour the tips by a known group to see whether your samples cluster the way you expect. Ward or complete linkage give compact, interpretable clusters.", "info"))
  output$dl_cl <- dl_handler("clustering", cl_plot, width = 7, height = 5)

  # --- Compare methods ----------------------------------------------------
  output$cmp_ord_vars_ui <- renderUI(checkboxGroupInput("cmp_ord_vars", "Matrix variables",
                                        choices = numeric_vars(),
                                        selected = grep("^Sp", numeric_vars(), value = TRUE) %||% numeric_vars()))

  ord_panel <- function(scores, xlab, ylab, grp, tag, shp = NULL) {
    d <- as.data.frame(scores); names(d)[1:2] <- c("Dim1", "Dim2")
    d$grp <- if (!is.null(grp)) grp else factor("All")
    has_shape <- !is.null(shp); d$shp <- if (has_shape) shp else factor("All")
    p <- ggplot(d, aes(Dim1, Dim2, colour = grp, fill = grp)) +
      geom_hline(yintercept = 0, colour = "grey75", linewidth = pt_to_mm(0.5)) +
      geom_vline(xintercept = 0, colour = "grey75", linewidth = pt_to_mm(0.5)) +
      geom_point(aes(shape = shp), size = 2.4, alpha = 0.85) +
      pub_shape_scale(input$cmp_ord_shape) +
      labs(x = xlab, y = ylab, tag = tag) + pub_colour() +
      locked_axis("x", d$Dim1, n = 5, pad = 0.03) + locked_axis("y", d$Dim2, n = 5, pad = 0.03) +
      thm() + tagthm()
    if (!has_shape) p <- p + guides(shape = "none")
    if (nlevels(d$grp) > 1) p <- p + stat_ellipse(type = "norm", linewidth = pt_to_mm(0.9), show.legend = FALSE)
    else p <- p + guides(colour = "none", fill = "none")
    p
  }

  cmp_ord <- reactive({ req(input$cmp_ord_vars); has_pkg("vegan") || return(NULL)
    if (length(input$cmp_ord_vars) < 3) return(NULL)
    df <- rv$data; m <- df[, input$cmp_ord_vars, drop = FALSE]
    keep <- stats::complete.cases(m); m <- m[keep, , drop = FALSE]
    grp <- if (nzchar(input$cmp_ord_group %||% "")) factor(df[[input$cmp_ord_group]][keep]) else NULL
    shp <- if (nzchar(input$cmp_ord_shape %||% "")) factor(df[[input$cmp_ord_shape]][keep]) else NULL
    tryCatch({
      pca <- stats::prcomp(m, scale. = TRUE)
      pca_ve <- (pca$sdev^2 / sum(pca$sdev^2))[1:2] * 100
      D <- vegan::vegdist(m, method = input$cmp_dist)
      pco <- stats::cmdscale(D, k = 2, eig = TRUE); pos <- pco$eig[pco$eig > 0]
      pco_ve <- pco$eig[1:2] / sum(pos) * 100
      nmds <- vegan::metaMDS(m, distance = input$cmp_dist, trace = 0, autotransform = FALSE)
      nmds_sc <- vegan::scores(nmds, display = "sites")
      pr_pco_nmds <- vegan::protest(pco$points, nmds_sc, permutations = 199)
      pr_pca_pco  <- vegan::protest(pca$x[, 1:2], pco$points, permutations = 199)
      list(pca = pca$x[, 1:2], pca_ve = pca_ve, pco = pco$points, pco_ve = pco_ve,
           nmds = nmds_sc, stress = nmds$stress, grp = grp, shp = shp,
           pr_pco_nmds = pr_pco_nmds, pr_pca_pco = pr_pca_pco)
    }, error = function(e) list(error = conditionMessage(e))) })

  cmp_grp <- reactive({ req(input$cmp_y, input$cmp_g)
    d <- rv$data[, c(input$cmp_y, input$cmp_g)]; names(d) <- c("y", "g")
    d <- d[stats::complete.cases(d), ]; d$g <- factor(d$g); req(nlevels(d$g) >= 2); d })

  output$cmp_tbl <- renderDT({
    if (input$cmp_type == "ord") { r <- cmp_ord()
      if (is.null(r)) return(datatable(data.frame(Message = "Select 3+ matrix variables; requires the 'vegan' package.")))
      if (!is.null(r$error)) return(datatable(data.frame(Error = r$error)))
      tab <- data.frame(
        Method = c("PCA", "PCoA", "NMDS"),
        Basis = c("Euclidean (scaled)", paste(input$cmp_dist, "distance"), paste(input$cmp_dist, "distance")),
        Axis1 = c(sprintf("%.1f%%", r$pca_ve[1]), sprintf("%.1f%%", r$pco_ve[1]), "-"),
        Axis2 = c(sprintf("%.1f%%", r$pca_ve[2]), sprintf("%.1f%%", r$pco_ve[2]), "-"),
        Fit = c("-", "-", sprintf("stress %.3f", r$stress)), check.names = FALSE)
      return(datatable(tab, options = list(dom = "t"), rownames = FALSE)) }
    if (input$cmp_type == "grp") { d <- cmp_grp(); k <- nlevels(d$g)
      if (k == 2) { lv <- levels(d$g); x <- d$y[d$g == lv[1]]; y <- d$y[d$g == lv[2]]
        t1 <- stats::t.test(y ~ g, d); t2 <- stats::wilcox.test(y ~ g, d, exact = FALSE)
        dd <- cohens_d(x, y); rb <- rank_biserial(unname(t2$statistic), length(x), length(y))
        tab <- data.frame(Test = c("Welch t-test", "Mann-Whitney U"),
          p_value = signif(c(t1$p.value, t2$p.value), 4),
          Effect = c(sprintf("d = %.2f (%s)", dd, d_magnitude(dd)),
                     sprintf("r = %.2f (%s)", rb, r_magnitude(rb))))
      } else { a <- stats::aov(y ~ g, d); pa <- summary(a)[[1]][["Pr(>F)"]][1]
        kw <- stats::kruskal.test(y ~ g, d)
        e2 <- eta_sq_aov(a); ek <- epsilon_sq_kw(unname(kw$statistic), nrow(d))
        tab <- data.frame(Test = c("One-way ANOVA", "Kruskal-Wallis"),
          p_value = signif(c(pa, kw$p.value), 4),
          Effect = c(sprintf("eta2 = %.3f (%s)", e2, eta_magnitude(e2)),
                     sprintf("eps2 = %.3f (%s)", ek, eta_magnitude(ek)))) }
      return(datatable(tab, options = list(dom = "t"), rownames = FALSE)) }
    # correlation
    req(input$cmp_x1, input$cmp_x2)
    d <- rv$data[, c(input$cmp_x1, input$cmp_x2)]; d <- d[stats::complete.cases(d), ]
    pe <- stats::cor.test(d[[1]], d[[2]], method = "pearson")
    sp <- stats::cor.test(d[[1]], d[[2]], method = "spearman", exact = FALSE)
    tab <- data.frame(Method = c("Pearson (linear)", "Spearman (monotonic rank)"),
      Coefficient = round(c(pe$estimate, sp$estimate), 3),
      p_value = signif(c(pe$p.value, sp$p.value), 4),
      Strength = c(r_magnitude(pe$estimate), r_magnitude(sp$estimate)))
    datatable(tab, options = list(dom = "t"), rownames = FALSE) })

  output$cmp_notes <- renderUI({
    if (input$cmp_type == "ord") { r <- cmp_ord()
      if (is.null(r) || !is.null(r$error)) return(note("Select 3+ community/matrix columns. PCA works on scaled Euclidean distance (best for linear gradients); PCoA and NMDS work on the ecological distance you choose (better for species data with many zeros).", "info"))
      c1 <- r$pr_pca_pco$t0; c2 <- r$pr_pco_nmds$t0
      tagList(
        note(sprintf("<b>Procrustes correlation</b> measures how similarly two methods arrange the samples (1 = identical layout). PCA vs PCoA: <b>%.2f</b>; PCoA vs NMDS: <b>%.2f</b>.", c1, c2), "info"),
        note(sprintf("NMDS stress = %.3f (%s). Rule of thumb: &lt; 0.1 excellent, &lt; 0.2 usable, &gt; 0.3 unreliable.", r$stress,
                     if (r$stress < 0.1) "excellent" else if (r$stress < 0.2) "good" else "high"),
             if (r$stress < 0.2) "ok" else "warn"),
        note("If the three panels look alike (high Procrustes correlation), your conclusions are robust to method choice. PCA on abundances is prone to the horseshoe artifact — divergence from PCoA/NMDS is a sign to prefer a distance-based method.", "info")) }
    else if (input$cmp_type == "grp")
      note("The parametric test (t / ANOVA) has more power when its normality and equal-variance assumptions hold; the rank-based test (Mann-Whitney / Kruskal-Wallis) is robust when they don't. If the two p-values land on the same side of 0.05, your conclusion is solid. If they disagree, trust the non-parametric test for skewed or small samples.", "info")
    else
      note("Pearson captures straight-line association and assumes roughly normal data; Spearman captures any monotonic (consistently increasing/decreasing) relationship and resists outliers. A large gap between them signals non-linearity or influential outliers — inspect the scatter below.", "info") })

  cmp_plot <- reactive({
    if (input$cmp_type == "ord") { r <- cmp_ord(); req(r); is.null(r$error) || return(NULL)
      req(has_pkg("patchwork"))
      pa <- ord_panel(r$pca, sprintf("PC1 (%.1f%%)", r$pca_ve[1]), sprintf("PC2 (%.1f%%)", r$pca_ve[2]), r$grp, "A", r$shp)
      pb <- ord_panel(r$pco, sprintf("PCoA1 (%.1f%%)", r$pco_ve[1]), sprintf("PCoA2 (%.1f%%)", r$pco_ve[2]), r$grp, "B", r$shp)
      pc <- ord_panel(r$nmds, "NMDS1", "NMDS2", r$grp, "C", r$shp)
      return(patchwork::wrap_plots(pa, pb, pc, nrow = 1) +
             patchwork::plot_layout(guides = "collect")) }
    if (input$cmp_type == "grp") { d <- cmp_grp()
      p <- ggplot(d, aes(g, y, fill = g)) +
        geom_boxplot(outlier.shape = NA, width = 0.55, colour = "black", linewidth = pt_to_mm(1.0), alpha = 0.35) +
        geom_jitter(aes(colour = g), width = 0.12, height = 0, size = 1.7, alpha = 0.8, show.legend = FALSE) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3.2, fill = "white", colour = "black", stroke = pt_to_mm(1.0)) +
        labs(x = input$cmp_g, y = input$cmp_y, tag = "A") + pub_colour() +
        locked_axis("y", d$y, n = 6, pad = 0.03) + guides(fill = "none") +
        thm() + tagthm()
      return(p) }
    req(input$cmp_x1, input$cmp_x2)
    d <- rv$data[, c(input$cmp_x1, input$cmp_x2)]; names(d) <- c("x", "y"); d <- d[stats::complete.cases(d), ]
    ggplot(d, aes(x, y)) + geom_point(size = 2, alpha = 0.75, colour = "#0072B2") +
      geom_smooth(method = "lm", se = FALSE, colour = "#D55E00", linewidth = pt_to_mm(1.4)) +
      labs(x = input$cmp_x1, y = input$cmp_x2, tag = "A") +
      locked_axis("x", d$x, n = 6, pad = 0.03) + locked_axis("y", d$y, n = 6, pad = 0.03) +
      thm() + tagthm() })
  output$cmp_plot <- renderPlot(cmp_plot())
  output$dl_cmp <- dl_handler(function() paste0("compare_", input$cmp_type), cmp_plot,
    width = function() if (input$cmp_type == "ord") 13 else 6.5, height = 5)

  # --- Power --------------------------------------------------------------
  pwr_res <- reactive({ if (!has_pkg("pwr")) return(NULL)
    alpha <- input$pwr_alpha; eff <- input$pwr_effect
    n <- if (is.na(input$pwr_n)) NULL else input$pwr_n
    power <- if (is.na(input$pwr_power)) NULL else input$pwr_power
    tryCatch(switch(input$pwr_test,
      t    = pwr::pwr.t.test(n = n, d = eff, sig.level = alpha, power = power),
      anova = pwr::pwr.anova.test(k = input$pwr_k, n = n, f = eff, sig.level = alpha, power = power),
      prop = pwr::pwr.2p.test(h = eff, n = n, sig.level = alpha, power = power),
      cor  = pwr::pwr.r.test(n = n, r = eff, sig.level = alpha, power = power)),
      error = function(e) e) })
  output$pwr_result <- renderPrint({ if (!has_pkg("pwr")) { cat("Install the 'pwr' package for power analysis."); return() }
    r <- pwr_res(); if (inherits(r, "error")) cat("Could not solve — leave exactly one of n / power blank.\n") else print(r) })
  output$pwr_interp <- renderUI({ if (!has_pkg("pwr")) return(note("The <b>pwr</b> package is required. install.packages('pwr').", "warn"))
    r <- pwr_res()
    conv <- switch(input$pwr_test,
      t = "Cohen's d: 0.2 small, 0.5 medium, 0.8 large.",
      anova = "Cohen's f: 0.1 small, 0.25 medium, 0.4 large.",
      prop = "Cohen's h: 0.2 small, 0.5 medium, 0.8 large.",
      cor = "r: 0.1 small, 0.3 medium, 0.5 large.")
    out <- list(note(sprintf("Leave <b>one</b> of sample size or power blank and the tool solves for it. Effect-size guide — %s", conv), "info"))
    if (!inherits(r, "error") && !is.null(r$n))
      out <- c(out, list(note(sprintf("Estimated n = <b>%.0f</b> per group for %.0f%% power.", ceiling(r$n), 100 * (r$power %||% input$pwr_power)), "ok")))
    tagList(out) })
  pwr_plot <- reactive({ req(has_pkg("pwr"))
    eff <- input$pwr_effect; alpha <- input$pwr_alpha; ns <- seq(5, 200, by = 5)
    pw <- vapply(ns, function(n) tryCatch(switch(input$pwr_test,
      t = pwr::pwr.t.test(n = n, d = eff, sig.level = alpha)$power,
      anova = pwr::pwr.anova.test(k = input$pwr_k, n = n, f = eff, sig.level = alpha)$power,
      prop = pwr::pwr.2p.test(h = eff, n = n, sig.level = alpha)$power,
      cor = pwr::pwr.r.test(n = n, r = eff, sig.level = alpha)$power), error = function(e) NA), numeric(1))
    d <- data.frame(n = ns, power = pw)
    ggplot(d, aes(n, power)) + geom_hline(yintercept = 0.8, colour = "#D55E00", linewidth = pt_to_mm(1.2), linetype = "dashed") +
      geom_line(colour = "#0072B2", linewidth = pt_to_mm(1.6)) +
      labs(x = "Sample size per group", y = "Power", tag = "A") +
      locked_axis("x", d$n, n = 6) + locked_axis("y", c(0, 1), n = 5) +
      thm() + tagthm() })
  output$pwr_plot <- renderPlot({ if (!has_pkg("pwr")) return(NULL); pwr_plot() })
}

shinyApp(ui, server)

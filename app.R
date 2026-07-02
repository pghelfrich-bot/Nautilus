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

pub_colour <- function(name = NULL) {
  list(scale_colour_manual(values = wong_values(64), na.value = "grey40", name = name),
       scale_fill_manual(values = wong_values(64), na.value = "grey40", name = name))
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
theme_panel_tag <- function() {
  theme(plot.tag = element_text(face = "bold", family = PUB_FONT, size = 16),
        plot.tag.position = c(0.02, 0.98))
}

save_publication <- function(plot, file, width = 6.5, height = 5) {
  ggsave(file, plot = plot, width = width, height = height,
         units = "in", dpi = 300, bg = "white", device = "png")
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
            c("PCA" = "pca", "Correspondence Analysis" = "ca",
              "Detrended CA" = "dca", "NMDS" = "nmds", "Redundancy Analysis" = "rda")),
          uiOutput("ord_vars_ui"), uiOutput("ord_group_ui"), uiOutput("ord_constrain_ui"),
          selectInput("nmds_dist", "NMDS distance", c("bray", "euclidean", "jaccard", "gower")),
          checkboxInput("ord_scale", "Scale variables (PCA)", TRUE),
          downloadButton("dl_ord", "Download figure (300 dpi)")),
        mainPanel(width = 9,
          h4("Ordination summary"), uiOutput("ord_notes"), verbatimTextOutput("ord_summary"),
          tags$hr(), plotOutput("ord_plot", height = "540px"))
      )
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
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(data = NULL)

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

  observeEvent(input$demo_exp, {
    set.seed(1)
    n <- 40
    rv$data <- data.frame(
      Group      = rep(c("Control", "Treatment"), each = n),
      Sex        = sample(c("F", "M"), 2 * n, TRUE),
      Response   = c(rnorm(n, 10, 2), rnorm(n, 12.5, 2.2)),
      Biomarker  = c(rlnorm(n, 1.4, 0.4), rlnorm(n, 1.7, 0.4)),
      Dose       = rep(c(0, 5, 10, 20), length.out = 2 * n),
      Improved   = rbinom(2 * n, 1, rep(c(0.35, 0.65), each = n)),
      SurvTime   = round(c(rexp(n, 0.10), rexp(n, 0.06)), 1),
      Event      = rbinom(2 * n, 1, 0.75),
      Counts     = rpois(2 * n, rep(c(3, 6), each = n))
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
    upd("cor_partial", c("None" = "", nv))
    upd("surv_time", nv, nv[1]); upd("surv_status", c(bv, nv), bv[1] %||% nv[1])
    upd("surv_group", c("None" = "", fv))
    upd("div_group", c("None" = "", fv)); upd("beta_group", fv, fv[1])
    upd("ord_group", c("None" = "", fv))
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
        pub_colour() + theme_publication() + theme_panel_tag()
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
      theme_publication() + theme_panel_tag()
    if (is.null(grp)) p <- p + guides(fill = "none", colour = "none")
    p })
  output$dist_plot <- renderPlot(dist_plot())
  output$dl_dist <- downloadHandler(function() paste0("distribution_", input$dist_var, ".png"),
                                    function(f) save_publication(dist_plot(), f))

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
    p <- ggplot(d, aes(g, y, fill = g)) +
      geom_boxplot(outlier.shape = NA, width = 0.55, colour = "black", linewidth = pt_to_mm(1.0), alpha = 0.35) +
      geom_jitter(aes(colour = g), width = 0.12, height = 0, size = 1.8, alpha = 0.8, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.4, fill = "white",
                   colour = "black", stroke = pt_to_mm(1.0)) +
      labs(x = input$gc_group, y = input$gc_response, tag = "A") + pub_colour() +
      locked_axis("y", d$y, n = 6, pad = 0.03) + guides(fill = "none") +
      theme_publication() + theme_panel_tag()
    if (isTRUE(input$gc_signif) && nlevels(d$g) == 2 && has_pkg("ggsignif")) {
      pv <- tryCatch(gc_fit()$p.value, error = function(e) NA)
      if (!is.na(pv)) p <- p + ggsignif::geom_signif(comparisons = list(levels(d$g)),
        annotations = sprintf("p = %s  %s", ifelse(pv < 0.001, "<0.001", formatC(pv, format = "f", digits = 3)),
                              p_to_stars(pv)), tip_length = 0.01, family = PUB_FONT, textsize = 4)
    }
    p })
  output$gc_plot <- renderPlot(gc_plot())
  output$dl_box <- downloadHandler(function() paste0("comparison_", input$gc_response, ".png"),
                                   function(f) save_publication(gc_plot(), f))

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
    p <- ggplot(d, aes(g, y, fill = g)) +
      geom_boxplot(outlier.shape = NA, width = 0.6, colour = "black", linewidth = pt_to_mm(1.0), alpha = 0.35) +
      geom_jitter(aes(colour = g), width = 0.12, height = 0, size = 1.7, alpha = 0.8, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.2, fill = "white",
                   colour = "black", stroke = pt_to_mm(1.0)) +
      labs(x = input$av_group, y = input$av_response, tag = "A") + pub_colour() +
      locked_axis("y", d$y, n = 6, pad = 0.03) + guides(fill = "none") +
      theme_publication() + theme_panel_tag()
    if (isTRUE(input$av_signif) && has_pkg("ggsignif")) {
      ph <- av_posthoc()
      if (!is.null(ph) && "p_adj" %in% names(ph)) {
        sig <- ph[ph$p_adj < 0.05, , drop = FALSE]
        if (nrow(sig)) { comps <- strsplit(sig$Comparison, " ?- ?")
          p <- p + ggsignif::geom_signif(comparisons = comps,
            annotations = p_to_stars(sig$p_adj), step_increase = 0.08,
            tip_length = 0.01, family = PUB_FONT, textsize = 5) }
      }
    }
    p })
  output$av_plot <- renderPlot(av_plot())
  output$dl_avbox <- downloadHandler(function() paste0("anova_", input$av_response, ".png"),
                                     function(f) save_publication(av_plot(), f))

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
    p + pub_colour() + theme_publication() + theme_panel_tag() })
  output$tw_plot <- renderPlot(tw_plot())
  output$dl_tw <- downloadHandler(function() "twoway.png", function(f) save_publication(tw_plot(), f))

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
      locked_axis("y", c(0, dd$n), n = 6) + theme_publication() + theme_panel_tag() })
  output$ct_plot <- renderPlot(ct_plot())
  output$dl_ct <- downloadHandler(function() "contingency.png", function(f) save_publication(ct_plot(), f))

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
               family = PUB_FONT, size = 5, hjust = 0) +
      labs(x = "False positive rate (1 - specificity)", y = "True positive rate (sensitivity)", tag = "A") +
      locked_axis("x", c(0, 1), n = 5) + locked_axis("y", c(0, 1), n = 5) +
      coord_fixed() + theme_publication() + theme_panel_tag() })
  output$dx_plot <- renderPlot(dx_plot())
  output$dl_roc <- downloadHandler(function() "roc.png", function(f) save_publication(dx_plot(), f, 6, 6))

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
        theme_publication() + theme_panel_tag()
    } else {
      d <- data.frame(fitted = stats::fitted(fit), resid = stats::resid(fit))
      ggplot(d, aes(fitted, resid)) + geom_hline(yintercept = 0, colour = "grey70", linewidth = pt_to_mm(1.0)) +
        geom_point(size = 2, alpha = 0.75, colour = "#0072B2") +
        labs(x = "Fitted values", y = "Residuals", tag = "A") +
        locked_axis("x", d$fitted, n = 6, pad = 0.03) + locked_axis("y", d$resid, n = 6, pad = 0.03) +
        theme_publication() + theme_panel_tag()
    } })
  output$reg_plot <- renderPlot(reg_plot())
  output$dl_reg <- downloadHandler(function() "regression.png", function(f) save_publication(reg_plot(), f))

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
      theme_publication() + theme_panel_tag() })
  output$logit_plot <- renderPlot({ tryCatch(logit_plot(), error = function(e)
    ggplot() + annotate("text", 0, 0, label = "Probability curve shown for a single numeric predictor.",
                        family = PUB_FONT, size = 5) + theme_void()) })
  output$dl_logit <- downloadHandler(function() "logistic.png", function(f) save_publication(logit_plot(), f))

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
      geom_text(aes(label = lab), family = PUB_FONT, size = 3.6) +
      scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#0072B2", midpoint = 0, limits = c(-1, 1), name = "r") +
      labs(x = NULL, y = NULL, tag = "A") + coord_fixed() +
      theme_publication() + theme_panel_tag() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) })
  output$cor_plot <- renderPlot(cor_plot())
  output$dl_cor <- downloadHandler(function() "correlation.png", function(f) save_publication(cor_plot(), f, 6.5, 6))

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
      theme_publication() + theme_panel_tag() +
      (if (nlevels(surv_data()$grp) < 2) guides(colour = "none") else NULL) })
  output$surv_plot <- renderPlot({ if (!surv_ok()) return(NULL); surv_plot() })
  output$dl_surv <- downloadHandler(function() "survival.png", function(f) save_publication(surv_plot(), f))

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
      theme_publication() + theme_panel_tag() })
  output$div_plot <- renderPlot({ t <- div_table()
    if (is.null(t) || !"Group" %in% names(t))
      return(ggplot() + annotate("text", 0, 0, label = "Choose a grouping factor to compare diversity across groups.",
                                 family = PUB_FONT, size = 5) + theme_void())
    div_plot() })
  output$dl_div <- downloadHandler(function() paste0("diversity_", input$div_index, ".png"),
                                   function(f) save_publication(div_plot(), f))

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
    method <- input$ord_method
    if (method != "pca" && !has_pkg("vegan")) return(list(error = "The 'vegan' package is required for this method."))
    res <- tryCatch(switch(method,
      pca = { pc <- stats::prcomp(m, scale. = isTRUE(input$ord_scale))
        ve <- (pc$sdev^2 / sum(pc$sdev^2))[1:2] * 100
        list(scores = as.data.frame(pc$x[, 1:2]), axes = c("PC1", "PC2"), ve = ve, obj = pc) },
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
    names(res$scores)[1:2] <- c("Dim1", "Dim2"); res$group <- grp; res })

  output$ord_summary <- renderPrint({ res <- ord_model(); req(res)
    if (!is.null(res$error)) { cat("Error:", res$error, "\n"); return(invisible()) }
    cat("Method:", toupper(input$ord_method), "\n")
    if (!any(is.na(res$ve))) cat(sprintf("%s: %.1f%%   %s: %.1f%%\n", res$axes[1], res$ve[1], res$axes[2], res$ve[2]))
    if (!is.null(res$stress)) cat(sprintf("NMDS stress: %.4f %s\n", res$stress,
                                          if (res$stress < 0.2) "(acceptable)" else "(high — interpret cautiously)"))
    cat("\n"); print(res$obj) })

  ord_plot <- reactive({ res <- ord_model(); req(res); is.null(res$error) || return(NULL)
    d <- res$scores; d$grp <- if (!is.null(res$group)) res$group else factor("All")
    xlab <- if (!is.na(res$ve[1])) sprintf("%s (%.1f%%)", res$axes[1], res$ve[1]) else res$axes[1]
    ylab <- if (!is.na(res$ve[2])) sprintf("%s (%.1f%%)", res$axes[2], res$ve[2]) else res$axes[2]
    p <- ggplot(d, aes(Dim1, Dim2, colour = grp, fill = grp)) +
      geom_hline(yintercept = 0, colour = "grey75", linewidth = pt_to_mm(0.5)) +
      geom_vline(xintercept = 0, colour = "grey75", linewidth = pt_to_mm(0.5)) +
      geom_point(size = 2.6, alpha = 0.85) +
      labs(x = xlab, y = ylab, tag = "A", colour = input$ord_group, fill = input$ord_group) + pub_colour() +
      locked_axis("x", d$Dim1, n = 6, pad = 0.03) + locked_axis("y", d$Dim2, n = 6, pad = 0.03) +
      theme_publication() + theme_panel_tag()
    if (nlevels(d$grp) > 1) p + stat_ellipse(type = "norm", linewidth = pt_to_mm(1.0), show.legend = FALSE)
    else p + guides(colour = "none", fill = "none") })
  output$ord_plot <- renderPlot(ord_plot())
  output$dl_ord <- downloadHandler(function() paste0("ordination_", input$ord_method, ".png"),
                                   function(f) save_publication(ord_plot(), f, 6.5, 6))

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
      theme_publication() + theme_panel_tag() })
  output$pwr_plot <- renderPlot({ if (!has_pkg("pwr")) return(NULL); pwr_plot() })
}

shinyApp(ui, server)

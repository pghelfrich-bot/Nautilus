# Statistics & Figure Producer
# Interactive platform for summary statistics, guided test selection,
# multivariate ordination, and publication-quality figure generation.
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
#   vegan        - Oksanen J et al. Community Ecology Package (NMDS, CA, DCA, RDA).
#   FactoMineR   - Le S, Josse J, Husson F. Multivariate exploratory analysis.
#   ade4         - Dray S, Dufour AB. Implementing the duality diagram.
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

# Optional backends loaded on demand; presence is checked before use.
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

# ---------------------------------------------------------------------------
# Graphical standards
# ---------------------------------------------------------------------------

# Wong et al. (2011) Nature Methods 8:441 colorblind-safe palette.
wong_palette <- c(
  blue    = "#0072B2",
  orange  = "#D55E00",
  amber   = "#E69F00",
  green   = "#009E73",
  yellow  = "#F0E442",
  magenta = "#CC79A7",
  skyblue = "#56B4E9",
  black   = "#000000"
)

wong_values <- function(n) {
  cols <- unname(wong_palette)
  if (n <= length(cols)) cols[seq_len(n)] else colorRampPalette(cols)(n)
}

# ggplot linewidth is expressed in millimetres; convert from points.
pt_to_mm <- function(pt) pt / 2.834645669

PUB_FONT   <- "Georgia"
SPINE_MM   <- pt_to_mm(1.2)
TICK_INSET <- unit(-4, "pt")   # negative length draws ticks inward

theme_publication <- function(base_size = 13, base_family = PUB_FONT) {
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(
      text             = element_text(family = base_family, colour = "black"),
      panel.grid       = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.border     = element_rect(fill = NA, colour = "black", linewidth = SPINE_MM),
      axis.line        = element_blank(),
      axis.ticks       = element_line(colour = "black", linewidth = SPINE_MM),
      axis.ticks.length = TICK_INSET,
      # Positive text margins keep labels clear of inward-pointing ticks.
      axis.text        = element_text(colour = "black"),
      axis.text.x      = element_text(margin = margin(t = 6)),
      axis.text.y      = element_text(margin = margin(r = 6)),
      axis.title       = element_text(colour = "black"),
      legend.key       = element_blank(),
      legend.background = element_blank(),
      legend.title     = element_text(face = "plain"),
      plot.title       = element_blank(),
      strip.background = element_blank(),
      strip.text       = element_blank(),
      complete = TRUE
    )
}

pub_colour <- function(name = NULL) {
  list(
    scale_colour_manual(values = wong_values(64), na.value = "grey40", name = name),
    scale_fill_manual(values = wong_values(64), na.value = "grey40", name = name)
  )
}

# Duplicate axes give unlabelled ticks on the top and right spines so all
# four sides carry inward tick marks. Continuous axes only.
four_side_x <- function(...) scale_x_continuous(..., sec.axis = dup_axis(name = NULL, labels = NULL))

# Lock a continuous axis to its labelled tick boundaries. A small symmetric
# pad (default 3%) is permitted only when marks would otherwise sit on the spine.
locked_axis <- function(axis = c("y", "x"), values, n = 6, pad = 0, sec = TRUE) {
  axis <- match.arg(axis)
  brks <- scales::extended_breaks(n = n)(values)
  brks <- brks[is.finite(brks)]
  lims <- range(brks)
  expand <- expansion(mult = pad)
  if (axis == "y") {
    if (sec) scale_y_continuous(breaks = brks, limits = lims, expand = expand,
                                sec.axis = dup_axis(name = NULL, labels = NULL))
    else scale_y_continuous(breaks = brks, limits = lims, expand = expand)
  } else {
    if (sec) scale_x_continuous(breaks = brks, limits = lims, expand = expand,
                                sec.axis = dup_axis(name = NULL, labels = NULL))
    else scale_x_continuous(breaks = brks, limits = lims, expand = expand)
  }
}

# Bold uppercase panel label anchored to the top-left of the panel; pair with
# labs(tag = "A") on each plot.
theme_panel_tag <- function() {
  theme(plot.tag = element_text(face = "bold", family = PUB_FONT, size = 16),
        plot.tag.position = c(0.02, 0.98))
}

save_publication <- function(plot, file, width = 6.5, height = 5) {
  ggsave(file, plot = plot, width = width, height = height,
         units = "in", dpi = 300, bg = "white", device = "png")
}

# ---------------------------------------------------------------------------
# Statistical helpers
# ---------------------------------------------------------------------------

classify_variable <- function(x) {
  if (is.numeric(x)) {
    u <- length(unique(stats::na.omit(x)))
    if (u <= 2) "discrete/binary"
    else if (u <= 8 && all(x == round(x), na.rm = TRUE)) "discrete"
    else "continuous"
  } else if (is.logical(x)) {
    "categorical"
  } else {
    "categorical"
  }
}

summary_table <- function(df, vars) {
  num <- vars[vapply(df[vars], is.numeric, logical(1))]
  if (!length(num)) return(NULL)
  out <- lapply(num, function(v) {
    x <- stats::na.omit(df[[v]])
    if (!length(x)) return(NULL)
    data.frame(
      Variable = v,
      N        = length(x),
      Mean     = mean(x),
      SD       = stats::sd(x),
      SE       = stats::sd(x) / sqrt(length(x)),
      Median   = stats::median(x),
      IQR      = stats::IQR(x),
      Min      = min(x),
      Max      = max(x),
      Skewness = if (has_pkg("moments")) moments::skewness(x) else NA_real_,
      Kurtosis = if (has_pkg("moments")) moments::kurtosis(x) else NA_real_,
      CV       = stats::sd(x) / mean(x),
      check.names = FALSE
    )
  })
  out <- do.call(rbind, out)
  num_cols <- setdiff(names(out), c("Variable", "N"))
  out[num_cols] <- lapply(out[num_cols], function(z) round(z, 4))
  out
}

normality_table <- function(df, vars) {
  num <- vars[vapply(df[vars], is.numeric, logical(1))]
  if (!length(num)) return(NULL)
  out <- lapply(num, function(v) {
    x <- stats::na.omit(df[[v]])
    sh <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
    ks <- tryCatch(stats::ks.test(x, "pnorm", mean(x), stats::sd(x)),
                   error = function(e) NULL)
    data.frame(
      Variable   = v,
      N          = length(x),
      Shapiro_W  = if (!is.null(sh)) round(unname(sh$statistic), 4) else NA_real_,
      Shapiro_p  = if (!is.null(sh)) signif(sh$p.value, 4) else NA_real_,
      KS_D       = if (!is.null(ks)) round(unname(ks$statistic), 4) else NA_real_,
      KS_p       = if (!is.null(ks)) signif(ks$p.value, 4) else NA_real_,
      Verdict    = if (!is.null(sh)) ifelse(sh$p.value >= 0.05,
                                            "consistent with normal",
                                            "departs from normal") else NA_character_,
      check.names = FALSE
    )
  })
  do.call(rbind, out)
}

p_to_stars <- function(p) {
  ifelse(p < 0.001, "***",
  ifelse(p < 0.01,  "**",
  ifelse(p < 0.05,  "*", "ns")))
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- navbarPage(
  title = "Statistics & Figure Producer",
  id = "nav",
  header = tags$head(tags$style(HTML("
    body, .form-control, .selectize-input { font-family: Georgia, serif; }
    .well { background: #fafafa; }
    .rec-box { border-left: 4px solid #0072B2; padding: 8px 12px; margin: 6px 0; background:#f4f9fd; }
  "))),

  # --- Data ---------------------------------------------------------------
  tabPanel("Data",
    sidebarLayout(
      sidebarPanel(width = 3,
        fileInput("file", "Upload CSV or Excel", accept = c(".csv", ".txt", ".xls", ".xlsx")),
        checkboxInput("header", "Header row", TRUE),
        radioButtons("sep", "Delimiter", c(Comma = ",", Tab = "\t", Semicolon = ";"), inline = TRUE),
        actionButton("demo", "Load demonstration dataset", class = "btn-primary"),
        tags$hr(),
        helpText("Numeric columns feed statistics and figures; text/factor columns act as grouping variables.")
      ),
      mainPanel(width = 9,
        h4("Preview"),
        DTOutput("preview"),
        tags$hr(),
        h4("Detected variable structure"),
        DTOutput("var_types")
      )
    )
  ),

  # --- Summary ------------------------------------------------------------
  tabPanel("Summary Statistics",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("sum_vars_ui"),
        uiOutput("sum_group_ui"),
        selectInput("dist_var", "Distribution figure variable", choices = NULL),
        downloadButton("dl_dist", "Download figure (300 dpi)")
      ),
      mainPanel(width = 9,
        h4("Descriptive statistics"), DTOutput("sum_tbl"),
        tags$hr(),
        h4("Normality assessment"), DTOutput("norm_tbl"),
        tags$hr(),
        h4("Distribution"), plotOutput("dist_plot", height = "480px")
      )
    )
  ),

  # --- Test guide ---------------------------------------------------------
  tabPanel("Test Guide",
    sidebarLayout(
      sidebarPanel(width = 4,
        selectInput("guide_response", "Response / outcome type",
                    c("Continuous", "Discrete count", "Categorical", "Multivariate matrix")),
        selectInput("guide_predictor", "Predictor / grouping type",
                    c("None (single sample)", "Categorical: 2 groups",
                      "Categorical: 3+ groups", "Continuous", "Multiple / mixed")),
        radioButtons("guide_paired", "Design", c("Independent", "Paired / repeated"), inline = TRUE),
        radioButtons("guide_normal", "Distribution", c("Normal", "Non-normal / unknown"), inline = TRUE)
      ),
      mainPanel(width = 8,
        h4("Recommended approach"),
        uiOutput("guide_out")
      )
    )
  ),

  # --- Group comparisons --------------------------------------------------
  tabPanel("Group Comparisons",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("gc_response", "Response (numeric)", choices = NULL),
        selectInput("gc_group", "Grouping factor", choices = NULL),
        selectInput("gc_test", "Test",
          c("Independent t-test (Welch)"     = "t",
            "Mann-Whitney U"                 = "mw",
            "One-way ANOVA"                  = "anova",
            "Welch ANOVA"                    = "welch",
            "Kruskal-Wallis"                 = "kw")),
        checkboxInput("gc_posthoc", "Pairwise post-hoc", TRUE),
        checkboxInput("gc_signif", "Significance brackets on figure", TRUE),
        downloadButton("dl_box", "Download figure (300 dpi)")
      ),
      mainPanel(width = 9,
        h4("Test result"), verbatimTextOutput("gc_result"),
        h4("Post-hoc pairwise"), DTOutput("gc_posthoc_tbl"),
        tags$hr(),
        plotOutput("gc_plot", height = "520px")
      )
    )
  ),

  # --- Correlation --------------------------------------------------------
  tabPanel("Correlation",
    sidebarLayout(
      sidebarPanel(width = 3,
        uiOutput("cor_vars_ui"),
        selectInput("cor_method", "Method",
                    c("Pearson" = "pearson", "Spearman" = "spearman", "Kendall" = "kendall")),
        selectInput("cor_partial", "Partial (control) variable", choices = c("None" = "")),
        downloadButton("dl_cor", "Download figure (300 dpi)")
      ),
      mainPanel(width = 9,
        h4("Correlation matrix"), DTOutput("cor_tbl"),
        h4("p-values"), DTOutput("cor_p_tbl"),
        tags$hr(),
        plotOutput("cor_plot", height = "520px")
      )
    )
  ),

  # --- Ordination ---------------------------------------------------------
  tabPanel("Ordination",
    sidebarLayout(
      sidebarPanel(width = 3,
        selectInput("ord_method", "Method",
          c("PCA (prcomp)"                 = "pca",
            "Correspondence Analysis (CA)" = "ca",
            "Detrended CA (DCA)"           = "dca",
            "NMDS (metaMDS)"               = "nmds",
            "Redundancy Analysis (RDA)"    = "rda")),
        uiOutput("ord_vars_ui"),
        uiOutput("ord_group_ui"),
        uiOutput("ord_constrain_ui"),
        selectInput("nmds_dist", "NMDS / distance metric",
                    c("bray", "euclidean", "jaccard", "gower"), selected = "bray"),
        checkboxInput("ord_scale", "Scale variables (PCA)", TRUE),
        downloadButton("dl_ord", "Download figure (300 dpi)")
      ),
      mainPanel(width = 9,
        h4("Ordination summary"), verbatimTextOutput("ord_summary"),
        tags$hr(),
        plotOutput("ord_plot", height = "560px")
      )
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
    path <- input$file$datapath
    ext <- tolower(tools::file_ext(input$file$name))
    df <- tryCatch({
      if (ext %in% c("xls", "xlsx") && has_pkg("readxl")) {
        as.data.frame(readxl::read_excel(path))
      } else {
        utils::read.table(path, header = input$header, sep = input$sep,
                          stringsAsFactors = FALSE, check.names = TRUE,
                          fill = TRUE, quote = "\"")
      }
    }, error = function(e) {
      showNotification(paste("Read error:", conditionMessage(e)), type = "error")
      NULL
    })
    rv$data <- df
  })

  observeEvent(input$demo, {
    set.seed(1)
    rv$data <- data.frame(
      Group     = rep(c("Control", "Low", "High"), each = 30),
      Response  = c(rnorm(30, 10, 2), rnorm(30, 12, 2.2), rnorm(30, 15, 2.5)),
      Biomass   = c(rnorm(30, 4, 1), rnorm(30, 5.5, 1.1), rnorm(30, 7, 1.3)),
      Diversity = c(rnorm(30, 2.1, .4), rnorm(30, 1.8, .4), rnorm(30, 1.4, .5)),
      Depth     = runif(90, 1, 20),
      pH        = rnorm(90, 7, 0.5)
    )
  })

  numeric_vars <- reactive({
    df <- rv$data; req(df)
    names(df)[vapply(df, is.numeric, logical(1))]
  })
  factor_vars <- reactive({
    df <- rv$data; req(df)
    names(df)[vapply(df, function(x) !is.numeric(x) ||
                       length(unique(stats::na.omit(x))) <= 8, logical(1))]
  })

  # --- Data tab -----------------------------------------------------------
  output$preview <- renderDT({
    req(rv$data); datatable(head(rv$data, 50), options = list(scrollX = TRUE, pageLength = 10))
  })
  output$var_types <- renderDT({
    df <- rv$data; req(df)
    tab <- data.frame(
      Variable = names(df),
      Class    = vapply(df, function(x) class(x)[1], character(1)),
      Type     = vapply(df, classify_variable, character(1)),
      Missing  = vapply(df, function(x) sum(is.na(x)), integer(1)),
      row.names = NULL
    )
    datatable(tab, options = list(pageLength = 15), rownames = FALSE)
  })

  # Keep selectors synced to data.
  observe({
    nv <- numeric_vars(); fv <- factor_vars()
    updateSelectInput(session, "dist_var", choices = nv, selected = nv[1])
    updateSelectInput(session, "gc_response", choices = nv, selected = nv[1])
    updateSelectInput(session, "gc_group", choices = fv, selected = fv[1])
    updateSelectInput(session, "cor_partial", choices = c("None" = "", nv))
  })

  output$sum_vars_ui <- renderUI({
    checkboxGroupInput("sum_vars", "Variables", choices = numeric_vars(),
                       selected = numeric_vars())
  })
  output$sum_group_ui <- renderUI({
    selectInput("sum_group", "Group by (optional)", choices = c("None" = "", factor_vars()))
  })
  output$cor_vars_ui <- renderUI({
    checkboxGroupInput("cor_vars", "Variables", choices = numeric_vars(),
                       selected = head(numeric_vars(), 6))
  })
  output$ord_vars_ui <- renderUI({
    checkboxGroupInput("ord_vars", "Matrix variables", choices = numeric_vars(),
                       selected = numeric_vars())
  })
  output$ord_group_ui <- renderUI({
    selectInput("ord_group", "Grouping (colour)", choices = c("None" = "", factor_vars()))
  })
  output$ord_constrain_ui <- renderUI({
    if (input$ord_method == "rda")
      checkboxGroupInput("ord_constrain", "Constraining variables (RDA)",
                         choices = numeric_vars())
  })

  # --- Summary tab --------------------------------------------------------
  output$sum_tbl <- renderDT({
    req(input$sum_vars)
    df <- rv$data
    if (!is.null(input$sum_group) && nzchar(input$sum_group)) {
      parts <- lapply(split(df, df[[input$sum_group]]), function(sub) {
        t <- summary_table(sub, input$sum_vars)
        if (!is.null(t)) cbind(Group = sub[[input$sum_group]][1], t) else NULL
      })
      tab <- do.call(rbind, parts)
    } else {
      tab <- summary_table(df, input$sum_vars)
    }
    datatable(tab, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE)
  })

  output$norm_tbl <- renderDT({
    req(input$sum_vars)
    datatable(normality_table(rv$data, input$sum_vars),
              options = list(scrollX = TRUE), rownames = FALSE)
  })

  dist_plot <- reactive({
    req(input$dist_var); df <- rv$data
    v <- input$dist_var
    grp <- if (!is.null(input$sum_group) && nzchar(input$sum_group)) input$sum_group else NULL
    d <- df[!is.na(df[[v]]), , drop = FALSE]
    if (!is.null(grp)) d[[grp]] <- factor(d[[grp]])
    # Upper bound spans the tallest density curve and histogram bar so the
    # locked y-axis never clips the distribution.
    split_x <- if (!is.null(grp)) split(d[[v]], d[[grp]]) else list(d[[v]])
    dens_max <- max(vapply(split_x, function(z) {
      if (length(z) > 1) max(stats::density(z)$y) else 0
    }, numeric(1)))
    hist_max <- max(graphics::hist(d[[v]], breaks = 30, plot = FALSE)$density)
    ymax <- max(dens_max, hist_max)
    p <- ggplot(d, aes(x = .data[[v]])) +
      geom_histogram(aes(y = after_stat(density),
                         fill = if (!is.null(grp)) .data[[grp]] else NULL),
                     bins = 30, colour = "black", linewidth = pt_to_mm(0.6),
                     position = "identity", alpha = 0.55) +
      geom_density(aes(colour = if (!is.null(grp)) .data[[grp]] else NULL),
                   linewidth = pt_to_mm(1.2)) +
      labs(x = v, y = "Density", tag = "A") +
      pub_colour() + four_side_x() +
      locked_axis("y", c(0, ymax), n = 5, pad = 0.03) +
      theme_publication() + theme_panel_tag()
    if (is.null(grp)) p <- p + guides(fill = "none", colour = "none")
    p
  })
  output$dist_plot <- renderPlot(dist_plot())
  output$dl_dist <- downloadHandler(
    filename = function() paste0("distribution_", input$dist_var, ".png"),
    content = function(f) save_publication(dist_plot(), f)
  )

  # --- Test guide ---------------------------------------------------------
  output$guide_out <- renderUI({
    resp <- input$guide_response; pred <- input$guide_predictor
    paired <- input$guide_paired == "Paired / repeated"
    normal <- input$guide_normal == "Normal"
    rec <- character(0)

    if (resp == "Multivariate matrix") {
      rec <- c("PCA / RDA for continuous gradients (vegan::rda, FactoMineR::PCA).",
               "Correspondence Analysis or DCA for count / abundance tables (vegan::cca, vegan::decorana).",
               "NMDS on a Bray-Curtis matrix for rank-based community structure (vegan::metaMDS).",
               "PERMANOVA to test group separation (vegan::adonis2).")
    } else if (resp == "Categorical") {
      rec <- c("Chi-squared test of independence (stats::chisq.test).",
               "Fisher's exact test for small expected counts (stats::fisher.test).",
               if (paired) "McNemar's test for paired proportions (stats::mcnemar.test).")
    } else if (pred == "None (single sample)") {
      rec <- c(if (normal) "One-sample t-test (stats::t.test)."
               else "Wilcoxon signed-rank test (stats::wilcox.test).",
               "Shapiro-Wilk / KS to confirm distribution before choosing.")
    } else if (pred == "Categorical: 2 groups") {
      rec <- if (normal)
        c(if (paired) "Paired t-test (stats::t.test, paired = TRUE)."
          else "Welch's independent t-test (stats::t.test).")
      else
        c(if (paired) "Wilcoxon signed-rank test (stats::wilcox.test, paired = TRUE)."
          else "Mann-Whitney U / Wilcoxon rank-sum (stats::wilcox.test; coin::wilcox_test).")
    } else if (pred == "Categorical: 3+ groups") {
      rec <- if (normal)
        c("One-way ANOVA (stats::aov); Welch ANOVA (stats::oneway.test) if variances differ.",
          "Levene's test for homogeneity (car::leveneTest).",
          "Tukey HSD post-hoc (stats::TukeyHSD).")
      else
        c("Kruskal-Wallis (stats::kruskal.test).",
          "Dunn's post-hoc with Holm/BH adjustment (FSA::dunnTest).")
    } else if (pred == "Continuous") {
      rec <- if (normal)
        c("Pearson correlation and linear regression (stats::cor.test, stats::lm).")
      else
        c("Spearman / Kendall rank correlation (stats::cor.test).")
    } else {
      rec <- c("Multiple regression or ANCOVA (stats::lm, car::Anova).",
               "Generalized linear model for non-normal responses (stats::glm).")
    }
    rec <- rec[!vapply(rec, is.null, logical(1))]
    tagList(lapply(rec, function(r) div(class = "rec-box", r)))
  })

  # --- Group comparisons --------------------------------------------------
  gc_data <- reactive({
    req(input$gc_response, input$gc_group)
    df <- rv$data
    d <- df[, c(input$gc_response, input$gc_group)]
    names(d) <- c("y", "g")
    d <- d[stats::complete.cases(d), ]
    d$g <- factor(d$g)
    d
  })

  gc_fit <- reactive({
    d <- gc_data(); req(nrow(d) > 2)
    switch(input$gc_test,
      t     = stats::t.test(y ~ g, data = d),
      mw    = stats::wilcox.test(y ~ g, data = d),
      anova = summary(stats::aov(y ~ g, data = d)),
      welch = stats::oneway.test(y ~ g, data = d, var.equal = FALSE),
      kw    = stats::kruskal.test(y ~ g, data = d))
  })

  output$gc_result <- renderPrint({
    d <- gc_data()
    cat("Groups:", paste(levels(d$g), collapse = ", "),
        "  (n =", paste(as.integer(table(d$g)), collapse = ", "), ")\n\n")
    print(gc_fit())
  })

  gc_posthoc <- reactive({
    if (!isTRUE(input$gc_posthoc)) return(NULL)
    d <- gc_data(); if (nlevels(d$g) < 2) return(NULL)
    res <- switch(input$gc_test,
      anova = , welch = {
        ph <- stats::TukeyHSD(stats::aov(y ~ g, data = d))$g
        data.frame(Comparison = rownames(ph), Diff = round(ph[, "diff"], 4),
                   p_adj = signif(ph[, "p adj"], 4), row.names = NULL)
      },
      kw = {
        if (has_pkg("FSA")) {
          dt <- FSA::dunnTest(y ~ g, data = d, method = "bh")$res
          data.frame(Comparison = dt$Comparison, Z = round(dt$Z, 4),
                     p_adj = signif(dt$P.adj, 4), row.names = NULL)
        } else {
          pw <- stats::pairwise.wilcox.test(d$y, d$g, p.adjust.method = "BH")
          long <- as.data.frame(as.table(pw$p.value))
          long <- long[!is.na(long$Freq), ]
          data.frame(Comparison = paste(long$Var1, "-", long$Var2),
                     p_adj = signif(long$Freq, 4), row.names = NULL)
        }
      },
      t = , mw = {
        method <- if (input$gc_test == "t") "t" else "wilcox"
        pw <- if (method == "t")
          stats::pairwise.t.test(d$y, d$g, p.adjust.method = "BH")
        else
          stats::pairwise.wilcox.test(d$y, d$g, p.adjust.method = "BH")
        long <- as.data.frame(as.table(pw$p.value))
        long <- long[!is.na(long$Freq), ]
        data.frame(Comparison = paste(long$Var1, "-", long$Var2),
                   p_adj = signif(long$Freq, 4), row.names = NULL)
      })
    if (!is.null(res)) res$Signif <- p_to_stars(res$p_adj)
    res
  })

  output$gc_posthoc_tbl <- renderDT({
    ph <- gc_posthoc(); if (is.null(ph)) return(NULL)
    datatable(ph, options = list(pageLength = 10), rownames = FALSE)
  })

  gc_plot <- reactive({
    d <- gc_data(); req(nrow(d) > 0)
    y_rng <- range(d$y)
    p <- ggplot(d, aes(x = g, y = y, fill = g)) +
      geom_boxplot(outlier.shape = NA, width = 0.55, colour = "black",
                   linewidth = pt_to_mm(1.0), alpha = 0.35) +
      geom_jitter(aes(colour = g), width = 0.12, height = 0,
                  size = 1.8, alpha = 0.8, show.legend = FALSE) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3.4,
                   fill = "white", colour = "black", stroke = pt_to_mm(1.0)) +
      labs(x = input$gc_group, y = input$gc_response, tag = "A") +
      pub_colour() +
      scale_x_discrete() +
      locked_axis("y", y_rng, n = 6, pad = 0.03, sec = TRUE) +
      guides(fill = "none") +
      theme_publication() + theme_panel_tag()

    if (isTRUE(input$gc_signif) && nlevels(d$g) >= 2 && has_pkg("ggsignif")) {
      combos <- utils::combn(levels(d$g), 2, simplify = FALSE)
      ph <- gc_posthoc()
      annot <- NULL
      if (!is.null(ph) && "p_adj" %in% names(ph)) {
        annot <- vapply(combos, function(cc) {
          key1 <- paste(cc[2], "-", cc[1]); key2 <- paste(cc[1], "-", cc[2])
          hit <- ph$p_adj[ph$Comparison %in% c(key1, key2)]
          if (length(hit)) format(signif(hit[1], 3)) else NA_character_
        }, character(1))
      }
      keep <- if (is.null(annot)) rep(TRUE, length(combos)) else !is.na(annot)
      if (any(keep)) {
        p <- p + ggsignif::geom_signif(
          comparisons = combos[keep],
          annotations = if (is.null(annot)) NULL else annot[keep],
          map_signif_level = is.null(annot),
          step_increase = 0.09, tip_length = 0.01,
          family = PUB_FONT, colour = "black", textsize = 4)
      }
    }
    p
  })
  output$gc_plot <- renderPlot(gc_plot())
  output$dl_box <- downloadHandler(
    filename = function() paste0("comparison_", input$gc_response, ".png"),
    content = function(f) save_publication(gc_plot(), f)
  )

  # --- Correlation --------------------------------------------------------
  cor_input <- reactive({
    req(input$cor_vars); length(input$cor_vars) >= 2 || return(NULL)
    m <- rv$data[, input$cor_vars, drop = FALSE]
    m[stats::complete.cases(m), , drop = FALSE]
  })

  cor_result <- reactive({
    m <- cor_input(); req(m)
    if (!is.null(input$cor_partial) && nzchar(input$cor_partial) &&
        has_pkg("ppcor") && !(input$cor_partial %in% names(m))) {
      vars <- input$cor_vars
      k <- length(vars)
      base <- rv$data[, c(vars, input$cor_partial), drop = FALSE]
      base <- base[stats::complete.cases(base), , drop = FALSE]
      r <- matrix(1, k, k, dimnames = list(vars, vars))
      pmat <- matrix(0, k, k, dimnames = list(vars, vars))
      # Pairwise partial correlation of each pair, controlling for the one
      # selected covariate (distinct from a full partial-correlation matrix).
      for (i in seq_len(k - 1)) for (j in (i + 1):k) {
        pc <- tryCatch(ppcor::pcor.test(base[[vars[i]]], base[[vars[j]]],
                                        base[[input$cor_partial]],
                                        method = input$cor_method),
                       error = function(e) NULL)
        if (!is.null(pc)) {
          r[i, j] <- r[j, i] <- pc$estimate
          pmat[i, j] <- pmat[j, i] <- pc$p.value
        }
      }
      list(r = r, p = pmat, partial = TRUE)
    } else {
      r <- stats::cor(m, method = input$cor_method)
      pmat <- outer(seq_len(ncol(m)), seq_len(ncol(m)), Vectorize(function(i, j) {
        if (i == j) return(0)
        tryCatch(stats::cor.test(m[[i]], m[[j]], method = input$cor_method)$p.value,
                 error = function(e) NA_real_)
      }))
      dimnames(pmat) <- dimnames(r)
      list(r = r, p = pmat, partial = FALSE)
    }
  })

  output$cor_tbl <- renderDT({
    cr <- cor_result(); req(cr)
    datatable(round(cr$r, 3), options = list(scrollX = TRUE))
  })
  output$cor_p_tbl <- renderDT({
    cr <- cor_result(); req(cr)
    datatable(signif(cr$p, 3), options = list(scrollX = TRUE))
  })

  cor_plot <- reactive({
    cr <- cor_result(); req(cr)
    r <- cr$r
    long <- as.data.frame(as.table(r)); names(long) <- c("V1", "V2", "r")
    long$lab <- sprintf("%.2f", long$r)
    p <- ggplot(long, aes(V1, V2, fill = r)) +
      geom_tile(colour = "black", linewidth = pt_to_mm(0.6)) +
      geom_text(aes(label = lab), family = PUB_FONT, size = 3.6) +
      scale_fill_gradient2(low = "#D55E00", mid = "white", high = "#0072B2",
                           midpoint = 0, limits = c(-1, 1), name = "r") +
      labs(x = NULL, y = NULL, tag = "A") +
      coord_fixed() +
      theme_publication() + theme_panel_tag() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    p
  })
  output$cor_plot <- renderPlot(cor_plot())
  output$dl_cor <- downloadHandler(
    filename = function() "correlation_matrix.png",
    content = function(f) save_publication(cor_plot(), f, width = 6.5, height = 6)
  )

  # --- Ordination ---------------------------------------------------------
  ord_model <- reactive({
    req(input$ord_vars); length(input$ord_vars) >= 2 || return(NULL)
    df <- rv$data
    m <- df[, input$ord_vars, drop = FALSE]
    keep <- stats::complete.cases(m)
    m <- m[keep, , drop = FALSE]
    grp <- if (!is.null(input$ord_group) && nzchar(input$ord_group))
      factor(df[[input$ord_group]][keep]) else NULL

    method <- input$ord_method
    if (method != "pca" && !has_pkg("vegan"))
      return(list(error = "Package 'vegan' is required for this method."))

    res <- tryCatch({
      switch(method,
        pca = {
          pc <- stats::prcomp(m, scale. = isTRUE(input$ord_scale))
          scores <- as.data.frame(pc$x[, 1:2])
          ve <- (pc$sdev^2 / sum(pc$sdev^2))[1:2] * 100
          list(scores = scores, axes = c("PC1", "PC2"), ve = ve,
               loadings = as.data.frame(pc$rotation[, 1:2]), obj = pc)
        },
        ca = {
          ca <- vegan::cca(m)
          sc <- vegan::scores(ca, display = "sites", choices = 1:2)
          ve <- ca$CA$eig[1:2] / sum(ca$CA$eig) * 100
          list(scores = as.data.frame(sc), axes = c("CA1", "CA2"), ve = ve, obj = ca)
        },
        dca = {
          dc <- vegan::decorana(m)
          sc <- vegan::scores(dc, display = "sites", choices = 1:2)
          list(scores = as.data.frame(sc), axes = c("DCA1", "DCA2"),
               ve = c(NA, NA), obj = dc)
        },
        nmds = {
          nm <- vegan::metaMDS(m, distance = input$nmds_dist, trace = 0,
                               autotransform = FALSE)
          sc <- as.data.frame(vegan::scores(nm, display = "sites"))
          list(scores = sc, axes = c("NMDS1", "NMDS2"),
               ve = c(NA, NA), stress = nm$stress, obj = nm)
        },
        rda = {
          req(input$ord_constrain)
          env <- df[keep, input$ord_constrain, drop = FALSE]
          rd <- vegan::rda(m ~ ., data = env)
          sc <- as.data.frame(vegan::scores(rd, display = "sites", choices = 1:2))
          ve <- summary(rd)$cont$importance[2, 1:2] * 100
          list(scores = sc, axes = c("RDA1", "RDA2"), ve = ve, obj = rd)
        })
    }, error = function(e) list(error = conditionMessage(e)))

    if (!is.null(res$error)) return(res)
    names(res$scores)[1:2] <- c("Dim1", "Dim2")
    res$group <- grp
    res
  })

  output$ord_summary <- renderPrint({
    res <- ord_model(); req(res)
    if (!is.null(res$error)) { cat("Error:", res$error, "\n"); return(invisible()) }
    cat("Method:", toupper(input$ord_method), "\n")
    if (!any(is.na(res$ve)))
      cat(sprintf("%s: %.1f%%   %s: %.1f%%\n", res$axes[1], res$ve[1],
                  res$axes[2], res$ve[2]))
    if (!is.null(res$stress))
      cat(sprintf("NMDS stress: %.4f\n", res$stress))
    cat("\n"); print(res$obj)
  })

  ord_plot <- reactive({
    res <- ord_model(); req(res); is.null(res$error) || return(NULL)
    d <- res$scores
    d$grp <- if (!is.null(res$group)) res$group else factor("All")
    xlab <- if (!is.na(res$ve[1])) sprintf("%s (%.1f%%)", res$axes[1], res$ve[1]) else res$axes[1]
    ylab <- if (!is.na(res$ve[2])) sprintf("%s (%.1f%%)", res$axes[2], res$ve[2]) else res$axes[2]
    p <- ggplot(d, aes(Dim1, Dim2, colour = grp, fill = grp)) +
      geom_hline(yintercept = 0, colour = "grey70", linewidth = pt_to_mm(0.5)) +
      geom_vline(xintercept = 0, colour = "grey70", linewidth = pt_to_mm(0.5)) +
      geom_point(size = 2.6, alpha = 0.85) +
      labs(x = xlab, y = ylab, tag = "A", colour = input$ord_group, fill = input$ord_group) +
      pub_colour() +
      locked_axis("x", d$Dim1, n = 6, pad = 0.03) +
      locked_axis("y", d$Dim2, n = 6, pad = 0.03) +
      theme_publication() + theme_panel_tag()
    if (nlevels(d$grp) > 1)
      p <- p + stat_ellipse(type = "norm", linewidth = pt_to_mm(1.0), show.legend = FALSE)
    else
      p <- p + guides(colour = "none", fill = "none")
    p
  })
  output$ord_plot <- renderPlot(ord_plot())
  output$dl_ord <- downloadHandler(
    filename = function() paste0("ordination_", input$ord_method, ".png"),
    content = function(f) save_publication(ord_plot(), f, width = 6.5, height = 6)
  )
}

shinyApp(ui, server)

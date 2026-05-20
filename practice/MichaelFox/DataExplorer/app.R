# ============================================================
# Data Explorer — R Shiny Application
# ============================================================
# install.packages(c(
#   "shiny", "bslib", "ggplot2", "readxl", "DT", "plotly",
#   "colourpicker", "dplyr", "writexl"
# ))

library(shiny)
library(bslib)
library(ggplot2)
library(readxl)
library(DT)
library(plotly)
library(colourpicker)
library(dplyr)
library(writexl)

UF_BLUE   <- "#003087"
UF_ORANGE <- "#FA4616"
MP_COLORS <- c(UF_BLUE, UF_ORANGE, "#2ca25f", "#8856a7")

# ----------------------------------------------------------
# Helper functions
# ----------------------------------------------------------

read_file_data <- function(path, ext, header = TRUE, sep = ",", dec = ".") {
  ext <- tolower(ext)
  if (ext %in% c("xlsx", "xls")) return(as.data.frame(read_excel(path)))
  if (ext == "rds")               return(as.data.frame(readRDS(path)))
  if (ext == "csv")
    return(read.csv(path, header = header, sep = sep, dec = dec, stringsAsFactors = FALSE))
  if (ext %in% c("tsv", "txt"))
    return(read.table(path, header = header,
                      sep = if (ext == "tsv") "\t" else sep,
                      dec = dec, stringsAsFactors = FALSE))
  stop("Unsupported file extension: .", ext)
}

label_or <- function(custom, default) if (nzchar(trimws(custom))) custom else default
pct_label <- function(x) paste0(round(x / sum(x) * 100, 1), "%")

info_icon <- function(..., placement = "right") {
  tooltip(
    tags$span(icon("circle-question"),
              style = "color:#aaa; cursor:help; margin-left:5px; font-size:0.82em;"),
    ...,
    placement = placement
  )
}

# Builds a simple ggplot for one multi-plot slot
build_mp_plot <- function(type, xv, yv, title, df, color = UF_BLUE) {
  if (is.null(df) || is.null(xv) || !nzchar(xv)) return(NULL)
  needs_y <- type %in% c("scatter", "line", "boxplot")
  if (needs_y && (is.null(yv) || !nzchar(yv))) return(NULL)

  p <- tryCatch({
    switch(type,
      scatter   = ggplot(df, aes(x = .data[[xv]], y = .data[[yv]])) +
                    geom_point(color = color, size = 2.2, alpha = 0.75),
      line      = ggplot(df, aes(x = .data[[xv]], y = .data[[yv]], group = 1)) +
                    geom_line(color = color, linewidth = 0.7) +
                    geom_point(color = color, size = 1.8, alpha = 0.75),
      bar       = {
        if (!is.null(yv) && nzchar(yv))
          ggplot(df, aes(x = .data[[xv]], y = .data[[yv]])) +
            geom_bar(stat = "identity", fill = color, alpha = 0.85, width = 0.7)
        else
          ggplot(df, aes(x = .data[[xv]])) +
            geom_bar(fill = color, alpha = 0.85, width = 0.7)
      },
      histogram = ggplot(df, aes(x = .data[[xv]])) +
                    geom_histogram(bins = 25, fill = color, color = "white", alpha = 0.85),
      boxplot   = ggplot(df, aes(x = .data[[xv]], y = .data[[yv]])) +
                    geom_boxplot(fill = color, alpha = 0.75),
      NULL
    )
  }, error = function(e) NULL)

  if (is.null(p)) return(NULL)
  ttl   <- if (!is.null(title) && nzchar(trimws(title))) title else NULL
  y_lbl <- if (!is.null(yv) && nzchar(yv)) yv else NULL
  p + theme_minimal(base_size = 11) +
      labs(title = ttl, x = xv, y = y_lbl) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
}

# Sidebar accordion panel for one multi-plot slot
mp_slot_ui <- function(i) {
  accordion_panel(
    paste("Plot", i),
    value = paste0("p", i),
    checkboxInput(paste0("mp", i, "_on"), "Enable this plot", value = i == 1),
    conditionalPanel(
      condition = paste0("input.mp", i, "_on == true"),
      selectInput(paste0("mp", i, "_type"), "Chart Type",
                  choices = c("Scatter" = "scatter", "Line" = "line",
                              "Bar" = "bar", "Histogram" = "histogram",
                              "Box Plot" = "boxplot")),
      uiOutput(paste0("ui_mp", i, "_x")),
      uiOutput(paste0("ui_mp", i, "_y")),
      textInput(paste0("mp", i, "_title"), "Title", placeholder = "(optional)")
    )
  )
}

# ============================================================
# UI
# ============================================================

ui <- page_navbar(
  title = "Data Explorer",
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = UF_BLUE,
    secondary   = UF_ORANGE,
    font_scale  = 0.95,
    "navbar-bg" = UF_BLUE
  ),
  window_title = "Data Explorer",

  # ──────────────────────────────────────────────────────────
  # TAB 1 — Import Data
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Import Data",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h5("Upload a File"),
        fileInput(
          "file", NULL,
          accept      = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds"),
          buttonLabel = "Browse...",
          placeholder = "CSV, Excel, TSV, RDS..."
        ),
        hr(),
        h6("Text File Options"),
        checkboxInput("header", "First row is header", TRUE),
        selectInput("sep", "Column separator",
                    choices = c("Comma (,)" = ",", "Semicolon (;)" = ";",
                                "Tab"       = "\t", "Space"        = " ")),
        selectInput("dec", "Decimal point",
                    choices = c("Period (.)" = ".", "Comma (,)" = ",")),
        hr(),
        actionButton("load_example", "Load mtcars Example",
                     class = "btn-outline-primary w-100", icon = icon("table"))
      ),
      layout_columns(
        card(
          card_header(icon("eye"), " Data Preview"),
          DTOutput("tbl_preview")
        ),
        card(
          card_header(icon("chart-bar"), " Summary Statistics"),
          verbatimTextOutput("tbl_summary")
        ),
        col_widths = c(8, 4)
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 2 — Visualize
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Visualize",
    layout_sidebar(
      sidebar = sidebar(
        width = 295,
        h5("Chart Settings"),
        selectInput("plot_type", "Chart Type",
                    choices = c(
                      "Scatter Plot" = "scatter",
                      "Line Graph"   = "line",
                      "Bar Chart"    = "bar",
                      "Histogram"    = "histogram",
                      "Box Plot"     = "boxplot",
                      "Pie Chart"    = "pie"
                    )),
        hr(),
        uiOutput("ui_xvar"),
        uiOutput("ui_yvar"),
        uiOutput("ui_colorvar"),
        hr(),
        h6("Labels"),
        textInput("plt_title", "Chart Title",  placeholder = "(optional)"),
        textInput("plt_xlab",  "X-Axis Label", placeholder = "auto"),
        textInput("plt_ylab",  "Y-Axis Label", placeholder = "auto"),
        hr(),
        h6("Style"),
        selectInput("plt_theme", "Theme",
                    choices = c("Minimal" = "minimal", "Classic" = "classic",
                                "Light"   = "light",   "B&W"     = "bw",
                                "Dark"    = "dark")),
        colourInput("plt_color", "Default Color", value = UF_BLUE),
        sliderInput("plt_size", "Point / Bar Size", min = 0.5, max = 5, value = 2, step = 0.5),
        hr(),
        conditionalPanel(
          condition = "input.plot_type == 'scatter' || input.plot_type == 'line'",
          h6("Regression Overlay"),
          checkboxInput("reg_overlay", "Add Fitted Line", FALSE),
          conditionalPanel(
            condition = "input.reg_overlay == true",
            selectInput("reg_overlay_type", "Method",
                        choices = c("Linear (lm)" = "lm",
                                    "Polynomial"  = "poly",
                                    "Loess"       = "loess")),
            conditionalPanel(
              condition = "input.reg_overlay_type == 'poly'",
              sliderInput("reg_overlay_deg", "Polynomial Degree", min = 2, max = 6, value = 2)
            ),
            checkboxInput("reg_ci", "Show 95% CI Band", TRUE),
            colourInput("reg_line_col", "Line Color", value = UF_ORANGE)
          )
        )
      ),
      card(
        card_header(icon("image"), " Plot"),
        full_screen = TRUE,
        plotlyOutput("main_plot", height = "520px")
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 3 — Multi-Plot
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Multi-Plot",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h5("Configure Plots"),
        accordion(
          open = "p1",
          mp_slot_ui(1),
          mp_slot_ui(2),
          mp_slot_ui(3),
          mp_slot_ui(4)
        ),
        hr(),
        numericInput("mp_dpi", "Export DPI", value = 150, min = 72, max = 600, step = 50),
        downloadButton("dl_multiplot", "Export All Active Plots (.png)",
                       class = "btn-success w-100")
      ),
      tagList(
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Plot 1"), plotOutput("mp_plot1", height = "300px")),
          card(card_header("Plot 2"), plotOutput("mp_plot2", height = "300px"))
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Plot 3"), plotOutput("mp_plot3", height = "300px")),
          card(card_header("Plot 4"), plotOutput("mp_plot4", height = "300px"))
        )
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 4 — Regression
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Regression",
    layout_sidebar(
      sidebar = sidebar(
        width = 290,
        h5("Model Setup"),
        uiOutput("ui_reg_resp"),
        uiOutput("ui_reg_pred"),
        selectInput("reg_type",
          label = tags$span("Model Type",
            info_icon(HTML(
              "<b>Simple Linear:</b> One predictor, straight line (Y ~ X).<br>
               <b>Multiple Linear:</b> Two or more predictors (Y ~ X1 + X2 + ...).<br>
               <b>Polynomial:</b> Curved fit using powers of one predictor (Y ~ X + X² + ...)."
            ))),
          choices = c(
            "Simple Linear"   = "linear",
            "Multiple Linear" = "multiple",
            "Polynomial"      = "polynomial"
          )
        ),
        conditionalPanel(
          condition = "input.reg_type == 'polynomial'",
          sliderInput("poly_deg_reg", "Polynomial Degree", min = 2, max = 6, value = 2)
        ),
        hr(),
        actionButton("btn_fit", "Fit Model",
                     class = "btn-primary w-100", icon = icon("play")),
        br(), br(),
        downloadButton("dl_reg", "Export Summary (.txt)",
                       class = "btn-outline-secondary w-100")
      ),
      layout_columns(
        col_widths = c(5, 7),
        tagList(
          card(
            card_header(icon("file-alt"), " Model Summary"),
            verbatimTextOutput("reg_summary")
          ),
          card(
            card_header(icon("lightbulb"), " Statistical Interpretation"),
            uiOutput("reg_interpretation")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Fitted vs Actual"),
            plotlyOutput("reg_plot_fitted", height = "300px")
          ),
          card(
            card_header("Residuals vs Fitted"),
            plotlyOutput("reg_plot_resid", height = "300px")
          )
        )
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 5 — Export
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Export",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(icon("image"), " Export Plot"),
        layout_sidebar(
          sidebar = sidebar(
            width = 225,
            numericInput("exp_w",   "Width (inches)",   value = 8,   min = 2, max = 24, step = 0.5),
            numericInput("exp_h",   "Height (inches)",  value = 6,   min = 2, max = 20, step = 0.5),
            numericInput("exp_dpi", "Resolution (DPI)", value = 150, min = 72, max = 600, step = 50),
            hr(),
            downloadButton("dl_png", "PNG",  class = "btn-success w-100"),
            br(), br(),
            downloadButton("dl_pdf", "PDF",  class = "btn-info    w-100"),
            br(), br(),
            downloadButton("dl_svg", "SVG",  class = "btn-warning w-100")
          ),
          plotOutput("exp_preview", height = "430px")
        )
      ),
      card(
        card_header(icon("table"), " Export Data"),
        layout_sidebar(
          sidebar = sidebar(
            width = 240,
            uiOutput("ui_export_vars"),
            hr(),
            downloadButton("dl_csv",  "CSV",   class = "btn-success w-100"),
            br(), br(),
            downloadButton("dl_xlsx", "Excel (.xlsx)", class = "btn-info w-100")
          ),
          DTOutput("exp_data_tbl")
        )
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 6 — Glossary
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Glossary",
    layout_columns(
      col_widths = c(6, 6),

      # ── Left column ───────────────────────────────────────
      tagList(
        card(
          card_header(icon("book"), " Regression & Model Terms"),
          tags$dl(
            class = "px-2",
            tags$dt("Simple Linear Regression"),
            tags$dd("Models the relationship between one predictor (X) and one response (Y) as a straight line: Y = a + b·X. Best when you expect a direct, linear relationship between two variables."),
            tags$hr(),
            tags$dt("Multiple Linear Regression"),
            tags$dd("Extends simple linear regression to two or more predictors: Y = a + b₁X₁ + b₂X₂ + … Each coefficient tells you the effect of one predictor while holding the others constant."),
            tags$hr(),
            tags$dt("Polynomial Regression"),
            tags$dd("Fits a curved (non-linear) relationship by adding powers of the predictor: Y = a + bX + cX² + … Use this when a scatter plot shows a clear curve rather than a straight line."),
            tags$hr(),
            tags$dt("R-squared (R²)"),
            tags$dd("How much of the variation in Y is explained by the model (0–1). R² = 0.85 means the model accounts for 85% of the variability."),
            tags$hr(),
            tags$dt("Adjusted R²"),
            tags$dd("Like R², but penalized for adding extra predictors that don't improve the fit. Better for comparing models with different numbers of variables."),
            tags$hr(),
            tags$dt("F-statistic & its p-value"),
            tags$dd("Tests whether the model as a whole explains a significant amount of variance. A p-value < 0.05 means the overall model is statistically significant."),
            tags$hr(),
            tags$dt("Residuals"),
            tags$dd("Observed value minus predicted value. Ideally scattered randomly around zero — patterns suggest the model is missing something."),
            tags$hr(),
            tags$dt("Confidence Interval (CI)"),
            tags$dd("A 95% CI means: if we repeated the study 100 times, the true value would fall within this range 95 times. A CI for a coefficient that does not include zero indicates significance.")
          )
        )
      ),

      # ── Right column ──────────────────────────────────────
      tagList(
        card(
          card_header(icon("book-open"), " Coefficient Table Terms"),
          tags$dl(
            class = "px-2",
            tags$dt("Estimate (Coefficient)"),
            tags$dd("The predicted change in Y for a one-unit increase in X, holding other variables constant. A coefficient of 3.2 for 'weight' means each unit increase in weight is associated with a 3.2-unit change in Y."),
            tags$hr(),
            tags$dt("Std. Error (Standard Error)"),
            tags$dd("The uncertainty around the coefficient estimate. Smaller = more precise. Used to compute the t-value and confidence intervals."),
            tags$hr(),
            tags$dt("t-value"),
            tags$dd("Coefficient divided by its standard error. A larger absolute value (generally > 2) suggests the predictor is statistically significant."),
            tags$hr(),
            tags$dt("p-value (Pr > |t|)"),
            tags$dd("Probability of seeing this result by chance if the predictor had no real effect. p < 0.05 is the conventional threshold for statistical significance."),
            tags$hr(),
            tags$dt("Intercept"),
            tags$dd("The predicted value of Y when all predictors equal zero. Often not directly meaningful on its own, but required for the model equation.")
          )
        ),
        card(
          card_header(icon("circle-info"), " App Settings Explained"),
          tags$dl(
            class = "px-2",
            tags$dt("First row is header"),
            tags$dd("Check this if the first row of your CSV/text file contains column names (e.g. 'weight', 'mpg'). Uncheck if the file starts directly with data values."),
            tags$hr(),
            tags$dt("Column separator"),
            tags$dd("The character used to split columns in your text file. CSV files use commas; TSV files use tabs. If your data looks jumbled after loading, try a different separator."),
            tags$hr(),
            tags$dt("Decimal point"),
            tags$dd("The character used for decimal numbers. Most English-language files use a period (1.5); some European files use a comma (1,5). Choose the one that matches your file."),
            tags$hr(),
            tags$dt("Resolution (DPI)"),
            tags$dd("Dots per inch — controls the sharpness of exported images. 72–96 DPI is screen quality. 150 DPI is good for presentations. 300+ DPI is recommended for print or publication. Higher DPI means a larger file size.")
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(data = NULL, model = NULL)

  # ── Load data ─────────────────────────────────────────────

  observeEvent(input$load_example, {
    d <- as.data.frame(mtcars)
    d$car <- rownames(d)
    rownames(d) <- NULL
    rv$data <- d
    showNotification("Loaded example dataset: mtcars (Motor Trend Cars)", type = "message")
  })

  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      rv$data <- read_file_data(
        input$file$datapath, ext,
        header = input$header, sep = input$sep, dec = input$dec
      )
      showNotification(paste("Loaded:", input$file$name), type = "message")
    }, error = function(e)
      showNotification(paste("Read error:", e$message), type = "error", duration = 8))
  })

  # ── Data preview ──────────────────────────────────────────

  output$tbl_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, rownames = FALSE, class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })

  output$tbl_summary <- renderPrint({
    req(rv$data)
    summary(rv$data)
  })

  # ── Reactive column lists ──────────────────────────────────

  cols_all <- reactive({ req(rv$data); names(rv$data) })
  cols_num <- reactive({
    req(rv$data)
    names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
  })

  # ── Variable selectors — Visualize tab ────────────────────

  output$ui_xvar <- renderUI({
    req(input$plot_type)
    selectInput("xvar", "X Variable", choices = cols_all())
  })

  output$ui_yvar <- renderUI({
    req(input$plot_type)
    if (input$plot_type == "histogram") return(NULL)
    choices <- if (input$plot_type == "pie")
                 c("(Count categories)" = "__count__", cols_num())
               else cols_num()
    selectInput("yvar", "Y Variable", choices = choices)
  })

  output$ui_colorvar <- renderUI({
    req(input$plot_type)
    if (input$plot_type == "pie") return(NULL)
    selectInput("colorvar", "Color / Group By (optional)",
                choices = c("None" = "__none__", cols_all()))
  })

  # ── Variable selectors — Regression tab ───────────────────

  output$ui_reg_resp <- renderUI({
    selectInput("reg_resp",
      label = tags$span("Response Variable (Y)",
        info_icon("The outcome you want to predict. Must be a numeric variable.")),
      choices = cols_num())
  })

  output$ui_reg_pred <- renderUI({
    is_multi <- !is.null(input$reg_type) && input$reg_type == "multiple"
    lbl <- if (is_multi) "Predictor Variables (X)" else "Predictor Variable (X)"
    tip <- if (is_multi)
      "Select two or more numeric variables. Hold Ctrl/Cmd to select multiple."
    else
      "Select one numeric variable to predict the response."
    selectInput("reg_pred",
      label    = tags$span(lbl, info_icon(tip)),
      choices  = cols_num(),
      multiple = is_multi)
  })

  # ── Variable selectors — Multi-Plot tab ───────────────────

  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("ui_mp", idx, "_x")]] <- renderUI({
        req(rv$data)
        selectInput(paste0("mp", idx, "_xvar"), "X Variable", choices = cols_all())
      })
      output[[paste0("ui_mp", idx, "_y")]] <- renderUI({
        req(rv$data)
        selectInput(paste0("mp", idx, "_yvar"), "Y Variable",
                    choices = c("None" = "", cols_num()))
      })
    })
  }

  # ── Build ggplot (reactive, used by Visualize + Export tabs) ─

  gg_plot <- reactive({
    req(rv$data, input$xvar, input$plot_type)

    df  <- rv$data
    pt  <- input$plot_type
    xv  <- input$xvar
    yv  <- if (!is.null(input$yvar)) input$yvar else NULL
    cv  <- if (!is.null(input$colorvar) && input$colorvar != "__none__") input$colorvar else NULL

    # If cv is numeric but low-cardinality (e.g. cyl = 4/6/8), treat as categorical
    # so discrete color scales (brewer) work without errors.
    if (!is.null(cv) && is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) <= 10) {
      df[[cv]] <- as.factor(df[[cv]])
    }

    # Bar charts and histograms can only sensibly group by categorical variables.
    # If cv is still numeric after the factor-conversion step, it's truly continuous
    # and grouping would produce one color per observation — ignore it.
    if (pt %in% c("histogram", "bar") && !is.null(cv) && is.numeric(df[[cv]])) {
      cv <- NULL
    }

    title <- if (nzchar(trimws(input$plt_title))) input$plt_title else NULL
    xlab  <- label_or(input$plt_xlab, xv)
    ylab  <- if (!is.null(yv) && yv != "__count__") label_or(input$plt_ylab, yv) else NULL

    base_theme <- switch(input$plt_theme,
      minimal = theme_minimal, classic = theme_classic,
      light   = theme_light,   bw      = theme_bw,
      dark    = theme_dark,    theme_minimal
    )(base_size = 13) +
      theme(
        plot.title      = element_text(hjust = 0.5, face = "bold",
                                       size = 14, margin = margin(b = 10)),
        axis.title      = element_text(size = 12),
        legend.title    = element_text(size = 11),
        legend.position = "right"
      )

    smooth_layer <- function() {
      if (!isTRUE(input$reg_overlay)) return(NULL)
      meth    <- input$reg_overlay_type
      formula <- if (meth == "poly")
                   as.formula(paste0("y ~ poly(x, ", input$reg_overlay_deg, ")"))
                 else y ~ x
      geom_smooth(
        mapping   = aes(x = .data[[xv]], y = .data[[yv]]),
        method    = if (meth == "poly") "lm" else meth,
        formula   = formula,
        se        = isTRUE(input$reg_ci),
        color     = input$reg_line_col,
        linewidth = 1.1
      )
    }

    if (pt == "scatter") {
      req(yv)
      aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], y = .data[[yv]], color = .data[[cv]])
               else               aes(x = .data[[xv]], y = .data[[yv]])
      p <- ggplot(df, aes_m)
      p <- if (is.null(cv))
             p + geom_point(size = input$plt_size, alpha = 0.75, color = input$plt_color)
           else
             p + geom_point(size = input$plt_size, alpha = 0.75)
      p <- p + smooth_layer()

    } else if (pt == "line") {
      req(yv)
      aes_m <- if (!is.null(cv))
                 aes(x = .data[[xv]], y = .data[[yv]], color = .data[[cv]], group = .data[[cv]])
               else
                 aes(x = .data[[xv]], y = .data[[yv]], group = 1)
      p <- ggplot(df, aes_m)
      p <- if (is.null(cv))
             p + geom_line(linewidth = input$plt_size * 0.4, color = input$plt_color) +
                 geom_point(size = input$plt_size * 0.7,     color = input$plt_color)
           else
             p + geom_line(linewidth = input$plt_size * 0.4) +
                 geom_point(size = input$plt_size * 0.7)
      p <- p + smooth_layer()

    } else if (pt == "bar") {
      has_y <- !is.null(yv) && yv != "__count__"
      bar_w <- 0.2 + (input$plt_size - 0.5) / 4.5 * 0.7   # maps 0.5–5 → 0.2–0.9
      aes_m <- if (!is.null(cv)) {
                 if (has_y) aes(x = .data[[xv]], y = .data[[yv]], fill = .data[[cv]])
                 else       aes(x = .data[[xv]], fill = .data[[cv]])
               } else {
                 if (has_y) aes(x = .data[[xv]], y = .data[[yv]])
                 else       aes(x = .data[[xv]])
               }
      p <- ggplot(df, aes_m)
      p <- if (is.null(cv))
             p + geom_bar(stat  = if (has_y) "identity" else "count",
                          fill  = input$plt_color, width = bar_w, alpha = 0.85)
           else
             p + geom_bar(stat     = if (has_y) "identity" else "count",
                          width    = bar_w, alpha = 0.85,
                          position = "dodge")
      ylab <- if (!has_y) "Count" else ylab

    } else if (pt == "histogram") {
      if (!is.null(cv)) {
        p <- ggplot(df, aes(x = .data[[xv]], fill = .data[[cv]])) +
             geom_histogram(bins = 30, color = "white", alpha = 0.75,
                            position = "dodge")
      } else {
        p <- ggplot(df, aes(x = .data[[xv]])) +
             geom_histogram(bins = 30, color = "white",
                            fill = input$plt_color, alpha = 0.85)
      }
      ylab <- "Count"

    } else if (pt == "boxplot") {
      req(yv)
      aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], y = .data[[yv]], fill = .data[[cv]])
               else               aes(x = .data[[xv]], y = .data[[yv]])
      p <- ggplot(df, aes_m)
      p <- if (is.null(cv))
             p + geom_boxplot(fill = input$plt_color, alpha = 0.75,
                              outlier.size = input$plt_size * 0.7, outlier.alpha = 0.6)
           else
             p + geom_boxplot(alpha = 0.75,
                              outlier.size = input$plt_size * 0.7, outlier.alpha = 0.6)

    } else if (pt == "pie") {
      use_count <- is.null(yv) || yv == "__count__"
      pie_df <- if (use_count) {
        df |> count(.data[[xv]], name = "val_")
      } else {
        df |> group_by(.data[[xv]]) |>
              summarise(val_ = sum(.data[[yv]], na.rm = TRUE), .groups = "drop")
      }
      names(pie_df)[1] <- "cat_"
      pie_df$label_ <- paste0(pie_df$cat_, "\n", pct_label(pie_df$val_))
      return(
        ggplot(pie_df, aes(x = "", y = val_, fill = cat_)) +
          geom_col(width = 1, color = "white", linewidth = 0.5) +
          coord_polar("y", start = 0) +
          geom_text(aes(label = label_), position = position_stack(vjust = 0.5),
                    size = 3.5, color = "white", fontface = "bold") +
          scale_fill_brewer(palette = "Set2", name = xv) +
          labs(title = title) +
          theme_void(base_size = 13) +
          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
                legend.position = "right", legend.title = element_text(size = 11))
      )
    }

    if (!is.null(cv)) {
      # Use a continuous scale only for truly continuous variables (> 10 unique values).
      # Numeric-but-categorical vars like cyl (3 values) get distinct palette colors.
      if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) > 10) {
        p <- p + scale_color_viridis_c() + scale_fill_viridis_c()
      } else {
        p <- p + scale_color_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1")
      }
    }
    # Only label the aesthetic that is actually mapped for this plot type
    uses_color <- pt %in% c("scatter", "line")
    p + base_theme + labs(
      title = title, x = xlab, y = ylab,
      color = if (uses_color) cv else NULL,
      fill  = if (!uses_color) cv else NULL
    )
  })

  output$main_plot <- renderPlotly({
    req(gg_plot())
    ggplotly(gg_plot()) |> layout(margin = list(t = 55, b = 55))
  })

  # ── Multi-plot renders ────────────────────────────────────

  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("mp_plot", idx)]] <- renderPlot({
        if (!isTRUE(input[[paste0("mp", idx, "_on")]])) return(NULL)
        req(rv$data)
        xv   <- input[[paste0("mp", idx, "_xvar")]]
        yv   <- input[[paste0("mp", idx, "_yvar")]]
        req(xv)
        build_mp_plot(
          type  = input[[paste0("mp", idx, "_type")]],
          xv    = xv,
          yv    = if (!is.null(yv) && nzchar(yv)) yv else NULL,
          title = input[[paste0("mp", idx, "_title")]],
          df    = rv$data,
          color = MP_COLORS[idx]
        )
      }, bg = "white")
    })
  }

  # ── Multi-plot export — uses base R grid, no extra packages ─

  output$dl_multiplot <- downloadHandler(
    filename = function() paste0("multiplot_", Sys.Date(), ".png"),
    content  = function(f) {
      df  <- isolate(rv$data)
      dpi <- isolate(input$mp_dpi)
      if (is.null(dpi) || is.na(dpi)) dpi <- 150

      plots <- list()
      if (!is.null(df)) {
        for (i in 1:4) {
          if (!isTRUE(isolate(input[[paste0("mp", i, "_on")]]))) next
          xv  <- isolate(input[[paste0("mp", i, "_xvar")]])
          yv  <- isolate(input[[paste0("mp", i, "_yvar")]])
          if (is.null(xv) || !nzchar(xv)) next
          p <- tryCatch(
            build_mp_plot(
              type  = isolate(input[[paste0("mp", i, "_type")]]),
              xv    = xv,
              yv    = if (!is.null(yv) && nzchar(yv)) yv else NULL,
              title = isolate(input[[paste0("mp", i, "_title")]]),
              df    = df,
              color = MP_COLORS[i]
            ),
            error = function(e) NULL
          )
          if (!is.null(p)) plots[[length(plots) + 1]] <- p
        }
      }

      n <- length(plots)
      if (n == 0) {
        png(f, width = 800, height = 500, res = dpi)
        plot.new()
        text(0.5, 0.5, "No enabled plots to export.\nEnable plots in the Multi-Plot tab.",
             cex = 1.4, col = "gray40")
        dev.off()
        return(invisible(NULL))
      }

      ncols <- min(2L, n)
      nrows <- ceiling(n / ncols)
      png(f, width = 7 * ncols, height = 5.5 * nrows, units = "in", res = dpi)
      grid::grid.newpage()
      grid::pushViewport(grid::viewport(
        layout = grid::grid.layout(nrows, ncols)
      ))
      for (i in seq_along(plots)) {
        r <- ceiling(i / ncols)
        cc <- ((i - 1L) %% ncols) + 1L
        print(plots[[i]], vp = grid::viewport(layout.pos.row = r, layout.pos.col = cc))
      }
      dev.off()
    }
  )

  # ── Regression model ──────────────────────────────────────

  observeEvent(input$btn_fit, {
    req(rv$data, input$reg_resp, input$reg_pred)
    df   <- rv$data
    resp <- input$reg_resp
    pred <- input$reg_pred
    tryCatch({
      fmla <- switch(input$reg_type,
        linear     = paste(resp, "~", paste(pred, collapse = " + ")),
        multiple   = paste(resp, "~", paste(pred, collapse = " + ")),
        polynomial = paste(resp, "~",
                           paste0("poly(", pred[1], ", ", input$poly_deg_reg, ", raw = TRUE)"))
      )
      rv$model <- lm(as.formula(fmla), data = df)
      showNotification("Model fitted successfully.", type = "message")
    }, error = function(e)
      showNotification(paste("Fitting error:", e$message), type = "error", duration = 8))
  })

  output$reg_summary <- renderPrint({
    if (is.null(rv$model)) cat("Fit a model using the panel on the left.\n")
    else                   summary(rv$model)
  })

  output$reg_interpretation <- renderUI({
    if (is.null(rv$model))
      return(tags$p(class = "text-muted fst-italic",
                    "Fit a model to see an interpretation of the results."))

    s      <- summary(rv$model)
    r2     <- round(s$r.squared, 3)
    adj_r2 <- round(s$adj.r.squared, 3)
    fstat  <- s$fstatistic
    overall_p <- if (!is.null(fstat)) pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA
    p_label   <- if (!is.na(overall_p)) {
      if (overall_p < 0.001) "p < 0.001" else paste0("p = ", round(overall_p, 4))
    } else "p = N/A"

    overall_tag <- if (!is.na(overall_p) && overall_p < 0.05) {
      tags$p(tags$span(style = "color:#2e7d32; font-weight:600;",
        icon("circle-check"), " The overall model is statistically significant (", p_label, ")."))
    } else {
      tags$p(tags$span(style = "color:#c62828; font-weight:600;",
        icon("circle-xmark"), " The overall model is NOT statistically significant (", p_label, ")."))
    }

    coef_df   <- as.data.frame(s$coefficients)
    pred_rows <- coef_df[rownames(coef_df) != "(Intercept)", , drop = FALSE]
    sig    <- rownames(pred_rows)[pred_rows[, 4] < 0.05]
    nonsig <- rownames(pred_rows)[pred_rows[, 4] >= 0.05]

    tagList(
      overall_tag,
      tags$p(tags$b("R² = ", r2), " — explains ",
             tags$b(paste0(round(r2 * 100, 1), "%")), " of the variance. ",
             tags$span(style = "color:#555;", paste0("(Adj. R² = ", adj_r2, ")"))),
      if (length(sig)    > 0) tags$p(tags$b(style = "color:#2e7d32;", "Significant (p < 0.05): "),    paste(sig,    collapse = ", ")),
      if (length(nonsig) > 0) tags$p(tags$b(style = "color:#c62828;", "Not significant (p ≥ 0.05): "), paste(nonsig, collapse = ", ")),
      tags$p(class = "text-muted small mt-2",
             "α = 0.05. Statistical significance does not imply practical importance.")
    )
  })

  output$reg_plot_fitted <- renderPlotly({
    req(rv$model)
    d <- data.frame(actual = rv$model$model[[1]], fitted = fitted(rv$model))
    p <- ggplot(d, aes(x = actual, y = fitted)) +
         geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
         geom_abline(color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
         theme_minimal(base_size = 12) +
         labs(title = "Fitted vs Actual", x = "Actual", y = "Fitted") +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p) |> layout(margin = list(t = 90, b = 40, l = 55, r = 20))
  })

  output$reg_plot_resid <- renderPlotly({
    req(rv$model)
    d <- data.frame(fitted = fitted(rv$model), resid = residuals(rv$model))
    p <- ggplot(d, aes(x = fitted, y = resid)) +
         geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
         geom_hline(yintercept = 0, color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
         theme_minimal(base_size = 12) +
         labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p) |> layout(margin = list(t = 90, b = 40, l = 55, r = 20))
  })

  output$dl_reg <- downloadHandler(
    filename = function() paste0("model_summary_", Sys.Date(), ".txt"),
    content  = function(f) { req(rv$model); capture.output(summary(rv$model), file = f) }
  )

  # ── Export ────────────────────────────────────────────────

  output$ui_export_vars <- renderUI({
    req(rv$data)
    selectInput("export_vars", "Select columns to export:",
                choices  = names(rv$data),
                selected = names(rv$data),
                multiple = TRUE)
  })

  export_df <- reactive({
    req(rv$data)
    vars <- input$export_vars
    if (is.null(vars) || length(vars) == 0) return(rv$data)
    rv$data[, intersect(vars, names(rv$data)), drop = FALSE]
  })

  output$exp_preview <- renderPlot({ req(gg_plot()); gg_plot() }, bg = "white")

  output$exp_data_tbl <- renderDT({
    req(rv$data)
    datatable(export_df(), rownames = FALSE, class = "compact stripe",
              options = list(scrollX = TRUE, pageLength = 8, dom = "tip"))
  })

  output$dl_png <- downloadHandler(
    filename = function() paste0("plot_", Sys.Date(), ".png"),
    content  = function(f) {
      req(gg_plot())
      ggsave(f, gg_plot(), width = input$exp_w, height = input$exp_h,
             dpi = input$exp_dpi, bg = "white")
    }
  )

  output$dl_pdf <- downloadHandler(
    filename = function() paste0("plot_", Sys.Date(), ".pdf"),
    content  = function(f) {
      req(gg_plot())
      ggsave(f, gg_plot(), width = input$exp_w, height = input$exp_h, device = cairo_pdf)
    }
  )

  output$dl_svg <- downloadHandler(
    filename = function() paste0("plot_", Sys.Date(), ".svg"),
    content  = function(f) {
      req(gg_plot())
      ggsave(f, gg_plot(), width = input$exp_w, height = input$exp_h, device = "svg")
    }
  )

  output$dl_csv <- downloadHandler(
    filename = function() paste0("data_", Sys.Date(), ".csv"),
    content  = function(f) { req(rv$data); write.csv(export_df(), f, row.names = FALSE) }
  )

  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("data_", Sys.Date(), ".xlsx"),
    content  = function(f) { req(rv$data); write_xlsx(export_df(), f) }
  )
}

shinyApp(ui, server)

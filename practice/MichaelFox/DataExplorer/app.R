# ============================================================
# Data Explorer — R Shiny Application
# ============================================================
# Install required packages once if needed:
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

# ============================================================
# UI
# ============================================================

ui <- page_navbar(
  title = "Data Explorer",
  theme = bs_theme(bootswatch = "flatly", primary = "#2C7BB6", font_scale = 0.95),
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
        textInput("plt_title", "Chart Title",    placeholder = "(optional)"),
        textInput("plt_xlab",  "X-Axis Label",   placeholder = "auto"),
        textInput("plt_ylab",  "Y-Axis Label",   placeholder = "auto"),
        hr(),

        h6("Style"),
        selectInput("plt_theme", "Theme",
                    choices = c("Minimal" = "minimal", "Classic" = "classic",
                                "Light"   = "light",   "B&W"     = "bw",
                                "Dark"    = "dark")),
        colourInput("plt_color", "Default Color", value = "#2C7BB6"),
        sliderInput("plt_size", "Point / Bar Size", min = 0.5, max = 5, value = 2, step = 0.5),
        hr(),

        # Regression overlay — scatter and line only
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
            colourInput("reg_line_col", "Line Color", value = "#D7191C")
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
  # TAB 3 — Regression
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Regression",
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        h5("Model Setup"),
        uiOutput("ui_reg_resp"),
        uiOutput("ui_reg_pred"),
        selectInput("reg_type", "Model Type",
                    choices = c(
                      "Simple Linear"   = "linear",
                      "Multiple Linear" = "multiple",
                      "Polynomial"      = "polynomial"
                    )),
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
        card(
          card_header(icon("file-alt"), " Model Summary"),
          verbatimTextOutput("reg_summary")
        ),
        layout_columns(
          card(
            card_header("Fitted vs Actual"),
            plotlyOutput("reg_plot_fitted", height = "360px")
          ),
          card(
            card_header("Residuals vs Fitted"),
            plotlyOutput("reg_plot_resid", height = "360px")
          ),
          col_widths = c(6, 6)
        ),
        col_widths = c(5, 7)
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 4 — Export
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Export",
    layout_columns(
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
            width = 225,
            downloadButton("dl_csv",  "CSV",   class = "btn-success w-100"),
            br(), br(),
            downloadButton("dl_xlsx", "Excel (.xlsx)", class = "btn-info w-100")
          ),
          DTOutput("exp_data_tbl")
        )
      ),
      col_widths = c(6, 6)
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

  # ── Variable selectors for Visualize tab ──────────────────

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

  # ── Variable selectors for Regression tab ─────────────────

  output$ui_reg_resp <- renderUI({
    selectInput("reg_resp", "Response Variable (Y)", choices = cols_num())
  })

  output$ui_reg_pred <- renderUI({
    selectInput("reg_pred", "Predictor Variable(s) (X)",
                choices = cols_num(), multiple = TRUE)
  })

  # ── Build ggplot (reactive) ────────────────────────────────

  gg_plot <- reactive({
    req(rv$data, input$xvar, input$plot_type)

    df <- rv$data
    pt <- input$plot_type
    xv <- input$xvar
    yv <- if (!is.null(input$yvar)) input$yvar else NULL
    cv <- if (!is.null(input$colorvar) && input$colorvar != "__none__") input$colorvar else NULL

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

    # Smooth overlay layer (used in scatter & line branches)
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

    # ── Scatter ─────────────────────────────────────────────
    if (pt == "scatter") {
      req(yv)
      aes_m <- if (!is.null(cv))
                 aes(x = .data[[xv]], y = .data[[yv]], color = .data[[cv]])
               else
                 aes(x = .data[[xv]], y = .data[[yv]])
      p <- ggplot(df, aes_m) +
           geom_point(size  = input$plt_size, alpha = 0.75,
                      color = if (is.null(cv)) input$plt_color else NULL)
      p <- p + smooth_layer()

    # ── Line ────────────────────────────────────────────────
    } else if (pt == "line") {
      req(yv)
      aes_m <- if (!is.null(cv))
                 aes(x = .data[[xv]], y = .data[[yv]],
                     color = .data[[cv]], group = .data[[cv]])
               else
                 aes(x = .data[[xv]], y = .data[[yv]], group = 1)
      p <- ggplot(df, aes_m) +
           geom_line(linewidth = input$plt_size * 0.4,
                     color = if (is.null(cv)) input$plt_color else NULL) +
           geom_point(size  = input$plt_size * 0.7,
                      color = if (is.null(cv)) input$plt_color else NULL)
      p <- p + smooth_layer()

    # ── Bar ─────────────────────────────────────────────────
    } else if (pt == "bar") {
      has_y <- !is.null(yv) && yv != "__count__"
      aes_m <- if (!is.null(cv)) {
                 if (has_y) aes(x = .data[[xv]], y = .data[[yv]], fill = .data[[cv]])
                 else       aes(x = .data[[xv]], fill = .data[[cv]])
               } else {
                 if (has_y) aes(x = .data[[xv]], y = .data[[yv]])
                 else       aes(x = .data[[xv]])
               }
      p <- ggplot(df, aes_m) +
           geom_bar(stat  = if (has_y) "identity" else "count",
                    fill  = if (is.null(cv)) input$plt_color else NULL,
                    width = pmin(input$plt_size / 5 + 0.5, 0.9),
                    alpha = 0.85)
      ylab <- if (!has_y) "Count" else ylab

    # ── Histogram ───────────────────────────────────────────
    } else if (pt == "histogram") {
      aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], fill = .data[[cv]])
               else              aes(x = .data[[xv]])
      p <- ggplot(df, aes_m) +
           geom_histogram(bins  = 30, color = "white",
                          fill  = if (is.null(cv)) input$plt_color else NULL,
                          alpha = 0.85)
      ylab <- "Count"

    # ── Box Plot ────────────────────────────────────────────
    } else if (pt == "boxplot") {
      req(yv)
      aes_m <- if (!is.null(cv))
                 aes(x = .data[[xv]], y = .data[[yv]], fill = .data[[cv]])
               else
                 aes(x = .data[[xv]], y = .data[[yv]])
      p <- ggplot(df, aes_m) +
           geom_boxplot(fill          = if (is.null(cv)) input$plt_color else NULL,
                        alpha         = 0.75,
                        outlier.size  = input$plt_size * 0.7,
                        outlier.alpha = 0.6)

    # ── Pie Chart ───────────────────────────────────────────
    } else if (pt == "pie") {
      use_count <- is.null(yv) || yv == "__count__"
      if (use_count) {
        pie_df <- df |> count(.data[[xv]], name = "val_")
      } else {
        pie_df <- df |>
          group_by(.data[[xv]]) |>
          summarise(val_ = sum(.data[[yv]], na.rm = TRUE), .groups = "drop")
      }
      names(pie_df)[1] <- "cat_"
      pie_df$label_ <- paste0(pie_df$cat_, "\n", pct_label(pie_df$val_))

      p <- ggplot(pie_df, aes(x = "", y = val_, fill = cat_)) +
           geom_col(width = 1, color = "white", linewidth = 0.5) +
           coord_polar("y", start = 0) +
           geom_text(aes(label = label_),
                     position = position_stack(vjust = 0.5),
                     size = 3.5, color = "white", fontface = "bold") +
           scale_fill_brewer(palette = "Set2", name = xv) +
           labs(title = title) +
           theme_void(base_size = 13) +
           theme(plot.title      = element_text(hjust = 0.5, face = "bold", size = 14),
                 legend.position = "right",
                 legend.title    = element_text(size = 11))
      return(p)
    }

    # Apply color scales for grouped plots
    if (!is.null(cv)) {
      if (is.numeric(df[[cv]])) {
        p <- p + scale_color_viridis_c() + scale_fill_viridis_c()
      } else {
        p <- p + scale_color_brewer(palette = "Set1") +
                 scale_fill_brewer(palette = "Set1")
      }
    }

    p + base_theme + labs(title = title, x = xlab, y = ylab, color = cv, fill = cv)
  })

  # ── Interactive plot output ────────────────────────────────

  output$main_plot <- renderPlotly({
    req(gg_plot())
    ggplotly(gg_plot()) |> layout(margin = list(t = 55, b = 55))
  })

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
                           paste0("poly(", pred[1], ", ",
                                  input$poly_deg_reg, ", raw = TRUE)"))
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

  output$reg_plot_fitted <- renderPlotly({
    req(rv$model)
    d <- data.frame(actual = rv$model$model[[1]], fitted = fitted(rv$model))
    p <- ggplot(d, aes(x = actual, y = fitted)) +
         geom_point(color = "#2C7BB6", size = 2.5, alpha = 0.7) +
         geom_abline(color = "#D7191C", linetype = "dashed", linewidth = 1) +
         theme_minimal(base_size = 12) +
         labs(title = "Fitted vs Actual", x = "Actual", y = "Fitted") +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p)
  })

  output$reg_plot_resid <- renderPlotly({
    req(rv$model)
    d <- data.frame(fitted = fitted(rv$model), resid = residuals(rv$model))
    p <- ggplot(d, aes(x = fitted, y = resid)) +
         geom_point(color = "#2C7BB6", size = 2.5, alpha = 0.7) +
         geom_hline(yintercept = 0, color = "#D7191C", linetype = "dashed", linewidth = 1) +
         theme_minimal(base_size = 12) +
         labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p)
  })

  output$dl_reg <- downloadHandler(
    filename = function() paste0("model_summary_", Sys.Date(), ".txt"),
    content  = function(f) {
      req(rv$model)
      capture.output(summary(rv$model), file = f)
    }
  )

  # ── Export ────────────────────────────────────────────────

  output$exp_preview <- renderPlot(
    { req(gg_plot()); gg_plot() },
    bg = "white"
  )

  output$exp_data_tbl <- renderDT({
    req(rv$data)
    datatable(rv$data, rownames = FALSE, class = "compact stripe",
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
      ggsave(f, gg_plot(), width = input$exp_w, height = input$exp_h,
             device = cairo_pdf)
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
    content  = function(f) { req(rv$data); write.csv(rv$data, f, row.names = FALSE) }
  )

  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("data_", Sys.Date(), ".xlsx"),
    content  = function(f) { req(rv$data); write_xlsx(rv$data, f) }
  )
}

shinyApp(ui, server)

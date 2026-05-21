library(shiny)
library(plotly)

ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "
      body { background: #f4f6f8; color: #0d3b66; }
      .shiny-input-container > label { color: #0d3b66; font-weight: 600; }
      .well, .box { background: #ffffff; border: 1px solid #d8dce0; border-radius: 10px; }
      .tabbable > .nav > li > a { color: #0d3b66; }
      .tabbable > .nav > li.active > a, .tabbable > .nav > li.active > a:hover { background: #e87722; color: #ffffff; }
      .btn-primary { background-color: #e87722 !important; border-color: #0d3b66 !important; }
      .btn-primary:hover { background-color: #d0641d !important; }
      .btn-info { background-color: #0d3b66 !important; border-color: #e87722 !important; }
      .btn-info:hover { background-color: #0b2b4d !important; }
      .card { border: 1px solid #d8dce0; border-radius: 10px; background: #fcfcfc; padding: 12px; margin-bottom: 12px; }
      .card-title { font-size: 1rem; font-weight: 700; margin-bottom: 6px; color: #0d3b66; }
      .card-value { font-size: 1.4rem; color: #e87722; }
      .metric-row { display: flex; justify-content: space-between; align-items: center; margin-bottom: 4px; }
      .metric-label { color: #444; }
      .section-title { margin-top: 0; margin-bottom: 12px; color: #0d3b66; }
      "
    )),
  ),
  fluidRow(
    column(
      width = 12,
      div(style = "padding: 18px 0 8px;",
        tags$h2("Regression Explorer"),
        tags$p("Interactive Shiny tool for learning linear regression both directions — data to line, and line to data."),
        tags$p("Designed with a clean UF-style orange and blue theme. Use the tabs to switch between the two workflows.")
      )
    )
  ),
  tabsetPanel(
    id = "main_tabs",
    tabPanel(
      title = "Data → Line",
      value = "tab1",
      fluidRow(
        column(
          width = 4,
          wellPanel(
            tags$h4("Add data points"),
            radioButtons(
              inputId = "tab1_mode",
              label = "Point entry mode",
              choices = c("Click to add" = "click", "Manual entry" = "manual"),
              selected = "click",
              inline = FALSE
            ),
            conditionalPanel(
              condition = "input.tab1_mode == 'manual'",
              numericInput("tab1_x", "X value", value = 0, step = 0.001),
              numericInput("tab1_y", "Y value", value = 0, step = 0.001),
              actionButton("tab1_add", "Add point", class = "btn-primary", width = "100%")
            ),
            tags$hr(),
            tags$h4("Dataset controls"),
            actionButton("tab1_remove_last", "Remove last point", class = "btn-info", width = "100%"),
            br(), br(),
            actionButton("tab1_clear", "Clear all points", class = "btn-info", width = "100%"),
            br(), br(),
            downloadButton("tab1_download", "Download CSV", class = "btn-primary", width = "100%")
          ),
          wellPanel(
            tags$h4("Current dataset"),
            textOutput("tab1_point_count"),
            tableOutput("tab1_data_table")
          ),
          wellPanel(
            tags$h4("Regression summary"),
            actionButton("tab1_fit", "Fit regression line", class = "btn-primary", width = "100%"),
            br(), br(),
            uiOutput("tab1_summary")
          )
        ),
        column(
          width = 8,
          wellPanel(
            tags$h4("Plot"),
            plotlyOutput("tab1_plot", height = "600px")
          )
        )
      )
    ),
    tabPanel(
      title = "Line → Data",
      value = "tab2",
      fluidRow(
        column(
          width = 4,
          wellPanel(
            tags$h4("True line"),
            numericInput("tab2_slope", "Slope (m)", value = 1, step = 0.1),
            numericInput("tab2_intercept", "Intercept (b)", value = 0, step = 0.1),
            tags$hr(),
            tags$h4("Sample settings"),
            sliderInput("tab2_n", "Sample size (n)", min = 10, max = 200, value = 50, step = 1),
            sliderInput("tab2_sigma", "Noise magnitude (σ)", min = 0, max = 10, value = 2, step = 0.1),
            sliderInput("tab2_hetero", "Homoskedastic → Heteroskedastic", min = 0, max = 1, value = 0.3, step = 0.01),
            actionButton("tab2_generate", "Generate sample", class = "btn-primary", width = "100%"),
            br(), br(),
            downloadButton("tab2_download", "Download CSV", class = "btn-primary", width = "100%")
          ),
          wellPanel(
            tags$h4("Sample summary"),
            uiOutput("tab2_summary")
          )
        ),
        column(
          width = 8,
          wellPanel(
            tags$h4("Plot"),
            plotlyOutput("tab2_plot", height = "600px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv_tab1 <- reactiveValues(
    data        = data.frame(x = numeric(0), y = numeric(0)),
    fit         = NULL,
    clear_count = 0L,
    xrange      = c(-20, 20),  # current visible viewport, updated by pan/zoom
    yrange      = c(-20, 20)
  )

  rv_tab2 <- reactiveValues(
    sample = data.frame(x = numeric(0), y = numeric(0)),
    fit    = NULL
  )

  # ── Track visible viewport so the click grid always covers the current view ──
  observeEvent(event_data("plotly_relayout", source = "tab1_plot"), {
    re <- event_data("plotly_relayout", source = "tab1_plot")
    if (is.null(re)) return()
    x0 <- re[["xaxis.range[0]"]]; x1 <- re[["xaxis.range[1]"]]
    y0 <- re[["yaxis.range[0]"]]; y1 <- re[["yaxis.range[1]"]]
    if (!is.null(x0) && !is.null(x1)) rv_tab1$xrange <- c(as.numeric(x0), as.numeric(x1))
    if (!is.null(y0) && !is.null(y1)) rv_tab1$yrange <- c(as.numeric(y0), as.numeric(y1))
    # Double-click reset sends autorange = TRUE instead of explicit ranges
    if (isTRUE(re[["xaxis.autorange"]])) rv_tab1$xrange <- c(-20, 20)
    if (isTRUE(re[["yaxis.autorange"]])) rv_tab1$yrange <- c(-20, 20)
  })

  # ── Tab 1 point entry via plotly click on invisible grid ────────────────────
  observeEvent(event_data("plotly_click", source = "tab1_plot"), {
    if (input$tab1_mode == "click") {
      click <- event_data("plotly_click", source = "tab1_plot")
      if (!is.null(click)) {
        rv_tab1$data <- rbind(
          rv_tab1$data,
          data.frame(x = round(click$x, 3), y = round(click$y, 3))
        )
      }
    }
  })

  # ── Tab 1 manual entry ───────────────────────────────────────────────────────
  observeEvent(input$tab1_add, {
    req(input$tab1_x, input$tab1_y)
    rv_tab1$data <- rbind(
      rv_tab1$data,
      data.frame(x = round(input$tab1_x, 3), y = round(input$tab1_y, 3))
    )
  })

  observeEvent(input$tab1_remove_last, {
    if (nrow(rv_tab1$data) > 0) rv_tab1$data <- head(rv_tab1$data, -1)
  })

  observeEvent(input$tab1_clear, {
    rv_tab1$data        <- data.frame(x = numeric(0), y = numeric(0))
    rv_tab1$fit         <- NULL
    rv_tab1$clear_count <- rv_tab1$clear_count + 1L
    rv_tab1$xrange      <- c(-20, 20)
    rv_tab1$yrange      <- c(-20, 20)
  })

  observeEvent(input$tab1_fit, {
    req(nrow(rv_tab1$data) >= 2)
    rv_tab1$fit <- summary(lm(y ~ x, data = rv_tab1$data))
  })

  # ── Tab 1 outputs ────────────────────────────────────────────────────────────
  output$tab1_point_count <- renderText({
    n <- nrow(rv_tab1$data)
    paste0(n, " point", if (n == 1) "" else "s")
  })

  output$tab1_data_table <- renderTable({
    if (nrow(rv_tab1$data) == 0) return(NULL)
    data.frame(X = round(rv_tab1$data$x, 3), Y = round(rv_tab1$data$y, 3))
  }, digits = 3)

  output$tab1_plot <- renderPlotly({
    uirev <- paste0("tab1_v", rv_tab1$clear_count)
    xr    <- rv_tab1$xrange
    yr    <- rv_tab1$yrange

    p <- plot_ly(
      source = "tab1_plot",
      x      = rv_tab1$data$x,
      y      = rv_tab1$data$y,
      type   = "scatter",
      mode   = "markers",
      marker = list(size = 10, color = "#e87722"),
      name   = "Data points",
      hovertemplate = "x: %{x:.3f}<br>y: %{y:.3f}<extra></extra>"
    )

    # Invisible 40×40 grid covering the current viewport so plotly_click fires
    # anywhere the user can see, regardless of zoom or pan position.
    if (input$tab1_mode == "click") {
      grid <- expand.grid(
        x = seq(xr[1], xr[2], length.out = 40),
        y = seq(yr[1], yr[2], length.out = 40)
      )
      p <- add_markers(
        p,
        x          = grid$x,
        y          = grid$y,
        marker     = list(size = 30, opacity = 0, color = "rgba(0,0,0,0)"),
        hoverinfo  = "none",
        showlegend = FALSE
      )
    }

    if (!is.null(rv_tab1$fit) && nrow(rv_tab1$data) >= 2) {
      fit_model <- lm(y ~ x, data = rv_tab1$data)
      # OLS is a straight line — two endpoints are all that's needed for a
      # perfectly clean segment with no intermediate-point rendering artifacts.
      line_x    <- c(xr[1], xr[2])
      line_y    <- predict(fit_model, newdata = data.frame(x = line_x))
      p <- add_trace(
        p,
        x    = line_x,
        y    = line_y,
        type = "scatter",
        mode = "lines",
        line = list(color = "#d71921", width = 3),
        name = "Fitted line",
        hoverinfo = "skip"
      )
    }

    p %>%
      layout(
        title      = list(text = "Data points and fitted regression line", x = 0.02),
        xaxis      = list(title = "x", range = xr),
        yaxis      = list(title = "y", range = yr),
        dragmode   = "pan",
        hovermode  = "closest",
        uirevision = uirev,
        plot_bgcolor  = "#ffffff",
        paper_bgcolor = "#f4f6f8"
      )
  })

  output$tab1_summary <- renderUI({
    if (is.null(rv_tab1$fit)) {
      tags$div("Press 'Fit regression line' once you have at least two points.")
    } else {
      coef         <- coef(rv_tab1$fit)
      intercept    <- round(coef[1, 1], 4)
      slope        <- round(coef[2, 1], 4)
      intercept_se <- round(coef[1, 2], 4)
      slope_se     <- round(coef[2, 2], 4)
      intercept_p  <- signif(coef[1, 4], 3)
      slope_p      <- signif(coef[2, 4], 3)
      r2           <- round(rv_tab1$fit$r.squared, 4)
      adj_r2       <- round(rv_tab1$fit$adj.r.squared, 4)
      sigma        <- round(rv_tab1$fit$sigma, 4)
      n            <- rv_tab1$fit$df[2] + 2

      tagList(
        div(class = "card",
          div(class = "card-title", "Estimated line"),
          div(class = "metric-row", span(class = "metric-label", "Slope"),     span(class = "card-value", slope)),
          div(class = "metric-row", span(class = "metric-label", "Intercept"), span(class = "card-value", intercept))
        ),
        div(class = "card",
          div(class = "card-title", "Model fit"),
          div(class = "metric-row", span(class = "metric-label", "R²"),       span(class = "card-value", r2)),
          div(class = "metric-row", span(class = "metric-label", "Adj. R²"),  span(class = "card-value", adj_r2)),
          div(class = "metric-row", span(class = "metric-label", "Residual SE"), span(class = "card-value", sigma)),
          div(class = "metric-row", span(class = "metric-label", "n"),           span(class = "card-value", n))
        ),
        div(class = "card",
          div(class = "card-title", "Coefficient tests"),
          div(class = "metric-row", span(class = "metric-label", "Slope p-value"),     span(class = "card-value", slope_p)),
          div(class = "metric-row", span(class = "metric-label", "Intercept p-value"), span(class = "card-value", intercept_p))
        )
      )
    }
  })

  output$tab1_download <- downloadHandler(
    filename = function() paste0("data_to_line_dataset_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(rv_tab1$data, file, row.names = FALSE)
  )

  # ── Tab 2 ────────────────────────────────────────────────────────────────────
  observeEvent(input$tab2_generate, {
    n            <- input$tab2_n
    x            <- sort(runif(n, min = -10, max = 10))
    scale_factor <- (1 - input$tab2_hetero) + input$tab2_hetero * (abs(x) / max(abs(x), 1))
    y            <- input$tab2_intercept + input$tab2_slope * x +
                    rnorm(n, mean = 0, sd = input$tab2_sigma * scale_factor)
    rv_tab2$sample <- data.frame(x = round(x, 3), y = round(y, 3))
    rv_tab2$fit    <- summary(lm(y ~ x, data = rv_tab2$sample))
  })

  output$tab2_plot <- renderPlotly({
    x_plot <- seq(-11, 11, length.out = 200)
    true_y <- input$tab2_intercept + input$tab2_slope * x_plot

    # Start with an empty base so no trace mode is inherited by later add_trace calls
    p <- plot_ly()

    p <- add_trace(
      p,
      x    = x_plot,
      y    = true_y,
      type = "scatter",
      mode = "lines",
      line = list(color = "#0d3b66", width = 3),
      name = "True line",
      hoverinfo = "skip"
    )

    if (nrow(rv_tab2$sample) > 0) {
      p <- add_trace(
        p,
        x      = rv_tab2$sample$x,
        y      = rv_tab2$sample$y,
        type   = "scatter",
        mode   = "markers",
        marker = list(color = "#e87722", size = 10),
        name   = "Generated sample",
        hovertemplate = "x: %{x:.3f}<br>y: %{y:.3f}<extra></extra>"
      )
    }

    if (!is.null(rv_tab2$fit) && nrow(rv_tab2$sample) >= 2) {
      fit_model <- lm(y ~ x, data = rv_tab2$sample)
      fit_y     <- predict(fit_model, newdata = data.frame(x = x_plot))
      p <- add_trace(
        p,
        x    = x_plot,
        y    = fit_y,
        type = "scatter",
        mode = "lines",
        line = list(color = "#d71921", width = 3, dash = "dash"),
        name = "Fitted line",
        hoverinfo = "skip"
      )
    }

    p %>%
      layout(
        title         = list(text = "True line, generated sample, and fitted regression", x = 0.02),
        xaxis         = list(title = "x", range = c(-11, 11)),
        yaxis         = list(title = "y"),
        dragmode      = "pan",
        hovermode     = "closest",
        plot_bgcolor  = "#ffffff",
        paper_bgcolor = "#f4f6f8"
      )
  })

  output$tab2_summary <- renderUI({
    if (nrow(rv_tab2$sample) == 0) {
      tags$div("Define the true line and generate a sample to see the fitted regression results.")
    } else {
      fitted_coef      <- coef(rv_tab2$fit)
      fitted_slope     <- round(fitted_coef[2, 1], 4)
      fitted_intercept <- round(fitted_coef[1, 1], 4)
      r2               <- round(rv_tab2$fit$r.squared, 4)
      adj_r2           <- round(rv_tab2$fit$adj.r.squared, 4)

      tagList(
        div(class = "card",
          div(class = "card-title", "True line"),
          div(class = "metric-row", span(class = "metric-label", "Slope (m)"),     span(class = "card-value", round(input$tab2_slope, 4))),
          div(class = "metric-row", span(class = "metric-label", "Intercept (b)"), span(class = "card-value", round(input$tab2_intercept, 4)))
        ),
        div(class = "card",
          div(class = "card-title", "Fitted line"),
          div(class = "metric-row", span(class = "metric-label", "Slope (m)"),     span(class = "card-value", fitted_slope)),
          div(class = "metric-row", span(class = "metric-label", "Intercept (b)"), span(class = "card-value", fitted_intercept))
        ),
        div(class = "card",
          div(class = "card-title", "Sample quality"),
          div(class = "metric-row", span(class = "metric-label", "R²"),             span(class = "card-value", r2)),
          div(class = "metric-row", span(class = "metric-label", "Adj. R²"),        span(class = "card-value", adj_r2)),
          div(class = "metric-row", span(class = "metric-label", "Noise σ"),        span(class = "card-value", round(input$tab2_sigma, 3))),
          div(class = "metric-row", span(class = "metric-label", "Heteroskedasticity"),  span(class = "card-value", round(input$tab2_hetero, 3)))
        )
      )
    }
  })

  output$tab2_download <- downloadHandler(
    filename = function() paste0("line_to_data_sample_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(rv_tab2$sample, file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)

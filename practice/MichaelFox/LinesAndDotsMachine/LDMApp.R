library(shiny)
library(plotly)

# ---- Info-tip helper ---------------------------------------------------------
# Returns a small blue (i) icon; on hover shows a Bootstrap tooltip.
info_tip <- function(text) {
  tags$span(
    class = "info-icon",
    `data-toggle`    = "tooltip",
    `data-placement` = "right",
    title = text,
    "i"
  )
}

# Shortcut: label text + info icon for input label= arguments
lbl <- function(text, tip) tagList(text, info_tip(tip))

# ---- UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML(
      "body { background: #f4f6f8; color: #0d3b66; }
      .shiny-input-container > label { color: #0d3b66; font-weight: 600; }
      .well, .box { background: #ffffff; border: 1px solid #d8dce0; border-radius: 10px; }
      .tabbable > .nav > li > a { color: #0d3b66; }
      .tabbable > .nav > li.active > a,
      .tabbable > .nav > li.active > a:hover { background: #e87722; color: #ffffff; }
      .btn-primary { background-color: #e87722 !important; border-color: #0d3b66 !important; }
      .btn-primary:hover { background-color: #d0641d !important; }
      .btn-info { background-color: #0d3b66 !important; border-color: #e87722 !important; }
      .btn-info:hover { background-color: #0b2b4d !important; }
      .card { border: 1px solid #d8dce0; border-radius: 10px; background: #fcfcfc;
              padding: 12px; margin-bottom: 12px; }
      .card-title { font-size: 1rem; font-weight: 700; margin-bottom: 6px; color: #0d3b66; }
      .card-value { font-size: 1.4rem; color: #e87722; }
      .metric-row { display: flex; justify-content: space-between;
                    align-items: center; margin-bottom: 4px; }
      .metric-label { color: #444; }
      .plot-hint { font-size: 0.97rem; color: #555; margin-bottom: 6px; font-style: italic; }
      .func-desc { font-size: 0.82rem; color: #666; font-style: italic; margin: -6px 0 8px 0; }
      .equation { font-size: 1.15rem; color: #0d3b66; font-family: 'Cambria Math', 'Times New Roman', serif;
                  font-style: italic; padding: 6px 8px 10px 0; border-bottom: 1px solid #ececec;
                  margin: 2px 0 10px 0; }
      .about-section { padding: 4px 4px 12px; }
      .about-section h4 { color: #0d3b66; margin-top: 18px; }
      .about-section p, .about-section li { color: #222; line-height: 1.5; }
      .info-icon {
        display: inline-block;
        width: 15px; height: 15px;
        background-color: #0d3b66;
        color: #ffffff;
        border-radius: 50%;
        font-size: 10px;
        font-weight: bold;
        font-style: italic;
        text-align: center;
        line-height: 15px;
        cursor: help;
        margin-left: 5px;
        vertical-align: middle;
        user-select: none;
      }"
    )),
    # Delegated tooltip init so dynamic (renderUI) content also gets tooltips
    tags$script(HTML(
      "$(function() { $('body').tooltip({ selector: '[data-toggle=\"tooltip\"]',
                                          container: 'body' }); });"
    ))
  ),

  fluidRow(
    column(12,
      div(style = "padding: 18px 0 8px;",
        tags$h2("Regression Explorer"),
        tags$p("Interactive tool for learning regression -- data to curve, and function to data."),
        tags$p("Supports polynomial fitting, non-linear true functions, residual diagnostics,
               and confidence/prediction bands.")
      )
    )
  ),

  tabsetPanel(
    id = "main_tabs",

    # ---- Tab 1: Data to Line -------------------------------------------------
    tabPanel(
      title = "Data to Line",
      value = "tab1",
      fluidRow(
        column(4,
          wellPanel(
            tags$h4("Add data points"),
            radioButtons("tab1_mode", "Point entry mode",
              choices  = c("Click to add" = "click", "Manual entry" = "manual"),
              selected = "click", inline = FALSE
            ),
            conditionalPanel(
              condition = "input.tab1_mode == 'manual'",
              numericInput("tab1_x", "X value", value = 0, step = 0.001),
              numericInput("tab1_y", "Y value", value = 0, step = 0.001),
              actionButton("tab1_add", "Add point", class = "btn-primary", width = "100%")
            ),
            tags$hr(),
            tags$h4("Dataset controls"),
            actionButton("tab1_remove_last", "Remove last point", class = "btn-info",  width = "100%"),
            br(), br(),
            actionButton("tab1_clear",       "Clear all points",  class = "btn-info",  width = "100%"),
            br(), br(),
            downloadButton("tab1_download_csv", "Download CSV",   class = "btn-primary", width = "100%")
          ),

          wellPanel(
            tags$h4("Regression settings"),

            sliderInput("tab1_degree",
              label = lbl("Polynomial degree",
                "Degree 1 is a straight line. Higher degrees allow curves but can overfit with too few data points -- the curve passes through every point but generalizes poorly."),
              min = 1, max = 5, value = 1, step = 1),

            checkboxInput("tab1_show_ci",
              label = lbl("Show 95% confidence band",
                "The range where the true regression line likely lies. Narrower near the center of your data, wider near the edges.")),

            checkboxInput("tab1_show_pi",
              label = lbl("Show 95% prediction band",
                "The range where a new single observation would likely fall. Always wider than the confidence band because it also accounts for natural scatter around the line.")),

            tags$hr(),
            actionButton("tab1_fit", "Fit regression", class = "btn-primary", width = "100%"),
            br(), br(),
            uiOutput("tab1_summary")
          ),

          wellPanel(
            tags$h4("Current dataset"),
            textOutput("tab1_point_count"),
            tableOutput("tab1_data_table")
          )
        ),

        column(8,
          wellPanel(
            tags$h4("Scatter plot & fitted curve"),
            tags$p(class = "plot-hint",
              "Tip: click the camera icon in the plot toolbar to save a PNG of this chart."),
            plotlyOutput("tab1_plot", height = "420px")
          ),
          wellPanel(
            tags$h4("Residuals vs. Fitted"),
            tags$p(class = "plot-hint",
              "Tip: click the camera icon in the plot toolbar to save a PNG of this chart."),
            plotlyOutput("tab1_resid_plot", height = "280px")
          )
        )
      )
    ),

    # ---- Tab 2: Line to Data -------------------------------------------------
    tabPanel(
      title = "Line to Data",
      value = "tab2",
      fluidRow(
        column(4,
          wellPanel(
            tags$h4("True function"),

            selectInput("tab2_func_type",
              label = lbl("Function type",
                "The true mathematical shape of the relationship. The app always fits a straight OLS line to the generated data -- observe what happens when the function is non-linear."),
              choices  = c("Linear", "Quadratic", "Exponential", "Logarithmic", "Sine"),
              selected = "Linear"
            ),

            # Linear
            conditionalPanel(
              condition = "input.tab2_func_type == 'Linear'",
              tags$p(class = "func-desc", "y = m * x + b  --  a straight line"),
              numericInput("tab2_slope",     "Slope (m)",     value = 1,   step = 0.1),
              numericInput("tab2_intercept", "Intercept (b)", value = 0,   step = 0.1)
            ),
            # Quadratic
            conditionalPanel(
              condition = "input.tab2_func_type == 'Quadratic'",
              tags$p(class = "func-desc", "y = a*x^2 + b*x + c  --  parabola; a controls direction and steepness"),
              numericInput("tab2_quad_a", "a  (x^2 coef)",  value =  0.5, step = 0.1),
              numericInput("tab2_quad_b", "b  (x coef)",    value =  0,   step = 0.1),
              numericInput("tab2_quad_c", "c  (intercept)", value =  0,   step = 0.1)
            ),
            # Exponential
            conditionalPanel(
              condition = "input.tab2_func_type == 'Exponential'",
              tags$p(class = "func-desc", "y = a * exp(b*x)  --  grows or decays at a rate proportional to its current value"),
              numericInput("tab2_exp_a", "a  (amplitude)", value =  1,   step = 0.1),
              numericInput("tab2_exp_b", "b  (rate)",      value =  0.3, step = 0.05)
            ),
            # Logarithmic
            conditionalPanel(
              condition = "input.tab2_func_type == 'Logarithmic'",
              tags$p(class = "func-desc", "y = a * ln(x) + b  --  rises quickly then levels off; x must be positive"),
              numericInput("tab2_log_a", "a  (log coef)",  value =  2,   step = 0.1),
              numericInput("tab2_log_b", "b  (intercept)", value =  0,   step = 0.1)
            ),
            # Sine
            conditionalPanel(
              condition = "input.tab2_func_type == 'Sine'",
              tags$p(class = "func-desc", "y = a * sin(b*x + c)  --  oscillates periodically; common in seasonal or cyclical data"),
              numericInput("tab2_sin_a", "a  (amplitude)", value =  3,   step = 0.1),
              numericInput("tab2_sin_b", "b  (frequency)", value =  1,   step = 0.1),
              numericInput("tab2_sin_c", "c  (phase)",     value =  0,   step = 0.1)
            ),

            tags$hr(),
            tags$h4("Sample settings"),

            sliderInput("tab2_n", "Sample size (n)",
              min = 10, max = 200, value = 50, step = 1),

            sliderInput("tab2_sigma",
              label = lbl("Noise magnitude (sigma)",
                "Standard deviation of random error added to each data point. Higher values create more scatter around the true function."),
              min = 0, max = 10, value = 2, step = 0.1),

            sliderInput("tab2_hetero",
              label = lbl("Homoskedastic to Heteroskedastic",
                "Left end: error variance is constant across all x values (homoskedastic). Right end: variance grows with |x| (heteroskedastic). OLS assumes constant variance -- check the residuals plot for a fan shape."),
              min = 0, max = 1, value = 0.3, step = 0.01),

            checkboxInput("tab2_show_ci",
              label = lbl("Show 95% confidence band",
                "The range where the true regression line likely lies. Narrower near the center of your data, wider near the edges.")),

            checkboxInput("tab2_show_pi",
              label = lbl("Show 95% prediction band",
                "The range where a new single observation would likely fall. Always wider than the confidence band because it also accounts for natural scatter around the line.")),

            tags$hr(),
            actionButton("tab2_generate", "Generate sample", class = "btn-primary", width = "100%"),
            br(), br(),
            downloadButton("tab2_download_csv", "Download CSV", class = "btn-primary", width = "100%")
          ),

          wellPanel(
            tags$h4("Sample summary"),
            uiOutput("tab2_summary")
          )
        ),

        column(8,
          wellPanel(
            tags$h4("True function, generated sample & fitted OLS line"),
            tags$p(class = "plot-hint",
              "Tip: click the camera icon in the plot toolbar to save a PNG of this chart."),
            plotlyOutput("tab2_plot", height = "420px")
          ),
          wellPanel(
            tags$h4("Residuals vs. Fitted"),
            tags$p(class = "plot-hint",
              "Tip: click the camera icon in the plot toolbar to save a PNG of this chart."),
            plotlyOutput("tab2_resid_plot", height = "280px")
          )
        )
      )
    ),

    # ---- Tab 3: About --------------------------------------------------------
    tabPanel(
      title = "About",
      value = "tab3",
      fluidRow(
        column(10, offset = 1,
          wellPanel(
            div(class = "about-section",
              tags$h3("About Regression Explorer"),
              tags$p("Regression Explorer is an interactive teaching tool for building intuition about how linear and polynomial regression work. You can experiment from two directions: place data points and watch a curve fit them, or define a true relationship and watch a noisy sample emerge from it. The contrast between the two perspectives is the point -- regression is easier to understand when you have seen both sides of the same coin."),

              tags$h4("Tab 1 -- Data to Line"),
              tags$p("Construct a dataset by hand, then fit a regression curve to it. Useful for exploring what 'best fit' means, how data shape constrains the fitted line, and what happens when you ask a model to do too much with too little."),
              tags$ol(
                tags$li(tags$b("Add points."), " Use click-to-add mode to drop points directly on the plot, or switch to manual entry and type X and Y values."),
                tags$li(tags$b("Choose a polynomial degree."), " Degree 1 is a straight line. Degrees 2 through 5 allow curvature. With only a few points, a high-degree curve will pass through every one of them but generalize poorly -- this is overfitting."),
                tags$li(tags$b("Press 'Fit regression'."), " The fitted equation, coefficients, R-squared, adjusted R-squared, and residual standard error appear below the controls."),
                tags$li(tags$b("Show the confidence and prediction bands."), " The 95% CI shows where the true line likely lies; the 95% PI shows where a new observation likely falls. PI is always wider."),
                tags$li(tags$b("Read the residuals plot."), " A good fit produces residuals scattered randomly around zero. Curved patterns mean the wrong model shape; fan shapes mean the noise level is not constant.")
              ),

              tags$h4("Tab 2 -- Line to Data"),
              tags$p("Define a 'true' relationship between x and y, then generate a random sample drawn from it. The app fits a straight OLS line to whatever sample you generate -- so you can directly observe what happens when OLS is the wrong tool for the job."),
              tags$ol(
                tags$li(tags$b("Pick a function type"), " (linear, quadratic, exponential, logarithmic, or sine) and set its parameters."),
                tags$li(tags$b("Choose sample size and noise level."), " Larger samples produce tighter estimates; more noise produces more scatter around the true curve."),
                tags$li(tags$b("Adjust heteroskedasticity."), " Slide right to make noise grow with the magnitude of x, producing a funnel shape. OLS assumes constant noise, so the residuals plot will reveal the violation."),
                tags$li(tags$b("Press 'Generate sample'."), " Each click draws a fresh random sample. Run it a few times in a row to see how much the OLS fit jumps around between samples."),
                tags$li(tags$b("Compare true vs. fitted."), " When the true function is non-linear, the straight OLS line will be visibly wrong and the residuals will show a clear curved pattern -- exactly the kind of diagnostic clue you would look for in real data.")
              ),

              tags$h4("Key concepts you will see"),
              tags$ul(
                tags$li(tags$b("R-squared (R²):"), " The proportion of variation in y explained by the model. Between 0 and 1; higher is better, but adding terms always inflates it."),
                tags$li(tags$b("Adjusted R-squared:"), " R-squared penalized for model complexity. Improves only when a new term genuinely helps -- use it when comparing models of different degrees."),
                tags$li(tags$b("Residual:"), " The vertical distance between an observed point and the value the model predicts for it. The residuals plot is where assumption violations show up most clearly."),
                tags$li(tags$b("Confidence band (95% CI):"), " The range where the true regression line is likely to lie. Narrowest near the center of your data, wider at the edges."),
                tags$li(tags$b("Prediction band (95% PI):"), " The range where a single new observation is likely to fall. Always wider than the CI because it includes the natural scatter around the line."),
                tags$li(tags$b("Homoskedasticity vs. heteroskedasticity:"), " Whether the spread of residuals stays constant across x. OLS assumes it does; a fan-shaped residuals plot says it does not.")
              ),

              tags$h4("Tips for using this tool"),
              tags$ul(
                tags$li("Hover over any small (i) icon in the app for a short explanation of that option."),
                tags$li("In Tab 1, try fitting the same handful of points at degree 1, 3, and 5 to see overfitting in action."),
                tags$li("In Tab 2, regenerate the same sample several times in a row -- you will see how much OLS estimates wander from one sample to the next, even with everything else held constant."),
                tags$li("Both tabs let you download the current dataset as a CSV, which is useful for practice exercises or for moving data into other tools."),
                tags$li("Click the camera icon in any plot toolbar to save it as a PNG.")
              )
            )
          )
        )
      )
    )
  )
)

# ---- Helpers -----------------------------------------------------------------

true_y_values <- function(x, func_type, params) {
  switch(func_type,
    "Linear"      = params$intercept + params$slope * x,
    "Quadratic"   = params$a * x^2 + params$b * x + params$c,
    "Exponential" = params$a * exp(params$b * x),
    "Logarithmic" = params$a * log(x) + params$b,
    "Sine"        = params$a * sin(params$b * x + params$c)
  )
}

func_x_range <- function(func_type) {
  switch(func_type,
    "Logarithmic" = c(0.5, 10),
    "Exponential" = c(-3, 3),
    c(-10, 10)
  )
}

dl_config <- function(p, filename) {
  config(p,
    toImageButtonOptions = list(format = "png", filename = filename, scale = 2),
    displaylogo = FALSE
  )
}

# Metric row with optional info tip on the label
metric_row <- function(label, value, tip = NULL) {
  lbl_content <- if (is.null(tip)) label else tagList(label, info_tip(tip))
  div(class = "metric-row",
    span(class = "metric-label", lbl_content),
    span(class = "card-value",   value)
  )
}

# Render a fitted polynomial as a readable equation.
# coefs: numeric vector c(b0, b1, b2, ...) where b0 is the intercept.
# Returns an HTML object suitable for inclusion in a tag.
format_equation <- function(coefs) {
  coefs <- round(coefs, 4)
  parts <- character(length(coefs))
  parts[1] <- format(coefs[1], trim = TRUE)
  if (length(coefs) >= 2) {
    for (i in 2:length(coefs)) {
      power    <- i - 1
      v        <- coefs[i]
      sign_str <- if (v >= 0) " + " else " &minus; "
      x_part   <- if (power == 1) "x" else paste0("x<sup>", power, "</sup>")
      parts[i] <- paste0(sign_str, format(abs(v), trim = TRUE), " ", x_part)
    }
  }
  HTML(paste0("y &asymp; ", paste(parts, collapse = "")))
}

# ---- Server ------------------------------------------------------------------

server <- function(input, output, session) {

  rv1 <- reactiveValues(
    data        = data.frame(x = numeric(0), y = numeric(0)),
    fit_lm      = NULL,
    clear_count = 0L,
    xrange      = c(-20, 20),
    yrange      = c(-20, 20)
  )

  rv2 <- reactiveValues(
    sample      = data.frame(x = numeric(0), y = numeric(0)),
    fit_lm      = NULL,
    func_type   = "Linear",
    true_params = list(),
    x_range     = c(-11, 11)
  )

  # ---- Tab 1: viewport tracking ----------------------------------------------
  observeEvent(event_data("plotly_relayout", source = "tab1_plot"), {
    re <- event_data("plotly_relayout", source = "tab1_plot")
    if (is.null(re)) return()
    x0 <- re[["xaxis.range[0]"]]; x1 <- re[["xaxis.range[1]"]]
    y0 <- re[["yaxis.range[0]"]]; y1 <- re[["yaxis.range[1]"]]
    if (!is.null(x0) && !is.null(x1)) rv1$xrange <- c(as.numeric(x0), as.numeric(x1))
    if (!is.null(y0) && !is.null(y1)) rv1$yrange <- c(as.numeric(y0), as.numeric(y1))
    if (isTRUE(re[["xaxis.autorange"]])) rv1$xrange <- c(-20, 20)
    if (isTRUE(re[["yaxis.autorange"]])) rv1$yrange <- c(-20, 20)
  })

  # ---- Tab 1: point entry ----------------------------------------------------
  observeEvent(event_data("plotly_click", source = "tab1_plot"), {
    if (input$tab1_mode == "click") {
      click <- event_data("plotly_click", source = "tab1_plot")
      if (!is.null(click))
        rv1$data <- rbind(rv1$data, data.frame(x = round(click$x, 3), y = round(click$y, 3)))
    }
  })

  observeEvent(input$tab1_add, {
    req(input$tab1_x, input$tab1_y)
    rv1$data <- rbind(rv1$data, data.frame(x = round(input$tab1_x, 3), y = round(input$tab1_y, 3)))
  })

  observeEvent(input$tab1_remove_last, {
    if (nrow(rv1$data) > 0) rv1$data <- head(rv1$data, -1)
  })

  observeEvent(input$tab1_clear, {
    rv1$data        <- data.frame(x = numeric(0), y = numeric(0))
    rv1$fit_lm      <- NULL
    rv1$clear_count <- rv1$clear_count + 1L
    rv1$xrange      <- c(-20, 20)
    rv1$yrange      <- c(-20, 20)
  })

  observeEvent(input$tab1_fit, {
    req(nrow(rv1$data) >= input$tab1_degree + 1)
    d <- input$tab1_degree
    rv1$fit_lm <- if (d == 1) {
      lm(y ~ x, data = rv1$data)
    } else {
      lm(y ~ poly(x, d, raw = TRUE), data = rv1$data)
    }
  })

  # ---- Tab 1: main plot ------------------------------------------------------
  output$tab1_plot <- renderPlotly({
    uirev <- paste0("tab1_v", rv1$clear_count)
    xr    <- rv1$xrange
    yr    <- rv1$yrange

    # Empty base avoids data-inheritance conflicts with add_ribbons
    p <- plot_ly(source = "tab1_plot")

    p <- add_trace(p,
      x = rv1$data$x, y = rv1$data$y,
      type = "scatter", mode = "markers",
      marker = list(size = 10, color = "#e87722"),
      name  = "Data points",
      hovertemplate = "x: %{x:.3f}<br>y: %{y:.3f}<extra></extra>"
    )

    if (input$tab1_mode == "click") {
      grid <- expand.grid(
        x = seq(xr[1], xr[2], length.out = 40),
        y = seq(yr[1], yr[2], length.out = 40)
      )
      p <- add_markers(p, x = grid$x, y = grid$y,
        marker = list(size = 30, opacity = 0, color = "rgba(0,0,0,0)"),
        hoverinfo = "none", showlegend = FALSE)
    }

    if (!is.null(rv1$fit_lm)) {
      line_x <- seq(xr[1], xr[2], length.out = 200)
      nd     <- data.frame(x = line_x)
      line_y <- predict(rv1$fit_lm, newdata = nd)

      if (input$tab1_show_ci) {
        ci <- predict(rv1$fit_lm, newdata = nd, interval = "confidence")
        p  <- add_ribbons(p, x = line_x, ymin = ci[, "lwr"], ymax = ci[, "upr"],
               fillcolor = "rgba(13,59,102,0.15)", line = list(color = "transparent"),
               name = "95% CI")
      }
      if (input$tab1_show_pi) {
        pi_int <- predict(rv1$fit_lm, newdata = nd, interval = "prediction")
        p      <- add_ribbons(p, x = line_x, ymin = pi_int[, "lwr"], ymax = pi_int[, "upr"],
               fillcolor = "rgba(232,119,34,0.10)", line = list(color = "transparent"),
               name = "95% PI")
      }

      curve_label <- if (input$tab1_degree == 1) "Fitted line" else
        paste0("Fitted curve (degree ", input$tab1_degree, ")")
      p <- add_trace(p,
        x = line_x, y = line_y,
        type = "scatter", mode = "lines",
        line = list(color = "#d71921", width = 3),
        name = curve_label, hoverinfo = "skip")
    }

    p %>%
      layout(
        xaxis      = list(title = "x", range = xr),
        yaxis      = list(title = "y", range = yr),
        dragmode   = "pan", hovermode = "closest",
        uirevision = uirev,
        plot_bgcolor = "#ffffff", paper_bgcolor = "#f4f6f8"
      ) %>%
      dl_config("data_to_line_plot")
  })

  # ---- Tab 1: residuals plot -------------------------------------------------
  output$tab1_resid_plot <- renderPlotly({
    validate(need(!is.null(rv1$fit_lm), "Fit a regression curve to see residuals."))
    fv <- fitted(rv1$fit_lm)
    rv <- residuals(rv1$fit_lm)

    plot_ly() %>%
      add_trace(x = fv, y = rv,
        type = "scatter", mode = "markers",
        marker = list(color = "#e87722", size = 8),
        name  = "Residuals",
        hovertemplate = "Fitted: %{x:.3f}<br>Residual: %{y:.3f}<extra></extra>") %>%
      add_trace(x = range(fv), y = c(0, 0),
        type = "scatter", mode = "lines",
        line = list(color = "#0d3b66", width = 2, dash = "dot"),
        showlegend = FALSE, hoverinfo = "skip") %>%
      layout(
        xaxis = list(title = "Fitted values"),
        yaxis = list(title = "Residuals"),
        dragmode = "pan", hovermode = "closest",
        plot_bgcolor = "#ffffff", paper_bgcolor = "#f4f6f8"
      ) %>%
      dl_config("data_to_line_residuals")
  })

  # ---- Tab 1: summary --------------------------------------------------------
  output$tab1_summary <- renderUI({
    if (is.null(rv1$fit_lm))
      return(tags$div("Press Fit regression -- need at least (degree + 1) points."))

    fit_s  <- summary(rv1$fit_lm)
    ctbl   <- coef(fit_s)
    degree <- nrow(ctbl) - 1
    r2     <- round(fit_s$r.squared,     4)
    adj_r2 <- round(fit_s$adj.r.squared, 4)
    sigma  <- round(fit_s$sigma,         4)
    n      <- fit_s$df[2] + nrow(ctbl)

    sup <- c("", "^2", "^3", "^4", "^5")
    coef_labels <- c("b0 (intercept)",
      paste0("b", seq_len(degree), " (x", sup[seq_len(degree)], ")"))
    coef_vals <- round(ctbl[, 1], 4)
    coef_rows <- mapply(function(l, v)
      div(class = "metric-row",
        span(class = "metric-label", l),
        span(class = "card-value",   v)),
      coef_labels, coef_vals, SIMPLIFY = FALSE)

    tagList(
      div(class = "card",
        div(class = "card-title", paste0("Fitted polynomial (degree ", degree, ")")),
        div(class = "equation", format_equation(coef_vals)),
        tagList(coef_rows)
      ),
      div(class = "card",
        div(class = "card-title", "Model fit"),
        metric_row("R-squared", r2,
          "Proportion of variance in y explained by the model. Ranges 0 to 1 -- higher is better, but can be inflated by adding more terms."),
        metric_row("Adj. R-sq", adj_r2,
          "R-squared penalized for model complexity. Only improves when a new term genuinely helps. Preferred over plain R-squared when comparing models of different degrees."),
        metric_row("Residual SE", sigma,
          "Average distance of data points from the fitted curve, in units of y. Smaller values mean the model fits more closely."),
        metric_row("n", n)
      )
    )
  })

  output$tab1_point_count <- renderText({
    n <- nrow(rv1$data)
    paste0(n, " point", if (n == 1) "" else "s")
  })

  output$tab1_data_table <- renderTable({
    if (nrow(rv1$data) == 0) return(NULL)
    data.frame(X = round(rv1$data$x, 3), Y = round(rv1$data$y, 3))
  }, digits = 3)

  output$tab1_download_csv <- downloadHandler(
    filename = function() paste0("data_to_line_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(rv1$data, file, row.names = FALSE)
  )

  # ---- Tab 2: generate sample ------------------------------------------------
  observeEvent(input$tab2_generate, {
    ft <- input$tab2_func_type
    xr <- func_x_range(ft)
    n  <- input$tab2_n
    x  <- sort(runif(n, xr[1], xr[2]))

    params <- switch(ft,
      "Linear"      = list(slope = input$tab2_slope, intercept = input$tab2_intercept),
      "Quadratic"   = list(a = input$tab2_quad_a, b = input$tab2_quad_b, c = input$tab2_quad_c),
      "Exponential" = list(a = input$tab2_exp_a,  b = input$tab2_exp_b),
      "Logarithmic" = list(a = input$tab2_log_a,  b = input$tab2_log_b),
      "Sine"        = list(a = input$tab2_sin_a,  b = input$tab2_sin_b, c = input$tab2_sin_c)
    )

    y_true       <- true_y_values(x, ft, params)
    scale_factor <- (1 - input$tab2_hetero) +
                    input$tab2_hetero * (abs(x) / max(abs(x), 1))
    y            <- y_true + rnorm(n, 0, input$tab2_sigma * scale_factor)

    rv2$sample      <- data.frame(x = round(x, 3), y = round(y, 3))
    rv2$fit_lm      <- lm(y ~ x, data = rv2$sample)
    rv2$func_type   <- ft
    rv2$true_params <- params
    rv2$x_range     <- xr
  })

  # True curve reactive -- updates live when sliders change
  tab2_curve <- reactive({
    ft     <- input$tab2_func_type
    xr     <- func_x_range(ft)
    x_plot <- seq(xr[1], xr[2], length.out = 300)

    params <- switch(ft,
      "Linear"      = list(slope = input$tab2_slope, intercept = input$tab2_intercept),
      "Quadratic"   = list(a = input$tab2_quad_a, b = input$tab2_quad_b, c = input$tab2_quad_c),
      "Exponential" = list(a = input$tab2_exp_a,  b = input$tab2_exp_b),
      "Logarithmic" = list(a = input$tab2_log_a,  b = input$tab2_log_b),
      "Sine"        = list(a = input$tab2_sin_a,  b = input$tab2_sin_b, c = input$tab2_sin_c)
    )

    list(x = x_plot, y = true_y_values(x_plot, ft, params), x_range = xr)
  })

  # ---- Tab 2: main plot ------------------------------------------------------
  output$tab2_plot <- renderPlotly({
    curve <- tab2_curve()
    xr    <- c(curve$x_range[1] - 0.3, curve$x_range[2] + 0.3)

    p <- plot_ly() %>%
      add_trace(x = curve$x, y = curve$y,
        type = "scatter", mode = "lines",
        line = list(color = "#0d3b66", width = 3),
        name = "True function", hoverinfo = "skip")

    if (nrow(rv2$sample) > 0) {
      p <- add_trace(p,
        x = rv2$sample$x, y = rv2$sample$y,
        type = "scatter", mode = "markers",
        marker = list(color = "#e87722", size = 8),
        name  = "Generated sample",
        hovertemplate = "x: %{x:.3f}<br>y: %{y:.3f}<extra></extra>")
    }

    if (!is.null(rv2$fit_lm)) {
      nd    <- data.frame(x = curve$x)
      fit_y <- predict(rv2$fit_lm, newdata = nd)

      if (input$tab2_show_ci) {
        ci <- predict(rv2$fit_lm, newdata = nd, interval = "confidence")
        p  <- add_ribbons(p, x = curve$x, ymin = ci[, "lwr"], ymax = ci[, "upr"],
               fillcolor = "rgba(215,25,33,0.12)", line = list(color = "transparent"),
               name = "95% CI")
      }
      if (input$tab2_show_pi) {
        pi_int <- predict(rv2$fit_lm, newdata = nd, interval = "prediction")
        p      <- add_ribbons(p, x = curve$x, ymin = pi_int[, "lwr"], ymax = pi_int[, "upr"],
               fillcolor = "rgba(232,119,34,0.08)", line = list(color = "transparent"),
               name = "95% PI")
      }

      p <- add_trace(p,
        x = curve$x, y = fit_y,
        type = "scatter", mode = "lines",
        line = list(color = "#d71921", width = 3, dash = "dash"),
        name = "Fitted OLS line", hoverinfo = "skip")
    }

    p %>%
      layout(
        xaxis = list(title = "x", range = xr),
        yaxis = list(title = "y"),
        dragmode = "pan", hovermode = "closest",
        plot_bgcolor = "#ffffff", paper_bgcolor = "#f4f6f8"
      ) %>%
      dl_config("line_to_data_plot")
  })

  # ---- Tab 2: residuals plot -------------------------------------------------
  output$tab2_resid_plot <- renderPlotly({
    validate(need(!is.null(rv2$fit_lm), "Generate a sample to see residuals."))
    fv <- fitted(rv2$fit_lm)
    rv <- residuals(rv2$fit_lm)

    plot_ly() %>%
      add_trace(x = fv, y = rv,
        type = "scatter", mode = "markers",
        marker = list(color = "#e87722", size = 8),
        name  = "Residuals",
        hovertemplate = "Fitted: %{x:.3f}<br>Residual: %{y:.3f}<extra></extra>") %>%
      add_trace(x = range(fv), y = c(0, 0),
        type = "scatter", mode = "lines",
        line = list(color = "#0d3b66", width = 2, dash = "dot"),
        showlegend = FALSE, hoverinfo = "skip") %>%
      layout(
        xaxis = list(title = "Fitted values"),
        yaxis = list(title = "Residuals"),
        dragmode = "pan", hovermode = "closest",
        plot_bgcolor = "#ffffff", paper_bgcolor = "#f4f6f8"
      ) %>%
      dl_config("line_to_data_residuals")
  })

  # ---- Tab 2: summary --------------------------------------------------------
  output$tab2_summary <- renderUI({
    if (nrow(rv2$sample) == 0)
      return(tags$div("Define the true function and generate a sample to see results."))

    fit_s  <- summary(rv2$fit_lm)
    fc     <- coef(fit_s)
    r2     <- round(fit_s$r.squared,     4)
    adj_r2 <- round(fit_s$adj.r.squared, 4)
    ft     <- rv2$func_type
    params <- rv2$true_params

    true_rows <- switch(ft,
      "Linear" = tagList(
        metric_row("Slope (m)",     round(params$slope,     4)),
        metric_row("Intercept (b)", round(params$intercept, 4))
      ),
      "Quadratic" = tagList(
        metric_row("a (x^2 coef)",  round(params$a, 4)),
        metric_row("b (x coef)",    round(params$b, 4)),
        metric_row("c (intercept)", round(params$c, 4))
      ),
      "Exponential" = tagList(
        metric_row("a (amplitude)", round(params$a, 4)),
        metric_row("b (rate)",      round(params$b, 4))
      ),
      "Logarithmic" = tagList(
        metric_row("a (log coef)",  round(params$a, 4)),
        metric_row("b (intercept)", round(params$b, 4))
      ),
      "Sine" = tagList(
        metric_row("a (amplitude)", round(params$a, 4)),
        metric_row("b (frequency)", round(params$b, 4)),
        metric_row("c (phase)",     round(params$c, 4))
      )
    )

    tagList(
      div(class = "card",
        div(class = "card-title", paste0("True function: ", ft)),
        true_rows
      ),
      div(class = "card",
        div(class = "card-title",
          "Fitted OLS line",
          info_tip("Ordinary Least Squares always fits a straight line through the data. When the true function is non-linear, this line is misspecified -- R-squared will be low and residuals will show a curved pattern rather than random scatter.")
        ),
        div(class = "equation", format_equation(c(fc[1, 1], fc[2, 1]))),
        metric_row("Slope (m)",     round(fc[2, 1], 4)),
        metric_row("Intercept (b)", round(fc[1, 1], 4))
      ),
      div(class = "card",
        div(class = "card-title", "Sample quality"),
        metric_row("R-squared", r2,
          "Proportion of variance in y explained by the straight OLS line. Low values here suggest the linear model is a poor fit for the true non-linear function."),
        metric_row("Adj. R-sq", adj_r2,
          "R-squared adjusted for model complexity. With a single predictor this is very close to R-squared."),
        metric_row("Noise sigma",        round(input$tab2_sigma,  3)),
        metric_row("Heteroskedasticity", round(input$tab2_hetero, 3))
      )
    )
  })

  output$tab2_download_csv <- downloadHandler(
    filename = function() paste0("line_to_data_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(rv2$sample, file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)

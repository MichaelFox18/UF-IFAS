# ============================================================
# mod_export.R — the Export stage
# ============================================================
# Download the data that flows in (CSV / Excel / RDS). Optionally also export
# charts (when a `plots` reactive is supplied, e.g. from mod_visualize) and
# regression results (when a `model` reactive is supplied, e.g. from
# mod_regression). Pattern A terminal stage: returns nothing.
#
# exportServer(id, data_in, plots = NULL, model = NULL)
#   data_in : reactive(data frame | NULL)
#   plots   : reactive(list of ggplots) | NULL   -> shows a "Export charts" block
#   model   : reactive(lm | NULL)        | NULL   -> shows a "Export regression" block
#
# The chart export needs render_plots_to_file() (helpers_plot.R) and ggplot2;
# only the apps that pass `plots` (data_explorer) attach those.

exportUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Export data"),
      textInput(ns("filename"), "File name (no extension)",
                value = "data-export"),
      radioButtons(ns("fmt"), "Format",
                   choices = c("CSV (.csv)"    = "csv",
                               "Excel (.xlsx)" = "xlsx",
                               "R data (.rds)" = "rds"),
                   selected = "csv"),
      downloadButton(ns("download"), "Download data", class = "btn-primary w-100"),
      helpText("Downloads the data exactly as it stands at this point in the ",
               "pipeline."),
      uiOutput(ns("charts_ui")),   # filled only when a plots reactive is given
      uiOutput(ns("model_ui"))     # filled only when a model reactive is given
    ),
    card(
      card_header(icon("file-export"), " Data to export"),
      textOutput(ns("caption")),
      DT::DTOutput(ns("preview"))
    ),
    uiOutput(ns("charts_preview_ui"))   # filled only when a plots reactive is given
  )
}

exportServer <- function(id, data_in, plots = NULL, model = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$caption <- renderText({
      d <- data_in()
      if (is.null(d) || !is.data.frame(d))
        return("Nothing to export yet — import (and optionally transform) first.")
      sprintf("%s rows × %s columns will be exported.",
              format(nrow(d), big.mark = ","), ncol(d))
    })

    output$preview <- DT::renderDT({
      d <- data_in()
      req(is.data.frame(d))
      DT::datatable(utils::head(d, 200), rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE))
    })

    # Sanitise the user's file name to a safe stem; fall back if blank.
    safe_stem <- reactive({
      stem <- gsub("[^A-Za-z0-9._-]+", "_", input$filename %||% "")
      if (nzchar(stem)) stem else "data-export"
    })

    output$download <- downloadHandler(
      filename = function() paste0(safe_stem(), ".", input$fmt %||% "csv"),
      content  = function(file) {
        d <- data_in()
        validate(need(is.data.frame(d), "No data to export."))
        switch(input$fmt %||% "csv",
          csv  = utils::write.csv(d, file, row.names = FALSE),
          xlsx = writexl::write_xlsx(d, file),
          rds  = saveRDS(d, file))
      }
    )

    # ── Optional: export charts (from mod_visualize) ──────────
    if (!is.null(plots)) {
      # Preview the exact grid that will be exported.
      output$charts_preview_ui <- renderUI({
        card(card_header(icon("chart-line"), " Chart preview"),
             plotOutput(ns("charts_preview"), height = "440px"))
      })
      output$charts_preview <- renderPlot({
        pl <- plots()
        validate(need(length(pl) >= 1L,
                      "Configure at least one chart on the Visualize tab."))
        draw_plot_grid(pl)
      }, bg = "white")

      output$charts_ui <- renderUI({
        tagList(
          hr(), h6("Export charts"),
          selectInput(ns("plot_fmt"), "Image format",
                      choices = c("PNG" = "png", "PDF" = "pdf")),
          sliderInput(ns("plot_w"), "Width per plot (in)",  3, 12, 6,   0.5),
          sliderInput(ns("plot_h"), "Height per plot (in)", 3, 12, 4.5, 0.5),
          conditionalPanel(
            sprintf("input['%s'] == 'png'", ns("plot_fmt")),
            sliderInput(ns("plot_dpi"), "Resolution (DPI)", 72, 300, 150, 1)),
          downloadButton(ns("dl_plots"), "Download chart(s)",
                         class = "btn-success w-100")
        )
      })
      output$dl_plots <- downloadHandler(
        filename = function()
          paste0(safe_stem(), "_charts.", input$plot_fmt %||% "png"),
        content = function(file) {
          pl <- plots()
          validate(need(length(pl) >= 1L,
                        "Configure at least one chart on the Visualize tab first."))
          render_plots_to_file(pl, file, input$plot_fmt %||% "png",
                               input$plot_w %||% 6, input$plot_h %||% 4.5,
                               input$plot_dpi %||% 150)
        }
      )
    }

    # ── Optional: export regression results (from mod_regression) ─
    if (!is.null(model)) {
      output$model_ui <- renderUI({
        tagList(
          hr(), h6("Export regression"),
          downloadButton(ns("dl_summary"), "Model summary (.txt)",
                         class = "btn-outline-secondary w-100 mb-1"),
          downloadButton(ns("dl_coefs"), "Coefficients (.csv)",
                         class = "btn-outline-secondary w-100 mb-1"),
          downloadButton(ns("dl_fitted"), "Fitted & actual (.csv)",
                         class = "btn-outline-secondary w-100 mb-1"),
          downloadButton(ns("dl_resid"), "Residuals (.csv)",
                         class = "btn-outline-secondary w-100")
        )
      })
      need_model <- function()
        validate(need(!is.null(model()), "Fit a model on the Regression tab first."))
      output$dl_summary <- downloadHandler(
        filename = function() paste0(safe_stem(), "_model_summary.txt"),
        content  = function(f) { need_model(); utils::capture.output(summary(model()), file = f) }
      )
      output$dl_coefs <- downloadHandler(
        filename = function() paste0(safe_stem(), "_coefficients.csv"),
        content  = function(f) {
          need_model()
          co <- as.data.frame(summary(model())$coefficients)
          co <- cbind(Term = rownames(co), co)
          utils::write.csv(co, f, row.names = FALSE)
        }
      )
      output$dl_fitted <- downloadHandler(
        filename = function() paste0(safe_stem(), "_fitted.csv"),
        content  = function(f) {
          need_model(); m <- model()
          utils::write.csv(data.frame(actual = m$model[[1]], fitted = fitted(m)),
                           f, row.names = FALSE)
        }
      )
      output$dl_resid <- downloadHandler(
        filename = function() paste0(safe_stem(), "_residuals.csv"),
        content  = function(f) {
          need_model(); m <- model()
          utils::write.csv(data.frame(fitted = fitted(m), residual = residuals(m)),
                           f, row.names = FALSE)
        }
      )
    }

    invisible(NULL)
  })
}

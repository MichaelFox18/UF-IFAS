# ============================================================
# mod_summarize.R — grouped summary statistics ("stats by ___")
# ============================================================
# Thin wrapper over R/helpers_stats.R. Takes the working data as a reactive
# (data_in) and shows count/mean/median/mode/min/max/SD of the chosen numeric
# variable(s) within each group, with a CSV download. Returns the summary
# table reactive in case a later stage wants it.
#
# Requires R/helpers_stats.R (grouped_summary, numeric_cols, groupable_cols).

summarizeUI <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      width = 300,
      h5("Summary by group"),
      selectizeInput(
        ns("vars"),
        tagList("Summarize (numeric)", info_tip(
          "The numeric variable(s) whose statistics you want within each ",
          "group — e.g. mpg.")),
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "pick numeric variable(s)")
      ),
      selectizeInput(
        ns("groups"),
        tagList("Grouped by", info_tip(
          "The variable(s) that define the groups — e.g. cyl. With several, ",
          "you get one row per combination.")),
        choices = NULL, multiple = TRUE,
        options = list(placeholder = "pick grouping variable(s)")
      ),
      helpText("Count, mean, median, mode, min, max, and SD within each ",
               "group. Mode is blank where a group has no repeated value."),
      hr(),
      downloadButton(ns("download"), "Download table (.csv)",
                     class = "btn-success w-100")
    ),
    card(
      card_header(icon("layer-group"), " Summary statistics by group"),
      textOutput(ns("caption")),
      DT::DTOutput(ns("table"))
    )
  )
}

summarizeServer <- function(id, data_in) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data_in(), {
      df <- data_in()
      req(is.data.frame(df))
      nums <- numeric_cols(df)
      grps <- groupable_cols(df)
      updateSelectizeInput(session, "vars", choices = nums,
                           selected = utils::head(nums, 1))
      updateSelectizeInput(session, "groups", choices = grps,
                           selected = utils::head(grps, 1))
    }, ignoreNULL = TRUE)

    summary_df <- reactive({
      df <- data_in()
      validate(need(is.data.frame(df),
                    "Import data on the Import tab to begin."))
      validate(
        need(length(input$vars)   >= 1L, "Pick at least one numeric variable."),
        need(length(input$groups) >= 1L, "Pick at least one grouping variable.")
      )
      out <- grouped_summary(df, input$vars, input$groups)
      validate(need(!is.null(out), "Couldn't summarize with those selections."))
      out
    })

    output$caption <- renderText({
      if (!length(input$vars) || !length(input$groups)) return("")
      sprintf("Count, mean, median, mode, min, max, and SD of %s, grouped by %s.",
              paste(input$vars, collapse = ", "),
              paste(input$groups, collapse = " × "))
    })

    output$table <- DT::renderDT({
      DT::datatable(summary_df(), rownames = FALSE,
                    class = "compact stripe hover",
                    options = list(scrollX = TRUE, pageLength = 15))
    })

    output$download <- downloadHandler(
      filename = function() paste0("group_summary_", Sys.Date(), ".csv"),
      content  = function(f) {
        d <- tryCatch(summary_df(), error = function(e) NULL)
        validate(need(!is.null(d), "Nothing to download yet."))
        utils::write.csv(d, f, row.names = FALSE)
      }
    )

    summary_df
  })
}

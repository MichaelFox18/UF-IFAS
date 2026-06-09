# ============================================================
# Data Explorer — the full pipeline, assembled from the kit
# ============================================================
# Import → Reshape → Summarize → Export, each a shared module wired
# Pattern A (every stage returns a reactive that feeds the next). Visualize
# and Regression land here next, behind the same pattern.
#
# Run from the projects/ root (launch via PowerShell, not git-bash):
#   setwd("C:/Users/michaelfox1/Desktop/UF-IFAS/projects")
#   shiny::runApp("apps/data_explorer")

library(shiny)
library(bslib)
library(here)
library(ggplot2)
library(plotly)
library(colourpicker)

options(shiny.maxRequestSize = 250 * 1024^2)   # lift the 5 MB upload cap

source(here::here("R", "components.R"))
source(here::here("R", "helpers_io.R"))
source(here::here("R", "helpers_clean.R"))
source(here::here("R", "helpers_stats.R"))
source(here::here("R", "helpers_plot.R"))
source(here::here("R", "helpers_model.R"))
source(here::here("R", "helpers_reshape.R"))
source(here::here("modules", "mod_import.R"))
source(here::here("modules", "mod_reshape.R"))
source(here::here("modules", "mod_summarize.R"))
source(here::here("modules", "mod_visualize.R"))
source(here::here("modules", "mod_regression.R"))
source(here::here("modules", "mod_export.R"))

# --- About tab (static orientation) -----------------------------------------
about_panel <- nav_panel(
  title = tagList(icon("circle-info"), " About"),
  value = "about",
  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header(icon("compass"), " What is Data Explorer?"),
      tags$div(
        class = "px-2",
        tags$p("A point-and-click tool for UF/IFAS students to import, reshape, ",
               "summarize, and export tabular data — no R code required."),
        tags$p(class = "mb-1", tags$b("The workflow runs left to right:")),
        tags$ol(
          class = "px-3",
          tags$li(tags$b("Import"), " — upload CSV/Excel/TSV/RDS or load an example."),
          tags$li(tags$b("Reshape"), " — stack, split, transpose, sort, or subset (optional)."),
          tags$li(tags$b("Summarize"), " — count, mean, median, mode, min, max, SD by group."),
          tags$li(tags$b("Export"), " — download the result as CSV, Excel, or RDS.")
        )
      )
    ),
    card(
      card_header(icon("lightbulb"), " Tips"),
      tags$ul(
        class = "px-3",
        tags$li("Each tab feeds the next: what you reshape is what gets summarized and exported."),
        tags$li("Leave Reshape on “None” to pass data straight through unchanged."),
        tags$li("Hover the ", icon("circle-question"), " icons for plain-English help."),
        tags$li("Visualize and Regression are coming to this same pipeline.")
      )
    )
  )
)

ui <- page_navbar(
  title        = uf_title("Data Explorer"),
  window_title = "UF/IFAS Data Explorer",
  theme        = uf_theme(),
  fillable     = c("reshape", "visualize", "regression"),

  about_panel,
  nav_panel(tagList(icon("file-arrow-up"), " Import"),    value = "import",
            importUI("imp")),
  nav_panel(tagList(icon("table-cells"),   " Reshape"),   value = "reshape",
            reshapeUI("rs")),
  nav_panel(tagList(icon("layer-group"),   " Summarize"), value = "summarize",
            summarizeUI("sm")),
  nav_panel(tagList(icon("chart-line"),    " Visualize"),  value = "visualize",
            visualizeUI("viz")),
  nav_panel(tagList(icon("chart-simple"),  " Regression"), value = "regression",
            regressionUI("reg")),
  nav_panel(tagList(icon("file-export"),   " Export"),     value = "export",
            exportUI("ex"))
)

server <- function(input, output, session) {
  imported <- importServer("imp")              # Import    -> reactive(data | NULL)
  working  <- reshapeServer("rs", imported)    # Reshape   -> reactive(working data)
  summarizeServer("sm", working)               # Summarize  reads the working data
  plots    <- visualizeServer("viz", working)  # Visualize -> reactive(list of ggplots)
  model    <- regressionServer("reg", working) # Regression -> reactive(fitted lm)
  exportServer("ex", working, plots = plots, model = model)  # data + charts + model
}

shinyApp(ui, server)

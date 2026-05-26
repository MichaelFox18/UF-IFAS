# StatGuide

A single-file R Shiny app that helps you pick the right statistical test for
your data. Walks through a short decision tree and returns ranked
recommendations with assumptions, when-to-use notes, and copy-ready R code.

## Run it

Requires R ([download](https://cloud.r-project.org/)).

- **RStudio:** open `app.R` and click **Run App**.
- **VS Code / console:** `shiny::runApp("app.R")`.

Missing packages (`shiny`, `bslib`, `shinyjs`) install automatically on first launch.

## References

- Palomares Carrascosa, I. (2024). Choosing the Right Statistical Test: A Decision Tree Approach. *Statology.*
- McCrum-Gardner, E. (2008). Which is the correct statistical test to use? *British Journal of Oral and Maxillofacial Surgery, 46*, 38–41.
- Marusteri, M. & Bacarea, V. (2010). Comparing groups for statistical differences. *Biochemia Medica, 20*(1), 15–32.

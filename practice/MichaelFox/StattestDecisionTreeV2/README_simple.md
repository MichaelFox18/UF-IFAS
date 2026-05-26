# StatGuide

A single-file R Shiny app that helps you pick the right statistical test for
your data. Walks through a short decision tree and returns ranked
recommendations with assumptions, when-to-use notes, and copy-ready R code.

## Run it

Requires R ([download](https://cloud.r-project.org/)).

- **RStudio:** open `app.R` and click **Run App**.
- **VS Code / console:** `shiny::runApp("app.R")`.

Missing packages (`shiny`, `bslib`, `shinyjs`) install automatically on first launch.

## What's in the app

- **Find My Test** — five short questions about your data (type, goal, group
  structure, parametric assumptions) lead to a tailored recommendation.
- **Results** — ranked test cards. Each one explains what the test does, when
  to use it (and when not), its assumptions, and includes a runnable R code
  example you can copy.
- **Glossary** — 37 plain-English definitions of common statistical terms.
- **About** — references and notes on scope.

## Tests covered

| Category | Tests |
| --- | --- |
| Compare two groups | Independent t-test, Paired t-test, Mann-Whitney U, Wilcoxon signed-rank |
| Compare 3+ groups | One-way ANOVA, Repeated-measures ANOVA, Kruskal-Wallis, Friedman |
| Relationships | Pearson correlation, Spearman correlation |
| Prediction | Simple linear regression, Multiple linear regression |
| Categorical | Chi-square, Fisher's exact, McNemar's, Binomial, Z-test for proportions |

Specialized methods (mixed models, logistic regression, survival analysis) are
out of scope.

## References

- Palomares Carrascosa, I. (2024). Choosing the Right Statistical Test: A Decision Tree Approach. *Statology.*
- McCrum-Gardner, E. (2008). Which is the correct statistical test to use? *British Journal of Oral and Maxillofacial Surgery, 46*, 38–41.
- Marusteri, M. & Bacarea, V. (2010). Comparing groups for statistical differences. *Biochemia Medica, 20*(1), 15–32.

# StatGuide — Statistical Test Selector

An interactive R Shiny app that guides students and researchers through a step-by-step decision tree to identify the most appropriate statistical test for their data.

---

## Overview

Choosing the right statistical test is one of the most common points of confusion in applied research. StatGuide walks users through five targeted questions about their data and returns ranked test recommendations with plain-English explanations, when-to-use guidance, and copy-ready R code examples.

**Target audience:** Undergraduate and graduate students in the biological, social, and health sciences.

---

## Features

- **Decision tree wizard** — one question at a time with back/forward navigation and a live progress bar
- **17 statistical tests** covered across numerical, categorical, and ordinal data
- **Ranked results** — Best Fit and Alternative options, each with a full test card
- **Collapsible help text** on every wizard question explaining technical terms in plain English
- **Clickable glossary links** — terms in the wizard jump directly to their definition in the Glossary tab
- **38-term glossary** — searchable, A–Z, plain-English definitions
- **R code examples** for every test, ready to copy and adapt
- **UF branding** — University of Florida orange (#FA4616) and blue (#003087) throughout
- **Mobile responsive** layout via Bootstrap 5

---

## Statistical Tests Covered

| Category | Tests |
|---|---|
| Comparing groups (numerical) | Independent t-Test, Paired t-Test, Mann-Whitney U, Wilcoxon Signed-Rank, One-Way ANOVA, Repeated Measures ANOVA, Kruskal-Wallis, Friedman |
| Relationships (numerical) | Pearson Correlation, Spearman Correlation |
| Prediction (numerical) | Simple Linear Regression, Multiple Linear Regression |
| Categorical | Chi-Square, Fisher's Exact, McNemar's, Binomial, Z-Test for Proportions |
| Prediction (categorical) | Simple Logistic Regression, Multiple Logistic Regression |

---

## Decision Tree Logic

The wizard asks up to five questions depending on the path:

1. **Data type** — Numerical, Categorical, or Ordinal
2. **Goal** — Compare groups, Examine relationships, or Predict an outcome
3. **Number of groups** *(compare path)* — Two or More than two
4. **Independence** *(compare path)* — Independent or Paired/Repeated
5. **Parametric assumptions** — Met, Not met, or Unsure

Selecting "Unsure" on assumptions displays both the parametric and non-parametric options side by side.

---

## File Structure

```
StatTestDecisionTree/
├── global.R               # Library loading and source files
├── ui.R                   # Page layout, navbar, Bootstrap 5 theme
├── server.R               # Wizard logic, results engine, glossary renderer
├── run_app.R              # Launch script
├── install_packages.R     # One-time package installer
├── R/
│   ├── decision_logic.R   # Step configurations and recommendation mapping
│   ├── test_definitions.R # All 19 test cards (descriptions, assumptions, R code)
│   └── glossary_terms.R   # 38 glossary terms with definitions
└── www/
    └── custom.css         # UF branding and layout styles
```

---

## Running the App

**Install dependencies (one time):**
```r
Rscript install_packages.R
```

**Launch:**
```r
Rscript run_app.R
```

Opens in your default browser at `http://127.0.0.1:3838`.

In VSCode: `Ctrl+Shift+B` → **R: Run StatGuide App**

**Dependencies:** `shiny`, `bslib`, `shinyWidgets`, `shinyjs`

---

## References

- Palomares Carrascosa, I. (2024). Choosing the Right Statistical Test: A Decision Tree Approach. *Statology.*
- McCrum-Gardner, E. (2008). Which is the correct statistical test to use? *British Journal of Oral and Maxillofacial Surgery, 46*, 38–41.
- Marusteri, M. & Bacarea, V. (2010). Comparing groups for statistical differences. *Biochemia Medica, 20*(1), 15–32.

# UF IFAS Data Explorer

An interactive R Shiny application for exploratory data analysis and regression modeling, built with UF IFAS branding.

## Overview

This app allows users to upload their own data (or use the built-in `mtcars` demo dataset) and explore it through interactive visualizations, statistical modeling, and flexible data export. It was developed as a practice project for the UF IFAS program.

## Features

### Import Data
Upload a CSV, TSV, or plain-text file, or use the built-in `mtcars` demo. Supports comma, tab, semicolon, and pipe delimiters. The raw data table is displayed on load.

### Visualize
Build interactive charts from any numeric columns in your dataset. Supported plot types:
- **Scatter plot** — with optional color/group-by variable
- **Line graph** — with optional group-by variable
- **Histogram** — with optional grouped coloring (discrete variables only)
- **Bar chart** — count or mean aggregation, with optional grouped/dodged bars
- **Box plot** — with optional fill/group-by variable

Controls include point/bar size, custom color picker (for ungrouped plots), custom axis labels, and a plot title. Low-cardinality numeric variables (≤ 10 unique values) are automatically treated as categorical for grouping purposes.

### Multi-Plot
Produce up to four plots simultaneously in a 2×2 grid. Each slot can be independently configured with a plot type, X and Y variables, and a title. All active plots are exported together as a single PNG at a user-selected DPI.

### Regression
Fit and interpret statistical models:
- **Simple Linear** — one predictor, one response
- **Multiple Linear** — multiple predictors, one response
- **Logistic** — binary outcome classification

Outputs include a coefficient table (estimate, std. error, t/z value, p-value), model summary statistics (R², F-statistic or AIC/BIC), fitted-vs-actual and residual plots, and an auto-generated plain-English interpretation of significance and model fit.

### Export
Select any subset of columns to include in the export, then download as CSV or TSV. DPI can be configured for multi-plot image exports.

### Glossary
Definitions for key statistical terms: p-value, R², standard error, t-value, F-statistic, AIC/BIC, residuals, fitted values, and more. Also covers model types, import file format options, and export DPI guidance.

## Tech Stack

| Package | Purpose |
|---|---|
| `shiny` | App framework |
| `bslib` | Bootstrap 5 theme, layout components, tooltips |
| `ggplot2` | Static plot construction |
| `plotly` / `ggplotly` | Interactive chart rendering |
| `dplyr` | Data wrangling |
| `readr` | File import |
| `grid` | Multi-plot PNG export (base R viewports) |

## Running the App

```r
# From RStudio or an R terminal
shiny::runApp("practice/MichaelFox/DataExplorer/app.R")
```

Or open `app.R` in RStudio and click **Run App**.

> **Note:** If you have the app running in one terminal and make edits, open a new R terminal and re-run rather than reloading the existing session to avoid stale state.

## Session Changes (v1.0 → current)

The following improvements were made during the initial development session:

### Branding & UI
- Applied UF IFAS color scheme throughout: **UF Blue** (`#003087`) and **UF Orange** (`#FA4616`)
- Fixed plotly modebar (zoom/pan toolbar) overlapping regression plot titles by increasing top margin
- Added contextual info tooltips (`ⓘ`) on regression inputs and other conceptual UI elements

### Regression Tab
- Added plain-English statistical interpretation (significance, R², model fit quality)
- Added info tooltips explaining regression methods, model types, and variable roles
- Restricted Simple Linear predictor selector to a single variable (prevents multi-predictor selection)
- Added residual and fitted-vs-actual interactive plots

### Visualize — Color/Group By Fixes
- Restructured all geom calls to avoid passing `color = NULL` or `fill = NULL` as fixed aesthetics (eliminated "Ignoring empty aesthetic" warnings)
- Fixed `labs()` to only set `color` or `fill` label depending on plot type (eliminated "Ignoring unknown labels" warnings)
- Added automatic `as.factor()` conversion for low-cardinality numeric group variables (≤ 10 unique values), fixing crashes when grouping line graphs by variables like `cyl`
- Added guard to null out group-by variable for histogram and bar chart when the variable is truly continuous (> 10 unique values), preventing broken per-observation coloring
- Fixed bar width slider formula to span a useful range (0.2–0.9) rather than a narrow band
- Added `position = "dodge"` for grouped bar charts so bars sit side-by-side instead of stacking

### Multi-Plot Tab
- Added new tab supporting up to 4 simultaneous configurable plots
- Implemented PNG export using base R `grid` viewports — avoids `patchwork` dependency and guarantees a valid PNG is always written (no more `.htm` error files)
- Fixed `downloadHandler` to use `isolate()` instead of `req()` inside the `content` function, which was silently aborting and causing Shiny to return an HTML error page saved as `.htm`

### Export Tab
- Replaced automatic "used variables" tracking with a user-selectable column picker, allowing export of any columns including string/character variables

### Glossary Tab
- Moved to last tab position (after Export)
- Expanded with entries for: all three model types, text file format options (CSV/TSV/pipe/semicolon), export DPI guidance, and additional statistical terms

## File Structure

```
DataExplorer/
├── app.R       # Full Shiny application (UI + server in one file)
└── README.md   # This file
```

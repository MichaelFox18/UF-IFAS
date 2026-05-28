# UF IFAS Data Explorer

An interactive R Shiny application for exploratory data analysis and regression modeling, built with UF IFAS branding.

## Overview

This app allows users to upload their own data (or use the built-in `mtcars` demo dataset) and explore it through interactive visualizations, statistical modeling, and flexible data export. It was developed as a practice project for the UF IFAS program.

## Features

### Import Data
Upload a CSV, TSV, plain-text, Excel (`.xlsx` / `.xls`), or `.rds` file, or load the built-in `mtcars` demo. Text files support comma, semicolon, tab, and space delimiters and either decimal point convention. The raw data table and a per-column summary are displayed on load.

### Visualize
Build one to four charts at once from any columns in your dataset. Use the **Number of plots** selector (1–4); each plot gets its own collapsible panel with the full set of controls:

- **Chart types** — Scatter, Line, Bar, Histogram, Box Plot, Pie
- **X / Y variables** and an optional **Color / Group By** variable
- **Bar aggregation** (Sum / Mean / Median) when a bar chart has a Y variable
- **Histogram bins**
- **Labels** — title and custom X/Y axis labels
- **Style** — theme, default color, and point/bar size
- **Regression overlay** (scatter & line) — linear, polynomial, or loess fit with an optional 95% CI band

Additional conveniences:

- **Copy R code** — each plot has a "R code for this plot" panel with a runnable `ggplot2` snippet and a one-click copy button, so students can reuse the exact code elsewhere.
- **Apply Plot 1 style to all** — copies Plot 1's theme, color, and size to the other plots for a consistent look.
- **Smart grouping** — low-cardinality numeric variables (≤ 10 unique values, e.g. `cyl`) are treated as categorical so discrete palettes work; truly continuous variables are ignored for bar/histogram grouping (where per-row coloring would be meaningless).

### Regression
Fit and interpret statistical models:

- **Simple Linear** — one predictor, one response
- **Multiple Linear** — two or more predictors, one response
- **Polynomial** — a curved fit using powers of one predictor (selectable degree)

Outputs include the full model summary (coefficient table with estimate, std. error, t-value, p-value; R², adjusted R², F-statistic), interactive fitted-vs-actual and residuals-vs-fitted plots, and an auto-generated plain-English interpretation of overall significance and per-predictor significance. The summary can be exported as a `.txt` file.

### Export
**Plots** — exports exactly the 1–4 charts configured on the Visualize tab. With a single plot, download it directly; with multiple plots, choose **Combined image** (one file laid out in a grid) or **Separate files** (one download per plot). Supported formats are **PNG**, **PDF**, and **SVG**, with configurable per-plot width, height, and resolution (DPI).

**Data** — select any subset of columns and download as CSV or Excel (`.xlsx`).

### Glossary
Plain-language definitions for key terms: regression model types, R² / adjusted R², F-statistic, residuals, confidence intervals, the coefficient-table columns (estimate, std. error, t-value, p-value, intercept), and the import/aggregation/DPI settings used throughout the app.

## Tech Stack

| Package | Purpose |
|---|---|
| `shiny` | App framework |
| `bslib` | Bootstrap 5 theme, layout components, tooltips |
| `ggplot2` | Plot construction |
| `plotly` / `ggplotly` | Interactive chart rendering |
| `dplyr` | Data wrangling / aggregation |
| `DT` | Interactive data tables |
| `readxl` | Excel file import |
| `writexl` | Excel file export |
| `colourpicker` | Color picker inputs |
| `grid` / `grDevices` | Multi-plot layout and PNG/PDF/SVG export (base R) |

## Running the App

```r
# From RStudio or an R terminal
shiny::runApp("practice/MichaelFox/DataExplorer/DataExplorerApp.R")
```

Or open `DataExplorerApp.R` in RStudio and click **Run App**.

> **Note:** If you have the app running in one terminal and make edits, open a new R terminal and re-run rather than reloading the existing session to avoid stale state.

## Changelog

### Merged Visualize + Multi-Plot
- Combined the former Visualize and Multi-Plot tabs into a single **Visualize** tab that builds 1–4 plots, each with the complete control set (chart type, variables, group-by, labels, style, and regression overlay).
- Added an **Apply Plot 1 style to all** button to sync theme/color/size across plots.

### Copy R code
- Every plot now exposes a runnable `ggplot2` code snippet with a copy-to-clipboard button, generated to reproduce the on-screen chart (including grouping, aggregation, and regression overlays).

### Export
- The Export tab now mirrors the 1–4 plots from Visualize, with a choice between a single combined image and separate per-plot files, in PNG, PDF, or SVG.

### Group-by & bar charts
- Bar charts with a Y variable now **aggregate** repeated categories (Sum / Mean / Median) instead of overlaying opaque identity bars.
- Fixed pie charts breaking when the category column was numeric (now coerced to a factor).
- Hardened group-by handling across all chart types.

### Earlier session (v1.0 → current)
- Applied the UF IFAS color scheme (UF Blue `#003087`, UF Orange `#FA4616`).
- Added plain-English regression interpretation, info tooltips, and residual/fitted plots.
- Restructured geoms and `labs()` to eliminate empty-aesthetic and unknown-label warnings.
- Replaced automatic export-column tracking with a user-selectable column picker.
- Expanded the Glossary and moved it to the last tab.

## File Structure

```
DataExplorer/
├── DataExplorerApp.R   # Full Shiny application (UI + server in one file)
└── README.md           # This file
```

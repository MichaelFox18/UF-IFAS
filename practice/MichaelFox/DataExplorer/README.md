# UF IFAS Data Explorer

An interactive R Shiny application for exploratory data analysis and regression modeling, built with UF IFAS branding.

## Overview

This app allows users to upload their own data (or use the built-in `mtcars` demo dataset) and explore it through interactive visualizations, statistical modeling, and flexible data export. It was developed as a practice project for the UF IFAS program.

## Features

### Import Data
Upload a CSV, TSV, plain-text, Excel (`.xlsx` / `.xls`), or `.rds` file, or load the built-in `mtcars` demo. Text files support comma, semicolon, tab, and space delimiters and either decimal point convention. The raw data table and a per-column summary are displayed on load. A **Clear Data** button removes the loaded dataset (and any fitted model) so you can start over.

**Data Health** — a guided cleaning panel diagnoses common spreadsheet problems and offers **opt-in, reversible** fixes (it never changes your data silently):

- **Column names** — make blank/duplicate names unique
- **Whitespace** — trim stray spaces from values and headers
- **Missing-value markers** — convert placeholders like `N/A`, `NULL`, or blanks to true `NA`
- **Numbers stored as text** — convert columns like `"$1,200"` / `"12%"` to numeric (so they become usable in plots and regression)
- **Dates stored as text** — convert ISO `yyyy-mm-dd` strings to real dates
- **Empty columns / rows** — drop entirely-blank ones
- **Duplicate rows** — remove exact duplicates

Each detected issue is listed with a count and a checkbox (the safe ones are pre-checked); **Apply selected fixes** transforms a working copy, and **Revert to original** restores the file exactly as uploaded. Numeric/date conversions only trigger when ≥ 90% of real values parse cleanly, and date parsing is limited to unambiguous ISO formats.

### Visualize
Build one to four charts at once from any columns in your dataset. Use the **Number of plots** selector (1–4); each plot gets its own collapsible panel with the full set of controls. Changing the number of plots **no longer resets** the plots you've already configured — settings only clear when you press **Reset settings to default**.

- **Chart types** — Scatter, Line, Bar, Histogram, Box Plot, Pie
- **X / Y variables** and an optional **Color / Group By** variable
- **Bar aggregation** (Sum / Mean / Median) when a bar chart has a Y variable
- **Maximum bars / slices** — a data-aware slider (bar & pie) that keeps the largest N categories and groups the rest into **Other**; defaults to a readable cap and ranges up to the number of categories in your data
- **Histogram bins**
- **Labels** — title and custom X/Y axis labels
- **Style** — theme, default color, and point/bar size
- **Regression overlay** (scatter & line) — linear, polynomial, or loess fit with an optional 95% CI band, plus an optional on-plot **equation & R²** label
- **Advanced options** (collapsible, to keep the main controls uncluttered):
  - **Group color palette** — Automatic, UF Brand, Viridis, Colorblind-safe (Okabe–Ito), ColorBrewer Set1/Set2, or Greyscale (palettes are recycled/ramped so they never run out of colors)
  - **Opacity** and scatter **point jitter** (for dense, overplotted data)
  - **Log scale** for the X, Y, or both axes (applied only to continuous axes)
  - **Facet by** a categorical variable — splits the chart into small-multiple panels
  - **Legend position** (right / bottom / top / hidden) and a **gridlines** toggle
  - **Horizontal orientation** for bar and box plots (useful for long category labels)

Additional conveniences:

- **Copy R code** — each plot has a "R code for this plot" panel with a runnable `ggplot2` snippet and a one-click copy button, so students can reuse the exact code elsewhere.
- **Apply Plot 1 style to all** — copies Plot 1's theme, color, and size to the other plots for a consistent look.
- **Smart grouping** — low-cardinality numeric variables (≤ 10 unique values, e.g. `cyl`) are treated as categorical so discrete palettes work; truly continuous variables are ignored for bar/histogram grouping (where per-row coloring would be meaningless).
- **Chart guidance** — the app steers you away from the data/chart mismatches that produce "strange-looking" plots: the histogram X picker only offers numeric columns, an inline hint warns when a pairing is questionable (e.g. a categorical X on a scatter, or a continuous X on a bar/pie), crowded category labels are angled automatically, line charts are sorted by X so they show a trend instead of a scribble, and bar/pie charts cap the number of categories (see **Maximum bars / slices**) so they stay readable.
- **Reset settings to default** — clears every plot's configuration in one click (changing the number of plots does *not*).

### Regression
Fit and interpret statistical models:

- **Simple Linear** — one predictor, one response
- **Multiple Linear** — two or more predictors, one response
- **Polynomial** — a curved fit using powers of one predictor (selectable degree)

Outputs include the full model summary (coefficient table with estimate, std. error, t-value, p-value; R², adjusted R², F-statistic), interactive fitted-vs-actual and residuals-vs-fitted plots, and an auto-generated plain-English interpretation of overall significance and per-predictor significance. The summary can be exported as a `.txt` file. On large datasets the model summary is computed once and the diagnostic scatterplots are thinned to a representative sample of points so the tab stays responsive (the model itself is still fit on every row).

### Export
**Plots** — exports exactly the 1–4 charts configured on the Visualize tab. With a single plot, download it directly; with multiple plots, choose **Combined image** (one file laid out in a grid) or **Separate files** (one download per plot). Supported formats are **PNG**, **PDF**, and **SVG**, with configurable per-plot width, height, and resolution (DPI).

**Data** — select any subset of columns and download as CSV or Excel (`.xlsx`).

**Regression** — once a model is fitted on the Regression tab, export its full summary as a `.txt` file or its coefficient table as a `.csv`.

### Glossary
Plain-language definitions for key terms: regression model types, R² / adjusted R², F-statistic, residuals, confidence intervals, the coefficient-table columns (estimate, std. error, t-value, p-value, intercept), and the app settings used throughout — including the newer visualization options (maximum bars/slices, color palette, opacity, jitter points, log scale, facet by, horizontal orientation, and the trendline equation label).

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

### Guided Data Health panel
- Added a **Data Health** panel to the Import tab that diagnoses common spreadsheet issues and offers opt-in, reversible fixes: clean column names, trim whitespace, standardize missing-value markers, convert numbers-stored-as-text to numeric, convert ISO date strings to dates, drop empty columns/rows, and remove duplicate rows.
- Fixes apply to a working copy with a **Revert to original** button; numeric/date conversions are guarded (≥ 90% of real values must parse, dates ISO-only) so nothing is mangled silently.

### Pie-chart cleanup & regression export
- **Pie charts now show only relevant controls** — theme, default color, point/bar size, axis labels, opacity, log scale, and gridlines are hidden (they had no effect on a pie).
- **Pie colors and legend position now work** — the **Color Palette** drives the slice colors and the **Legend Position** control applies, instead of being ignored.
- **Export Regression** — the Export tab can now download the fitted model's summary (`.txt`) and coefficient table (`.csv`), in addition to the existing summary download on the Regression tab.

### Usability, persistence & performance
- **Clear Data** button on the Import tab removes the loaded dataset and any fitted model.
- **Reset settings to default** button on the Visualize tab clears all plot configurations at once.
- **Plot settings now persist** when you change the **Number of plots** — the config UI is no longer rebuilt on that change, so configuring plot 1 and then switching to two plots keeps plot 1 intact. (Settings only reset on the explicit Reset button or when new data is loaded.)
- Replaced the on/off bar-cap checkbox with a **data-aware “Maximum bars / slices” slider** (applies to bar *and* pie charts): default to a readable cap, slide up to show every category or down to simplify, and the graph updates live.
- **Regression tab performance** — `summary()` is cached and reused, and the fitted/residual diagnostic plots are thinned to a representative sample of points on large datasets (the fit still uses all rows).
- **Glossary expanded** with the new visualization options (maximum bars/slices, color palette, opacity, jitter points, log scale, facet by, horizontal orientation, trendline equation label).

### More chart customization
- Added a collapsible **Advanced options** panel to each plot's controls, so the new settings don't clutter the main interface.
- **Color palette picker** for grouped charts (UF Brand, Viridis, Colorblind-safe, ColorBrewer Set1/Set2, Greyscale), replacing the previous automatic-only logic.
- **Opacity** slider, scatter **point jitter**, **log-scale** (X/Y/both), **legend position**, **gridlines** toggle, and **horizontal orientation** for bar/box plots.
- **Faceting** — split a chart into small-multiple panels by a categorical variable.
- **Trendline label** — optionally annotate a fitted scatter/line with its equation and R².
- The **Apply Plot 1 style to all** button now also syncs palette, opacity, legend position, and gridlines.
- The generated R code reflects every one of these settings (palette, alpha, jitter, log scales, facet, flip, legend/gridline theming, and the fitted-equation annotation).

### Chart guidance (data ↔ chart-type fit)
- **Type-aware X picker** — histograms now only offer numeric columns, so a text column can no longer produce a broken or empty plot.
- **Inline mismatch hints** — a warning appears under the variable pickers when the chosen column doesn't suit the chart (categorical X on a scatter/line, continuous X on a bar/pie, or a high-cardinality bar), explaining *why* and suggesting a better chart.
- **Auto-rotated axis labels** — crowded or long categorical x-axis labels are angled so they stay legible instead of overlapping.
- **Sorted line charts** — line data is ordered by X before plotting, so an unsorted file draws a trend rather than a scribble.
- **Capped bar/pie categories** — bar and pie charts keep the largest N categories and roll the rest into a single **Other** (with an on-plot note) so they stay readable.
- The generated R code reflects all of the above (sorting, rotation, and a top-N note for lumped bars).

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
├── _test_helpers.R     # Headless logic checks for the plot/hint/code helpers
├── _test_render.R      # Forces ggplot_build() to catch render-time errors
└── README.md           # This file
```

Run the checks with `Rscript _test_helpers.R` and `Rscript _test_render.R` from the `DataExplorer` folder.

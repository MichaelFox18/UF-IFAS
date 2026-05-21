# Regression Explorer (R Shiny)

An interactive teaching tool for building intuition about linear regression — in both directions. Users can either build a dataset and fit a regression to it, or define a regression line and generate data that matches it.

## Stack

- **R Shiny** for the app framework
- **plotly** for interactive plots (pan, zoom, box/lasso select, native click events)
- Standard R for the statistics (`lm`, `rnorm`, etc.)

## Tab 1 — Data → Line

The user constructs a dataset by hand, then fits an OLS regression to it.

**Point entry — two methods, both feed the same underlying dataset:**

1. **Click entry.** A toggleable "data mode" switch. When ON, clicking the plot places a point at the clicked coordinates, rounded to 3 decimal places. When OFF, the plot behaves as a normal plotly chart — pan, zoom, box select, lasso, etc. The toggle should be visually obvious so the user always knows which mode they're in.
2. **Manual entry.** Numeric inputs for x and y (3 decimal places), and an Add button. Works in either mode.

**Editing the dataset:**
- Remove last point
- Clear all points
- A visible table or counter showing the current dataset

**Fitting:**
- A "Fit regression line" button runs OLS via `lm()` and overlays the fitted line on the plot.
- Display the standard OLS output: slope, intercept, R², adjusted R², standard errors, p-values, residual standard error, n. Use whatever layout feels clean — stat cards, a summary table, or both.

**Export:**
- Download the constructed dataset as CSV (just x, y columns).

**Forward-looking design note:** The regression engine should be structured so that adding other model types later (polynomial, LOESS, GAM, etc.) is straightforward. Don't build that now, but don't hardcode OLS so deeply that it's painful to extend.

## Tab 2 — Line → Data

The user defines a "true" regression line, then generates a random sample that line could plausibly have been fit to.

**Line specification:**
- Numeric inputs for slope (m) and intercept (b).
- The line renders live on the plot as the user changes these.

**Data generation controls:**
- Slider for sample size (n).
- Slider for noise magnitude (σ).
- Slider for homoskedasticity ↔ heteroskedasticity. At one extreme, residual variance is constant across x; at the other, variance scales with x (the classic funnel shape). Interpolate sensibly between the two.

**Generate button:**
- Each click produces a fresh random sample from the same line and settings.
- After generation, also fit an OLS regression to the *generated* sample and overlay it (visually distinct from the true line). Show the true vs. fitted slope/intercept side by side so the user can see how well the sample recovers the underlying line — this contrast is the pedagogical core of the tab.

**Export:**
- Download the generated dataset as CSV.

## General requirements

- Clean, minimal UI. Looks professional, and branded with UF Gators orange and blue theme/clors.
- All numeric values displayed in the UI should be rounded to a reasonable number of decimals (3–4 typically).
- Use `reactiveValues` for the datasets so both tabs maintain state independently.
- The two tabs are independent — no shared state between them.
- Deployable to shinyapps.io with `rsconnect::deployApp()`. Expose the app cleanly enough that deployment is a one-liner.


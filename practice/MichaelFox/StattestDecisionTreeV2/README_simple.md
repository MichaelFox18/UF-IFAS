# StatGuide — Statistical Test Selector (Simple Version)

## Running the App

**RStudio:** Open `app.R` and click **Run App**, or select all and press `Ctrl+Enter`.

**VS Code:** Press `Ctrl+Shift+B` to run the pre-configured "Run StatGuide" task, which launches the app in your browser.
- For a line-by-line interactive experience in VS Code (like RStudio), install the [R extension](https://marketplace.visualstudio.com/items?itemName=REditorSupport.r). It adds an R console panel and lets you send code with `Ctrl+Enter`.

**Terminal:**
```r
Rscript practice/MichaelFox/StattestDecisionTreeV2/app.R
```

---

## Overview
A single-file R Shiny app (`app.R`) that helps students and researchers pick the right statistical test. It walks users through a step-by-step decision tree based on their data characteristics and returns ranked test recommendations. Audience: undergrad and graduate students.

---

## Everything in one app.R
All UI, server logic, and data (test descriptions, glossary terms) should live in a single `app.R` file. Keep it simple.

---

## Features

**Decision Tree Wizard (main tab)**
- One question at a time with a progress indicator
- Back and forward navigation
- Each question has a collapsible "What does this mean?" section in plain English
- Questions cover: data type → goal → number of groups → independence → parametric assumptions
- Leads to a results section when complete

**Results**
- All valid tests ranked by fit (Best Fit first, then Alternatives)
- Each test shows: plain-English explanation, when to use / not use, assumptions, example R code
- "Start Over" button

**Glossary tab**
- Searchable list of statistical terms with plain-English definitions
- Terms to include: alternative hypothesis, ANOVA, categorical data, confidence interval, continuous data, correlation, degrees of freedom, dependent/independent variable, effect size, homogeneity of variance, hypothesis testing, interval data, Likert scale, mean, median, non-parametric test, normal distribution, null hypothesis, ordinal data, outlier, p-value, paired data, parametric test, post-hoc test, power, ratio data, regression, sample size, significance level, skewness, sphericity, standard deviation, statistical significance, Type I/II error, variance

**About tab**
- Brief instructions and references to the three source papers

---

## Tests to Include
Claude Code can write all descriptions and R code examples.

- Independent t-test, Paired t-test, Mann-Whitney U, Wilcoxon signed-rank
- One-way ANOVA, Repeated measures ANOVA, Kruskal-Wallis, Friedman test
- Pearson correlation, Spearman correlation, Simple linear regression, Multiple linear regression
- Chi-square test, Fisher's exact test, McNemar's test, Binomial test, Z-test for proportions

---

## UI & Branding
- UF colors: Orange `#FA4616` and Blue `#003087`
- Use `bslib` for layout and theming, `shinyWidgets` for buttons, `shinyjs` for collapsibles
- Clean and mobile-friendly
- Keep styling simple — inline CSS is fine

---

## References
- Palomares Carrascosa, I. (2024). Choosing the Right Statistical Test: A Decision Tree Approach. *Statology.*
- McCrum-Gardner, E. (2008). Which is the correct statistical test to use? *British Journal of Oral and Maxillofacial Surgery, 46*, 38–41.
- Marusteri, M. & Bacarea, V. (2010). Comparing groups for statistical differences. *Biochemia Medica, 20*(1), 15–32.

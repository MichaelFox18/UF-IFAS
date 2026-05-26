# ─────────────────────────────────────────────────────────────────────────────
# StatGuide — Statistical Test Selector (Single-file version)
#
# To run: open this file in RStudio and click "Run App", or run
#   shiny::runApp("app.R")
# from an R console in this folder. Missing packages will install automatically.
# ─────────────────────────────────────────────────────────────────────────────

required_pkgs <- c("shiny", "bslib", "shinyjs")
missing_pkgs  <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  message("StatGuide: installing missing packages: ",
          paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs, repos = "https://cloud.r-project.org")
}

library(shiny)
library(bslib)
library(shinyjs)

`%||%` <- function(a, b) if (!is.null(a)) a else b

glink <- function(text, term_id) {
  sprintf('<span class="glossary-link" data-term="%s">%s</span>', term_id, text)
}

# ── CSS ────────────────────────────────────────────────────────────────────────
app_css <- "
:root {
  --uf-orange:       #FA4616;
  --uf-blue:         #003087;
  --uf-orange-hover: #d93b10;
  --uf-blue-hover:   #002060;
  --uf-orange-light: #fff5f2;
  --uf-blue-light:   #edf1f9;
  --shadow-sm:       0 2px 8px rgba(0,0,0,0.08);
  --shadow-md:       0 4px 16px rgba(0,0,0,0.12);
  --radius:          12px;
}
.navbar { background-color: var(--uf-blue) !important; box-shadow: 0 2px 8px rgba(0,0,0,0.20); }
.navbar .navbar-brand { color: white !important; font-weight: 700; font-size: 1.25rem; }
.navbar .nav-link { color: rgba(255,255,255,0.85) !important; font-weight: 500; padding: 0.5rem 1rem !important; border-radius: 6px; transition: background 0.15s, color 0.15s; }
.navbar .nav-link:hover { color: white !important; background: rgba(255,255,255,0.15); }
.navbar .nav-link.active { color: white !important; background: rgba(255,255,255,0.20); font-weight: 600; }
.btn-primary { background-color: var(--uf-orange) !important; border-color: var(--uf-orange) !important; color: white !important; font-weight: 600; border-radius: 8px; padding: 0.5rem 1.4rem; transition: background 0.2s, transform 0.1s; }
.btn-primary:hover, .btn-primary:focus { background-color: var(--uf-orange-hover) !important; border-color: var(--uf-orange-hover) !important; transform: translateY(-1px); }
.btn-primary:active { transform: translateY(0); }
.btn-outline-secondary { border-color: var(--uf-blue) !important; color: var(--uf-blue) !important; font-weight: 600; border-radius: 8px; padding: 0.5rem 1.4rem; transition: background 0.2s, color 0.2s; }
.btn-outline-secondary:hover, .btn-outline-secondary:focus { background-color: var(--uf-blue) !important; color: white !important; }
.btn-link { color: var(--uf-blue); text-decoration: none; padding: 0; }
.btn-link:hover { color: var(--uf-orange); text-decoration: underline; }
.card { border: none !important; border-radius: var(--radius) !important; box-shadow: var(--shadow-sm); }
.wizard-container { max-width: 680px; margin: 2rem auto; padding: 0 1rem; }
.wizard-card { background: white; border-radius: var(--radius); box-shadow: var(--shadow-md); padding: 2rem 2.5rem; }
.wizard-step-label { font-size: 0.8rem; font-weight: 600; text-transform: uppercase; letter-spacing: 0.08em; color: #6c757d; margin-bottom: 0.25rem; }
.wizard-question { font-size: 1.4rem; font-weight: 700; color: var(--uf-blue); line-height: 1.3; margin-bottom: 1.5rem; }
.wizard-choice { display: block; width: 100%; padding: 1rem 1.25rem; border: 2px solid #e9ecef; border-radius: 10px; background: white; text-align: left; cursor: pointer; transition: border-color 0.18s, background 0.18s, transform 0.12s; margin-bottom: 0.6rem; font-family: inherit; }
.wizard-choice:hover { border-color: var(--uf-blue); background: var(--uf-blue-light); transform: translateX(4px); }
.wizard-choice.active { border-color: var(--uf-orange) !important; background: var(--uf-orange-light) !important; }
.wizard-choice.active .choice-label { color: var(--uf-orange); font-weight: 700; }
.choice-label { font-size: 1rem; font-weight: 600; color: #212529; display: block; margin-bottom: 0.1rem; }
.choice-desc { font-size: 0.85rem; color: #6c757d; display: block; }
.wizard-choice.active .choice-desc { color: #c23a10; }
.progress { height: 6px; border-radius: 3px; background: #e9ecef; margin-bottom: 1.75rem; }
.progress-bar { background-color: var(--uf-orange) !important; border-radius: 3px; transition: width 0.4s ease; }
.help-toggle-btn { font-size: 0.875rem; color: #6c757d; border: none; background: none; padding: 0; cursor: pointer; display: inline-flex; align-items: center; gap: 6px; transition: color 0.15s; }
.help-toggle-btn:hover { color: var(--uf-blue); }
.help-content { background: #f8f9fa; border-left: 3px solid var(--uf-blue); border-radius: 0 8px 8px 0; padding: 0.9rem 1.1rem; font-size: 0.9rem; line-height: 1.6; margin-top: 0.5rem; }
.help-content ul { margin-bottom: 0; padding-left: 1.2rem; }
.help-content li { margin-bottom: 0.3rem; }
.wizard-nav { display: flex; align-items: center; justify-content: space-between; margin-top: 1.75rem; padding-top: 1.25rem; border-top: 1px solid #f0f0f0; flex-wrap: wrap; gap: 0.5rem; }
.wizard-nav .btn-next { margin-left: auto; }
.wizard-error { color: #dc3545; font-size: 0.875rem; margin-top: 0.5rem; display: none; }
.wizard-error.visible { display: block; }
.results-container { max-width: 800px; margin: 2rem auto; padding: 0 1rem; }
.test-card { border-left: 5px solid var(--uf-blue) !important; border-radius: 0 var(--radius) var(--radius) 0 !important; box-shadow: var(--shadow-sm); margin-bottom: 1.5rem; background: white; overflow: hidden; }
.test-card.best-fit { border-left-color: var(--uf-orange) !important; }
.test-card-header { display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 0.5rem; padding: 1.25rem 1.5rem 0.75rem; border-bottom: 1px solid #f0f0f0; }
.test-card-body { padding: 1.25rem 1.5rem; }
.test-card-name { font-size: 1.15rem; font-weight: 700; color: var(--uf-blue); margin: 0; }
.test-card.best-fit .test-card-name { color: #c23a10; }
.badge-fit { display: inline-block; padding: 3px 12px; border-radius: 20px; font-size: 0.72rem; font-weight: 700; letter-spacing: 0.05em; text-transform: uppercase; }
.badge-best { background-color: var(--uf-orange); color: white; }
.badge-alt { background-color: var(--uf-blue); color: white; }
.test-section-label { font-size: 0.72rem; font-weight: 700; text-transform: uppercase; letter-spacing: 0.07em; color: #6c757d; margin-bottom: 0.35rem; margin-top: 1rem; display: block; }
.test-section-label:first-child { margin-top: 0; }
.test-card ul { padding-left: 1.2rem; margin-bottom: 0; font-size: 0.9rem; line-height: 1.6; }
.test-card p { font-size: 0.9rem; line-height: 1.6; margin-bottom: 0.5rem; }
.test-card p:last-child { margin-bottom: 0; }
pre { background-color: #1b1f23 !important; color: #e1e4e8 !important; border-radius: 8px; padding: 1.1rem 1.25rem; font-size: 0.82rem; overflow-x: auto; margin: 0; line-height: 1.55; border: none; }
code { font-family: 'Fira Code', 'Cascadia Code', 'Consolas', monospace; }
.answer-summary { background: var(--uf-blue-light); border: 1px solid #c5d0e8; border-radius: var(--radius); padding: 1rem 1.25rem; margin-bottom: 1.5rem; }
.answer-chip { display: inline-block; background: white; border: 1.5px solid var(--uf-blue); color: var(--uf-blue); border-radius: 20px; padding: 3px 12px; font-size: 0.8rem; font-weight: 600; margin: 3px 4px 3px 0; }
.answer-chip .chip-label { color: #6c757d; font-weight: 500; margin-right: 4px; }
.recommendation-note { background: #fffbea; border-left: 4px solid #ffc107; border-radius: 0 8px 8px 0; padding: 0.7rem 1rem; font-size: 0.875rem; margin-bottom: 1.25rem; line-height: 1.55; }
.no-results-box { background: #f8f9fa; border: 2px dashed #dee2e6; border-radius: var(--radius); padding: 3rem 2rem; text-align: center; color: #6c757d; }
.glossary-search-box { position: relative; max-width: 480px; margin-bottom: 1.5rem; }
.glossary-term-card { padding: 0.9rem 0; border-bottom: 1px solid #f0f0f0; }
.glossary-term-card:last-child { border-bottom: none; }
.glossary-term-heading { font-size: 1rem; font-weight: 700; color: var(--uf-blue); margin-bottom: 0.2rem; }
.glossary-term-def { font-size: 0.9rem; line-height: 1.6; color: #444; margin: 0; }
.glossary-term-card.highlighted { background: var(--uf-orange-light); border-radius: 8px; padding: 0.9rem 0.75rem; margin-bottom: 0.5rem; }
.glossary-section-letter { font-size: 1.25rem; font-weight: 700; color: var(--uf-blue); padding: 0.75rem 0 0.4rem; border-bottom: 2px solid var(--uf-blue); margin-bottom: 0.25rem; margin-top: 0.5rem; }
.glossary-link { color: var(--uf-blue); text-decoration: underline dotted var(--uf-blue); cursor: pointer; font-style: italic; }
.glossary-link:hover { color: var(--uf-orange); text-decoration: underline solid var(--uf-orange); }
.about-container { max-width: 760px; margin: 2rem auto; padding: 0 1rem; }
.about-step { display: flex; gap: 1rem; margin-bottom: 1.25rem; align-items: flex-start; }
.about-step-num { flex-shrink: 0; width: 36px; height: 36px; border-radius: 50%; background: var(--uf-orange); color: white; font-weight: 700; font-size: 1rem; display: flex; align-items: center; justify-content: center; }
.text-uf-blue   { color: var(--uf-blue) !important; }
.text-uf-orange { color: var(--uf-orange) !important; }
.bg-uf-blue     { background-color: var(--uf-blue) !important; }
.divider        { height: 1px; background: #f0f0f0; margin: 1.5rem 0; }
@media (max-width: 576px) {
  .wizard-card { padding: 1.25rem 1.1rem; }
  .wizard-question { font-size: 1.2rem; }
  .wizard-choice { padding: 0.85rem 1rem; }
  .test-card-header { flex-direction: column; align-items: flex-start; }
  .about-container, .results-container, .wizard-container { padding: 0 0.5rem; }
}
"

# ── JavaScript ─────────────────────────────────────────────────────────────────
app_js <- "
function selectWizardChoice(stepId, value, el) {
  document.querySelectorAll('.wizard-choice').forEach(function(btn) {
    btn.classList.remove('active');
  });
  el.classList.add('active');
  var err = document.getElementById('wizard-error');
  if (err) err.classList.remove('visible');
  Shiny.setInputValue('wizard_selection', stepId + '|' + value, {priority: 'event'});
}

$(document).on('click', '.glossary-link', function(e) {
  e.preventDefault();
  var term = $(this).data('term');
  Shiny.setInputValue('glossary_navigate', term, {priority: 'event'});
});

Shiny.addCustomMessageHandler('scrollToGlossaryTerm', function(termId) {
  setTimeout(function() {
    var el = document.getElementById('glossary-term-' + termId);
    if (el) {
      el.scrollIntoView({behavior: 'smooth', block: 'center'});
      el.classList.add('highlighted');
      setTimeout(function() { el.classList.remove('highlighted'); }, 2500);
    }
  }, 300);
});
"

# ── Glossary Terms ─────────────────────────────────────────────────────────────
glossary_terms <- data.frame(
  id = c(
    "alternative-hypothesis", "anova", "categorical-data", "confidence-interval",
    "continuous-data", "correlation", "degrees-of-freedom", "dependent-variable",
    "effect-size", "homogeneity-of-variance", "hypothesis-testing", "independent-variable",
    "interval-data", "likert-scale", "mean", "median",
    "non-parametric-test", "normal-distribution", "null-hypothesis", "ordinal-data",
    "outlier", "p-value", "paired-data", "parametric-test",
    "post-hoc-test", "power", "ratio-data", "regression",
    "sample-size", "significance-level", "skewness", "sphericity",
    "standard-deviation", "statistical-significance", "type-i-error", "type-ii-error",
    "variance"
  ),
  term = c(
    "Alternative Hypothesis", "ANOVA", "Categorical Data", "Confidence Interval",
    "Continuous Data", "Correlation", "Degrees of Freedom", "Dependent Variable",
    "Effect Size", "Homogeneity of Variance", "Hypothesis Testing", "Independent Variable",
    "Interval Data", "Likert Scale", "Mean", "Median",
    "Non-Parametric Test", "Normal Distribution", "Null Hypothesis", "Ordinal Data",
    "Outlier", "P-Value", "Paired Data", "Parametric Test",
    "Post-Hoc Test", "Power", "Ratio Data", "Regression",
    "Sample Size", "Significance Level", "Skewness", "Sphericity",
    "Standard Deviation", "Statistical Significance", "Type I Error", "Type II Error",
    "Variance"
  ),
  definition = c(
    "The hypothesis that there IS a real effect, difference, or relationship in the population. It is what you are trying to find evidence for, often written as H₁ or Hₐ. A small p-value provides evidence in favor of the alternative hypothesis.",
    "Analysis of Variance — a statistical test used to compare the means of three or more groups simultaneously. ANOVA tests whether at least one group mean differs from the others, but does not tell you which groups differ (that requires a post-hoc test).",
    "Data that falls into distinct, named groups or categories with no meaningful numerical order. Examples include species (dog/cat/bird), blood type (A/B/AB/O), and survey responses (yes/no/maybe). You cannot calculate a meaningful average of categorical data.",
    "A range of values computed from your sample that is likely to contain the true population parameter. A 95% confidence interval means if you repeated the study many times, 95% of the calculated intervals would contain the true value. Wider intervals indicate more uncertainty.",
    "Numerical data that can take any value within a range, including decimals. Examples include height (1.73 m), reaction time (342.5 ms), and temperature (98.6°F). Continuous data is always numerical.",
    "A measure of the direction and strength of the linear relationship between two numerical variables. Ranges from −1 (perfect negative relationship) to +1 (perfect positive relationship), with 0 indicating no linear relationship. Correlation does not imply causation.",
    "The number of values in a statistical calculation that are free to vary. In practice it is closely related to sample size — larger samples give more degrees of freedom, which generally makes tests more reliable and powerful.",
    "The variable you are measuring or trying to predict — the outcome. Also called the response variable. Example: if studying the effect of fertilizer on plant height, plant height is the dependent variable. In a regression equation it is usually labeled Y.",
    "A standardized measure of how large or practically meaningful a difference or relationship is, separate from whether it is statistically significant. Common measures include Cohen's d (for group differences) and r (for correlations). A result can be statistically significant but have a tiny effect size.",
    "An assumption that the groups being compared have similar variances (spread) in their data. Also called homoscedasticity. When violated, tests like ANOVA become unreliable. Levene's test checks this assumption.",
    "A formal procedure for using sample data to evaluate a claim about a population. Steps include: state null and alternative hypotheses, collect data, compute a test statistic, calculate a p-value, and decide whether to reject the null hypothesis based on a pre-set significance level.",
    "The variable you manipulate or use to predict another variable. Also called the predictor or explanatory variable. Example: in a drug trial, the dosage is the independent variable. In a regression equation it is usually labeled X.",
    "Numerical data with equal, meaningful spacing between values, but no true zero point. Examples: temperature in Celsius and IQ scores. Meaningful differences can be calculated, but not ratios.",
    "A rating scale commonly ranging from 1–5 or 1–7, anchored by labels like “Strongly Disagree” to “Strongly Agree.” Technically ordinal data, though many researchers treat 5- or 7-point Likert items as continuous in practice.",
    "The arithmetic average of a dataset: the sum of all values divided by the count. The mean is sensitive to outliers — a single very large or small value can pull it away from the center of most of the data.",
    "The middle value of a dataset when all values are sorted in order. Half the values fall above the median and half below it. Less sensitive to outliers than the mean, making it a better measure of center for skewed distributions.",
    "A statistical test that does not assume the data follows a specific distribution like the normal distribution. Non-parametric tests typically rank the data and analyze those ranks. They are appropriate for ordinal data, small samples, and situations where normality assumptions are violated.",
    "A symmetric, bell-shaped distribution where most values cluster near the mean and fewer values appear in the tails. Many parametric tests assume data is normally distributed. You can assess normality with histograms, Q-Q plots, or formal tests like Shapiro-Wilk.",
    "The hypothesis of no effect, no difference, and no relationship — written as H₀. Statistical tests try to find evidence against it. Failing to reject H₀ does not prove it is true; it only means your data did not provide enough evidence to reject it.",
    "Data that has a meaningful order but unknown or unequal spacing between categories. Examples include rankings (1st, 2nd, 3rd), satisfaction levels (poor/fair/good/excellent), and pain ratings (0–10). You can say one value is greater than another, but not by how much.",
    "An observation that falls far from the rest of the data. Outliers can distort means, inflate variances, and violate normality assumptions. They should always be investigated — they may represent data entry errors, measurement mistakes, or genuinely unusual cases.",
    "The probability of observing a result at least as extreme as yours, assuming the null hypothesis is true. A small p-value (typically < 0.05) is evidence against the null hypothesis. The p-value is NOT the probability that the null hypothesis is true.",
    "Data where observations come in matched pairs — for example, measuring the same person before and after a treatment, or pairing subjects by a matching variable. Paired data violates the independence assumption of tests like the independent t-test; paired tests must be used instead.",
    "A statistical test that assumes the data follows a specific distribution, usually the normal distribution. Parametric tests (t-tests, ANOVA, Pearson correlation) are generally more powerful than non-parametric tests when assumptions are met.",
    "A follow-up test performed after a significant ANOVA result to identify which specific group pairs differ from each other. Common options include Tukey's HSD, Bonferroni correction, and Dunn's test (for Kruskal-Wallis). Post-hoc tests control the Type I error rate when making multiple comparisons.",
    "The probability that a statistical test will correctly detect a real effect (i.e., reject a false null hypothesis). Power = 1 − β. Increasing sample size, effect size, or the significance level all increase power. A power of 0.80 (80%) is commonly considered adequate.",
    "Numerical data with equal spacing between values AND a true zero point, meaning zero represents the complete absence of the quantity. Examples: height, weight, income, and reaction time. Ratio data allows meaningful ratio statements.",
    "A statistical method for modeling the relationship between a dependent (outcome) variable and one or more predictor (independent) variables. Regression can be used to predict outcomes and understand which predictors have the strongest relationship with the outcome.",
    "The number of observations in your study (n). Larger sample sizes reduce sampling error, increase statistical power, and make tests more robust to violations of assumptions.",
    "The threshold p-value (called α, alpha) used to decide whether to reject the null hypothesis. The most common choice is α = 0.05 (5%). A lower threshold makes it harder to reject H₀ and reduces Type I error, but also reduces power.",
    "A measure of the asymmetry of a distribution. Positive skew means a long tail to the right (a few very large values); negative skew means a long tail to the left. Strongly skewed data may violate normality assumptions for parametric tests.",
    "An assumption required for repeated measures ANOVA stating that the variances of the differences between all pairs of time points are equal. Mauchly's test checks this. If violated, apply a correction such as Greenhouse–Geisser.",
    "A measure of the spread or variability in a dataset, expressed in the same units as the data. It equals the square root of the variance. In a normal distribution, approximately 68% of values fall within one standard deviation of the mean.",
    "A result is statistically significant when its p-value falls below the chosen significance level (α). It means the result is unlikely to have occurred by chance alone under the null hypothesis. Statistical significance does not equal practical importance — always consider effect size too.",
    "Incorrectly rejecting the null hypothesis when it is actually true — a false positive. The probability of a Type I error equals the significance level (α). Example: concluding a new drug is effective when it actually has no effect.",
    "Failing to reject the null hypothesis when it is actually false — a false negative. Denoted β. The probability of avoiding Type II error is statistical power (1 − β). Example: concluding a treatment has no effect when it actually does. Increasing sample size reduces Type II error.",
    "A measure of how spread out the values in a dataset are around the mean. Calculated as the average squared deviation from the mean. Standard deviation is its square root and is easier to interpret because it is in the original units."
  ),
  stringsAsFactors = FALSE
)

# ── Test Definitions ──────────────────────────────────────────────────────────
TESTS <- list(

  t_test_indep = list(
    name = "Independent Samples t-Test",
    description = "Compares the means of two separate, unrelated groups to see if they differ more than you would expect by chance. One of the most widely used statistical tests in research.",
    when_to_use = c(
      "You have two independent groups (different people or units in each group)",
      "Your outcome variable is numerical (continuous or interval/ratio scale)",
      "You want to test whether the group means differ significantly",
      "Parametric assumptions are reasonably met"
    ),
    when_not_to_use = c(
      "The same subjects appear in both groups (use Paired t-Test instead)",
      "You have more than two groups (use One-Way ANOVA instead)",
      "Data are severely non-normal and sample is small (use Mann-Whitney U instead)",
      "Your outcome variable is categorical or ordinal"
    ),
    assumptions = c(
      "Both groups are independently sampled",
      "The outcome is approximately normally distributed within each group (or n > 30 per group)",
      "Homogeneity of variance — similar variances in both groups (Welch's t-test, R's default, relaxes this)",
      "No major outliers"
    ),
    r_code = '# Independent samples t-test (Welch\'s, unequal variances by default)
group1 <- c(23, 25, 28, 30, 22, 27, 24, 29)
group2 <- c(18, 20, 24, 19, 22, 17, 21, 20)

result <- t.test(group1, group2, var.equal = FALSE)
print(result)

# With a data frame in long format:
# df <- data.frame(score = c(group1, group2),
#                  group = rep(c("A","B"), each = 8))
# t.test(score ~ group, data = df)'
  ),

  mann_whitney = list(
    name = "Mann-Whitney U Test",
    description = "A non-parametric alternative to the independent t-test. Compares the rank distributions of two groups rather than their means. Also known as the Wilcoxon rank-sum test.",
    when_to_use = c(
      "You have two independent groups",
      "Your outcome variable is numerical or ordinal",
      "Normality assumptions are violated or sample size is small",
      "Data contain outliers that would unduly influence a t-test"
    ),
    when_not_to_use = c(
      "The same subjects appear in both groups (use Wilcoxon signed-rank test instead)",
      "You have more than two groups (use Kruskal-Wallis instead)",
      "Data are clearly normally distributed and samples are large (t-test would be more powerful)"
    ),
    assumptions = c(
      "Observations within each group are independent",
      "Both groups come from populations with similar distributional shapes (if comparing medians)",
      "The outcome variable is at least ordinal (can be ranked)"
    ),
    r_code = '# Mann-Whitney U test (Wilcoxon rank-sum test)
group1 <- c(23, 25, 28, 30, 22, 27, 24, 29)
group2 <- c(18, 20, 24, 19, 22, 17, 21, 20)

result <- wilcox.test(group1, group2)
print(result)

# With a data frame in long format:
# wilcox.test(score ~ group, data = df)'
  ),

  t_test_paired = list(
    name = "Paired Samples t-Test",
    description = "Compares means from the same group measured twice (e.g., before and after a treatment), or from two groups matched on key characteristics. Accounts for the natural correlation between paired observations.",
    when_to_use = c(
      "The same subjects are measured under two conditions (pre/post, treatment/control)",
      "Observations are explicitly matched in pairs",
      "Your outcome variable is numerical",
      "Parametric assumptions are reasonably met"
    ),
    when_not_to_use = c(
      "The two measurements come from completely different, unrelated groups (use independent t-test instead)",
      "You have more than two time points (use Repeated Measures ANOVA instead)",
      "Differences between pairs are severely non-normal and sample is small (use Wilcoxon signed-rank instead)"
    ),
    assumptions = c(
      "Paired observations are randomly and independently sampled",
      "The differences between pairs are approximately normally distributed (or n > 30)",
      "No major outliers in the paired differences"
    ),
    r_code = '# Paired samples t-test
before <- c(85, 92, 78, 95, 88, 82, 90, 87)
after  <- c(89, 90, 83, 98, 91, 86, 93, 89)

result <- t.test(before, after, paired = TRUE)
print(result)

cat("Mean difference:", round(mean(after - before), 2), "\n")
cat("95% CI:", round(result$conf.int, 2), "\n")'
  ),

  wilcoxon = list(
    name = "Wilcoxon Signed-Rank Test",
    description = "A non-parametric alternative to the paired t-test. Ranks the absolute differences between pairs and tests whether the signed ranks are symmetric around zero, without assuming normally distributed differences.",
    when_to_use = c(
      "You have paired or repeated measurements from the same subjects",
      "Normality of the paired differences is violated or sample is small",
      "Your outcome is numerical or ordinal",
      "Data contain outliers in the paired differences"
    ),
    when_not_to_use = c(
      "The two groups are independent (use Mann-Whitney U instead)",
      "You have more than two time points (use Friedman test instead)",
      "The differences between pairs are clearly normally distributed (paired t-test would be more powerful)"
    ),
    assumptions = c(
      "Observations are paired and each pair is independent of other pairs",
      "The differences between pairs can be meaningfully ranked",
      "The distribution of differences is approximately symmetric around the median"
    ),
    r_code = '# Wilcoxon signed-rank test
before <- c(85, 92, 78, 95, 88, 82, 90, 87)
after  <- c(89, 90, 83, 98, 91, 86, 93, 89)

result <- wilcox.test(before, after, paired = TRUE)
print(result)

cat("Median difference:", median(after - before), "\n")'
  ),

  anova_oneway = list(
    name = "One-Way ANOVA",
    description = "Tests whether the means of three or more independent groups differ significantly. ANOVA stands for Analysis of Variance — it compares variability between groups to variability within groups.",
    when_to_use = c(
      "You have three or more independent groups",
      "Your outcome variable is numerical",
      "You want to test whether at least one group mean differs from the others",
      "Parametric assumptions (normality, equal variance) are reasonably met"
    ),
    when_not_to_use = c(
      "Groups are related or repeated measurements (use Repeated Measures ANOVA)",
      "Normality or equal variance assumptions are clearly violated (use Kruskal-Wallis instead)",
      "You want to know which specific groups differ — ANOVA alone does not tell you (follow up with post-hoc tests)"
    ),
    assumptions = c(
      "Groups are independently sampled",
      "The outcome is approximately normally distributed within each group (or group sizes are large)",
      "Homogeneity of variance across groups (Levene's test can check this)",
      "No major outliers"
    ),
    r_code = '# One-way ANOVA
data <- data.frame(
  score = c(85,90,78,92, 76,80,84,79, 95,99,93,97),
  group = rep(c("Control","Low Dose","High Dose"), each = 4)
)

result <- aov(score ~ group, data = data)
summary(result)

# Post-hoc test if ANOVA is significant (p < .05)
TukeyHSD(result)'
  ),

  kruskal_wallis = list(
    name = "Kruskal-Wallis Test",
    description = "A non-parametric alternative to one-way ANOVA. Tests whether three or more independent groups come from the same distribution by comparing rank distributions across groups.",
    when_to_use = c(
      "You have three or more independent groups",
      "Your outcome variable is numerical or ordinal",
      "Normality or equal variance assumptions are violated",
      "Sample sizes are small or data contain outliers"
    ),
    when_not_to_use = c(
      "You have only two groups (use Mann-Whitney U instead)",
      "Groups are repeated measurements (use Friedman test instead)",
      "All parametric assumptions are clearly met (one-way ANOVA is more powerful)"
    ),
    assumptions = c(
      "Groups are independently sampled",
      "The outcome variable is at least ordinal (can be ranked)",
      "All groups come from populations with similar distributional shapes (if comparing medians)"
    ),
    r_code = '# Kruskal-Wallis test
data <- data.frame(
  score = c(85,90,78,92, 76,80,84,79, 95,99,93,97),
  group = rep(c("Control","Low Dose","High Dose"), each = 4)
)

result <- kruskal.test(score ~ group, data = data)
print(result)

# Post-hoc pairwise comparisons (Dunn test)
# install.packages("dunn.test")
library(dunn.test)
dunn.test(data$score, data$group, method = "bonferroni")'
  ),

  anova_rm = list(
    name = "Repeated Measures ANOVA",
    description = "Tests whether means differ across three or more time points or conditions when the same subjects are measured repeatedly. Accounts for the correlation between repeated measurements on the same person.",
    when_to_use = c(
      "The same subjects are measured at three or more time points or conditions",
      "Your outcome variable is numerical",
      "Parametric assumptions are reasonably met",
      "You want to test for an overall change across time or conditions"
    ),
    when_not_to_use = c(
      "Groups are independent, not repeated (use one-way ANOVA instead)",
      "You have only two time points (use paired t-test instead)",
      "Sphericity is violated and you do not apply a correction (use Greenhouse-Geisser or Huynh-Feldt)",
      "Normality assumptions are clearly violated (use Friedman test instead)"
    ),
    assumptions = c(
      "The same subjects are measured under all conditions",
      "The outcome is approximately normally distributed at each time point",
      "Sphericity — the variances of differences between all pairs of conditions are equal (Mauchly's test checks this)"
    ),
    r_code = '# Repeated measures ANOVA
data <- data.frame(
  subject = rep(1:6, 3),
  time    = rep(c("Pre","Post","Follow-up"), each = 6),
  score   = c(78,82,80,75,77,81,  84,86,83,80,82,85,  88,90,87,85,89,91)
)
data$time <- factor(data$time, levels = c("Pre","Post","Follow-up"))

result <- aov(score ~ time + Error(subject/time), data = data)
summary(result)

# For sphericity tests and corrections:
# install.packages("ez")
# library(ez)
# ezANOVA(data, dv=score, wid=subject, within=time)'
  ),

  friedman = list(
    name = "Friedman Test",
    description = "A non-parametric alternative to repeated measures ANOVA. Ranks observations within each subject across conditions and tests whether the rank distributions differ across conditions.",
    when_to_use = c(
      "The same subjects are measured at three or more time points or conditions",
      "Your outcome variable is numerical or ordinal",
      "Normality assumptions are violated or sample is small",
      "Data contain outliers that would distort a repeated measures ANOVA"
    ),
    when_not_to_use = c(
      "Groups are independent (use Kruskal-Wallis instead)",
      "You have only two conditions (use Wilcoxon signed-rank instead)",
      "All parametric assumptions are clearly met (repeated measures ANOVA is more powerful)"
    ),
    assumptions = c(
      "The same subjects are measured under all conditions",
      "The outcome variable is at least ordinal (can be ranked)",
      "Observations are independent across subjects (though repeated within each subject)"
    ),
    r_code = '# Friedman test
# Rows = subjects, Columns = conditions
scores <- matrix(
  c(78, 84, 88,
    82, 86, 90,
    80, 83, 87,
    75, 80, 85,
    77, 82, 89,
    81, 85, 91),
  nrow = 6, byrow = TRUE,
  dimnames = list(NULL, c("Pre","Post","Follow-up"))
)

result <- friedman.test(scores)
print(result)

# Post-hoc pairwise comparisons
# install.packages("PMCMRplus")
library(PMCMRplus)
frdAllPairsConoverTest(scores)'
  ),

  pearson = list(
    name = "Pearson Correlation",
    description = "Measures the strength and direction of the linear relationship between two continuous numerical variables. The result (r) ranges from −1 to +1, where values near ±1 indicate a strong linear relationship.",
    when_to_use = c(
      "You want to quantify the relationship between two numerical variables",
      "Both variables are continuous and measured on an interval or ratio scale",
      "You expect the relationship to be approximately linear",
      "Parametric assumptions are reasonably met"
    ),
    when_not_to_use = c(
      "The relationship appears strongly non-linear (Pearson r only captures linear relationships)",
      "Either variable is ordinal or heavily skewed (use Spearman instead)",
      "Data contain influential outliers (use Spearman instead)",
      "You want to predict one variable from another (use linear regression)"
    ),
    assumptions = c(
      "Both variables are continuous (interval or ratio scale)",
      "Both variables are approximately normally distributed",
      "The relationship between the two variables is linear",
      "No influential outliers"
    ),
    r_code = '# Pearson correlation
x <- c(2, 4, 5, 6, 8, 9, 10, 11, 12, 14)
y <- c(3, 6, 5, 7, 10, 9, 11, 12, 13, 16)

result <- cor.test(x, y, method = "pearson")
print(result)

cat("r =", round(result$estimate, 3), "\n")
cat("p =", round(result$p.value, 4), "\n")
cat("95% CI:", round(result$conf.int, 3), "\n")

# Visualize
plot(x, y, pch = 16, col = "#003087", main = "Pearson Correlation")
abline(lm(y ~ x), col = "#FA4616", lwd = 2)'
  ),

  spearman = list(
    name = "Spearman Rank Correlation",
    description = "A non-parametric measure of the monotonic relationship between two variables. Converts values to ranks before calculating the correlation, making it robust to outliers and non-normal distributions.",
    when_to_use = c(
      "You want to measure the relationship between two variables",
      "One or both variables are ordinal, or the data are not normally distributed",
      "The relationship may be monotonic but not strictly linear",
      "Data contain outliers that would distort Pearson's r"
    ),
    when_not_to_use = c(
      "Both variables are continuous, normal, and the relationship is clearly linear (Pearson r is more powerful)",
      "You want to predict one variable from another (consider regression alternatives)"
    ),
    assumptions = c(
      "Both variables are at least ordinal (can be meaningfully ranked)",
      "The relationship between the variables is monotonic (consistently increases or decreases)",
      "Pairs of observations are independent of other pairs"
    ),
    r_code = '# Spearman rank correlation
x <- c(2, 4, 5, 6, 8, 9, 10, 11, 12, 14)
y <- c(3, 6, 5, 7, 10, 9, 11, 12, 13, 16)

result <- cor.test(x, y, method = "spearman")
print(result)

cat("rho =", round(result$estimate, 3), "\n")
cat("p =", round(result$p.value, 4), "\n")

# For ordinal data (e.g., Likert ratings vs. performance score):
# cor.test(survey_rating, performance, method = "spearman")'
  ),

  simple_regression = list(
    name = "Simple Linear Regression",
    description = "Models the relationship between one predictor variable (X) and one numerical outcome variable (Y) by fitting the best straight line through the data. Used for prediction and for quantifying how much Y changes per unit increase in X.",
    when_to_use = c(
      "You want to predict a numerical outcome from a single predictor variable",
      "You want to quantify the relationship between one predictor and one outcome",
      "The relationship appears linear",
      "Parametric assumptions are reasonably met"
    ),
    when_not_to_use = c(
      "You have multiple predictor variables (use Multiple Linear Regression)",
      "Your outcome variable is categorical (use logistic regression instead)",
      "The relationship is clearly non-linear (consider polynomial or non-linear regression)",
      "Residuals are not normally distributed or show a clear pattern (check diagnostic plots)"
    ),
    assumptions = c(
      "A linear relationship exists between X and Y",
      "Residuals (prediction errors) are approximately normally distributed",
      "Residuals have constant variance across all levels of X (homoscedasticity)",
      "Observations are independent",
      "No severe outliers or high-leverage points"
    ),
    r_code = '# Simple linear regression
data <- data.frame(
  x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  y = c(2.3, 4.1, 5.8, 7.9, 9.4, 11.2, 13.0, 15.1, 16.9, 19.0)
)

model <- lm(y ~ x, data = data)
summary(model)

# Check the four diagnostic plots
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# Make a prediction
new_data <- data.frame(x = 11)
predict(model, new_data, interval = "confidence")'
  ),

  multiple_regression = list(
    name = "Multiple Linear Regression",
    description = "Extends simple linear regression to model the relationship between two or more predictor variables and a numerical outcome. Estimates the unique contribution of each predictor while holding the others constant.",
    when_to_use = c(
      "You have two or more predictor variables and one numerical outcome",
      "You want to control for confounding variables while examining a focal predictor",
      "You want to build a predictive model using multiple variables",
      "The relationship between each predictor and the outcome is approximately linear"
    ),
    when_not_to_use = c(
      "You have only one predictor (use Simple Linear Regression for simplicity)",
      "Your outcome variable is categorical (use logistic regression)",
      "Predictors are highly correlated with each other (multicollinearity; check VIF)",
      "You have more predictors than observations"
    ),
    assumptions = c(
      "A linear relationship exists between each predictor and the outcome",
      "Residuals are approximately normally distributed",
      "Homoscedasticity — residuals have constant variance",
      "Observations are independent",
      "No severe multicollinearity among predictors (check VIF; values > 10 are concerning)"
    ),
    r_code = '# Multiple linear regression
data <- data.frame(
  y  = c(10, 14, 18, 22, 26, 30, 34, 38, 42, 46),
  x1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  x2 = c(5, 6, 4, 7, 8, 9, 7, 10, 11, 9)
)

model <- lm(y ~ x1 + x2, data = data)
summary(model)

# Check for multicollinearity
# install.packages("car")
library(car)
vif(model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))'
  ),

  chi_square = list(
    name = "Chi-Square Test of Independence",
    description = "Tests whether there is an association between two categorical variables. Compares observed frequencies in each cell of a contingency table to the frequencies expected if the variables were independent.",
    when_to_use = c(
      "Both variables are categorical",
      "You want to test whether two categorical variables are related or associated",
      "You have a reasonably large sample (expected cell counts ≥ 5 in most cells)",
      "Observations are independent"
    ),
    when_not_to_use = c(
      "Expected cell counts are small (< 5 in 20% or more of cells) — use Fisher's Exact Test instead",
      "Observations are paired (use McNemar's Test instead)",
      "You have one variable and want to compare to a known distribution"
    ),
    assumptions = c(
      "Observations are independent (no person counted twice)",
      "Expected frequency in each cell is at least 5 (check with result$expected in R)",
      "The data are counts (frequencies), not percentages or proportions"
    ),
    r_code = '# Chi-square test of independence
data <- data.frame(
  treatment = c(rep("Drug",50), rep("Placebo",50)),
  outcome   = c(rep("Improved",35), rep("Not Improved",15),
                rep("Improved",20), rep("Not Improved",30))
)

table_data <- table(data$treatment, data$outcome)
print(table_data)

result <- chisq.test(table_data)
print(result)

# Check expected counts (all should be >= 5)
result$expected'
  ),

  fisher_exact = list(
    name = "Fisher's Exact Test",
    description = "An exact non-parametric test of association between two categorical variables, particularly useful when sample sizes are small and expected cell counts are too low for the chi-square approximation to be reliable.",
    when_to_use = c(
      "Both variables are categorical (usually 2×2 tables)",
      "Sample size is small or expected cell counts are less than 5",
      "You want an exact p-value rather than an approximation",
      "The chi-square test's assumptions are not met"
    ),
    when_not_to_use = c(
      "Sample sizes are large (chi-square test is equally accurate and faster)",
      "Observations are paired (use McNemar's Test)",
      "You have a very large contingency table (computation becomes intensive)"
    ),
    assumptions = c(
      "Observations are independent",
      "The marginal totals of the table are fixed",
      "Data are counts (frequencies)"
    ),
    r_code = '# Fisher\'s exact test (best for small samples)
table_data <- matrix(
  c(5, 2, 1, 8),
  nrow = 2,
  dimnames = list(Group   = c("Treatment","Control"),
                  Outcome = c("Success","Failure"))
)
print(table_data)

result <- fisher.test(table_data)
print(result)

cat("Odds ratio:", round(result$estimate, 3), "\n")
cat("95% CI:", round(result$conf.int, 3), "\n")'
  ),

  mcnemar = list(
    name = "McNemar's Test",
    description = "Tests whether the marginal probabilities of a binary outcome differ between two paired measurements — for example, testing whether the proportion of 'yes' responses changes before vs. after an intervention in the same group of people.",
    when_to_use = c(
      "You have paired or repeated binary (two-category) measurements from the same subjects",
      "You want to test whether a proportion changed between two time points",
      "Example: measuring agree/disagree before and after an intervention on the same individuals"
    ),
    when_not_to_use = c(
      "The two measurements come from independent groups (use chi-square or Fisher's Exact Test)",
      "You have more than two categories or more than two time points",
      "Your outcome is numerical (use paired t-test or Wilcoxon signed-rank)"
    ),
    assumptions = c(
      "Observations are paired (same subjects measured twice)",
      "The outcome variable has exactly two categories",
      "The number of discordant pairs (cells b + c in the 2×2 table) is at least 10 for reliable results"
    ),
    r_code = '# McNemar\'s test
# Table rows = Before, Cols = After
table_data <- matrix(
  c(45, 15, 5, 35),
  nrow = 2,
  dimnames = list(Before = c("Yes","No"),
                  After  = c("Yes","No"))
)
print(table_data)

result <- mcnemar.test(table_data)
print(result)

n <- sum(table_data)
cat("Proportion changed:", round((table_data[1,2] + table_data[2,1]) / n, 3), "\n")'
  ),

  binomial = list(
    name = "Binomial Test",
    description = "Tests whether the observed proportion of successes in a binary outcome matches a hypothesized probability. An exact test, making it reliable for any sample size.",
    when_to_use = c(
      "You have a single group with a binary (two-outcome) variable",
      "You want to test whether the proportion of one outcome matches a specific hypothesized value",
      "Example: testing whether a coin is fair (H₀: p = 0.5), or whether a pass rate equals 70%"
    ),
    when_not_to_use = c(
      "You are comparing two or more groups (use chi-square or Fisher's Exact Test instead)",
      "Your outcome variable has more than two categories",
      "Your outcome is numerical (use a one-sample t-test)"
    ),
    assumptions = c(
      "Each trial has exactly two possible outcomes (success or failure)",
      "Each trial is independent of the others",
      "The probability of success is constant across all trials"
    ),
    r_code = '# Binomial test
# Example: is the success rate different from 50%?
successes    <- 65    # number of successes observed
n            <- 100   # total number of trials
hypothesized <- 0.5   # hypothesized probability under H0

result <- binom.test(successes, n, p = hypothesized)
print(result)

cat("Observed proportion:", round(result$estimate, 3), "\n")
cat("95% CI:", round(result$conf.int, 3), "\n")

# Two-sided by default; use alternative = "greater" or "less" for one-sided'
  ),

  z_test_prop = list(
    name = "Z-Test for Proportions",
    description = "Tests whether a proportion (or the difference between two proportions) equals a hypothesized value. Based on the normal approximation to the binomial, suitable for large samples.",
    when_to_use = c(
      "You want to test whether a single proportion equals a hypothesized value (one-sample)",
      "You want to compare proportions between two independent groups (two-sample)",
      "Your sample is large enough for the normal approximation (n×p ≥ 5 and n×(1−p) ≥ 5)"
    ),
    when_not_to_use = c(
      "Sample sizes are small (use Binomial Test for one-sample, or Fisher's Exact Test for two samples)",
      "Comparing proportions across three or more groups (use chi-square)",
      "Data are paired (use McNemar's Test)"
    ),
    assumptions = c(
      "Observations are independent",
      "Large enough sample so normal approximation holds (n×p ≥ 5 and n×(1−p) ≥ 5 for each group)",
      "Random sampling"
    ),
    r_code = '# Z-test for proportions --- one sample
prop.test(65, 100, p = 0.5)

# Z-test for proportions --- two independent groups
successes <- c(65, 48)
totals    <- c(100, 100)

result <- prop.test(successes, totals)
print(result)

cat("Group proportions:", round(result$estimate, 3), "\n")
cat("95% CI for difference:", round(result$conf.int, 3), "\n")'
  )
)

# ── Decision Logic ─────────────────────────────────────────────────────────────
STEP_CONFIGS <- list(

  data_type = list(
    id       = "data_type",
    question = "What type of data are you working with?",
    help_text = paste0(
      "<p>The type of data you have is the first key decision.</p><ul>",
      "<li><strong>Numerical</strong> — numbers you can do math with: averages, sums, standard deviations. ",
      "Examples: test scores, blood pressure, plant height, reaction time.</li>",
      "<li><strong>Categorical</strong> — distinct groups with no inherent order. ",
      "Examples: species, yes/no answers, treatment group, blood type. See: ",
      glink("Categorical Data", "categorical-data"), ".</li>",
      "<li><strong>Ordinal</strong> — categories with a meaningful order but unknown spacing between them. ",
      "Examples: ", glink("Likert Scale", "likert-scale"), " ratings (1–5), education level, pain ratings. See: ",
      glink("Ordinal Data", "ordinal-data"), ".</li></ul>"
    ),
    choices = list(
      list(value = "numerical",   label = "Numerical",
           desc  = "Continuous numbers — heights, scores, temperatures, times"),
      list(value = "categorical", label = "Categorical",
           desc  = "Distinct groups or categories — yes/no, species, treatment group"),
      list(value = "ordinal",     label = "Ordinal",
           desc  = "Ordered categories with unknown spacing — Likert scales, rankings")
    )
  ),

  goal = list(
    id       = "goal",
    question = "What is your main research goal?",
    help_text = paste0(
      "<p>Think about what question you are trying to answer with your data.</p><ul>",
      "<li><strong>Compare groups</strong> — Is there a difference between groups? ",
      "Example: Do students who received tutoring score higher than those who did not?</li>",
      "<li><strong>Examine relationships</strong> — Are two variables associated? ",
      "Example: Is study time related to exam score? See: ", glink("Correlation", "correlation"), ".</li>",
      "<li><strong>Predict an outcome</strong> — Can I use one or more variables to predict another? ",
      "Example: Can age and diet predict blood pressure? See: ", glink("Regression", "regression"), ".</li>",
      "</ul>"
    ),
    choices = list(
      list(value = "compare", label = "Compare groups",
           desc  = "Test whether groups differ on some outcome"),
      list(value = "relate",  label = "Examine relationships",
           desc  = "Measure the association or correlation between two variables"),
      list(value = "predict", label = "Predict an outcome",
           desc  = "Use predictor variables to explain or forecast an outcome")
    )
  ),

  num_groups = list(
    id       = "num_groups",
    question = "How many groups are you comparing?",
    help_text = paste0(
      "<p>Count the number of distinct groups in your study.</p><ul>",
      "<li><strong>Two groups</strong> — e.g., treated vs. control, male vs. female, before vs. after.</li>",
      "<li><strong>More than two groups</strong> — e.g., three dosage levels, four schools, five time points. ",
      "Comparing more than two groups simultaneously requires tests like ANOVA or Kruskal-Wallis to avoid inflating the ",
      glink("Type I Error", "type-i-error"), " rate.</li></ul>"
    ),
    choices = list(
      list(value = "two",           label = "Two groups",
           desc  = "Comparing exactly two groups or conditions"),
      list(value = "more_than_two", label = "More than two groups",
           desc  = "Comparing three or more groups or conditions simultaneously")
    )
  ),

  independence = list(
    id       = "independence",
    question = "Are your groups independent or paired/repeated?",
    help_text = paste0(
      "<p>This question asks whether the same subjects appear in more than one group.</p><ul>",
      "<li><strong>Independent groups</strong> — completely different people (or units) in each group. ",
      "Example: randomly assigning half the students to tutoring and the other half to a control group.</li>",
      "<li><strong>Paired / Repeated measures</strong> — the same subjects measured more than once, or subjects ",
      "explicitly matched across groups. Examples: pre/post measurements; twins assigned to different treatments. ",
      "See: ", glink("Paired Data", "paired-data"), ".</li></ul>",
      "<p>Using the wrong test here (independent vs. paired) can seriously distort your results.</p>"
    ),
    choices = list(
      list(value = "independent", label = "Independent groups",
           desc  = "Different subjects in each group — no matching or repeated measures"),
      list(value = "paired",      label = "Paired / Repeated measures",
           desc  = "Same subjects measured multiple times, or subjects matched across groups")
    )
  ),

  num_predictors = list(
    id       = "num_predictors",
    question = "How many predictor variables do you have?",
    help_text = paste0(
      "<p>A predictor variable (also called an ", glink("independent variable", "independent-variable"),
      ") is a variable you use to predict or explain another variable.</p><ul>",
      "<li><strong>One predictor</strong> — e.g., using study hours to predict exam score. → Simple linear regression.</li>",
      "<li><strong>Multiple predictors</strong> — e.g., using study hours, sleep, and prior GPA together to predict exam score. ",
      "→ Multiple linear regression. This lets you examine each predictor's unique contribution while controlling for the others.</li>",
      "</ul>"
    ),
    choices = list(
      list(value = "one",      label = "One predictor",
           desc  = "A single variable used to predict the outcome (simple regression)"),
      list(value = "multiple", label = "Multiple predictors",
           desc  = "Two or more variables used together to predict the outcome")
    )
  ),

  parametric = list(
    id       = "parametric",
    question = "Are the parametric assumptions met for your data?",
    help_text = paste0(
      "<p>Most parametric tests require that your data meet certain assumptions:</p><ul>",
      "<li><strong>", glink("Normal distribution", "normal-distribution"), "</strong> — ",
      "the outcome variable is roughly bell-shaped within each group (or your sample is large, n > 30).</li>",
      "<li><strong>", glink("Homogeneity of variance", "homogeneity-of-variance"), "</strong> — ",
      "the spread of scores is similar across groups.</li>",
      "<li><strong>No severe outliers</strong> — extreme values can distort parametric tests.</li></ul>",
      "<p>How to check: Use a histogram, Q-Q plot, or Shapiro-Wilk test for normality; Levene's test for equal variances.</p>",
      "<p>If you are <strong>unsure</strong>, this guide will show you both the parametric and non-parametric options. ",
      "See: ", glink("Parametric Test", "parametric-test"), " and ",
      glink("Non-Parametric Test", "non-parametric-test"), ".</p>"
    ),
    choices = list(
      list(value = "met",     label = "Yes — assumptions are satisfied",
           desc  = "Data are approximately normal, variances are similar, no major outliers"),
      list(value = "not_met", label = "No — assumptions are violated",
           desc  = "Data are skewed, non-normal, ordinal, or contain influential outliers"),
      list(value = "unsure",  label = "Unsure — I have not checked or am not certain",
           desc  = "Show me both the parametric and non-parametric options side by side")
    )
  )
)

get_active_steps <- function(answers) {
  steps <- c("data_type", "goal")
  if (!is.null(answers[["goal"]])) {
    g <- answers[["goal"]]
    if (g == "compare")  steps <- c(steps, "num_groups", "independence", "parametric")
    if (g == "relate")   steps <- c(steps, "parametric")
    if (g == "predict")  steps <- c(steps, "num_predictors", "parametric")
  }
  steps
}

get_recommendations <- function(answers) {
  dt   <- answers[["data_type"]]
  goal <- answers[["goal"]]
  par  <- answers[["parametric"]]

  # ── Compare groups ──────────────────────────────────────────────────────────
  if (goal == "compare") {
    ng  <- answers[["num_groups"]]
    ind <- answers[["independence"]]

    if (dt == "numerical") {
      if (ng == "two" && ind == "independent") {
        if (par == "met")     return(list(best = "t_test_indep",  alternatives = "mann_whitney", note = NULL))
        if (par == "not_met") return(list(best = "mann_whitney",  alternatives = "t_test_indep", note = NULL))
        if (par == "unsure")  return(list(best = c("t_test_indep","mann_whitney"), alternatives = character(0),
                                          note = "Both tests are shown because you are unsure about assumptions. Run Shapiro-Wilk and Levene’s tests to decide."))
      }
      if (ng == "two" && ind == "paired") {
        if (par == "met")     return(list(best = "t_test_paired", alternatives = "wilcoxon", note = NULL))
        if (par == "not_met") return(list(best = "wilcoxon",      alternatives = "t_test_paired", note = NULL))
        if (par == "unsure")  return(list(best = c("t_test_paired","wilcoxon"), alternatives = character(0),
                                          note = "Both tests are shown. Check normality of the paired differences."))
      }
      if (ng == "more_than_two" && ind == "independent") {
        if (par == "met")     return(list(best = "anova_oneway",   alternatives = "kruskal_wallis", note = NULL))
        if (par == "not_met") return(list(best = "kruskal_wallis", alternatives = "anova_oneway",   note = NULL))
        if (par == "unsure")  return(list(best = c("anova_oneway","kruskal_wallis"), alternatives = character(0),
                                          note = "Both tests are shown. Check normality within each group and homogeneity of variance."))
      }
      if (ng == "more_than_two" && ind == "paired") {
        if (par == "met")     return(list(best = "anova_rm",  alternatives = "friedman", note = NULL))
        if (par == "not_met") return(list(best = "friedman",  alternatives = "anova_rm", note = NULL))
        if (par == "unsure")  return(list(best = c("anova_rm","friedman"), alternatives = character(0),
                                          note = "Both tests are shown. Also check the sphericity assumption if using Repeated Measures ANOVA."))
      }
    }

    if (dt == "categorical") {
      if (ng == "two" && ind == "independent") {
        if (par == "met")     return(list(best = "chi_square",   alternatives = c("fisher_exact","z_test_prop","binomial"),
                                          note = "Chi-square is appropriate when expected cell counts are ≥ 5. For smaller samples, prefer Fisher’s Exact Test. The z-test and binomial test are useful when testing or comparing specific proportions."))
        if (par == "not_met") return(list(best = "fisher_exact", alternatives = c("chi_square","z_test_prop","binomial"),
                                          note = "Fisher’s Exact Test is preferred for small samples (expected counts < 5). The binomial test is appropriate when comparing one group to a fixed hypothesized proportion."))
        if (par == "unsure")  return(list(best = c("chi_square","fisher_exact"), alternatives = c("z_test_prop","binomial"),
                                          note = "Check expected cell counts in R with result$expected. Use Fisher’s Exact Test if any expected count is < 5."))
      }
      if (ng == "two" && ind == "paired") {
        return(list(best = "mcnemar", alternatives = character(0),
                    note = "McNemar’s Test is the correct test for paired binary categorical data (e.g., same subjects measured before and after)."))
      }
      if (ng == "more_than_two" && ind == "independent") {
        return(list(best = "chi_square", alternatives = "fisher_exact",
                    note = "For three or more independent categorical groups, chi-square tests for an overall association. Follow up with post-hoc pairwise chi-square tests if significant."))
      }
      if (ng == "more_than_two" && ind == "paired") {
        return(list(best = "chi_square", alternatives = character(0),
                    note = "For three or more paired categorical conditions, Cochran’s Q test is the ideal choice, but it is beyond this guide’s scope. Chi-square provides a reasonable approximation for larger samples."))
      }
    }

    if (dt == "ordinal") {
      if (ng == "two" && ind == "independent") {
        return(list(best = "mann_whitney", alternatives = character(0),
                    note = "Ordinal data should use non-parametric tests. Mann-Whitney U compares rank distributions between two independent groups."))
      }
      if (ng == "two" && ind == "paired") {
        return(list(best = "wilcoxon", alternatives = character(0),
                    note = "The Wilcoxon signed-rank test is the appropriate paired non-parametric test for ordinal outcome data."))
      }
      if (ng == "more_than_two" && ind == "independent") {
        return(list(best = "kruskal_wallis", alternatives = character(0),
                    note = "Kruskal-Wallis is the non-parametric equivalent of one-way ANOVA, appropriate for ordinal data across three or more independent groups."))
      }
      if (ng == "more_than_two" && ind == "paired") {
        return(list(best = "friedman", alternatives = character(0),
                    note = "The Friedman test is the non-parametric equivalent of repeated measures ANOVA, appropriate for ordinal data measured across three or more conditions."))
      }
    }
  }

  # ── Examine relationships ────────────────────────────────────────────────────
  if (goal == "relate") {
    if (dt == "numerical") {
      if (par == "met")     return(list(best = "pearson",  alternatives = "spearman", note = NULL))
      if (par == "not_met") return(list(best = "spearman", alternatives = "pearson",  note = NULL))
      if (par == "unsure")  return(list(best = c("pearson","spearman"), alternatives = character(0),
                                        note = "Both are shown. Pearson r requires normality and linearity; Spearman rho is robust to non-normality and monotonic (not just linear) relationships."))
    }
    if (dt == "categorical") {
      if (par == "met")     return(list(best = "chi_square",   alternatives = "fisher_exact",
                                        note = "Chi-square tests whether two categorical variables are associated. Use Fisher’s Exact Test if expected cell counts are small."))
      if (par == "not_met") return(list(best = "fisher_exact", alternatives = "chi_square",
                                        note = "Fisher’s Exact Test is preferred for small samples. Check expected counts with result$expected."))
      if (par == "unsure")  return(list(best = c("chi_square","fisher_exact"), alternatives = character(0),
                                        note = "Run chisq.test() first and check result$expected. If any expected count < 5, use Fisher’s Exact Test."))
    }
    if (dt == "ordinal") {
      return(list(best = "spearman", alternatives = character(0),
                  note = "Spearman rank correlation is the standard choice for ordinal data or non-normal continuous data. It measures the monotonic relationship between two variables."))
    }
  }

  # ── Predict an outcome ───────────────────────────────────────────────────────
  if (goal == "predict") {
    np <- answers[["num_predictors"]]

    if (dt == "numerical") {
      if (np == "one") {
        if (par == "met")     return(list(best = "simple_regression", alternatives = "spearman", note = NULL))
        if (par == "not_met") return(list(best = "spearman", alternatives = "simple_regression",
                                          note = "When parametric assumptions are violated, Spearman correlation can assess the strength of a monotonic relationship. For formal non-parametric regression, consider quantile regression (quantreg package)."))
        if (par == "unsure")  return(list(best = c("simple_regression","spearman"), alternatives = character(0),
                                          note = "Check diagnostic plots after fitting the regression model. Spearman correlation is shown as a non-parametric complement."))
      }
      if (np == "multiple") {
        if (par == "met")     return(list(best = "multiple_regression", alternatives = character(0), note = NULL))
        if (par == "not_met") return(list(best = "multiple_regression", alternatives = character(0),
                                          note = "Multiple regression is recommended, but check residual diagnostic plots carefully. Consider transforming skewed variables or using robust regression methods."))
        if (par == "unsure")  return(list(best = "multiple_regression", alternatives = character(0),
                                          note = "Fit the model and review diagnostic plots (Residuals vs Fitted, Normal Q-Q). Residuals — not raw data — need to be approximately normal."))
      }
    }

    if (dt == "ordinal") {
      if (np == "one") {
        return(list(best = "spearman", alternatives = "simple_regression",
                    note = "For ordinal outcomes or predictors, Spearman correlation provides a non-parametric measure of the predictive relationship. If the ordinal variable has many levels (5+), simple linear regression may also be reasonable."))
      }
      if (np == "multiple") {
        return(list(best = "multiple_regression", alternatives = character(0),
                    note = "With ordinal data and multiple predictors, multiple regression is commonly used (especially for Likert scales with 5+ levels). Check residual normality in diagnostic plots."))
      }
    }

    if (dt == "categorical") {
      return(list(best = "chi_square", alternatives = "fisher_exact",
                  note = "Predicting a categorical outcome from predictors typically requires logistic regression, which is beyond this guide’s scope. To test whether your predictor is associated with your categorical outcome, the chi-square or Fisher’s Exact Test can be used as a starting point."))
    }
  }

  list(best = character(0), alternatives = character(0),
       note = "No recommendation could be generated for this combination.")
}

answer_label <- function(step_id, value) {
  cfg <- STEP_CONFIGS[[step_id]]
  for (ch in cfg$choices) {
    if (ch$value == value) return(ch$label)
  }
  value
}

step_label <- function(step_id) {
  c(
    data_type      = "Data Type",
    goal           = "Goal",
    num_groups     = "Number of Groups",
    independence   = "Group Structure",
    num_predictors = "Number of Predictors",
    parametric     = "Parametric Assumptions"
  )[[step_id]]
}

# ── Theme ──────────────────────────────────────────────────────────────────────
uf_theme <- bs_theme(
  version   = 5,
  primary   = "#FA4616",
  secondary = "#003087",
  bg        = "#f5f6fa",
  fg        = "#212529",
  base_font = font_google("Inter"),
  code_font = font_google("Fira Code")
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  id    = "main_nav",
  title = tags$span(
    style = "font-weight:800; letter-spacing:-0.01em;",
    tags$span(style = "color:#FA4616;", "Stat"),
    tags$span(style = "color:white;",   "Guide")
  ),
  theme          = uf_theme,
  navbar_options = navbar_options(bg = "#003087"),
  fillable       = FALSE,
  header = tagList(
    useShinyjs(),
    tags$head(
      tags$style(HTML(app_css)),
      tags$script(HTML(app_js))
    )
  ),

  # ── Tab 1: Find My Test ──────────────────────────────────────────────────────
  nav_panel(
    title = "Find My Test",
    value = "wizard",
    div(class = "wizard-container", uiOutput("wizard_ui"))
  ),

  # ── Tab 2: Results ───────────────────────────────────────────────────────────
  nav_panel(
    title = "Results",
    value = "results",
    div(class = "results-container", uiOutput("results_ui"))
  ),

  # ── Tab 3: Glossary ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Glossary",
    value = "glossary",
    div(
      style = "max-width:800px; margin:2rem auto; padding:0 1rem;",
      h2("Statistical Glossary", class = "text-uf-blue mb-1"),
      p("Plain-English definitions of common statistical terms, A to Z.", class = "text-muted mb-3"),
      div(
        class = "glossary-search-box",
        textInput("glossary_search", label = NULL, placeholder = "Search terms...", width = "100%")
      ),
      uiOutput("glossary_ui")
    )
  ),

  # ── Tab 4: About ─────────────────────────────────────────────────────────────
  nav_panel(
    title = "About",
    value = "about",
    div(
      class = "about-container",
      h2("About StatGuide", class = "text-uf-blue mb-1"),
      p("A step-by-step decision tool for choosing the right statistical test.", class = "text-muted mb-4"),

      h4("How to Use StatGuide", class = "fw-bold mb-3"),
      div(class = "about-step",
          div(class = "about-step-num", "1"),
          div(tags$strong("Open the Find My Test tab"), tags$br(),
              "Answer five short questions about your data, one at a time. Use the \"What does this mean?\" sections if any term is unfamiliar.")),
      div(class = "about-step",
          div(class = "about-step-num", "2"),
          div(tags$strong("Review your recommendations"), tags$br(),
              "The Results tab shows ranked test cards — the Best Fit test plus alternatives. Each card explains when to use the test, its assumptions, and includes copy-ready R code.")),
      div(class = "about-step",
          div(class = "about-step-num", "3"),
          div(tags$strong("Look up unfamiliar terms"), tags$br(),
              "Use the Glossary tab for plain-English definitions of key statistical terms.")),
      div(class = "about-step",
          div(class = "about-step-num", "4"),
          div(tags$strong("Start Over any time"), tags$br(),
              "Use the Start Over button to reset the wizard and try a different path.")),

      div(class = "divider"),
      h4("Who is this for?", class = "fw-bold mb-2"),
      p("StatGuide is designed for undergraduate and graduate students in the biological, social, and health sciences who need guidance selecting an appropriate statistical test."),

      div(class = "divider"),
      h4("Important Notes", class = "fw-bold mb-2"),
      tags$ul(
        tags$li("This guide covers the most commonly used classical tests. Specialized designs (e.g., mixed models, survival analysis, logistic regression) are beyond its scope."),
        tags$li("Statistical significance (p < 0.05) is not the only criterion for a good analysis — always report effect sizes and confidence intervals."),
        tags$li("When in doubt about your data, consult a statistician.")
      ),

      div(class = "divider"),
      h4("References", class = "fw-bold mb-2"),
      tags$ul(
        style = "font-size:0.9rem; line-height:1.8;",
        tags$li("Palomares Carrascosa, I. (2024). Choosing the Right Statistical Test: A Decision Tree Approach. ", tags$em("Statology.")),
        tags$li("McCrum-Gardner, E. (2008). Which is the correct statistical test to use? ", tags$em("British Journal of Oral and Maxillofacial Surgery, 46"), ", 38–41."),
        tags$li("Marusteri, M. & Bacarea, V. (2010). Comparing groups for statistical differences. ", tags$em("Biochemia Medica, 20"), "(1), 15–32.")
      ),

      div(class = "divider"),
      p(class = "text-muted", style = "font-size:0.8rem;",
        "StatGuide was built with R Shiny for the University of Florida Institute of Food and Agricultural Sciences (UF/IFAS).")
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  rv <- reactiveValues(
    answers    = list(),
    step_index = 1,
    pending    = NULL
  )

  active_steps <- reactive({ get_active_steps(rv$answers) })

  current_step_id <- reactive({
    steps <- active_steps()
    if (rv$step_index <= length(steps)) steps[[rv$step_index]] else NULL
  })

  total_steps <- reactive({ length(active_steps()) })

  display_total <- reactive({
    if (is.null(rv$answers[["goal"]])) 5L else total_steps()
  })

  is_last_step <- reactive({ rv$step_index == total_steps() })

  observeEvent(input$wizard_selection, { rv$pending <- input$wizard_selection })

  output$wizard_ui <- renderUI({
    step_id <- current_step_id()

    if (is.null(step_id)) {
      return(div(
        class = "no-results-box",
        h4("Wizard complete"),
        p("Switch to the Results tab to see your recommendations."),
        actionButton("reset_from_empty", "Start Over", class = "btn btn-outline-secondary mt-2")
      ))
    }

    config    <- STEP_CONFIGS[[step_id]]
    existing  <- rv$answers[[step_id]]
    step_idx  <- rv$step_index
    total     <- display_total()
    pct       <- round((step_idx - 1) / total * 100)
    last_step <- is_last_step()

    choice_btns <- lapply(config$choices, function(ch) {
      is_active <- !is.null(existing) && existing == ch$value
      tags$button(
        class   = paste("wizard-choice", if (is_active) "active" else ""),
        onclick = sprintf("selectWizardChoice('%s','%s',this)", step_id, ch$value),
        type    = "button",
        tags$span(class = "choice-label", ch$label),
        tags$span(class = "choice-desc",  ch$desc)
      )
    })

    back_btn <- if (step_idx > 1) {
      actionButton("back_btn", label = tagList(icon("arrow-left"), " Back"),
                   class = "btn btn-outline-secondary")
    } else {
      tags$span()
    }

    next_label <- if (last_step) tagList("See Results ", icon("arrow-right")) else
                                 tagList("Next ",        icon("arrow-right"))

    tagList(div(
      class = "wizard-card",
      div(class = "wizard-step-label", sprintf("Step %d of %d", step_idx, total)),
      div(class = "progress",
          div(class = "progress-bar", role = "progressbar",
              style = sprintf("width:%d%%", pct),
              `aria-valuenow` = pct, `aria-valuemin` = "0", `aria-valuemax` = "100")),
      div(class = "wizard-question", config$question),
      tagList(choice_btns),
      div(id = "wizard-error", class = "wizard-error",
          icon("exclamation-circle"), " Please select an option before continuing."),
      div(class = "mt-3",
          tags$button(type = "button", class = "help-toggle-btn",
                      `data-bs-toggle` = "collapse", `data-bs-target` = "#help-panel",
                      `aria-expanded` = "false",
                      icon("info-circle"), " What does this mean?"),
          div(id = "help-panel", class = "collapse",
              div(class = "help-content mt-2", HTML(config$help_text)))),
      div(class = "wizard-nav",
          back_btn,
          actionButton("start_over", label = tagList(icon("undo"), " Start Over"),
                       class = "btn btn-link text-muted"),
          actionButton("next_btn", label = next_label, class = "btn btn-primary btn-next"))
    ))
  })

  observeEvent(input$next_btn, {
    step_id <- current_step_id()
    if (is.null(step_id)) return()

    pending      <- rv$pending
    stored       <- rv$answers[[step_id]]
    chosen_value <- NULL

    if (!is.null(pending)) {
      parts <- strsplit(pending, "\\|")[[1]]
      if (length(parts) == 2 && parts[1] == step_id) chosen_value <- parts[2]
    }
    if (is.null(chosen_value) && !is.null(stored)) chosen_value <- stored

    if (is.null(chosen_value)) {
      runjs("var err=document.getElementById('wizard-error'); if(err) err.classList.add('visible');")
      return()
    }

    rv$answers[[step_id]] <- chosen_value
    rv$pending <- NULL

    if (is_last_step()) {
      updateNavbarPage(session, "main_nav", selected = "results")
    } else {
      rv$step_index <- rv$step_index + 1
    }
  })

  observeEvent(input$back_btn, {
    if (rv$step_index > 1) {
      steps <- active_steps()
      for (i in rv$step_index:length(steps)) rv$answers[[steps[[i]]]] <- NULL
      rv$step_index <- rv$step_index - 1
      rv$pending    <- NULL
    }
  })

  reset_wizard <- function() {
    rv$answers    <- list()
    rv$step_index <- 1
    rv$pending    <- NULL
    updateNavbarPage(session, "main_nav", selected = "wizard")
  }

  observeEvent(input$start_over,         { reset_wizard() })
  observeEvent(input$start_over_results, { reset_wizard() })
  observeEvent(input$reset_from_empty,   { reset_wizard() })

  output$results_ui <- renderUI({
    steps        <- active_steps()
    all_answered <- all(vapply(steps, function(s) !is.null(rv$answers[[s]]), logical(1)))

    if (!all_answered || length(steps) < 3) {
      return(div(
        class = "no-results-box",
        h4("No results yet"),
        p("Complete the wizard in the \"Find My Test\" tab to see your test recommendations."),
        actionButton("go_to_wizard", "Go to Find My Test", class = "btn btn-primary mt-2",
                     onclick = "Shiny.setInputValue('switch_to_wizard', 1, {priority:'event'})")
      ))
    }

    recs <- get_recommendations(rv$answers)

    answer_chips <- lapply(steps, function(s) {
      tags$span(class = "answer-chip",
                tags$span(class = "chip-label", step_label(s), ": "),
                answer_label(s, rv$answers[[s]]))
    })

    summary_section <- div(
      class = "answer-summary mb-3",
      div(style = "display:flex; align-items:center; justify-content:space-between; flex-wrap:wrap; gap:0.5rem;",
          tags$button(type = "button", class = "help-toggle-btn fw-bold",
                      `data-bs-toggle` = "collapse", `data-bs-target` = "#summary-collapse",
                      `aria-expanded` = "true",
                      icon("chevron-down"), " Your Answers")),
      div(id = "summary-collapse", class = "collapse show",
          div(class = "mt-2", tagList(answer_chips)))
    )

    note_box <- if (!is.null(recs$note) && nchar(recs$note) > 0) {
      div(class = "recommendation-note", icon("lightbulb"), " ", HTML(recs$note))
    } else NULL

    best_ids <- recs$best
    alt_ids  <- recs$alternatives

    if (length(best_ids) == 0 && length(alt_ids) == 0) {
      return(tagList(
        summary_section, note_box,
        div(class = "no-results-box",
            h4("No specific test found"),
            p(if (!is.null(recs$note)) recs$note else
              "The combination you selected does not map to a test in this guide."),
            br(),
            actionButton("start_over_results", "Start Over", class = "btn btn-primary"))
      ))
    }

    make_test_card <- function(test_id, rank_label) {
      test <- TESTS[[test_id]]
      if (is.null(test)) return(NULL)
      is_best   <- rank_label == "Best Fit"
      card_cls  <- paste("test-card", if (is_best) "best-fit" else "")
      badge_cls <- paste("badge-fit", if (is_best) "badge-best" else "badge-alt")
      make_list <- function(items) tags$ul(lapply(items, tags$li))
      div(class = card_cls,
          div(class = "test-card-header",
              h5(class = "test-card-name", test$name),
              span(class = badge_cls, rank_label)),
          div(class = "test-card-body",
              span(class = "test-section-label", "What it does"), p(test$description),
              span(class = "test-section-label", "When to use"),     make_list(test$when_to_use),
              span(class = "test-section-label", "When NOT to use"), make_list(test$when_not_to_use),
              span(class = "test-section-label", "Assumptions"),     make_list(test$assumptions),
              span(class = "test-section-label mt-3", "Example R Code"),
              tags$pre(tags$code(test$r_code))))
    }

    best_cards <- lapply(best_ids, make_test_card, rank_label = "Best Fit")
    alt_cards  <- lapply(alt_ids,  make_test_card, rank_label = "Alternative")

    tagList(
      h2("Test Recommendations", class = "text-uf-blue mb-1 mt-2"),
      p("Based on your answers, here are the most appropriate statistical tests.", class = "text-muted mb-3"),
      summary_section, note_box,
      tagList(best_cards),
      if (length(alt_cards) > 0) tagList(
        h5("Alternative Options", class = "text-muted mt-4 mb-2"),
        tagList(alt_cards)
      ),
      div(class = "text-center mt-4 mb-4",
          actionButton("start_over_results", label = tagList(icon("undo"), " Start Over"),
                       class = "btn btn-outline-secondary"))
    )
  })

  observeEvent(input$switch_to_wizard, {
    updateNavbarPage(session, "main_nav", selected = "wizard")
  })

  output$glossary_ui <- renderUI({
    query <- tolower(trimws(input$glossary_search %||% ""))
    df    <- glossary_terms
    if (nchar(query) > 0) {
      df <- df[grepl(query, tolower(df$term)) | grepl(query, tolower(df$definition)), ]
    }
    if (nrow(df) == 0) {
      return(div(class = "text-center text-muted py-4",
                 p("No terms found matching \"", input$glossary_search, "\".")))
    }
    df <- df[order(df$term), ]
    letters_present <- unique(toupper(substr(df$term, 1, 1)))
    letter_sections <- lapply(letters_present, function(ltr) {
      sub_df    <- df[toupper(substr(df$term, 1, 1)) == ltr, ]
      term_tags <- do.call(tagList, lapply(seq_len(nrow(sub_df)), function(i) {
        div(id    = paste0("glossary-term-", sub_df$id[i]),
            class = "glossary-term-card",
            h6(class = "glossary-term-heading", sub_df$term[i]),
            p(class  = "glossary-term-def",     sub_df$definition[i]))
      }))
      tagList(div(class = "glossary-section-letter", ltr), term_tags)
    })
    div(tagList(letter_sections))
  })

  observeEvent(input$glossary_navigate, {
    updateTextInput(session, "glossary_search", value = "")
    updateNavbarPage(session, "main_nav", selected = "glossary")
    session$sendCustomMessage("scrollToGlossaryTerm", input$glossary_navigate)
  })
}

# ── Run ────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)

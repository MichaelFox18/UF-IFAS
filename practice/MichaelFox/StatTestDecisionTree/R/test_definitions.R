TESTS <- list(

  t_test_indep = list(
    name = "Independent Samples t-Test",
    description = "Compares the means of two separate, unrelated groups to see if they differ more than you would expect by chance. It is one of the most widely used statistical tests in research.",
    when_to_use = c(
      "You have two independent groups (different people or units in each group)",
      "Your outcome variable is numerical (continuous or interval/ratio scale)",
      "You want to test whether the group means differ significantly",
      "Each observation belongs to exactly one group"
    ),
    when_not_to_use = c(
      "The same subjects appear in both groups (use Paired t-Test instead)",
      "You have more than two groups (use One-Way ANOVA instead)",
      "Your data are severely non-normal and your sample is small (use Mann-Whitney U instead)",
      "Your outcome variable is categorical or ordinal"
    ),
    assumptions = c(
      "Both groups are independently sampled from their populations",
      "The outcome variable is approximately normally distributed within each group (or n > 30 in each group by central limit theorem)",
      "Homogeneity of variance — the two groups have similar variances (use Welch's t-test, the default in R, if unsure)",
      "No major outliers"
    ),
    r_code = '# Independent samples t-test (Welch\'s, unequal variances by default)
group1 <- c(23, 25, 28, 30, 22, 27, 24, 29)
group2 <- c(18, 20, 24, 19, 22, 17, 21, 20)

result <- t.test(group1, group2, var.equal = FALSE)
print(result)

# Or with a data frame in long format:
# df <- data.frame(score = c(group1, group2),
#                  group = rep(c("A","B"), each = 8))
# t.test(score ~ group, data = df)'
  ),

  mann_whitney = list(
    name = "Mann-Whitney U Test",
    description = "A non-parametric alternative to the independent t-test. Instead of comparing means, it compares the rank distributions of two groups. Also known as the Wilcoxon rank-sum test.",
    when_to_use = c(
      "You have two independent groups",
      "Your outcome variable is numerical or ordinal",
      "Normality assumptions are violated or sample size is small",
      "Your data contain outliers that would unduly influence a t-test"
    ),
    when_not_to_use = c(
      "The same subjects appear in both groups (use Wilcoxon signed-rank test instead)",
      "You have more than two groups (use Kruskal-Wallis instead)",
      "Your data are clearly normally distributed and samples are large (a t-test would be more powerful)"
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
    description = "Compares means from the same group measured twice (e.g., before and after a treatment), or from two groups matched on key characteristics. It accounts for the natural correlation between paired observations.",
    when_to_use = c(
      "The same subjects are measured under two conditions (pre/post, treatment/control)",
      "Observations are explicitly matched in pairs",
      "Your outcome variable is numerical",
      "You want to test whether the mean difference between pairs is different from zero"
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

# Mean difference and 95% CI
cat("Mean difference:", round(mean(after - before), 2), "\n")
cat("95% CI:", round(result$conf.int, 2), "\n")'
  ),

  wilcoxon = list(
    name = "Wilcoxon Signed-Rank Test",
    description = "A non-parametric alternative to the paired t-test. It ranks the absolute differences between pairs and tests whether the signed ranks are symmetric around zero, without assuming normally distributed differences.",
    when_to_use = c(
      "You have paired or repeated measurements from the same subjects",
      "Normality of the paired differences is violated or sample is small",
      "Your outcome is numerical or ordinal",
      "Data contain outliers in the paired differences"
    ),
    when_not_to_use = c(
      "The two groups are independent (use Mann-Whitney U instead)",
      "You have more than two time points (use Friedman test instead)",
      "The differences between pairs are clearly normally distributed (a paired t-test would be more powerful)"
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

# Median difference
cat("Median difference:", median(after - before), "\n")'
  ),

  anova_oneway = list(
    name = "One-Way ANOVA",
    description = "Tests whether the means of three or more independent groups differ significantly. ANOVA stands for Analysis of Variance — it compares the variability between groups to the variability within groups.",
    when_to_use = c(
      "You have three or more independent groups",
      "Your outcome variable is numerical",
      "You want to test whether at least one group mean differs from the others",
      "Parametric assumptions (normality, equal variance) are reasonably met"
    ),
    when_not_to_use = c(
      "You have only two groups (use t-test instead — though ANOVA gives the same result)",
      "Groups are related or repeated measurements (use Repeated Measures ANOVA)",
      "Normality or equal variance assumptions are clearly violated (use Kruskal-Wallis instead)",
      "You want to know which specific groups differ — ANOVA alone does not tell you (use post-hoc tests)"
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
    description = "A non-parametric alternative to one-way ANOVA. It tests whether three or more independent groups come from the same distribution by comparing the rank distributions across groups.",
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

# Post-hoc: Dunn test for pairwise comparisons
# install.packages("dunn.test")
library(dunn.test)
dunn.test(data$score, data$group, method = "bonferroni")'
  ),

  anova_rm = list(
    name = "Repeated Measures ANOVA",
    description = "Tests whether means differ across three or more time points or conditions when the same subjects are measured repeatedly. It accounts for the correlation between repeated measurements on the same person.",
    when_to_use = c(
      "The same subjects are measured at three or more time points or conditions",
      "Your outcome variable is numerical",
      "Parametric assumptions are reasonably met",
      "You want to test for an overall change across time/conditions"
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
      "Sphericity — the variances of differences between all pairs of conditions are equal (Mauchly's test checks this; apply corrections if violated)"
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

# For more complete output including sphericity tests:
# install.packages("ez")
# library(ez)
# ezANOVA(data, dv=score, wid=subject, within=time)'
  ),

  friedman = list(
    name = "Friedman Test",
    description = "A non-parametric alternative to repeated measures ANOVA. It ranks the observations within each subject across conditions and tests whether the rank distributions differ across conditions.",
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
plot(x, y, pch = 16, col = "#003087",
     main = "Pearson Correlation")
abline(lm(y ~ x), col = "#FA4616", lwd = 2)'
  ),

  spearman = list(
    name = "Spearman Rank Correlation",
    description = "A non-parametric measure of the monotonic relationship between two variables. It converts values to ranks before calculating the correlation, making it robust to outliers and non-normal distributions.",
    when_to_use = c(
      "You want to measure the relationship between two variables",
      "One or both variables are ordinal, or the data are not normally distributed",
      "The relationship may be monotonic but not strictly linear",
      "Data contain outliers that would distort Pearson's r"
    ),
    when_not_to_use = c(
      "Both variables are continuous, normal, and the relationship is clearly linear (Pearson r is more powerful)",
      "You want to predict one variable from another (consider rank-based regression alternatives)"
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

# For ordinal data (e.g., Likert ratings vs. performance score)
# cor.test(survey_rating, performance, method = "spearman")'
  ),

  simple_regression = list(
    name = "Simple Linear Regression",
    description = "Models the relationship between one predictor variable (X) and one numerical outcome variable (Y) by fitting the best straight line through the data. It can be used for prediction and for quantifying how much Y changes per unit increase in X.",
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
    description = "Extends simple linear regression to model the relationship between two or more predictor variables and a numerical outcome. It estimates the unique contribution of each predictor while holding the others constant.",
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
      "A linear relationship exists between each predictor and the outcome (after accounting for others)",
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
    description = "Tests whether there is an association between two categorical variables. It compares the observed frequencies in each cell of a contingency table to the frequencies you would expect if the variables were independent.",
    when_to_use = c(
      "Both variables are categorical",
      "You want to test whether two categorical variables are related or associated",
      "You have a reasonably large sample (expected cell counts ≥ 5 in most cells)",
      "Observations are independent"
    ),
    when_not_to_use = c(
      "Expected cell counts are small (< 5 in 20% or more of cells) — use Fisher's Exact Test instead",
      "Observations are paired (use McNemar's Test instead)",
      "You have one variable and want to compare to a known distribution (use a goodness-of-fit chi-square)"
    ),
    assumptions = c(
      "Observations are independent (no person counted twice)",
      "Expected frequency in each cell is at least 5 (check with result$expected in R)",
      "The data are counts (frequencies), not percentages or proportions"
    ),
    r_code = '# Chi-square test of independence
# Create contingency table from raw data
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
      "Both variables are categorical (usually 2×2 tables, though it can extend to larger tables)",
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
      "The marginal totals of the table are fixed (conservative assumption that makes the test exact)",
      "Data are counts (frequencies)"
    ),
    r_code = '# Fisher\'s exact test (best for small samples)
# 2x2 contingency table: rows = group, cols = outcome
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
      "Example: measuring agreement/disagreement before and after an intervention"
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
    r_code = '# McNemar\'s test — same subjects measured before and after
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

# Proportion changed
n <- sum(table_data)
cat("Proportion changed:", round((table_data[1,2] + table_data[2,1]) / n, 3), "\n")'
  ),

  binomial = list(
    name = "Binomial Test",
    description = "Tests whether the observed proportion of successes in a binary outcome matches a hypothesized probability. It is an exact test, making it reliable for any sample size.",
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
    description = "Tests whether a proportion (or the difference between two proportions) equals a hypothesized value. Based on the normal approximation to the binomial, it is suitable for large samples.",
    when_to_use = c(
      "You want to test whether a single proportion equals a hypothesized value (one-sample)",
      "You want to compare proportions between two independent groups (two-sample)",
      "Your sample is large enough for the normal approximation (n×p ≥ 5 and n×(1−p) ≥ 5)"
    ),
    when_not_to_use = c(
      "Sample sizes are small (use a Binomial Test for one-sample, or Fisher's Exact Test for two samples)",
      "Comparing proportions across three or more groups (use chi-square)",
      "Data are paired (use McNemar's Test)"
    ),
    assumptions = c(
      "Observations are independent",
      "Large enough sample so normal approximation holds (n×p ≥ 5 and n×(1−p) ≥ 5 for each group)",
      "Random sampling"
    ),
    r_code = '# Z-test for proportions — one-sample
prop.test(65, 100, p = 0.5)

# Z-test for proportions — two-sample (comparing two groups)
successes <- c(65, 48)    # successes in each group
totals    <- c(100, 100)  # total in each group

result <- prop.test(successes, totals)
print(result)

cat("Group proportions:", round(result$estimate, 3), "\n")
cat("95% CI for difference:", round(result$conf.int, 3), "\n")'
  ),

  logistic_simple = list(
    name = "Simple Logistic Regression",
    description = "Models the probability of a binary categorical outcome (two possible values, e.g., yes/no, pass/fail, disease/no disease) using a single predictor variable. It outputs odds ratios showing how much each unit increase in the predictor changes the odds of the outcome.",
    when_to_use = c(
      "Your outcome variable has exactly two categories (binary)",
      "You have one predictor variable (continuous, ordinal, or categorical)",
      "You want to predict the probability of an outcome or estimate the effect of a predictor",
      "Examples: predicting whether a student passes (yes/no) based on study hours; whether a patient has a disease (yes/no) based on a biomarker"
    ),
    when_not_to_use = c(
      "Your outcome has more than two categories — use Multinomial Logistic Regression instead",
      "Your outcome is ordered with 3+ levels — consider Ordinal Logistic Regression",
      "Your outcome is numerical/continuous — use Simple Linear Regression instead",
      "You have multiple predictor variables — use Multiple Logistic Regression"
    ),
    assumptions = c(
      "The outcome variable is binary (two mutually exclusive categories)",
      "Observations are independent",
      "Little or no multicollinearity among predictors (less relevant with one predictor)",
      "A sufficiently large sample — rule of thumb: at least 10 events per predictor variable",
      "No extreme outliers in continuous predictors"
    ),
    r_code = '# Simple (binary) logistic regression
# Outcome: 1 = event occurred, 0 = did not occur
data <- data.frame(
  outcome  = c(1, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 1, 0),
  predictor = c(2.1,1.3,3.2,2.8,1.1,1.5,3.0,1.8,2.5,2.9,1.2,3.1,1.6,2.7,1.4)
)

model <- glm(outcome ~ predictor, data = data, family = binomial)
summary(model)

# Odds ratios and 95% confidence intervals
exp(coef(model))
exp(confint(model))

# Predicted probabilities for new data
new_data <- data.frame(predictor = c(1.5, 2.0, 2.5, 3.0))
predict(model, new_data, type = "response")

# Note: for outcomes with 3+ categories, use multinomial logistic regression:
# install.packages("nnet")
# library(nnet); multinom(outcome ~ predictor, data = data)'
  ),

  logistic_multiple = list(
    name = "Multiple Logistic Regression",
    description = "Extends simple logistic regression to model a binary categorical outcome using two or more predictor variables simultaneously. Each predictor's odds ratio reflects its unique contribution while holding all other predictors constant.",
    when_to_use = c(
      "Your outcome variable has exactly two categories (binary)",
      "You have two or more predictor variables",
      "You want to control for confounding variables while examining a focal predictor",
      "You want to build a multi-variable predictive model for a categorical outcome"
    ),
    when_not_to_use = c(
      "Your outcome has more than two categories — use Multinomial Logistic Regression",
      "You have only one predictor — use Simple Logistic Regression",
      "Your outcome is numerical/continuous — use Multiple Linear Regression",
      "You have more predictors than events (overfitting risk; apply regularization)"
    ),
    assumptions = c(
      "The outcome variable is binary",
      "Observations are independent",
      "Little or no multicollinearity among predictors (check VIF)",
      "At least 10–20 outcome events per predictor variable included in the model",
      "No extreme outliers or highly influential observations"
    ),
    r_code = '# Multiple logistic regression
data <- data.frame(
  outcome = c(1,0,1,1,0,0,1,0,1,1,0,1,0,1,0,1,0,1,0,1),
  age     = c(45,32,58,61,28,35,52,40,55,63,30,47,38,60,25,50,33,57,42,65),
  score   = c(72,65,80,85,60,68,78,70,82,88,62,75,67,83,58,79,64,81,71,87)
)

model <- glm(outcome ~ age + score, data = data, family = binomial)
summary(model)

# Odds ratios with 95% CIs
exp(cbind(OR = coef(model), confint(model)))

# Check for multicollinearity
# install.packages("car")
library(car)
vif(model)

# Note: for outcomes with 3+ categories, use multinomial logistic regression:
# install.packages("nnet")
# library(nnet); multinom(outcome ~ age + score, data = data)'
  )
)

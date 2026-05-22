# ── Glossary link helper ──────────────────────────────────────────────────────
# Wraps a term in a clickable span that switches to the Glossary tab
glink <- function(text, term_id) {
  sprintf('<span class="glossary-link" data-term="%s">%s</span>', term_id, text)
}

# ── Step configurations ───────────────────────────────────────────────────────
STEP_CONFIGS <- list(

  data_type = list(
    id       = "data_type",
    question = "What type of data are you working with?",
    help_text = paste0(
      "<p>The type of data you have is the first key decision.</p>",
      "<ul>",
      "<li><strong>Numerical</strong> — numbers you can do math with: averages, sums, standard deviations. ",
      "Examples: test scores, blood pressure, plant height, reaction time.</li>",
      "<li><strong>Categorical</strong> — distinct groups with no inherent order. ",
      "Examples: species, yes/no answers, treatment group, blood type. ",
      "See: ", glink("Categorical Data", "categorical-data"), ".</li>",
      "<li><strong>Ordinal</strong> — categories that have a meaningful order, but the gaps between them are unknown or unequal. ",
      "Examples: ", glink("Likert Scale", "likert-scale"), " ratings (1–5), education level, pain ratings. ",
      "See: ", glink("Ordinal Data", "ordinal-data"), ".</li>",
      "</ul>"
    ),
    choices = list(
      list(value = "numerical",   label = "Numerical",   icon = "123",
           desc  = "Continuous numbers — heights, scores, temperatures, times"),
      list(value = "categorical", label = "Categorical", icon = "tag",
           desc  = "Distinct groups or categories — yes/no, species, treatment group"),
      list(value = "ordinal",     label = "Ordinal",     icon = "list-ol",
           desc  = "Ordered categories with unknown spacing — Likert scales, rankings")
    )
  ),

  goal = list(
    id       = "goal",
    question = "What is your main research goal?",
    help_text = paste0(
      "<p>Think about what question you are trying to answer with your data.</p>",
      "<ul>",
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
           icon = "bar-chart-line",
           desc  = "Test whether groups differ on some outcome"),
      list(value = "relate",  label = "Examine relationships",
           icon = "arrow-left-right",
           desc  = "Measure the association between two variables"),
      list(value = "predict", label = "Predict an outcome",
           icon = "graph-up-arrow",
           desc  = "Use predictor variables to explain or forecast an outcome")
    )
  ),

  num_groups = list(
    id       = "num_groups",
    question = "How many groups are you comparing?",
    help_text = paste0(
      "<p>Count the number of distinct groups in your study.</p>",
      "<ul>",
      "<li><strong>Two groups</strong> — e.g., treated vs. control, male vs. female, before vs. after (two conditions).</li>",
      "<li><strong>More than two groups</strong> — e.g., three dosage levels, four schools, five time points. ",
      "Comparing more than two groups simultaneously requires tests like ANOVA or Kruskal-Wallis to avoid inflating the ",
      glink("Type I Error", "type-i-error"), " rate.</li>",
      "</ul>"
    ),
    choices = list(
      list(value = "two",           label = "Two groups",           icon = "2-square",
           desc  = "Comparing exactly two groups or conditions"),
      list(value = "more_than_two", label = "More than two groups", icon = "grid-3x3",
           desc  = "Comparing three or more groups or conditions simultaneously")
    )
  ),

  independence = list(
    id       = "independence",
    question = "Are your groups independent or paired/repeated?",
    help_text = paste0(
      "<p>This question asks whether the same subjects appear in more than one group.</p>",
      "<ul>",
      "<li><strong>Independent groups</strong> — completely different people (or units) in each group. ",
      "Example: randomly assigning half the students to tutoring and the other half to a control group.</li>",
      "<li><strong>Paired / Repeated measures</strong> — the same subjects measured more than once, or subjects explicitly ",
      "matched across groups. Examples: pre/post measurements on the same people; twins assigned to different treatments. ",
      "See: ", glink("Paired Data", "paired-data"), ".</li>",
      "</ul>",
      "<p>Using the wrong test here (independent vs. paired) can seriously inflate or deflate your results.</p>"
    ),
    choices = list(
      list(value = "independent", label = "Independent groups",
           icon = "people",
           desc  = "Different subjects in each group — no matching or repeated measures"),
      list(value = "paired",      label = "Paired / Repeated measures",
           icon = "arrow-repeat",
           desc  = "Same subjects measured multiple times, or subjects matched across groups")
    )
  ),

  num_predictors = list(
    id       = "num_predictors",
    question = "How many predictor variables do you have?",
    help_text = paste0(
      "<p>A predictor variable (also called an ", glink("independent variable", "independent-variable"),
      " or explanatory variable) is a variable you use to predict or explain another variable.</p>",
      "<ul>",
      "<li><strong>One predictor</strong> — e.g., using study hours to predict exam score. → Simple linear regression.</li>",
      "<li><strong>Multiple predictors</strong> — e.g., using study hours, sleep, and prior GPA together to predict exam score. ",
      "→ Multiple linear regression. This lets you examine each predictor's unique contribution while controlling for the others.</li>",
      "</ul>"
    ),
    choices = list(
      list(value = "one",      label = "One predictor",       icon = "1-square",
           desc  = "A single variable used to predict the outcome (simple regression)"),
      list(value = "multiple", label = "Multiple predictors", icon = "collection",
           desc  = "Two or more variables used together to predict the outcome")
    )
  ),

  parametric = list(
    id       = "parametric",
    question = "Are the parametric assumptions met for your data?",
    help_text = paste0(
      "<p>Most parametric tests require that your data meet certain assumptions:</p>",
      "<ul>",
      "<li><strong>", glink("Normal distribution", "normal-distribution"), "</strong> — ",
      "the outcome variable is roughly bell-shaped within each group (or your sample is large, n > 30).</li>",
      "<li><strong>", glink("Homogeneity of variance", "homogeneity-of-variance"), "</strong> — ",
      "the spread of scores is similar across groups.</li>",
      "<li><strong>No severe outliers</strong> — extreme values can distort parametric tests.</li>",
      "</ul>",
      "<p>How to check: Use a histogram, Q-Q plot, or Shapiro-Wilk test for normality; Levene's test for equal variances.</p>",
      "<p>If you are <strong>unsure</strong>, this guide will show you both the parametric and non-parametric options. ",
      "See: ", glink("Parametric Test", "parametric-test"), " and ", glink("Non-Parametric Test", "non-parametric-test"), ".</p>"
    ),
    choices = list(
      list(value = "met",     label = "Met — assumptions are satisfied",
           icon = "check-circle",
           desc  = "Data are approximately normal, variances are similar, no major outliers"),
      list(value = "not_met", label = "Not met — assumptions are violated",
           icon = "x-circle",
           desc  = "Data are skewed, non-normal, ordinal, or contain influential outliers"),
      list(value = "unsure",  label = "Unsure — I have not checked or I am not certain",
           icon = "question-circle",
           desc  = "Show me both the parametric and non-parametric options side by side")
    )
  )
)

# ── Active steps calculator ───────────────────────────────────────────────────
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

# ── Recommendation engine ─────────────────────────────────────────────────────
# Returns list(best = c("test_id",...), alternatives = c("test_id",...), note = NULL)
get_recommendations <- function(answers) {
  dt   <- answers[["data_type"]]
  goal <- answers[["goal"]]
  par  <- answers[["parametric"]]

  # ── Compare groups ──
  if (goal == "compare") {
    ng  <- answers[["num_groups"]]
    ind <- answers[["independence"]]

    if (dt == "numerical") {
      if (ng == "two" && ind == "independent") {
        if (par == "met")     return(list(best = "t_test_indep",  alternatives = "mann_whitney"))
        if (par == "not_met") return(list(best = "mann_whitney",  alternatives = "t_test_indep"))
        if (par == "unsure")  return(list(best = c("t_test_indep","mann_whitney"), alternatives = character(0),
                                          note = "Both tests are shown because you are unsure about assumptions. Run Shapiro-Wilk and Levene's tests to decide."))
      }
      if (ng == "two" && ind == "paired") {
        if (par == "met")     return(list(best = "t_test_paired", alternatives = "wilcoxon"))
        if (par == "not_met") return(list(best = "wilcoxon",      alternatives = "t_test_paired"))
        if (par == "unsure")  return(list(best = c("t_test_paired","wilcoxon"), alternatives = character(0),
                                          note = "Both tests are shown. Check normality of the paired differences."))
      }
      if (ng == "more_than_two" && ind == "independent") {
        if (par == "met")     return(list(best = "anova_oneway",  alternatives = "kruskal_wallis"))
        if (par == "not_met") return(list(best = "kruskal_wallis",alternatives = "anova_oneway"))
        if (par == "unsure")  return(list(best = c("anova_oneway","kruskal_wallis"), alternatives = character(0),
                                          note = "Both tests are shown. Check normality within each group and homogeneity of variance."))
      }
      if (ng == "more_than_two" && ind == "paired") {
        if (par == "met")     return(list(best = "anova_rm",  alternatives = "friedman"))
        if (par == "not_met") return(list(best = "friedman",  alternatives = "anova_rm"))
        if (par == "unsure")  return(list(best = c("anova_rm","friedman"), alternatives = character(0),
                                          note = "Both tests are shown. Also check the sphericity assumption if using Repeated Measures ANOVA."))
      }
    }

    if (dt == "categorical") {
      if (ng == "two" && ind == "independent") {
        if (par == "met")     return(list(best = "chi_square",   alternatives = c("fisher_exact","z_test_prop","binomial"),
                                          note = "The chi-square test is appropriate when expected cell counts are ≥ 5. For smaller samples, prefer Fisher's Exact Test."))
        if (par == "not_met") return(list(best = "fisher_exact", alternatives = c("chi_square","z_test_prop","binomial"),
                                          note = "Fisher's Exact Test is preferred for small samples (expected counts < 5). The binomial test is appropriate when comparing one group to a fixed hypothesized proportion."))
        if (par == "unsure")  return(list(best = c("chi_square","fisher_exact"), alternatives = c("z_test_prop","binomial"),
                                          note = "Check expected cell counts in R with result$expected. Use Fisher's Exact Test if any expected count is < 5."))
      }
      if (ng == "two" && ind == "paired") {
        return(list(best = "mcnemar", alternatives = character(0),
                    note = "McNemar's Test is the correct test for paired binary categorical data (e.g., same subjects measured before and after)."))
      }
      if (ng == "more_than_two" && ind == "independent") {
        return(list(best = "chi_square", alternatives = character(0),
                    note = "For three or more independent categorical groups, chi-square tests for an overall association. Follow up with post-hoc pairwise chi-square tests if significant."))
      }
      if (ng == "more_than_two" && ind == "paired") {
        return(list(best = "chi_square", alternatives = character(0),
                    note = "For three or more paired categorical conditions, Cochran's Q test is the ideal choice, but it is not covered here. Chi-square provides a reasonable approximation for larger samples."))
      }
    }

    if (dt == "ordinal") {
      if (ng == "two" && ind == "independent") {
        return(list(best = "mann_whitney",  alternatives = character(0),
                    note = "Ordinal data should use non-parametric tests. Mann-Whitney U compares rank distributions between two independent groups."))
      }
      if (ng == "two" && ind == "paired") {
        return(list(best = "wilcoxon",      alternatives = character(0),
                    note = "The Wilcoxon signed-rank test is the appropriate paired non-parametric test for ordinal outcome data."))
      }
      if (ng == "more_than_two" && ind == "independent") {
        return(list(best = "kruskal_wallis",alternatives = character(0),
                    note = "Kruskal-Wallis is the non-parametric equivalent of one-way ANOVA, appropriate for ordinal data across three or more independent groups."))
      }
      if (ng == "more_than_two" && ind == "paired") {
        return(list(best = "friedman",      alternatives = character(0),
                    note = "The Friedman test is the non-parametric equivalent of repeated measures ANOVA, appropriate for ordinal data measured across three or more conditions."))
      }
    }
  }

  # ── Examine relationships ──
  if (goal == "relate") {
    if (dt == "numerical") {
      if (par == "met")     return(list(best = "pearson",  alternatives = "spearman"))
      if (par == "not_met") return(list(best = "spearman", alternatives = "pearson"))
      if (par == "unsure")  return(list(best = c("pearson","spearman"), alternatives = character(0),
                                        note = "Both are shown. Pearson r requires normality and linearity; Spearman rho is robust to non-normality and monotonic (not just linear) relationships."))
    }
    if (dt == "categorical") {
      if (par == "met")     return(list(best = "chi_square",   alternatives = "fisher_exact",
                                        note = "Chi-square tests whether two categorical variables are associated. Use Fisher's Exact Test if expected cell counts are small."))
      if (par == "not_met") return(list(best = "fisher_exact", alternatives = "chi_square",
                                        note = "Fisher's Exact Test is preferred for small samples. Check expected counts in R with result$expected."))
      if (par == "unsure")  return(list(best = c("chi_square","fisher_exact"), alternatives = character(0),
                                        note = "Run chisq.test() first and check result$expected. If any expected count < 5, use Fisher's Exact Test."))
    }
    if (dt == "ordinal") {
      return(list(best = "spearman", alternatives = character(0),
                  note = "Spearman rank correlation is the standard choice for ordinal data or non-normal continuous data. It measures the monotonic (consistently increasing or decreasing) relationship between two variables."))
    }
  }

  # ── Predict an outcome ──
  if (goal == "predict") {
    np <- answers[["num_predictors"]]

    if (dt == "numerical") {
      if (np == "one") {
        if (par == "met")     return(list(best = "simple_regression",   alternatives = "spearman"))
        if (par == "not_met") return(list(best = "spearman",            alternatives = "simple_regression",
                                          note = "When parametric assumptions are violated, Spearman correlation can assess the direction and strength of a monotonic relationship. For formal non-parametric regression, consider quantile regression (quantreg package)."))
        if (par == "unsure")  return(list(best = c("simple_regression","spearman"), alternatives = character(0),
                                          note = "Check diagnostic plots after fitting the regression model. Spearman correlation is shown as a non-parametric complement."))
      }
      if (np == "multiple") {
        if (par == "met")     return(list(best = "multiple_regression",  alternatives = character(0)))
        if (par == "not_met") return(list(best = "multiple_regression",  alternatives = character(0),
                                          note = "Multiple regression is recommended, but check the residual diagnostic plots carefully. Consider transforming skewed variables or using robust regression methods."))
        if (par == "unsure")  return(list(best = "multiple_regression",  alternatives = character(0),
                                          note = "Fit the model and review diagnostic plots (Residuals vs Fitted, Normal Q-Q). Residuals — not raw data — need to be approximately normal."))
      }
    }

    if (dt == "ordinal") {
      if (np == "one") {
        return(list(best = "spearman", alternatives = "simple_regression",
                    note = "For ordinal predictors or outcomes, Spearman correlation provides a non-parametric measure of the predictive relationship. If the ordinal variable has many levels (5+), simple linear regression may also be appropriate."))
      }
      if (np == "multiple") {
        return(list(best = "multiple_regression", alternatives = character(0),
                    note = "With ordinal data and multiple predictors, multiple regression is commonly used (especially for Likert scales with 5+ levels). Check residual normality in diagnostic plots."))
      }
    }

    if (dt == "categorical") {
      if (np == "one") {
        return(list(best = "logistic_simple", alternatives = "chi_square",
                    note = "Logistic regression predicts the probability of a binary outcome (e.g., yes/no). If your outcome has 3+ categories, use Multinomial Logistic Regression (nnet::multinom() in R). Chi-square is shown as an alternative for testing association without a directional prediction."))
      }
      if (np == "multiple") {
        return(list(best = "logistic_multiple", alternatives = "chi_square",
                    note = "Multiple logistic regression models a binary outcome from two or more predictors. If your outcome has 3+ categories, use Multinomial Logistic Regression (nnet::multinom() in R). Aim for at least 10 outcome events per predictor to avoid overfitting."))
      }
    }
  }

  # Fallback
  list(best = character(0), alternatives = character(0), note = "No recommendation could be generated for this combination.")
}

# ── Answer label lookup ───────────────────────────────────────────────────────
answer_label <- function(step_id, value) {
  cfg     <- STEP_CONFIGS[[step_id]]
  choices <- cfg$choices
  for (ch in choices) {
    if (ch$value == value) return(ch$label)
  }
  value
}

step_label <- function(step_id) {
  labels <- c(
    data_type      = "Data Type",
    goal           = "Goal",
    num_groups     = "Number of Groups",
    independence   = "Group Structure",
    num_predictors = "Number of Predictors",
    parametric     = "Parametric Assumptions"
  )
  labels[[step_id]]
}

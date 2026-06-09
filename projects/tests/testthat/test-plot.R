# Tests for the ggplot2-free plotting logic in helpers_plot.R: chart-suitability
# hints, the code generator, and the small helpers. Building actual ggplot
# objects is exercised in the PowerShell smoke test (needs ggplot2 attached).

# ---- chart_hint --------------------------------------------------------------

test_that("chart_hint flags a categorical X on a scatter", {
  df  <- data.frame(g = letters[1:5], y = 1:5)
  msg <- chart_hint(df, list(type = "scatter", x = "g"))
  expect_true(grepl("categorical", msg))
})

test_that("chart_hint is silent for a numeric scatter X", {
  df <- data.frame(x = 1:20, y = 1:20)
  expect_null(chart_hint(df, list(type = "scatter", x = "x")))
})

test_that("chart_hint needs >= 2 numeric columns for a heatmap", {
  df  <- data.frame(x = 1:5, g = letters[1:5])
  msg <- chart_hint(df, list(type = "heatmap"))
  expect_true(grepl("2 numeric", msg))
})

# ---- generate_code -----------------------------------------------------------

test_that("generate_code emits parseable ggplot2 for a scatter", {
  df   <- data.frame(x = 1:10, y = (1:10) * 2)
  code <- generate_code(df, list(type = "scatter", x = "x", y = "y"))
  expect_true(grepl("ggplot", code))
  expect_true(grepl("geom_point", code))
  expect_silent(parse(text = code))          # it's runnable R
})

test_that("generate_code prompts for a chart type up front", {
  expect_match(generate_code(data.frame(x = 1), list()), "Choose a chart type")
})

test_that("generate_code backticks non-syntactic column names", {
  df   <- data.frame(`my x` = 1:3, y = 1:3, check.names = FALSE)
  code <- generate_code(df, list(type = "scatter", x = "my x", y = "y"))
  expect_true(grepl("`my x`", code, fixed = TRUE))
})

# ---- small helpers -----------------------------------------------------------

test_that("needs_x_rotation triggers for many/long discrete labels", {
  df <- data.frame(g = paste0("category_", 1:12), y = 1:12)
  expect_true(needs_x_rotation(df, "bar", "g"))
  expect_false(needs_x_rotation(df, "scatter", "y"))
})

test_that("lump_bar_x keeps the top categories and lumps the rest", {
  df  <- data.frame(g = c("a", "a", "a", "b", "b", "c", "d"))
  out <- lump_bar_x(df, "g", NULL, 2)          # keep a(3), b(2); c,d -> Other
  expect_true("Other" %in% levels(out$g))
  expect_setequal(as.character(unique(out$g)), c("a", "b", "Other"))
})

test_that("bq backticks only non-syntactic names; qq quotes", {
  expect_equal(bq("mpg"), "mpg")
  expect_equal(bq("my var"), "`my var`")
  expect_equal(qq("hi"), '"hi"')
})

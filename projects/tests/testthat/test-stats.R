# Tests for the grouped-summary + column-classifier helpers in helpers_stats.R.
# Helpers are sourced by setup.R before this file runs.

test_that("grouped_summary returns one row per group with the right stats", {
  df  <- data.frame(g = c("a", "a", "b"), x = c(1, 3, 10))
  out <- grouped_summary(df, vars = "x", groups = "g")
  expect_equal(out$g, c("a", "b"))
  expect_equal(out$Variable, c("x", "x"))
  expect_equal(out$N, c(2L, 1L))
  expect_equal(out$Mean, c(2, 10))
  expect_true(all(c("g", "Variable", "N", "Mean", "Median", "Mode", "Min",
                    "Max", "SD") %in% names(out)))
})

test_that("grouped_summary returns NULL when inputs are unusable", {
  expect_null(grouped_summary(data.frame(x = 1), vars = character(0),
                              groups = "x"))
  expect_null(grouped_summary(NULL, "x", "g"))
})

test_that("numeric_cols and groupable_cols classify columns", {
  df <- data.frame(id = 1:40, grp = rep(letters[1:4], 10), val = (1:40) / 7)
  expect_setequal(numeric_cols(df), c("id", "val"))
  expect_true("grp" %in% groupable_cols(df))    # 4 distinct -> groupable
  expect_false("id" %in% groupable_cols(df))     # 40 distinct -> too many
})

test_that("the mode reducer is NA when nothing repeats", {
  expect_true(is.na(.s_mode(c(1, 2, 3))))
  expect_equal(.s_mode(c(1, 2, 2, 3)), 2)
})

test_that("friendly_type labels columns in plain English", {
  expect_equal(friendly_type(1L), "integer")
  expect_equal(friendly_type(1.5), "numeric")
  expect_equal(friendly_type(factor("a")), "factor")
  expect_equal(friendly_type("a"), "text")
  expect_equal(friendly_type(as.Date("2026-01-01")), "date")
})

test_that("column_profile reports type, missingness, and distinct counts", {
  df  <- data.frame(x = c(1, 2, NA), g = c("a", "a", "b"),
                    stringsAsFactors = FALSE)
  prof <- column_profile(df)
  expect_setequal(prof$Column, c("x", "g"))
  expect_equal(prof$Distinct[prof$Column == "g"], 2L)
  expect_match(prof$Missing[prof$Column == "x"], "1 \\(33%\\)")
})

test_that("data_glance counts rows, columns, and complete cases", {
  g <- data_glance(data.frame(x = c(1, NA), y = c("a", "b")))
  expect_equal(g$n, 2L)
  expect_equal(g$m, 2L)
  expect_equal(g$complete, 1L)        # one row has an NA
})

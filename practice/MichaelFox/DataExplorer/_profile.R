# Times the heavy data paths on a large dataset (flights, 336k x 19) so we can
# target real bottlenecks. Run:  Rscript _profile.R
suppressWarnings(suppressMessages(
  source("DataExplorerApp.R", local = (env <- new.env()))
))
attach(env, warn.conflicts = FALSE)

tt <- function(label, expr) {
  t <- system.time(force(expr))[["elapsed"]]
  cat(sprintf("%-26s %6.2fs\n", label, t))
  invisible(NULL)
}

path <- "C:/Users/michaelfox1/Desktop/de-test-data/flights.csv"
if (!file.exists(path)) { cat("flights.csv not found — run _make_testdata.R first\n"); quit() }

f <- NULL
tt("read_file_data (30MB csv)", { f <<- read_file_data(path, "csv") })
cat(sprintf("dims: %s x %d\n\n", format(nrow(f), big.mark = ","), ncol(f)))

tt("detect_issues (all)", detect_issues(f))
specs <- clean_specs()
for (id in names(specs)) tt(paste0("  detect: ", id), specs[[id]]$detect(f))
cat("\n")
tt("column_profile", column_profile(f))
tt("summary()", summary(f))
tt("cols_num scan", names(f)[vapply(f, is.numeric, logical(1))])
tt("text_numeric scan", names(f)[vapply(f, is_numeric_text, logical(1))])
num <- f[vapply(f, is.numeric, logical(1))]
tt("cor (heatmap)", suppressWarnings(stats::cor(num, use = "pairwise.complete.obs")))

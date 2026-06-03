# Exports challenge datasets to ~/Desktop/de-test-data for testing the app.
# Run:  Rscript _make_testdata.R
dir <- "C:/Users/michaelfox1/Desktop/de-test-data"
dir.create(dir, showWarnings = FALSE)

# diamonds: bundled with ggplot2 (already installed) — 53,940 x 10
d <- ggplot2::diamonds
write.csv(d, file.path(dir, "diamonds.csv"), row.names = FALSE)
cat(sprintf("diamonds.csv  %d rows x %d cols\n", nrow(d), ncol(d)))

# flights: from nycflights13 — 336,776 x 19
if (requireNamespace("nycflights13", quietly = TRUE)) {
  f <- nycflights13::flights
  write.csv(f, file.path(dir, "flights.csv"), row.names = FALSE)
  cat(sprintf("flights.csv   %d rows x %d cols\n", nrow(f), ncol(f)))
} else {
  cat('flights: nycflights13 not installed.\n')
  cat('         Run  install.packages("nycflights13")  then re-run this script.\n')
}

# A deliberately messy file to exercise Data Health.
messy <- c(
  "Quarterly Report",
  "region,revenue,zip,opened",
  'North , "$1,200", 02134, 2020-01-15',
  "South,N/A,90210,2021/03/02",
  'North,"3,400",30301,',
  'North,"3,400",30301,',
  ",,,",
  "Source: internal")
writeLines(messy, file.path(dir, "messy_demo.csv"))
cat("messy_demo.csv  (for Data Health)\n")

# Multi-sheet Excel workbook (for the worksheet picker).
writexl::write_xlsx(
  list(iris = iris, mtcars = mtcars, diamonds_head = head(ggplot2::diamonds, 500)),
  file.path(dir, "multi_sheet.xlsx"))
cat("multi_sheet.xlsx  (3 sheets: iris, mtcars, diamonds_head)\n")

cat("\nFiles are in:", dir, "\n")

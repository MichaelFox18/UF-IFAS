# Forces ggplot_build() on the new feature combinations so render-time errors
# (log scales, faceting, coord_flip, manual palettes, annotations) surface here
# rather than only in the running app.
# Run:  Rscript _test_render.R   (from the DataExplorer folder)
suppressWarnings(suppressMessages(
  source("DataExplorerApp.R", local = (env <- new.env()))
))
build_full_plot <- env$build_full_plot

mt <- as.data.frame(mtcars)
mkp <- function(...) modifyList(list(theme = "minimal", size = 2), list(...))

cases <- list(
  facet      = mkp(type = "scatter", x = "wt", y = "mpg", facet = "cyl"),
  log_both   = mkp(type = "scatter", x = "wt", y = "mpg", logscale = "both"),
  flip_bar   = mkp(type = "bar", x = "gear", y = "mpg", bar_agg = "mean", flip = TRUE),
  jitter     = mkp(type = "scatter", x = "wt", y = "mpg", jitter = TRUE),
  pal_cb     = mkp(type = "scatter", x = "wt", y = "mpg", color = "cyl", palette = "cb"),
  pal_uf     = mkp(type = "bar", x = "gear", color = "cyl", palette = "uf"),
  trend      = mkp(type = "scatter", x = "wt", y = "mpg", reg_overlay = TRUE,
                   reg_type = "lm", trend_label = TRUE),
  legend_btm = mkp(type = "scatter", x = "wt", y = "mpg", color = "cyl",
                   legend_pos = "bottom", gridlines = FALSE),
  pie_cb     = mkp(type = "pie", x = "cyl", palette = "cb", legend_pos = "bottom"),
  heatmap    = mkp(type = "heatmap", corr_method = "pearson", corr_label = TRUE),
  heatmap_sp = mkp(type = "heatmap", corr_method = "spearman",
                   corr_vars = c("mpg", "wt", "hp", "disp"))
)

bad <- 0L
for (nm in names(cases)) {
  r <- tryCatch({
    suppressWarnings(ggplot2::ggplot_build(build_full_plot(mt, cases[[nm]])))
    "OK"
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  if (!identical(r, "OK")) bad <- bad + 1L
  cat(sprintf("%-11s %s\n", nm, r))
}
cat(if (bad == 0L) "\nALL RENDER OK\n" else sprintf("\n%d FAILED\n", bad))
if (bad > 0L) quit(status = 1)

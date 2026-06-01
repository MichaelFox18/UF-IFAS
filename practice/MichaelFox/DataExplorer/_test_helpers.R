# Functional sanity checks for the "strange graphs" fixes.
# Run:  Rscript _test_helpers.R   (from the DataExplorer folder)
suppressWarnings(suppressMessages(
  source("DataExplorerApp.R", local = (env <- new.env()))
))
attach(env, warn.conflicts = FALSE)

ok <- 0L; bad <- 0L
chk <- function(label, cond) {
  if (isTRUE(cond)) { ok <<- ok + 1L; cat(sprintf("PASS  %s\n", label)) }
  else              { bad <<- bad + 1L; cat(sprintf("FAIL  %s\n", label)) }
}

# ---- fixtures -------------------------------------------------------------
mt <- as.data.frame(mtcars); mt$car <- rownames(mt); rownames(mt) <- NULL
cat_df <- data.frame(
  grp  = rep(c("North", "South", "East", "West"), each = 5),
  val  = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9, 3, 2, 3, 8, 4),
  name = paste0("item", 1:20),                 # 20 unique -> categorical
  stringsAsFactors = FALSE
)
many <- data.frame(
  city = paste0("City", sprintf("%03d", 1:60)),  # 60 categories
  pop  = sample(1:1000, 60),
  stringsAsFactors = FALSE
)
unsorted <- data.frame(t = c(3, 1, 2, 5, 4), y = c(30, 10, 20, 50, 40))

# ---- predicates -----------------------------------------------------------
chk("is_discrete_col(character)", is_discrete_col(cat_df$grp))
chk("is_discrete_col(numeric) FALSE", !is_discrete_col(mt$mpg))
chk("needs_x_rotation: 60 cats -> TRUE", needs_x_rotation(many, "bar", "city"))
chk("needs_x_rotation: numeric scatter -> FALSE", !needs_x_rotation(mt, "scatter", "mpg"))

# ---- lump_bar_x -----------------------------------------------------------
lp <- lump_bar_x(many, "city", NULL, BAR_MAX)
chk("lump_bar_x caps levels to BAR_MAX+1", nlevels(lp$city) == BAR_MAX + 1L)
chk("lump_bar_x adds 'Other'", "Other" %in% levels(lp$city))
chk("lump_bar_x keeps all rows", nrow(lp) == nrow(many))

# ---- chart_hint -----------------------------------------------------------
chk("hint: scatter on categorical X",
    !is.null(chart_hint(cat_df, list(type = "scatter", x = "name"))))
chk("hint: bar on continuous X",
    !is.null(chart_hint(mt, list(type = "bar", x = "mpg"))))
chk("hint: bar on >BAR_MAX categories",
    !is.null(chart_hint(many, list(type = "bar", x = "city"))))
chk("hint: clean pairing -> NULL",
    is.null(chart_hint(cat_df, list(type = "bar", x = "grp"))))
chk("hint: histogram numeric -> NULL",
    is.null(chart_hint(mt, list(type = "histogram", x = "mpg"))))

# ---- build_full_plot ------------------------------------------------------
mkp <- function(...) modifyList(list(theme = "minimal", size = 2), list(...))
build_ok <- function(df, p) {
  pl <- tryCatch(build_full_plot(df, p), error = function(e) e)
  !inherits(pl, "error") && inherits(pl, "ggplot")
}
chk("build: scatter numeric",   build_ok(mt, mkp(type = "scatter", x = "wt", y = "mpg")))
chk("build: line numeric",      build_ok(unsorted, mkp(type = "line", x = "t", y = "y")))
chk("build: bar count (cat)",   build_ok(cat_df, mkp(type = "bar", x = "grp")))
chk("build: bar value+agg",     build_ok(cat_df, mkp(type = "bar", x = "grp", y = "val", bar_agg = "mean")))
chk("build: bar high-card lumps", build_ok(many, mkp(type = "bar", x = "city")))
chk("build: histogram numeric", build_ok(mt, mkp(type = "histogram", x = "mpg", bins = 20)))
chk("build: boxplot",           build_ok(cat_df, mkp(type = "boxplot", x = "grp", y = "val")))
chk("build: pie",               build_ok(cat_df, mkp(type = "pie", x = "grp")))
chk("build: histogram on text -> NULL (guarded)",
    is.null(build_full_plot(cat_df, mkp(type = "histogram", x = "name"))))

# line plot data is sorted by x
lp2 <- build_full_plot(unsorted, mkp(type = "line", x = "t", y = "y"))
chk("build: line data sorted by x", !is.unsorted(lp2$data$t))

# bar high-card carries the explanatory subtitle
bp <- build_full_plot(many, mkp(type = "bar", x = "city"))
chk("build: lumped bar has subtitle", !is.null(bp$labels$subtitle))

# category limit raised to show everything -> no lump, no subtitle, no hint
bp_all <- build_full_plot(many, mkp(type = "bar", x = "city", cat_limit = 60))
chk("build: limit=all keeps every category", nrow(bp_all$data) == nrow(many))
chk("build: limit=all has no subtitle", is.null(bp_all$labels$subtitle))
chk("hint: limit=all -> no warning", is.null(chart_hint(many, list(type = "bar", x = "city", cat_limit = 60))))

# lowering the limit lumps to that many categories + Other
bp_lim <- build_full_plot(many, mkp(type = "bar", x = "city", cat_limit = 10))
chk("build: custom limit lumps to N+Other", nlevels(bp_lim$data$city) == 11)
chk("hint: over-limit warns", !is.null(chart_hint(many, list(type = "bar", x = "city", cat_limit = 10))))
chk("code: custom limit in note", grepl("top 10 categories", generate_code(many, mkp(type = "bar", x = "city", cat_limit = 10))))
chk("build: pie respects cat_limit", build_ok(many, mkp(type = "pie", x = "city", cat_limit = 8)))

# ---- generate_code --------------------------------------------------------
gc_line <- generate_code(unsorted, mkp(type = "line", x = "t", y = "y"))
chk("code: line includes order()", grepl("order\\(df", gc_line))
gc_bar  <- generate_code(many, mkp(type = "bar", x = "city"))
chk("code: lumped bar notes top-N", grepl("top 30 categories", gc_bar))
gc_rot  <- generate_code(many, mkp(type = "bar", x = "city"))
chk("code: rotated axis emitted", grepl("angle = 40", gc_rot))

# ---- customization: palettes ---------------------------------------------
chk("palette_code: cb -> manual",   grepl("^scale_color_manual", palette_code("cb", "color", FALSE, 3)))
chk("palette_code: viridis",        identical(palette_code("viridis", "fill", FALSE, 5), "scale_fill_viridis_d()"))
chk("palette_code: auto continuous",identical(palette_code("auto", "color", TRUE, 50), "scale_color_viridis_c()"))
chk("palette_code: uf gradient",    grepl("scale_fill_gradient", palette_code("uf", "fill", TRUE, 5)))
chk("palette_code: set1 overflow -> viridis", identical(palette_code("set1", "fill", FALSE, 20), "scale_fill_viridis_d()"))
mt_f <- mt; mt_f$cyl <- as.factor(mt_f$cyl)
gs <- group_scales(mt_f, "cyl", "cb")
chk("group_scales returns two Scales", length(gs) == 2 && inherits(gs[[1]], "Scale"))

# ---- customization: trend label ------------------------------------------
tl <- trend_label_text(mt, "wt", "mpg", "lm", 2)
chk("trend_label_text: lm equation", grepl("y =", tl) && grepl("R²", tl))
chk("trend_label_text: poly", grepl("polynomial", trend_label_text(mt, "wt", "mpg", "poly", 2)))

# ---- customization: build_full_plot variants -----------------------------
chk("build: palette colorblind", build_ok(mt, mkp(type = "scatter", x = "wt", y = "mpg", color = "cyl", palette = "cb")))
chk("build: jitter",             build_ok(mt, mkp(type = "scatter", x = "wt", y = "mpg", jitter = TRUE)))
chk("build: log both",           build_ok(mt, mkp(type = "scatter", x = "wt", y = "mpg", logscale = "both")))
chk("build: facet",              build_ok(mt, mkp(type = "scatter", x = "wt", y = "mpg", facet = "cyl")))
chk("build: horizontal bar",     build_ok(cat_df, mkp(type = "bar", x = "grp", flip = TRUE)))
chk("build: legend+grid",        build_ok(mt, mkp(type = "scatter", x = "wt", y = "mpg", color = "cyl", legend_pos = "bottom", gridlines = FALSE)))
chk("build: trend label",        build_ok(mt, mkp(type = "scatter", x = "wt", y = "mpg", reg_overlay = TRUE, reg_type = "lm", trend_label = TRUE)))

# ---- customization: generate_code reflects settings ----------------------
chk("code: jitter -> geom_jitter", grepl("geom_jitter", generate_code(mt, mkp(type = "scatter", x = "wt", y = "mpg", jitter = TRUE))))
chk("code: flip -> coord_flip",    grepl("coord_flip\\(\\)", generate_code(cat_df, mkp(type = "bar", x = "grp", flip = TRUE))))
chk("code: facet_wrap",            grepl("facet_wrap\\(vars\\(cyl\\)\\)", generate_code(mt, mkp(type = "scatter", x = "wt", y = "mpg", facet = "cyl"))))
gc_log <- generate_code(mt, mkp(type = "scatter", x = "wt", y = "mpg", logscale = "both"))
chk("code: log scales",            grepl("scale_x_log10", gc_log) && grepl("scale_y_log10", gc_log))
chk("code: palette manual",        grepl("scale_color_manual", generate_code(mt, mkp(type = "scatter", x = "wt", y = "mpg", color = "cyl", palette = "cb"))))
gc_th <- generate_code(mt, mkp(type = "scatter", x = "wt", y = "mpg", color = "cyl", legend_pos = "bottom", gridlines = FALSE))
chk("code: legend + gridlines",    grepl('legend.position = "bottom"', gc_th) && grepl("panel.grid = element_blank", gc_th))
chk("code: trend annotate",        grepl('annotate\\("text"', generate_code(mt, mkp(type = "scatter", x = "wt", y = "mpg", reg_overlay = TRUE, reg_type = "lm", trend_label = TRUE))))
chk("code: custom alpha",          grepl("alpha = 0.3", generate_code(mt, mkp(type = "scatter", x = "wt", y = "mpg", alpha = 0.3))))

# ---- pie palette + legend -------------------------------------------------
chk("pie_fill_scale: auto -> Scale",  inherits(pie_fill_scale("auto", 4, "g"), "Scale"))
chk("pie_fill_scale: cb -> Scale",    inherits(pie_fill_scale("cb", 4, "g"), "Scale"))
chk("build: pie with palette",        build_ok(cat_df, mkp(type = "pie", x = "grp", palette = "cb")))
chk("build: pie legend hidden",       build_ok(cat_df, mkp(type = "pie", x = "grp", legend_pos = "none")))
chk("code: pie palette manual",       grepl("scale_fill_manual", generate_code(cat_df, mkp(type = "pie", x = "grp", palette = "cb"))))
chk("code: pie legend position",      grepl('legend.position = "bottom"', generate_code(cat_df, mkp(type = "pie", x = "grp", legend_pos = "bottom"))))

# ---- UI: the control panel (with nested Advanced accordion) builds --------
psp <- tryCatch(plot_slot_panel(1), error = function(e) e)
chk("plot_slot_panel builds without error", !inherits(psp, "error"))

# The restructured config UI: per-plot accordions wrapped in conditionalPanels.
cfg <- tryCatch(as.character(shiny::tagList(
  accordion(plot_slot_panel(1), open = "panel1"),
  shiny::conditionalPanel("input.n_plots >= 2", accordion(plot_slot_panel(2), open = FALSE))
)), error = function(e) "")
chk("config UI renders (accordion + conditionalPanel)",
    nchar(cfg) > 1000 && grepl("ui_mp1_catlimit", cfg))

cat(sprintf("\n%d passed, %d failed\n", ok, bad))
if (bad > 0) quit(status = 1)

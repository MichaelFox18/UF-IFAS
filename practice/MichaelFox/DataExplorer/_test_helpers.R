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

# ---- plot export to file (used by the Export-tab image downloads) ---------
tmp_png <- tempfile(fileext = ".png")
render_plots_to_file(list(ggplot(mt, aes(wt, mpg)) + geom_point()),
                     tmp_png, "png", 4, 3, 72)
chk("render_plots_to_file writes a PNG", file.exists(tmp_png) && file.size(tmp_png) > 0)

# ---- correlation heatmap --------------------------------------------------
chk("build: heatmap (all numeric)", build_ok(mt, mkp(type = "heatmap")))
chk("build: heatmap spearman + labels",
    build_ok(mt, mkp(type = "heatmap", corr_method = "spearman", corr_label = TRUE)))
chk("build: heatmap subset of vars",
    build_ok(mt, mkp(type = "heatmap", corr_vars = c("mpg", "wt", "hp"))))
chk("build: heatmap needs >= 2 numeric (NULL)",
    is.null(build_full_plot(data.frame(g = c("a", "b"), n = 1:2), mkp(type = "heatmap"))))
chk("hint: heatmap with < 2 numeric warns",
    !is.null(chart_hint(data.frame(g = c("a", "b")), list(type = "heatmap"))))
chk("hint: heatmap with enough numeric -> NULL",
    is.null(chart_hint(mt, list(type = "heatmap"))))
gc_h <- generate_code(mt, mkp(type = "heatmap", corr_method = "spearman"))
chk("code: heatmap uses cor()",        grepl('cor\\(num', gc_h) && grepl('spearman', gc_h))
chk("code: heatmap uses geom_tile",    grepl("geom_tile", gc_h))

# ---- pie palette + legend -------------------------------------------------
chk("pie_fill_scale: auto -> Scale",  inherits(pie_fill_scale("auto", 4, "g"), "Scale"))
chk("pie_fill_scale: cb -> Scale",    inherits(pie_fill_scale("cb", 4, "g"), "Scale"))
chk("build: pie with palette",        build_ok(cat_df, mkp(type = "pie", x = "grp", palette = "cb")))
chk("build: pie legend hidden",       build_ok(cat_df, mkp(type = "pie", x = "grp", legend_pos = "none")))
chk("code: pie palette manual",       grepl("scale_fill_manual", generate_code(cat_df, mkp(type = "pie", x = "grp", palette = "cb"))))
chk("code: pie legend position",      grepl('legend.position = "bottom"', generate_code(cat_df, mkp(type = "pie", x = "grp", legend_pos = "bottom"))))

# ---- Data Health: detection + reversible fixes ---------------------------
clean_df <- data.frame(a = 1:5, b = letters[1:5], stringsAsFactors = FALSE)
chk("detect: clean data -> no issues", length(detect_issues(clean_df)) == 0)
chk("detect: mtcars -> no issues", length(detect_issues(mt)) == 0)

messy <- data.frame(
  amount = c("$1,200", "2,500", "N/A", "750"),   # numbers-as-text + NA token
  region = c("North ", " South", "North", "N/A"), # whitespace + NA token
  empty  = c("", "", "", ""),                     # entirely blank column
  stringsAsFactors = FALSE
)
messy <- rbind(messy, messy[3, ])                  # duplicate row
ids_messy <- vapply(detect_issues(messy), `[[`, character(1), "id")
chk("detect: finds numeric-as-text", "numeric"   %in% ids_messy)
chk("detect: finds whitespace",      "trim"      %in% ids_messy)
chk("detect: finds NA tokens",       "na_tokens" %in% ids_messy)
chk("detect: finds empty column",    "empty_cols" %in% ids_messy)
chk("detect: finds duplicate rows",  "dups"      %in% ids_messy)

cleaned <- clean_apply(messy, ids_messy)
chk("clean: amount is now numeric",  is.numeric(cleaned$amount))
chk("clean: $1,200 parsed to 1200",  isTRUE(cleaned$amount[1] == 1200))
chk("clean: N/A -> NA in amount",    is.na(cleaned$amount[3]))
chk("clean: whitespace trimmed",     identical(cleaned$region[2], "South"))
chk("clean: empty column dropped",   !("empty" %in% names(cleaned)))
chk("clean: duplicate row removed",  nrow(cleaned) == nrow(messy) - 1)
chk("clean: re-diagnose is clean",   length(detect_issues(cleaned)) == 0)

# num_from_text strips $, commas, %
chk("num_from_text strips symbols", isTRUE(all.equal(num_from_text(c("$1,000", "12%")), c(1000, 12))))
# dates_from_text honours ISO only
chk("dates_from_text: ISO parses",   !is.null(dates_from_text(c("2020-01-02", "2021-12-31"))))
chk("dates_from_text: ambiguous NULL", is.null(dates_from_text(c("01/02/2020", "12/31/2021"))))

# ---- numeric-coercion guards (ID/leading-zero columns) --------------------
chk("has_leading_zeros: ZIP -> TRUE",       has_leading_zeros(c("02134", "90210")))
chk("has_leading_zeros: plain ints -> FALSE", !has_leading_zeros(c("1", "23", "0", "0.5")))
chk("is_numeric_text: money -> TRUE",       is_numeric_text(c("$1,200", "3,400", "750")))
chk("is_numeric_text: ZIP -> FALSE",        !is_numeric_text(c("02134", "90210", "30301")))
zips <- data.frame(zip = c("02134", "90210", "30301", "10001"), stringsAsFactors = FALSE)
chk("clean: ZIP column not flagged numeric",
    !("numeric" %in% vapply(detect_issues(zips), `[[`, character(1), "id")))

# ---- Excel multi-sheet reading --------------------------------------------
xl <- tempfile(fileext = ".xlsx")
writexl::write_xlsx(list(alpha = data.frame(a = 1:3),
                         beta  = data.frame(b = c("x", "y"))), xl)
chk("excel_sheets lists sheets", identical(readxl::excel_sheets(xl), c("alpha", "beta")))
d_a <- read_file_data(xl, "xlsx", sheet = "alpha")
d_b <- read_file_data(xl, "xlsx", sheet = "beta")
chk("read_file_data: sheet alpha", identical(names(d_a), "a") && nrow(d_a) == 3)
chk("read_file_data: sheet beta",  identical(names(d_b), "b") && nrow(d_b) == 2)
chk("read_file_data: default sheet is the first", identical(names(read_file_data(xl, "xlsx")), "a"))

# ---- CSV import bounds (detect_table_bounds via read_file_data) -----------
write_tmp <- function(lines) { f <- tempfile(fileext = ".csv"); writeLines(lines, f); f }

d_clean <- read_file_data(write_tmp(c("a,b,c", "1,2,3", "4,5,6", "7,8,9")), "csv")
chk("import: clean file keeps all rows", nrow(d_clean) == 3 && ncol(d_clean) == 3)
chk("import: clean file no skip",        (attr(d_clean, "n_skip_head") %||% 0) == 0)

d_msg <- read_file_data(write_tmp(c(
  "Bureau of Stuff - Table 1", "Released 2024",
  "year,value", "2020,10", "2021,20", "2022,30",
  "Source: somewhere", "Note: provisional")), "csv")
chk("import: trims title + footnote lines", nrow(d_msg) == 3 && ncol(d_msg) == 2)
chk("import: header detected after titles", identical(names(d_msg), c("year", "value")))
chk("import: reports 2 skipped head lines", (attr(d_msg, "n_skip_head") %||% 0) == 2)
chk("import: reports 2 skipped tail lines", (attr(d_msg, "n_skip_tail") %||% 0) == 2)
chk("import: no footnote text leaked",      all(d_msg$year %in% c(2020, 2021, 2022)))

# regression test for the data-loss risk: an interior non-data line must NOT
# drop the rows that follow it.
d_gap <- read_file_data(write_tmp(c("a,b", "1,2", "3,4", "", "5,6", "7,8")), "csv")
chk("import: interior blank line keeps all rows", nrow(d_gap) == 4)
d_div <- read_file_data(write_tmp(c("a,b", "1,2", "3,4", "SECTION TWO", "5,6", "7,8")), "csv")
chk("import: interior divider keeps later rows", sum(d_div$a %in% c(1, 3, 5, 7)) == 4)

# Data Health UI fragment (HTML choiceNames + buttons) renders.
dh_iss <- detect_issues(messy)
dh_ui <- tryCatch(as.character(shiny::tagList(
  checkboxGroupInput("dh_fixes", NULL,
    choiceNames  = unname(lapply(dh_iss, function(z) HTML(z$desc))),
    choiceValues = unname(vapply(dh_iss, `[[`, character(1), "id"))),
  actionButton("dh_apply", "Apply selected fixes")
)), error = function(e) "")
chk("Data Health UI renders", nchar(dh_ui) > 200 && grepl("dh_fixes", dh_ui))

# ---- Data overview: column profile + glance -------------------------------
chk("friendly_type: numeric", friendly_type(c(1.5, 2.5)) == "numeric")
chk("friendly_type: integer", friendly_type(1:5) == "integer")
chk("friendly_type: text",    friendly_type(c("a", "b")) == "text")
chk("friendly_type: date",    friendly_type(as.Date("2020-01-01")) == "date")
chk("friendly_type: logical", friendly_type(c(TRUE, FALSE)) == "logical")

prof <- column_profile(mt)
chk("column_profile: one row per column", nrow(prof) == ncol(mt))
chk("column_profile: has expected fields",
    all(c("Column", "Type", "Missing", "Distinct", "Mean", "Top") %in% names(prof)))
chk("column_profile: numeric col has Mean", !is.na(prof$Mean[prof$Column == "mpg"]))
chk("column_profile: text col has Top, no Mean",
    is.na(prof$Mean[prof$Column == "car"]) && !is.na(prof$Top[prof$Column == "car"]))

# all-NA numeric column must not error or produce Inf
prof_na <- column_profile(data.frame(x = c(1, 2, NA), y = c(NA_real_, NA, NA)))
chk("column_profile: all-NA column -> NA (no Inf)", is.na(prof_na$Min[prof_na$Column == "y"]))
chk("column_profile: missing % counts blanks",
    grepl("33%", column_profile(data.frame(z = c("a", "", "b")))$Missing[1]))

g <- data_glance(mt)
chk("data_glance: row/col counts", g$n == nrow(mt) && g$m == ncol(mt))
chk("data_glance: numeric vs categorical split", g$num == 11 && g$cat == 1)
chk("data_glance: complete rows", g$complete == 32)

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

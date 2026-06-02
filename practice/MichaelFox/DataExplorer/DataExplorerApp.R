# ============================================================
# Data Explorer — R Shiny Application
# ============================================================
# install.packages(c(
#   "shiny", "bslib", "ggplot2", "readxl", "DT", "plotly",
#   "colourpicker", "dplyr", "writexl"
# ))

library(shiny)
library(bslib)
library(ggplot2)
library(readxl)
library(DT)
library(plotly)
library(colourpicker)
library(dplyr)
library(writexl)

UF_BLUE   <- "#003087"
UF_ORANGE <- "#FA4616"
MP_COLORS <- c(UF_BLUE, UF_ORANGE, "#2ca25f", "#8856a7")

# Above this many rows the Visualize tab renders static (fast) plots instead
# of interactive plotly ones. ggplotly() gets slow well before a few thousand
# points (scatter/line/box draw one mark per row) and can make the whole
# single-threaded app appear frozen, so keep this conservative.
BIG_ROWS <- 1000

# Pie charts become unreadable with many slices, so categories beyond this
# many are grouped into a single "Other" slice.
PIE_MAX <- 12

# ColorBrewer "Set1"/"Set2" run out of colours past 8-9 levels (extra levels
# render as invisible NA), so switch to viridis (which scales to any count)
# above this many discrete groups.
BREWER_MAX <- 8

# Bar charts with more than this many categories become an unreadable picket
# fence, so the largest BAR_MAX are kept and the rest are rolled into a single
# "Other" bar (mirroring the pie-chart behaviour above).
BAR_MAX <- 30

# ----------------------------------------------------------
# Helper functions
# ----------------------------------------------------------

# Many real-world CSVs (e.g. BEA exports) start with a few title lines and
# end with quoted footnotes. Find the longest contiguous run of lines that
# share a field count > 1: that block is the header + data. Skip whatever
# sits before/after so read.csv doesn't choke on "more columns than column
# names" or read footnote text as data.
detect_table_bounds <- function(path, sep, header_in = TRUE) {
  fields <- tryCatch(
    utils::count.fields(path, sep = sep, quote = "\"", comment.char = ""),
    error = function(e) integer(0)
  )
  if (!length(fields)) return(list(skip = 0, nrows = -1L, n_skip_tail = 0L))
  fields[is.na(fields)] <- 0L
  if (max(fields) < 2) return(list(skip = 0, nrows = -1L, n_skip_tail = 0L))

  r   <- rle(fields)
  big <- which(r$values > 1)
  if (!length(big)) return(list(skip = 0, nrows = -1L, n_skip_tail = 0L))
  chosen <- big[which.max(r$lengths[big])]
  start  <- if (chosen == 1L) 1L else sum(r$lengths[seq_len(chosen - 1L)]) + 1L
  len    <- r$lengths[chosen]
  total  <- length(fields)
  end    <- start + len - 1L

  # count.fields can drop a trailing blank line, so get the honest line total
  # from readLines for the user-facing "skipped tail" count.
  true_total <- tryCatch(length(readLines(path, warn = FALSE)),
                         error = function(e) total)

  if (start == 1L && end >= true_total)
    return(list(skip = 0, nrows = -1L, n_skip_tail = 0L))

  data_n <- if (isTRUE(header_in)) len - 1L else len
  list(skip = start - 1L,
       nrows = max(0L, data_n),
       n_skip_tail = max(0L, true_total - end))
}

read_delim_smart <- function(path, sep, header, dec, reader) {
  b <- detect_table_bounds(path, sep, header)
  args <- list(file = path, header = header, sep = sep, dec = dec,
               stringsAsFactors = FALSE, skip = b$skip)
  if (b$nrows > 0) args$nrows <- b$nrows
  d <- do.call(reader, args)
  attr(d, "n_skip_head") <- b$skip
  attr(d, "n_skip_tail") <- b$n_skip_tail
  d
}

read_file_data <- function(path, ext, header = TRUE, sep = ",", dec = ".") {
  ext <- tolower(ext)
  if (ext %in% c("xlsx", "xls")) return(as.data.frame(read_excel(path)))
  if (ext == "rds")               return(as.data.frame(readRDS(path)))
  if (ext == "csv")
    return(read_delim_smart(path, sep, header, dec, read.csv))
  if (ext %in% c("tsv", "txt"))
    return(read_delim_smart(path, if (ext == "tsv") "\t" else sep, header, dec, read.table))
  stop("Unsupported file extension: .", ext)
}

label_or  <- function(custom, default) if (nzchar(trimws(custom))) custom else default
pct_label <- function(x) paste0(round(x / sum(x) * 100, 1), "%")

info_icon <- function(..., placement = "right") {
  tooltip(
    tags$span(icon("circle-question"),
              style = "color:#aaa; cursor:help; margin-left:5px; font-size:0.82em;"),
    ...,
    placement = placement
  )
}

# Maps the 0.5–5 size slider onto a sensible 0.2–0.9 bar width.
bar_width <- function(size) 0.2 + (size - 0.5) / 4.5 * 0.7

# Picks an aggregation function for value bar charts.
agg_fun <- function(agg) switch(agg %||% "sum",
  mean   = function(z) mean(z, na.rm = TRUE),
  median = function(z) stats::median(z, na.rm = TRUE),
  function(z) sum(z, na.rm = TRUE)
)

theme_call <- function(name, base_size = 13) {
  fn <- switch(name %||% "minimal",
    minimal = theme_minimal, classic = theme_classic,
    light   = theme_light,   bw      = theme_bw,
    dark    = theme_dark,    theme_minimal)
  fn(base_size = base_size)
}

# ----------------------------------------------------------
# Data Health — diagnose common spreadsheet problems and offer
# opt-in, reversible fixes. Detection and the fix transform live
# together in clean_specs() so they can't drift apart.
# ----------------------------------------------------------

# Strings treated as "missing" placeholders. "-"/"." are deliberately excluded
# so legitimate category values aren't clobbered.
NA_TOKENS <- c("", "NA", "N/A", "n/a", "NULL", "null", "#N/A", "#n/a")

# Parse a character vector as numbers after stripping $, commas, %, spaces.
num_from_text <- function(x) suppressWarnings(as.numeric(gsub("[,$%[:space:]]", "", x)))

# Fraction of values that parse cleanly as numbers, ignoring blanks and NA
# placeholders (so a stray "N/A" doesn't hide an otherwise-numeric column).
numeric_frac <- function(x) {
  v <- x[!is.na(x) & !(trimws(x) %in% NA_TOKENS)]
  if (!length(v)) return(0)
  mean(!is.na(num_from_text(v)))
}

# Returns a Date vector if >= 90% of real (non-blank, non-placeholder) values
# match an unambiguous ISO format, else NULL (we avoid guessing m/d vs d/m).
dates_from_text <- function(x) {
  keep <- !is.na(x) & !(trimws(x) %in% NA_TOKENS)
  if (!any(keep)) return(NULL)
  for (fmt in c("%Y-%m-%d", "%Y/%m/%d")) {
    d <- suppressWarnings(as.Date(x, format = fmt))
    if (mean(!is.na(d[keep])) >= 0.9) return(d)
  }
  NULL
}

# Logical "is this cell blank" (NA, or empty/whitespace-only string).
blank_cell <- function(v) is.na(v) | (is.character(v) & trimws(v) == "")

# Each spec: default (pre-checked?), detect(df) -> HTML string or NULL,
# apply(df) -> df. Order here is the order fixes are applied.
clean_specs <- function() list(
  names = list(
    default = TRUE,
    detect = function(df) {
      nm  <- names(df)
      bad <- sum(is.na(nm) | trimws(nm) == "" | duplicated(nm))
      if (bad == 0) return(NULL)
      sprintf("<b>Column names:</b> make %d blank or duplicated name(s) unique.", bad)
    },
    apply = function(df) {
      nm <- trimws(names(df)); nm[is.na(nm) | nm == ""] <- "V"
      names(df) <- make.unique(nm, sep = "_"); df
    }
  ),
  trim = list(
    default = TRUE,
    detect = function(df) {
      ch    <- vapply(df, is.character, logical(1))
      cells <- if (any(ch)) sum(vapply(df[ch], function(v)
                 sum(!is.na(v) & v != trimws(v)), integer(1))) else 0L
      nmws  <- sum(names(df) != trimws(names(df)))
      if (cells == 0 && nmws == 0) return(NULL)
      sprintf("<b>Whitespace:</b> trim leading/trailing spaces from %d value(s)%s.",
              cells, if (nmws) sprintf(" and %d header(s)", nmws) else "")
    },
    apply = function(df) {
      names(df) <- trimws(names(df))
      for (c in names(df)) if (is.character(df[[c]])) df[[c]] <- trimws(df[[c]])
      df
    }
  ),
  na_tokens = list(
    default = TRUE,
    detect = function(df) {
      ch    <- vapply(df, is.character, logical(1))
      cells <- if (any(ch)) sum(vapply(df[ch], function(v)
                 sum(!is.na(v) & v %in% NA_TOKENS), integer(1))) else 0L
      if (cells == 0) return(NULL)
      sprintf("<b>Missing-value markers:</b> convert %d placeholder cell(s) (e.g. \"N/A\", blank) to true missing (NA).", cells)
    },
    apply = function(df) {
      for (c in names(df)) if (is.character(df[[c]])) {
        v <- df[[c]]; v[v %in% NA_TOKENS] <- NA; df[[c]] <- v
      }
      df
    }
  ),
  numeric = list(
    default = TRUE,
    detect = function(df) {
      cols <- names(df)[vapply(df, function(v)
        is.character(v) && numeric_frac(v) >= 0.9 && any(!is.na(num_from_text(v))),
        logical(1))]
      if (!length(cols)) return(NULL)
      sprintf("<b>Numbers stored as text:</b> convert %s to numeric (strips $, commas, %%).",
              paste(sprintf("<code>%s</code>", cols), collapse = ", "))
    },
    apply = function(df) {
      for (c in names(df)) if (is.character(df[[c]]) && numeric_frac(df[[c]]) >= 0.9 &&
                               any(!is.na(num_from_text(df[[c]]))))
        df[[c]] <- num_from_text(df[[c]])
      df
    }
  ),
  dates = list(
    default = FALSE,
    detect = function(df) {
      cols <- names(df)[vapply(df, function(v)
        is.character(v) && !is.null(dates_from_text(v)), logical(1))]
      if (!length(cols)) return(NULL)
      sprintf("<b>Dates stored as text:</b> convert %s to Date (ISO yyyy-mm-dd).",
              paste(sprintf("<code>%s</code>", cols), collapse = ", "))
    },
    apply = function(df) {
      for (c in names(df)) if (is.character(df[[c]])) {
        d <- dates_from_text(df[[c]]); if (!is.null(d)) df[[c]] <- d
      }
      df
    }
  ),
  empty_cols = list(
    default = TRUE,
    detect = function(df) {
      if (!nrow(df)) return(NULL)
      n <- sum(vapply(df, function(v) all(blank_cell(v)), logical(1)))
      if (n == 0) return(NULL)
      sprintf("<b>Empty columns:</b> drop %d column(s) that are entirely blank.", n)
    },
    apply = function(df) {
      if (!nrow(df)) return(df)
      df[, !vapply(df, function(v) all(blank_cell(v)), logical(1)), drop = FALSE]
    }
  ),
  empty_rows = list(
    default = TRUE,
    detect = function(df) {
      if (!nrow(df) || !ncol(df)) return(NULL)
      m <- sapply(df, blank_cell)
      if (is.null(dim(m))) m <- matrix(m, nrow = nrow(df))
      n <- sum(rowSums(m) == ncol(df))
      if (n == 0) return(NULL)
      sprintf("<b>Empty rows:</b> drop %d row(s) that are entirely blank.", n)
    },
    apply = function(df) {
      if (!nrow(df) || !ncol(df)) return(df)
      m <- sapply(df, blank_cell)
      if (is.null(dim(m))) m <- matrix(m, nrow = nrow(df))
      df[rowSums(m) != ncol(df), , drop = FALSE]
    }
  ),
  dups = list(
    default = TRUE,
    detect = function(df) {
      n <- sum(duplicated(df))
      if (n == 0) return(NULL)
      sprintf("<b>Duplicate rows:</b> remove %d exact duplicate row(s).", n)
    },
    apply = function(df) df[!duplicated(df), , drop = FALSE]
  )
)

# Issues present in df, in spec order: list of list(id, desc, default).
detect_issues <- function(df) {
  specs <- clean_specs()
  out   <- list()
  for (id in names(specs)) {
    d <- specs[[id]]$detect(df)
    if (!is.null(d)) out[[id]] <- list(id = id, desc = d, default = specs[[id]]$default)
  }
  out
}

# Apply the selected fix ids (always in canonical spec order).
clean_apply <- function(df, ids) {
  specs <- clean_specs()
  for (id in names(specs)) if (id %in% ids) df <- specs[[id]]$apply(df)
  df
}

# ----------------------------------------------------------
# Chart-suitability helpers — keep questionable variable/chart
# pairings from producing the "strange-looking" plots that come
# from feeding the wrong data type into a chart.
# ----------------------------------------------------------

# Column-type predicates.
is_discrete_col <- function(x) is.character(x) || is.factor(x) || is.logical(x)
is_date_col     <- function(x) inherits(x, c("Date", "POSIXct", "POSIXt"))

# A discrete x-axis with many or long labels gets its ticks angled so they
# stay legible instead of overlapping into mush.
needs_x_rotation <- function(df, pt, xv) {
  if (is.null(xv) || pt == "pie" || !xv %in% names(df)) return(FALSE)
  x <- df[[xv]]
  if (!(pt %in% c("bar", "boxplot") || is_discrete_col(x))) return(FALSE)
  uvals <- unique(as.character(x))
  length(uvals) > 8 || max(nchar(uvals), 0L) > 10
}

# Keep the top `n_keep` categories (by count, or by summed weight `w` when a Y
# variable is present) and roll everything else into a single "Other" bar.
lump_bar_x <- function(df, xv, w, n_keep) {
  x   <- as.character(df[[xv]])
  wt  <- if (is.null(w)) rep(1, length(x)) else w
  tot <- sort(tapply(wt, x, function(z) sum(z, na.rm = TRUE)), decreasing = TRUE)
  keep <- names(tot)[seq_len(min(n_keep, length(tot)))]
  x[!x %in% keep] <- "Other"
  df[[xv]] <- factor(x, levels = unique(c(keep, "Other")))
  df
}

# Returns an HTML warning when the chosen variable doesn't suit the chosen
# chart type, or NULL when the pairing is fine. Shown inline under the variable
# pickers so students learn *why* a chart looks off rather than just seeing a
# mess (or an empty plot).
chart_hint <- function(df, p) {
  if (is.null(df) || is.null(p$type) || is.null(p$x) || !nzchar(p$x)) return(NULL)
  if (!p$x %in% names(df)) return(NULL)
  pt <- p$type
  xv <- p$x
  x  <- df[[xv]]
  n_x    <- dplyr::n_distinct(x, na.rm = TRUE)
  cont_x <- is.numeric(x) && !is_date_col(x) && n_x > 10

  if (pt %in% c("scatter", "line") && is_discrete_col(x))
    return(sprintf("<b>%s</b> is categorical. %s charts read best with a numeric or date X &mdash; a <b>box plot</b> or <b>bar chart</b> may show this better.",
                   xv, tools::toTitleCase(pt)))
  if (pt == "bar" && cont_x)
    return(sprintf("<b>%s</b> looks continuous (%s distinct values), so a bar chart draws many thin bars. A <b>histogram</b> is usually the better choice for a numeric variable.",
                   xv, format(n_x, big.mark = ",")))
  if (pt == "boxplot" && cont_x)
    return(sprintf("<b>%s</b> looks continuous, so you'll get one box per value. Box plots group a numeric Y by a <b>categorical</b> X.", xv))
  if (pt == "pie" && cont_x)
    return(sprintf("<b>%s</b> looks continuous, which makes an unreadable pie. Pie charts need a <b>categorical</b> variable with a handful of values.", xv))
  barlim <- p$cat_limit %||% BAR_MAX
  if (pt == "bar" && is_discrete_col(x) && n_x > barlim)
    return(sprintf("<b>%s</b> has %s categories; only the largest %d are shown (the rest grouped as &ldquo;Other&rdquo;). Use the &ldquo;Maximum bars&rdquo; slider to show more or fewer.",
                   xv, format(n_x, big.mark = ","), barlim))
  NULL
}

# ----------------------------------------------------------
# Palettes for the group-color picker. Discrete palettes are
# recycled / ramped so they never run out of colours.
# ----------------------------------------------------------

PALETTES <- c("Automatic" = "auto", "UF Brand" = "uf", "Viridis" = "viridis",
              "Colorblind-safe" = "cb", "ColorBrewer Set1" = "set1",
              "ColorBrewer Set2" = "set2", "Greyscale" = "greys")

okabe_ito  <- function(n) rep_len(
  c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
    "#0072B2", "#D55E00", "#CC79A7", "#000000"), n)

uf_discrete <- function(n) {
  base <- c(UF_BLUE, UF_ORANGE, "#2ca25f", "#8856a7",
            "#e6550d", "#3182bd", "#31a354", "#756bb1")
  if (n <= length(base)) base[seq_len(n)]
  else grDevices::colorRampPalette(c(UF_BLUE, UF_ORANGE))(n)
}

# Returns the color+fill scales for the chosen group palette. Both aesthetics
# are returned so the geom picks up whichever it uses.
group_scales <- function(df, cv, palette) {
  is_cont <- is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) > 10
  n       <- dplyr::n_distinct(df[[cv]])
  if (palette == "auto" || is.null(palette)) {
    if (is_cont)          return(list(scale_color_viridis_c(), scale_fill_viridis_c()))
    if (n > BREWER_MAX)   return(list(scale_color_viridis_d(), scale_fill_viridis_d()))
    return(list(scale_color_brewer(palette = "Set1"), scale_fill_brewer(palette = "Set1")))
  }
  if (is_cont) {
    if (palette == "uf")
      return(list(scale_color_gradient(low = UF_BLUE, high = UF_ORANGE),
                  scale_fill_gradient(low = UF_BLUE, high = UF_ORANGE)))
    return(list(scale_color_viridis_c(), scale_fill_viridis_c()))
  }
  # Set1/Set2 produce NA fills past their size, so fall back to viridis.
  if (palette %in% c("set1", "set2") && n > BREWER_MAX)
    return(list(scale_color_viridis_d(), scale_fill_viridis_d()))
  switch(palette,
    uf      = list(scale_color_manual(values = uf_discrete(n)),
                   scale_fill_manual(values  = uf_discrete(n))),
    viridis = list(scale_color_viridis_d(), scale_fill_viridis_d()),
    cb      = list(scale_color_manual(values = okabe_ito(n)),
                   scale_fill_manual(values  = okabe_ito(n))),
    set1    = list(scale_color_brewer(palette = "Set1"), scale_fill_brewer(palette = "Set1")),
    set2    = list(scale_color_brewer(palette = "Set2"), scale_fill_brewer(palette = "Set2")),
    greys   = list(scale_color_grey(start = 0.2, end = 0.75),
                   scale_fill_grey(start = 0.2, end = 0.75)),
    list(scale_color_brewer(palette = "Set1"), scale_fill_brewer(palette = "Set1")))
}

# Single fill scale for pie slices. "Automatic" keeps the historical Set2 /
# viridis behaviour; the other names mirror the group palette picker.
pie_fill_scale <- function(palette, n, name) {
  if (palette %in% c("auto", "") || is.null(palette))
    return(if (n <= BREWER_MAX) scale_fill_brewer(palette = "Set2", name = name)
           else                 scale_fill_viridis_d(name = name))
  if (palette %in% c("set1", "set2") && n > BREWER_MAX)
    return(scale_fill_viridis_d(name = name))
  switch(palette,
    uf      = scale_fill_manual(values = uf_discrete(n), name = name),
    viridis = scale_fill_viridis_d(name = name),
    cb      = scale_fill_manual(values = okabe_ito(n), name = name),
    set1    = scale_fill_brewer(palette = "Set1", name = name),
    set2    = scale_fill_brewer(palette = "Set2", name = name),
    greys   = scale_fill_grey(start = 0.2, end = 0.75, name = name),
    scale_fill_brewer(palette = "Set2", name = name))
}

# Code-snippet form of group_scales() for a single aesthetic ("color"/"fill").
palette_code <- function(palette, aes_fn, is_cont, n) {
  s <- function(suffix, args = "") sprintf("scale_%s_%s(%s)", aes_fn, suffix, args)
  vals <- function(cols) sprintf('values = c(%s)', paste(sprintf('"%s"', cols), collapse = ", "))
  if (palette == "auto" || is.null(palette)) {
    if (is_cont)        return(s("viridis_c"))
    if (n > BREWER_MAX) return(s("viridis_d"))
    return(s("brewer", 'palette = "Set1"'))
  }
  if (is_cont) {
    if (palette == "uf") return(s("gradient", sprintf('low = "%s", high = "%s"', UF_BLUE, UF_ORANGE)))
    return(s("viridis_c"))
  }
  if (palette %in% c("set1", "set2") && n > BREWER_MAX) return(s("viridis_d"))
  switch(palette,
    uf      = s("manual", vals(uf_discrete(n))),
    viridis = s("viridis_d"),
    cb      = s("manual", vals(okabe_ito(n))),
    set1    = s("brewer", 'palette = "Set1"'),
    set2    = s("brewer", 'palette = "Set2"'),
    greys   = s("grey", "start = 0.2, end = 0.75"),
    s("brewer", 'palette = "Set1"'))
}

# Builds the "y = a + b·x, R² = ..." annotation for a fitted scatter/line.
trend_label_text <- function(df, xv, yv, meth, deg) {
  d <- data.frame(x = df[[xv]], y = df[[yv]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (!is.numeric(d$x) || !is.numeric(d$y) || nrow(d) < 3) return(NULL)
  if (meth == "loess") {
    fit <- tryCatch(stats::loess(y ~ x, data = d), error = function(e) NULL)
    if (is.null(fit)) return(NULL)
    r2 <- 1 - sum(stats::residuals(fit)^2) / sum((d$y - mean(d$y))^2)
    return(sprintf("loess fit,  R² = %.3f", r2))
  }
  fit <- if (meth == "poly")
           tryCatch(stats::lm(y ~ poly(x, deg, raw = TRUE), data = d), error = function(e) NULL)
         else
           tryCatch(stats::lm(y ~ x, data = d), error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  r2 <- summary(fit)$r.squared
  if (meth == "poly") return(sprintf("polynomial (degree %d),  R² = %.3f", deg, r2))
  co <- stats::coef(fit)
  sprintf("y = %.3g %+.3g·x,  R² = %.3f", co[1], co[2], r2)
}

# ----------------------------------------------------------
# Plot builder — one function used by every plot slot,
# the Visualize previews, and the Export tab.
#
# p is a plain list of settings:
#   type, x, y, color, title, xlab, ylab,
#   theme, color_hex, size, bins, bar_agg, cat_limit,
#   reg_overlay, reg_type, reg_deg, reg_ci, reg_col, trend_label,
#   palette, alpha, jitter, logscale, facet, legend_pos, gridlines, flip
# ----------------------------------------------------------

build_full_plot <- function(df, p) {
  if (is.null(df) || is.null(p$type) || is.null(p$x) || !nzchar(p$x)) return(NULL)
  if (!p$x %in% names(df)) return(NULL)

  pt <- p$type
  xv <- p$x
  yv <- if (!is.null(p$y) && nzchar(p$y) && p$y != "__count__") p$y else NULL
  cv <- if (!is.null(p$color) && nzchar(p$color) && p$color != "__none__") p$color else NULL

  if (pt %in% c("scatter", "line", "boxplot") && is.null(yv)) return(NULL)
  if (!is.null(yv) && !yv %in% names(df)) return(NULL)
  if (pt == "histogram" && !is.numeric(df[[xv]])) return(NULL)

  # Group-by resolution:
  #  - low-cardinality numeric (<= 10 unique) -> treat as categorical so
  #    discrete palettes work (e.g. cyl = 4/6/8).
  #  - bar / histogram can only group by categories; a truly continuous
  #    variable would make one colour per row, so drop it.
  if (!is.null(cv) && cv %in% names(df)) {
    if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) <= 10)
      df[[cv]] <- as.factor(df[[cv]])
    if (pt %in% c("histogram", "bar") && is.numeric(df[[cv]])) cv <- NULL
  } else {
    cv <- NULL
  }

  size  <- p$size %||% 2
  col   <- p$color_hex %||% UF_BLUE
  bins  <- p$bins %||% 30
  alpha <- p$alpha %||% 0.8
  legend_pos <- p$legend_pos %||% "right"
  facet_v <- if (!is.null(p$facet) && nzchar(p$facet) &&
                 p$facet != "__none__" && p$facet %in% names(df)) p$facet else NULL

  title <- if (!is.null(p$title) && nzchar(trimws(p$title))) p$title else NULL
  xlab  <- label_or(p$xlab %||% "", xv)
  ylab  <- if (!is.null(yv)) label_or(p$ylab %||% "", yv) else NULL
  subtitle <- NULL

  smooth_layer <- function() {
    if (!isTRUE(p$reg_overlay)) return(NULL)
    meth <- p$reg_type %||% "lm"
    fml  <- if (meth == "poly")
              stats::as.formula(paste0("y ~ poly(x, ", p$reg_deg %||% 2, ")"))
            else y ~ x
    geom_smooth(
      mapping   = aes(x = .data[[xv]], y = .data[[yv]]),
      method    = if (meth == "poly") "lm" else meth,
      formula   = fml,
      se        = isTRUE(p$reg_ci),
      color     = p$reg_col %||% UF_ORANGE,
      linewidth = 1.1
    )
  }

  p_obj <- NULL

  if (pt == "scatter") {
    aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], y = .data[[yv]], color = .data[[cv]])
             else               aes(x = .data[[xv]], y = .data[[yv]])
    pt_geom <- if (isTRUE(p$jitter)) geom_jitter else geom_point
    p_obj <- ggplot(df, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + pt_geom(size = size, alpha = alpha, color = col)
             else
               p_obj + pt_geom(size = size, alpha = alpha)
    p_obj <- p_obj + smooth_layer()

  } else if (pt == "line") {
    # Lines connect points in row order, so an unsorted file draws a scribble.
    df <- df[order(df[[xv]]), , drop = FALSE]
    aes_m <- if (!is.null(cv))
               aes(x = .data[[xv]], y = .data[[yv]], color = .data[[cv]], group = .data[[cv]])
             else
               aes(x = .data[[xv]], y = .data[[yv]], group = 1)
    p_obj <- ggplot(df, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + geom_line(linewidth = size * 0.4, color = col) +
                       geom_point(size = size * 0.7,     color = col)
             else
               p_obj + geom_line(linewidth = size * 0.4) +
                       geom_point(size = size * 0.7)
    p_obj <- p_obj + smooth_layer()

  } else if (pt == "bar") {
    has_y <- !is.null(yv)
    bw    <- bar_width(size)
    # Too many categories make an unreadable picket fence: keep the biggest
    # `barmax` and roll the rest into "Other" (only for genuine categories;
    # a continuous numeric x is flagged by chart_hint instead). The user sets
    # barmax with the "Maximum bars" slider; it defaults to BAR_MAX.
    barmax <- p$cat_limit %||% BAR_MAX
    if (is_discrete_col(df[[xv]]) && dplyr::n_distinct(df[[xv]]) > barmax) {
      df <- lump_bar_x(df, xv, if (has_y) df[[yv]] else NULL, barmax)
      subtitle <- sprintf("Showing the %d largest categories; the rest are grouped as “Other”.", barmax)
    }
    if (has_y) {
      # Aggregate values per category (and group) so repeated x-values don't
      # stack opaque bars on top of each other.
      grp <- c(xv, cv)
      pdat <- df |>
        dplyr::group_by(dplyr::across(dplyr::all_of(grp))) |>
        dplyr::summarise(.value = agg_fun(p$bar_agg)(.data[[yv]]), .groups = "drop")
      aes_m <- if (!is.null(cv))
                 aes(x = .data[[xv]], y = .data[[".value"]], fill = .data[[cv]])
               else
                 aes(x = .data[[xv]], y = .data[[".value"]])
      p_obj <- ggplot(pdat, aes_m)
      p_obj <- if (is.null(cv))
                 p_obj + geom_col(fill = col, width = bw, alpha = alpha)
               else
                 p_obj + geom_col(width = bw, alpha = alpha, position = "dodge")
      ylab <- label_or(p$ylab %||% "",
                       paste0(tools::toTitleCase(p$bar_agg %||% "sum"), " of ", yv))
    } else {
      aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], fill = .data[[cv]])
               else               aes(x = .data[[xv]])
      p_obj <- ggplot(df, aes_m)
      p_obj <- if (is.null(cv))
                 p_obj + geom_bar(stat = "count", fill = col, width = bw, alpha = alpha)
               else
                 p_obj + geom_bar(stat = "count", width = bw, alpha = alpha, position = "dodge")
      ylab <- "Count"
    }

  } else if (pt == "histogram") {
    if (!is.null(cv)) {
      p_obj <- ggplot(df, aes(x = .data[[xv]], fill = .data[[cv]])) +
               geom_histogram(bins = bins, color = "white", alpha = alpha, position = "dodge")
    } else {
      p_obj <- ggplot(df, aes(x = .data[[xv]])) +
               geom_histogram(bins = bins, color = "white", fill = col, alpha = alpha)
    }
    ylab <- "Count"

  } else if (pt == "boxplot") {
    aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], y = .data[[yv]], fill = .data[[cv]])
             else               aes(x = .data[[xv]], y = .data[[yv]])
    p_obj <- ggplot(df, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + geom_boxplot(fill = col, alpha = alpha,
                                    outlier.size = size * 0.7, outlier.alpha = 0.6)
             else
               p_obj + geom_boxplot(alpha = alpha,
                                    outlier.size = size * 0.7, outlier.alpha = 0.6)

  } else if (pt == "pie") {
    use_count <- is.null(yv)
    pie_df <- if (use_count) {
      df |> count(.data[[xv]], name = "val_")
    } else {
      df |> group_by(.data[[xv]]) |>
            summarise(val_ = sum(.data[[yv]], na.rm = TRUE), .groups = "drop")
    }
    names(pie_df)[1] <- "cat_"
    pie_df$cat_ <- as.character(pie_df$cat_)

    # Too many slices are unreadable: keep the biggest (pielim - 1) and roll the
    # rest into a single "Other" slice. pielim comes from the "Maximum slices"
    # slider and defaults to PIE_MAX.
    pielim <- p$cat_limit %||% PIE_MAX
    lumped <- nrow(pie_df) > pielim
    if (lumped) {
      pie_df <- pie_df[order(pie_df$val_, decreasing = TRUE), ]
      keep   <- pie_df[seq_len(pielim - 1), ]
      other  <- data.frame(cat_ = "Other", val_ = sum(pie_df$val_[-seq_len(pielim - 1)]))
      pie_df <- rbind(keep, other)
    }
    pie_df$cat_ <- factor(pie_df$cat_, levels = pie_df$cat_)
    # Only label slices big enough to read; the legend covers the rest.
    pct <- pie_df$val_ / sum(pie_df$val_) * 100
    pie_df$label_ <- ifelse(pct >= 5, paste0(pie_df$cat_, "\n", round(pct, 1), "%"), "")

    n_slices   <- nrow(pie_df)
    fill_scale <- pie_fill_scale(p$palette %||% "auto", n_slices, xv)
    subtitle <- if (lumped)
      paste0("Showing the ", pielim - 1, " largest categories; the rest are grouped as “Other”.")
    else NULL

    return(
      ggplot(pie_df, aes(x = "", y = val_, fill = cat_)) +
        geom_col(width = 1, color = "white", linewidth = 0.5) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = label_), position = position_stack(vjust = 0.5),
                  size = 3.5, color = "white", fontface = "bold") +
        fill_scale +
        labs(title = title, subtitle = subtitle) +
        theme_void(base_size = 13) +
        theme(plot.title    = element_text(hjust = 0.5, face = "bold", size = 14),
              plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#666"),
              legend.position = legend_pos, legend.title = element_text(size = 11))
    )
  }

  if (is.null(p_obj)) return(NULL)

  # Group color palette (user-selectable, recycled so it never runs out).
  if (!is.null(cv))
    for (s in group_scales(df, cv, p$palette %||% "auto")) p_obj <- p_obj + s

  # Fitted-equation / R² annotation for an overlaid scatter or line.
  if (isTRUE(p$reg_overlay) && isTRUE(p$trend_label) &&
      pt %in% c("scatter", "line") && !is.null(yv)) {
    lab <- trend_label_text(df, xv, yv, p$reg_type %||% "lm", p$reg_deg %||% 2)
    if (!is.null(lab))
      p_obj <- p_obj + annotate("text", x = -Inf, y = Inf, label = lab,
                                hjust = -0.05, vjust = 1.5, size = 4,
                                color = "#333333", fontface = "italic")
  }

  # Log scales — guarded so they only apply to continuous axes.
  ls <- p$logscale %||% "none"
  if (ls %in% c("x", "both") && is.numeric(df[[xv]]) &&
      pt %in% c("scatter", "line", "histogram"))
    p_obj <- p_obj + scale_x_log10()
  if (ls %in% c("y", "both") &&
      pt %in% c("scatter", "line", "bar", "histogram", "boxplot"))
    p_obj <- p_obj + scale_y_log10()

  # Small multiples — capped so a stray continuous column can't explode panels.
  if (!is.null(facet_v) && dplyr::n_distinct(df[[facet_v]]) <= 30)
    p_obj <- p_obj + facet_wrap(vars(.data[[facet_v]]))

  # Horizontal orientation (best for bar / box with long category labels).
  if (isTRUE(p$flip)) p_obj <- p_obj + coord_flip()

  uses_color <- pt %in% c("scatter", "line")
  base_theme <- theme_call(p$theme, 13) +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold", size = 14,
                                     margin = margin(b = 10)),
      plot.subtitle   = element_text(hjust = 0.5, size = 10, color = "#666"),
      axis.title      = element_text(size = 12),
      legend.title    = element_text(size = 11),
      legend.position = legend_pos
    )
  if (!isTRUE(p$gridlines %||% TRUE))
    base_theme <- base_theme + theme(panel.grid = element_blank())
  if (needs_x_rotation(df, pt, xv) && !isTRUE(p$flip))
    base_theme <- base_theme + theme(axis.text.x = element_text(angle = 40, hjust = 1))

  p_obj + base_theme + labs(
    title = title, subtitle = subtitle, x = xlab, y = ylab,
    color = if (uses_color) cv else NULL,
    fill  = if (!uses_color) cv else NULL
  )
}

# ----------------------------------------------------------
# Code generator — emits a runnable ggplot2 snippet that
# reproduces build_full_plot() for the same settings.
# ----------------------------------------------------------

bq <- function(n) {
  if (is.null(n) || !nzchar(n)) return(n)
  if (make.names(n) == n) n else sprintf('`%s`', n)
}
qq <- function(s) sprintf('"%s"', gsub('"', '\\\\"', s))

generate_code <- function(df, p) {
  if (is.null(df) || is.null(p$type) || is.null(p$x) || !nzchar(p$x))
    return("# Choose a chart type and an X variable to generate R code.")

  pt <- p$type
  xv <- p$x
  yv <- if (!is.null(p$y) && nzchar(p$y) && p$y != "__count__") p$y else NULL
  cv <- if (!is.null(p$color) && nzchar(p$color) && p$color != "__none__") p$color else NULL
  facet_v <- if (!is.null(p$facet) && nzchar(p$facet) &&
                 p$facet != "__none__" && p$facet %in% names(df)) p$facet else NULL

  if (pt %in% c("scatter", "line", "boxplot") && is.null(yv))
    return("# Select a Y variable to generate code for this chart type.")

  pre <- character(0)
  if (!is.null(cv) && cv %in% names(df)) {
    if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) <= 10)
      pre <- c(pre, sprintf('df[["%s"]] <- as.factor(df[["%s"]])', cv, cv))
    if (pt %in% c("histogram", "bar") && is.numeric(df[[cv]]) &&
        dplyr::n_distinct(df[[cv]]) > 10) cv <- NULL
  } else {
    cv <- NULL
  }

  if (pt == "line")
    pre <- c(pre, sprintf('df <- df[order(df[["%s"]]), ]  # lines connect points in row order', xv))
  barmax <- p$cat_limit %||% BAR_MAX
  if (pt == "bar" && is_discrete_col(df[[xv]]) && dplyr::n_distinct(df[[xv]]) > barmax)
    pre <- c(pre, sprintf("# The app showed only the top %d categories of '%s'; this code plots them all.",
                          barmax, xv),
                  sprintf("# To match it, lump the rest: df[[\"%s\"]] <- forcats::fct_lump_n(df[[\"%s\"]], %d)",
                          xv, xv, barmax))

  size  <- p$size %||% 2
  col   <- p$color_hex %||% UF_BLUE
  bins  <- p$bins %||% 30
  agg   <- p$bar_agg %||% "sum"
  alpha <- round(p$alpha %||% 0.8, 2)
  theme_str <- switch(p$theme %||% "minimal",
    minimal = "theme_minimal()", classic = "theme_classic()",
    light   = "theme_light()",   bw      = "theme_bw()",
    dark    = "theme_dark()",    "theme_minimal()")

  title <- if (!is.null(p$title) && nzchar(trimws(p$title))) p$title else NULL
  xlab  <- label_or(p$xlab %||% "", xv)
  ylab  <- if (!is.null(yv)) label_or(p$ylab %||% "", yv) else NULL

  uses_color  <- pt %in% c("scatter", "line")
  needs_dplyr <- (pt == "bar" && !is.null(yv)) || pt == "pie"

  scale_line <- NULL
  if (!is.null(cv))
    scale_line <- palette_code(
      p$palette %||% "auto",
      if (uses_color) "color" else "fill",
      is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) > 10,
      dplyr::n_distinct(df[[cv]]))

  smooth_line <- NULL
  if (isTRUE(p$reg_overlay) && pt %in% c("scatter", "line")) {
    meth <- p$reg_type %||% "lm"
    se   <- if (isTRUE(p$reg_ci)) "TRUE" else "FALSE"
    rcol <- p$reg_col %||% UF_ORANGE
    smooth_line <- if (meth == "poly")
      sprintf('geom_smooth(method = "lm", formula = y ~ poly(x, %s), se = %s, color = %s, linewidth = 1.1)',
              p$reg_deg %||% 2, se, qq(rcol))
    else
      sprintf('geom_smooth(method = "%s", formula = y ~ x, se = %s, color = %s, linewidth = 1.1)',
              meth, se, qq(rcol))
  }

  # ── labs() ────────────────────────────────────────────────
  labs_parts <- character(0)
  if (!is.null(title)) labs_parts <- c(labs_parts, sprintf("title = %s", qq(title)))

  # ── per-type body ─────────────────────────────────────────
  data_obj <- "df"
  if (pt == "pie") {
    if (is.null(yv)) {
      pre <- c(pre, sprintf('plot_df <- dplyr::count(df, %s, name = "val")', bq(xv)))
    } else {
      pre <- c(pre, sprintf('plot_df <- dplyr::summarise(dplyr::group_by(df, %s), val = sum(%s, na.rm = TRUE), .groups = "drop")',
                            bq(xv), bq(yv)))
    }
    pre <- c(pre, sprintf('plot_df[["%s"]] <- as.factor(plot_df[["%s"]])', xv, xv))
    pal_pie <- p$palette %||% "auto"
    nslice  <- dplyr::n_distinct(df[[xv]])
    pie_fill <- if (pal_pie %in% c("auto", "")) {
                  if (nslice <= BREWER_MAX) 'scale_fill_brewer(palette = "Set2")'
                  else                       'scale_fill_viridis_d()'
                } else palette_code(pal_pie, "fill", FALSE, nslice)
    lp <- p$legend_pos %||% "right"
    code <- paste0(
      sprintf('ggplot(plot_df, aes(x = "", y = val, fill = %s)) +', bq(xv)),
      '\n  geom_col(width = 1, color = "white") +',
      '\n  coord_polar("y") +',
      '\n  ', pie_fill, ' +',
      '\n  theme_void()',
      if (!identical(lp, "right")) sprintf(' +\n  theme(legend.position = "%s")', lp) else '',
      if (!is.null(title)) paste0(' +\n  labs(title = ', qq(title), ')') else ''
    )
    return(assemble_code(pre, code, needs_dplyr))
  }

  # aes()
  aes_inner <- sprintf("x = %s", bq(xv))
  if (pt == "bar" && !is.null(yv)) aes_inner <- paste0(aes_inner, ", y = .value")
  else if (!is.null(yv))           aes_inner <- paste0(aes_inner, sprintf(", y = %s", bq(yv)))
  if (!is.null(cv)) aes_inner <- paste0(
    aes_inner, sprintf(", %s = %s", if (uses_color) "color" else "fill", bq(cv)))
  if (pt == "line" && is.null(cv)) aes_inner <- paste0(aes_inner, ", group = 1")
  if (pt == "line" && !is.null(cv)) aes_inner <- paste0(aes_inner, sprintf(", group = %s", bq(cv)))

  # bar with aggregation needs a pre-aggregated data frame
  if (pt == "bar" && !is.null(yv)) {
    grp_cols <- paste(c(bq(xv), if (!is.null(cv)) bq(cv)), collapse = ", ")
    aggfn <- switch(agg, mean = "mean", median = "median", "sum")
    pre <- c(pre, sprintf(
      'plot_df <- dplyr::summarise(dplyr::group_by(df, %s), .value = %s(%s, na.rm = TRUE), .groups = "drop")',
      grp_cols, aggfn, bq(yv)))
    data_obj <- "plot_df"
    ylab <- label_or(p$ylab %||% "", paste0(tools::toTitleCase(agg), " of ", yv))
  }

  pt_fn <- if (isTRUE(p$jitter)) "geom_jitter" else "geom_point"
  geom_lines <- switch(pt,
    scatter = if (is.null(cv))
                sprintf('%s(size = %s, alpha = %s, color = %s)', pt_fn, size, alpha, qq(col))
              else
                sprintf('%s(size = %s, alpha = %s)', pt_fn, size, alpha),
    line    = if (is.null(cv))
                sprintf('geom_line(linewidth = %s, color = %s) +\n  geom_point(size = %s, color = %s)',
                        size * 0.4, qq(col), size * 0.7, qq(col))
              else
                sprintf('geom_line(linewidth = %s) +\n  geom_point(size = %s)',
                        size * 0.4, size * 0.7),
    bar     = if (!is.null(yv)) {
                if (is.null(cv))
                  sprintf('geom_col(fill = %s, width = %s, alpha = %s)', qq(col), round(bar_width(size), 3), alpha)
                else
                  sprintf('geom_col(width = %s, alpha = %s, position = "dodge")', round(bar_width(size), 3), alpha)
              } else {
                if (is.null(cv))
                  sprintf('geom_bar(fill = %s, width = %s, alpha = %s)', qq(col), round(bar_width(size), 3), alpha)
                else
                  sprintf('geom_bar(width = %s, alpha = %s, position = "dodge")', round(bar_width(size), 3), alpha)
              },
    histogram = if (is.null(cv))
                  sprintf('geom_histogram(bins = %s, color = "white", fill = %s, alpha = %s)', bins, qq(col), alpha)
                else
                  sprintf('geom_histogram(bins = %s, color = "white", alpha = %s, position = "dodge")', bins, alpha),
    boxplot = if (is.null(cv))
                sprintf('geom_boxplot(fill = %s, alpha = %s)', qq(col), alpha)
              else
                sprintf('geom_boxplot(alpha = %s)', alpha)
  )

  if (pt %in% c("bar", "histogram") && is.null(yv)) ylab <- "Count"
  if (pt == "histogram") ylab <- "Count"

  labs_parts <- c(labs_parts, sprintf("x = %s", qq(xlab)))
  if (!is.null(ylab)) labs_parts <- c(labs_parts, sprintf("y = %s", qq(ylab)))
  if (!is.null(cv))
    labs_parts <- c(labs_parts, sprintf("%s = %s", if (uses_color) "color" else "fill", qq(cv)))

  lines <- c(sprintf("ggplot(%s, aes(%s))", data_obj, aes_inner))
  lines <- c(lines, geom_lines)
  if (!is.null(smooth_line)) lines <- c(lines, smooth_line)
  if (!is.null(scale_line))  lines <- c(lines, scale_line)

  if (isTRUE(p$reg_overlay) && isTRUE(p$trend_label) &&
      pt %in% c("scatter", "line") && !is.null(yv)) {
    tl <- trend_label_text(df, xv, yv, p$reg_type %||% "lm", p$reg_deg %||% 2)
    if (!is.null(tl))
      lines <- c(lines, sprintf(
        'annotate("text", x = -Inf, y = Inf, label = %s, hjust = -0.05, vjust = 1.5, size = 4, color = "#333333", fontface = "italic")',
        qq(tl)))
  }

  ls <- p$logscale %||% "none"
  if (ls %in% c("x", "both") && is.numeric(df[[xv]]) &&
      pt %in% c("scatter", "line", "histogram"))
    lines <- c(lines, "scale_x_log10()")
  if (ls %in% c("y", "both") &&
      pt %in% c("scatter", "line", "bar", "histogram", "boxplot"))
    lines <- c(lines, "scale_y_log10()")

  if (!is.null(facet_v)) lines <- c(lines, sprintf("facet_wrap(vars(%s))", bq(facet_v)))
  if (isTRUE(p$flip))    lines <- c(lines, "coord_flip()")

  lines <- c(lines, theme_str)

  theme_args <- character(0)
  lp <- p$legend_pos %||% "right"
  if (!identical(lp, "right"))            theme_args <- c(theme_args, sprintf('legend.position = "%s"', lp))
  if (!isTRUE(p$gridlines %||% TRUE))     theme_args <- c(theme_args, "panel.grid = element_blank()")
  if (needs_x_rotation(df, pt, xv) && !isTRUE(p$flip))
    theme_args <- c(theme_args, "axis.text.x = element_text(angle = 40, hjust = 1)")
  if (length(theme_args))
    lines <- c(lines, sprintf("theme(%s)", paste(theme_args, collapse = ", ")))

  lines <- c(lines, sprintf("labs(%s)", paste(labs_parts, collapse = ", ")))

  code <- paste0(lines[1], " +\n  ",
                 paste(lines[-1], collapse = " +\n  "))
  assemble_code(pre, code, needs_dplyr)
}

assemble_code <- function(pre, code, needs_dplyr) {
  head_lines <- c(
    "library(ggplot2)",
    if (needs_dplyr) "library(dplyr)",
    "",
    "# Replace `df` with your own data frame, e.g.:",
    '# df <- read.csv("your_data.csv")',
    ""
  )
  if (length(pre)) pre <- c(pre, "")
  paste(c(head_lines, pre, code), collapse = "\n")
}

# ----------------------------------------------------------
# Multi-plot drawing / export (base grid — no extra packages)
# ----------------------------------------------------------

draw_plot_grid <- function(plots) {
  n <- length(plots)
  if (n == 0) {
    plot.new()
    text(0.5, 0.5, "No plots to show.\nConfigure plots in the Visualize tab.",
         cex = 1.3, col = "gray40")
    return(invisible(NULL))
  }
  ncols <- if (n == 1) 1L else 2L
  nrows <- ceiling(n / ncols)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(nrows, ncols)))
  for (i in seq_along(plots)) {
    r  <- ceiling(i / ncols)
    cc <- ((i - 1L) %% ncols) + 1L
    print(plots[[i]], vp = grid::viewport(layout.pos.row = r, layout.pos.col = cc))
  }
  invisible(NULL)
}

render_plots_to_file <- function(plots, file, fmt, w_each, h_each, dpi) {
  n     <- max(1L, length(plots))
  ncols <- if (n <= 1) 1L else 2L
  nrows <- ceiling(n / ncols)
  W <- w_each * ncols
  H <- h_each * nrows
  switch(fmt,
    png = grDevices::png(file, width = W, height = H, units = "in", res = dpi),
    pdf = grDevices::cairo_pdf(file, width = W, height = H),
    svg = grDevices::svg(file, width = W, height = H),
    grDevices::png(file, width = W, height = H, units = "in", res = dpi)
  )
  on.exit(grDevices::dev.off())
  draw_plot_grid(plots)
}

# ----------------------------------------------------------
# UI builder for one plot's controls (used inside the
# dynamically rendered Visualize accordion).
# ----------------------------------------------------------

plot_slot_panel <- function(i) {
  accordion_panel(
    paste("Plot", i),
    value = paste0("panel", i),
    selectInput(paste0("mp", i, "_type"), "Chart Type",
                choices = c("Scatter Plot" = "scatter", "Line Graph" = "line",
                            "Bar Chart" = "bar", "Histogram" = "histogram",
                            "Box Plot" = "boxplot", "Pie Chart" = "pie")),
    uiOutput(paste0("ui_mp", i, "_x")),
    uiOutput(paste0("ui_mp", i, "_y")),
    uiOutput(paste0("ui_mp", i, "_color")),
    uiOutput(paste0("ui_mp", i, "_hint")),
    conditionalPanel(
      sprintf("input.mp%d_type == 'bar'", i),
      selectInput(paste0("mp", i, "_baragg"),
                  tags$span("Bar Aggregation",
                            info_icon("How to combine Y values within each category. Ignored when no Y variable is set (then bars show counts).")),
                  choices = c("Sum" = "sum", "Mean" = "mean", "Median" = "median"))
    ),
    conditionalPanel(
      sprintf("input.mp%d_type == 'histogram'", i),
      sliderInput(paste0("mp", i, "_bins"), "Bins", min = 5, max = 60, value = 30, step = 1)
    ),
    # Max bars / slices (rendered for bar & pie, sized to the data).
    uiOutput(paste0("ui_mp", i, "_catlimit")),
    tags$hr(),
    tags$h6("Labels"),
    textInput(paste0("mp", i, "_title"), "Title",        placeholder = "(optional)"),
    # Pie charts have no axes, so axis labels don't apply.
    conditionalPanel(
      sprintf("input.mp%d_type != 'pie'", i),
      textInput(paste0("mp", i, "_xlab"),  "X-Axis Label", placeholder = "auto"),
      textInput(paste0("mp", i, "_ylab"),  "Y-Axis Label", placeholder = "auto")
    ),
    # Theme / default color / size are meaningless for a pie (it uses a fixed
    # legend-driven layout and the palette controls its colors), so hide them.
    conditionalPanel(
      sprintf("input.mp%d_type != 'pie'", i),
      tags$hr(),
      tags$h6("Style"),
      selectInput(paste0("mp", i, "_theme"), "Theme",
                  choices = c("Minimal" = "minimal", "Classic" = "classic",
                              "Light" = "light", "B&W" = "bw", "Dark" = "dark")),
      colourInput(paste0("mp", i, "_color"), "Default Color",
                  value = MP_COLORS[((i - 1) %% length(MP_COLORS)) + 1]),
      sliderInput(paste0("mp", i, "_size"), "Point / Bar Size",
                  min = 0.5, max = 5, value = 2, step = 0.5)
    ),
    conditionalPanel(
      sprintf("input.mp%d_type == 'scatter' || input.mp%d_type == 'line'", i, i),
      tags$hr(),
      tags$h6("Regression Overlay"),
      checkboxInput(paste0("mp", i, "_reg"), "Add Fitted Line", FALSE),
      conditionalPanel(
        sprintf("input.mp%d_reg == true", i),
        selectInput(paste0("mp", i, "_regtype"), "Method",
                    choices = c("Linear (lm)" = "lm", "Polynomial" = "poly", "Loess" = "loess")),
        conditionalPanel(
          sprintf("input.mp%d_regtype == 'poly'", i),
          sliderInput(paste0("mp", i, "_regdeg"), "Polynomial Degree",
                      min = 2, max = 6, value = 2)
        ),
        checkboxInput(paste0("mp", i, "_regci"), "Show 95% CI Band", TRUE),
        colourInput(paste0("mp", i, "_regcol"), "Line Color", value = UF_ORANGE),
        checkboxInput(paste0("mp", i, "_trendlab"),
                      tags$span("Show equation & R² on plot",
                                info_icon("Annotates the chart with the fitted equation (or model type) and its R².")),
                      FALSE)
      )
    ),
    # Less-common appearance settings live in a collapsed panel so the main
    # controls stay uncluttered.
    accordion(
      open = FALSE,
      accordion_panel(
        icon("sliders"), " Advanced options",
        value = paste0("adv", i),
        selectInput(paste0("mp", i, "_palette"),
                    tags$span("Color Palette",
                              info_icon("Colors for grouped charts and for the slices of a pie chart. “Automatic” keeps the built-in choice; “Colorblind-safe” uses the Okabe–Ito palette.")),
                    choices = PALETTES),
        # Opacity / log scale / gridlines don't apply to a pie chart.
        conditionalPanel(
          sprintf("input.mp%d_type != 'pie'", i),
          sliderInput(paste0("mp", i, "_alpha"), "Opacity",
                      min = 0.1, max = 1, value = 0.8, step = 0.05)
        ),
        conditionalPanel(
          sprintf("input.mp%d_type == 'scatter'", i),
          checkboxInput(paste0("mp", i, "_jitter"),
                        tags$span("Jitter points",
                                  info_icon("Nudges overlapping points apart so dense scatters stay readable.")),
                        FALSE)
        ),
        conditionalPanel(
          sprintf("input.mp%d_type != 'pie'", i),
          selectInput(paste0("mp", i, "_logscale"),
                      tags$span("Log Scale",
                                info_icon("Log10-transforms an axis — useful for skewed or wide-ranging values. Applied only to continuous axes.")),
                      choices = c("None" = "none", "X axis" = "x", "Y axis" = "y", "Both" = "both"))
        ),
        uiOutput(paste0("ui_mp", i, "_facet")),
        selectInput(paste0("mp", i, "_legendpos"), "Legend Position",
                    choices = c("Right" = "right", "Bottom" = "bottom",
                                "Top" = "top", "Hidden" = "none")),
        conditionalPanel(
          sprintf("input.mp%d_type == 'bar' || input.mp%d_type == 'boxplot'", i, i),
          checkboxInput(paste0("mp", i, "_flip"),
                        tags$span("Horizontal orientation",
                                  info_icon("Flips the chart on its side — handy when category labels are long or numerous.")),
                        FALSE)
        ),
        conditionalPanel(
          sprintf("input.mp%d_type != 'pie'", i),
          checkboxInput(paste0("mp", i, "_grid"), "Show gridlines", TRUE)
        )
      )
    )
  )
}

copy_js <- "
function DEcopy(id, btn){
  var el = document.getElementById(id);
  if(!el) return;
  var txt = el.innerText || el.textContent || '';
  navigator.clipboard.writeText(txt).then(function(){
    if(btn){ var o = btn.innerHTML; btn.innerHTML = 'Copied!'; setTimeout(function(){ btn.innerHTML = o; }, 1200); }
  });
}
"

# ============================================================
# UI
# ============================================================

ui <- page_navbar(
  title = "Data Explorer",
  theme = bs_theme(
    bootswatch  = "flatly",
    primary     = UF_BLUE,
    secondary   = UF_ORANGE,
    font_scale  = 0.95,
    "navbar-bg" = UF_BLUE
  ),
  window_title = "Data Explorer",
  header = tags$head(tags$script(HTML(copy_js))),
  # Only the plot-heavy tabs need to fill the viewport. Letting Import Data
  # and Glossary scroll like a normal page keeps the Data Health card from
  # being squeezed below the data preview.
  fillable = c("Visualize", "Regression", "Export"),

  # ──────────────────────────────────────────────────────────
  # TAB 1 — Import Data
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Import Data",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        h5("Upload a File"),
        fileInput(
          "file", NULL,
          accept      = c(".csv", ".tsv", ".txt", ".xlsx", ".xls", ".rds"),
          buttonLabel = "Browse...",
          placeholder = "CSV, Excel, TSV, RDS..."
        ),
        hr(),
        h6("Text File Options"),
        checkboxInput("header", "First row is header", TRUE),
        selectInput("sep", "Column separator",
                    choices = c("Comma (,)" = ",", "Semicolon (;)" = ";",
                                "Tab"       = "\t", "Space"        = " ")),
        selectInput("dec", "Decimal point",
                    choices = c("Period (.)" = ".", "Comma (,)" = ",")),
        hr(),
        actionButton("load_example", "Load mtcars Example",
                     class = "btn-outline-primary w-100", icon = icon("table")),
        br(), br(),
        actionButton("clear_data", "Clear Data",
                     class = "btn-outline-danger w-100", icon = icon("trash"))
      ),
      layout_columns(
        card(
          card_header(icon("eye"), " Data Preview"),
          DTOutput("tbl_preview")
        ),
        card(
          card_header(icon("chart-bar"), " Summary Statistics"),
          verbatimTextOutput("tbl_summary")
        ),
        col_widths = c(8, 4)
      ),
      card(
        card_header(icon("broom"), " Data Health"),
        uiOutput("data_health_ui")
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 2 — Visualize (now also covers multi-plot)
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Visualize",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        h5("Chart Settings"),
        radioButtons("n_plots", "Number of plots",
                     choices = c(1, 2, 3, 4), selected = 1, inline = TRUE),
        conditionalPanel(
          "input.n_plots != '1'",
          actionButton("copy_style", "Apply Plot 1 style to all",
                       icon = icon("brush"), class = "btn-outline-secondary btn-sm w-100"),
          tags$div(class = "form-text mb-2",
                   "Copies Plot 1's theme, color, and size to the other plots.")
        ),
        actionButton("reset_plots", "Reset settings to default",
                     icon = icon("rotate-left"), class = "btn-outline-secondary btn-sm w-100"),
        tags$div(class = "form-text mb-2",
                 "Clears every plot's settings. Changing the number of plots no longer resets them."),
        hr(),
        uiOutput("plot_config_accordion")
      ),
      uiOutput("plots_area")
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 3 — Regression
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Regression",
    layout_sidebar(
      sidebar = sidebar(
        width = 290,
        h5("Model Setup"),
        uiOutput("ui_reg_resp"),
        uiOutput("ui_reg_pred"),
        selectInput("reg_type",
          label = tags$span("Model Type",
            info_icon(HTML(
              "<b>Simple Linear:</b> One predictor, straight line (Y ~ X).<br>
               <b>Multiple Linear:</b> Two or more predictors (Y ~ X1 + X2 + ...).<br>
               <b>Polynomial:</b> Curved fit using powers of one predictor (Y ~ X + X² + ...)."
            ))),
          choices = c(
            "Simple Linear"   = "linear",
            "Multiple Linear" = "multiple",
            "Polynomial"      = "polynomial"
          )
        ),
        conditionalPanel(
          condition = "input.reg_type == 'polynomial'",
          sliderInput("poly_deg_reg", "Polynomial Degree", min = 2, max = 6, value = 2)
        ),
        hr(),
        actionButton("btn_fit", "Fit Model",
                     class = "btn-primary w-100", icon = icon("play")),
        br(), br(),
        downloadButton("dl_reg", "Export Summary (.txt)",
                       class = "btn-outline-secondary w-100")
      ),
      layout_columns(
        col_widths = c(5, 7),
        tagList(
          card(
            card_header(icon("file-alt"), " Model Summary"),
            verbatimTextOutput("reg_summary")
          ),
          card(
            card_header(icon("lightbulb"), " Statistical Interpretation"),
            uiOutput("reg_interpretation")
          )
        ),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Fitted vs Actual"),
            plotlyOutput("reg_plot_fitted", height = "300px")
          ),
          card(
            card_header("Residuals vs Fitted"),
            plotlyOutput("reg_plot_resid", height = "300px")
          )
        )
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 4 — Export
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Export",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header(icon("image"), " Export Plots"),
        layout_sidebar(
          sidebar = sidebar(
            width = 250,
            tags$p(class = "text-muted small",
                   "Exports the plots configured on the Visualize tab."),
            conditionalPanel(
              "input.n_plots != '1'",
              radioButtons("exp_mode", "Download as",
                           choices = c("Combined image" = "combined",
                                       "Separate files" = "separate"))
            ),
            selectInput("exp_fmt", "Format",
                        choices = c("PNG" = "png", "PDF" = "pdf", "SVG" = "svg")),
            numericInput("exp_w",   "Width per plot (in)",  value = 7,   min = 2, max = 24, step = 0.5),
            numericInput("exp_h",   "Height per plot (in)", value = 5.5, min = 2, max = 20, step = 0.5),
            numericInput("exp_dpi", "Resolution (DPI)",     value = 150, min = 72, max = 600, step = 50),
            hr(),
            conditionalPanel(
              "input.n_plots == '1' || input.exp_mode == 'combined'",
              downloadButton("dl_combined", "Download", class = "btn-success w-100")
            ),
            conditionalPanel(
              "input.n_plots != '1' && input.exp_mode == 'separate'",
              uiOutput("exp_sep_buttons")
            )
          ),
          plotOutput("exp_preview", height = "470px")
        )
      ),
      card(
        card_header(icon("table"), " Export Data"),
        layout_sidebar(
          sidebar = sidebar(
            width = 240,
            uiOutput("ui_export_vars"),
            hr(),
            downloadButton("dl_csv",  "CSV",   class = "btn-success w-100"),
            br(), br(),
            downloadButton("dl_xlsx", "Excel (.xlsx)", class = "btn-info w-100")
          ),
          DTOutput("exp_data_tbl")
        )
      )
    ),
    card(
      card_header(icon("square-root-variable"), " Export Regression"),
      layout_sidebar(
        sidebar = sidebar(
          width = 260,
          tags$p(class = "text-muted small",
                 "Exports the model fitted on the Regression tab."),
          downloadButton("dl_exp_reg_txt", "Summary (.txt)",      class = "btn-success w-100"),
          br(), br(),
          downloadButton("dl_exp_reg_csv", "Coefficients (.csv)", class = "btn-info w-100")
        ),
        verbatimTextOutput("reg_export_preview")
      )
    )
  ),

  # ──────────────────────────────────────────────────────────
  # TAB 5 — Glossary
  # ──────────────────────────────────────────────────────────
  nav_panel(
    title = "Glossary",
    layout_columns(
      col_widths = c(6, 6),

      tagList(
        card(
          card_header(icon("book"), " Regression & Model Terms"),
          tags$dl(
            class = "px-2",
            tags$dt("Simple Linear Regression"),
            tags$dd("Models the relationship between one predictor (X) and one response (Y) as a straight line: Y = a + b·X. Best when you expect a direct, linear relationship between two variables."),
            tags$hr(),
            tags$dt("Multiple Linear Regression"),
            tags$dd("Extends simple linear regression to two or more predictors: Y = a + b₁X₁ + b₂X₂ + … Each coefficient tells you the effect of one predictor while holding the others constant."),
            tags$hr(),
            tags$dt("Polynomial Regression"),
            tags$dd("Fits a curved (non-linear) relationship by adding powers of the predictor: Y = a + bX + cX² + … Use this when a scatter plot shows a clear curve rather than a straight line."),
            tags$hr(),
            tags$dt("R-squared (R²)"),
            tags$dd("How much of the variation in Y is explained by the model (0–1). R² = 0.85 means the model accounts for 85% of the variability."),
            tags$hr(),
            tags$dt("Adjusted R²"),
            tags$dd("Like R², but penalized for adding extra predictors that don't improve the fit. Better for comparing models with different numbers of variables."),
            tags$hr(),
            tags$dt("F-statistic & its p-value"),
            tags$dd("Tests whether the model as a whole explains a significant amount of variance. A p-value < 0.05 means the overall model is statistically significant."),
            tags$hr(),
            tags$dt("Residuals"),
            tags$dd("Observed value minus predicted value. Ideally scattered randomly around zero — patterns suggest the model is missing something."),
            tags$hr(),
            tags$dt("Confidence Interval (CI)"),
            tags$dd("A 95% CI means: if we repeated the study 100 times, the true value would fall within this range 95 times. A CI for a coefficient that does not include zero indicates significance.")
          )
        )
      ),

      tagList(
        card(
          card_header(icon("book-open"), " Coefficient Table Terms"),
          tags$dl(
            class = "px-2",
            tags$dt("Estimate (Coefficient)"),
            tags$dd("The predicted change in Y for a one-unit increase in X, holding other variables constant. A coefficient of 3.2 for 'weight' means each unit increase in weight is associated with a 3.2-unit change in Y."),
            tags$hr(),
            tags$dt("Std. Error (Standard Error)"),
            tags$dd("The uncertainty around the coefficient estimate. Smaller = more precise. Used to compute the t-value and confidence intervals."),
            tags$hr(),
            tags$dt("t-value"),
            tags$dd("Coefficient divided by its standard error. A larger absolute value (generally > 2) suggests the predictor is statistically significant."),
            tags$hr(),
            tags$dt("p-value (Pr > |t|)"),
            tags$dd("Probability of seeing this result by chance if the predictor had no real effect. p < 0.05 is the conventional threshold for statistical significance."),
            tags$hr(),
            tags$dt("Intercept"),
            tags$dd("The predicted value of Y when all predictors equal zero. Often not directly meaningful on its own, but required for the model equation.")
          )
        ),
        card(
          card_header(icon("circle-info"), " App Settings Explained"),
          tags$dl(
            class = "px-2",
            tags$dt("First row is header"),
            tags$dd("Check this if the first row of your CSV/text file contains column names (e.g. 'weight', 'mpg'). Uncheck if the file starts directly with data values."),
            tags$hr(),
            tags$dt("Column separator"),
            tags$dd("The character used to split columns in your text file. CSV files use commas; TSV files use tabs. If your data looks jumbled after loading, try a different separator."),
            tags$hr(),
            tags$dt("Decimal point"),
            tags$dd("The character used for decimal numbers. Most English-language files use a period (1.5); some European files use a comma (1,5). Choose the one that matches your file."),
            tags$hr(),
            tags$dt("Bar Aggregation"),
            tags$dd("When a bar chart has a Y variable, repeated categories are combined with this function. 'Sum' totals the values, 'Mean' averages them, 'Median' takes the middle value. With no Y variable, bars simply count rows per category."),
            tags$hr(),
            tags$dt("Maximum bars / slices"),
            tags$dd("Bar and pie charts keep this many of the largest categories and group the rest into a single 'Other' bar/slice, so a column with many categories stays readable. Defaults to a sensible cap; slide it up to show more categories (up to the number in your data) or down to simplify."),
            tags$hr(),
            tags$dt("Group Color Palette"),
            tags$dd("The set of colors used when a Color / Group By variable is set. 'Automatic' chooses for you; 'Colorblind-safe' (Okabe–Ito) is the safest for accessibility; 'UF Brand', 'Viridis', 'ColorBrewer', and 'Greyscale' are alternatives. Palettes are recycled so they never run out of colors."),
            tags$hr(),
            tags$dt("Opacity"),
            tags$dd("How see-through the points, bars, or boxes are (0.1 = nearly transparent, 1 = solid). Lowering it helps when many points or bars overlap."),
            tags$hr(),
            tags$dt("Jitter points"),
            tags$dd("Adds a small random nudge to each scatter point so that points sharing the same value don't sit exactly on top of one another. Useful for dense or rounded data; it changes only the display, not the underlying values."),
            tags$hr(),
            tags$dt("Log Scale"),
            tags$dd("Plots an axis on a base-10 logarithmic scale, so each step is ×10 (1, 10, 100, …). Helpful when values span several orders of magnitude or are heavily right-skewed. Only applies to continuous (numeric) axes and to positive values."),
            tags$hr(),
            tags$dt("Facet By (small multiples)"),
            tags$dd("Splits one chart into a grid of small panels — one per category of the chosen variable — so you can compare groups side by side (e.g. one scatter per region). All panels share the same axes for easy comparison."),
            tags$hr(),
            tags$dt("Horizontal orientation"),
            tags$dd("Flips a bar or box plot onto its side. This is the easiest fix when category labels are long or there are many of them and they overlap along the bottom."),
            tags$hr(),
            tags$dt("Show equation & R² (trendline label)"),
            tags$dd("When a fitted line is overlaid on a scatter or line chart, this prints the fitted equation (or the model type) and its R² directly on the plot. R² ranges 0–1 and is the share of variation the fit explains."),
            tags$hr(),
            tags$dt("Resolution (DPI)"),
            tags$dd("Dots per inch — controls the sharpness of exported images. 72–96 DPI is screen quality. 150 DPI is good for presentations. 300+ DPI is recommended for print or publication. Higher DPI means a larger file size.")
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # `reset` is bumped to force the plot-config UI to rebuild at its defaults.
  # `data_raw` keeps the file exactly as uploaded so Data Health can revert.
  rv <- reactiveValues(data = NULL, data_raw = NULL, model = NULL, reset = 0L)

  # ── Load data ─────────────────────────────────────────────

  observeEvent(input$load_example, {
    d <- as.data.frame(mtcars)
    d$car <- rownames(d)
    rownames(d) <- NULL
    rv$data <- d; rv$data_raw <- d
    showNotification("Loaded example dataset: mtcars (Motor Trend Cars)", type = "message")
  })

  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      d <- read_file_data(
        input$file$datapath, ext,
        header = input$header, sep = input$sep, dec = input$dec
      )
      rv$data <- d; rv$data_raw <- d
      showNotification(paste("Loaded:", input$file$name), type = "message")
      nh <- attr(d, "n_skip_head") %||% 0L
      nt <- attr(d, "n_skip_tail") %||% 0L
      if (nh > 0 || nt > 0) {
        showNotification(
          sprintf("Auto-skipped %d title line(s) at the top and %d footnote line(s) at the bottom so the data could be read cleanly.",
                  nh, nt),
          type = "warning", duration = 10)
      }
    }, error = function(e)
      showNotification(paste("Read error:", e$message), type = "error", duration = 8))
  })

  observeEvent(input$clear_data, {
    rv$data  <- NULL
    rv$data_raw <- NULL
    rv$model <- NULL
    showNotification("Cleared the loaded data.", type = "message")
  })

  # ── Data Health: diagnose + opt-in, reversible fixes ──────

  output$data_health_ui <- renderUI({
    if (is.null(rv$data))
      return(helpText("Load a dataset to run a quick health check."))
    iss <- detect_issues(rv$data)
    if (!length(iss))
      return(div(class = "alert alert-success py-2 px-3 mb-0",
                 icon("circle-check"),
                 " No common data issues detected — your data is ready to explore."))
    # choiceNames/Values must be unnamed (detect_issues returns a named list).
    ids  <- unname(vapply(iss, `[[`, character(1), "id"))
    defs <- unname(vapply(iss, `[[`, logical(1), "default"))
    nms  <- unname(lapply(iss, function(z) HTML(z$desc)))
    tagList(
      tags$p(sprintf("Spotted %d potential issue%s. Tick the fixes you want, then Apply — everything is reversible:",
                     length(iss), if (length(iss) == 1) "" else "s")),
      checkboxGroupInput(
        "dh_fixes", NULL,
        choiceNames  = nms,
        choiceValues = ids,
        selected     = ids[defs]),
      div(class = "d-flex gap-2",
          actionButton("dh_apply", "Apply selected fixes",
                       class = "btn-primary btn-sm", icon = icon("broom")),
          actionButton("dh_revert", "Revert to original",
                       class = "btn-outline-secondary btn-sm", icon = icon("rotate-left"))),
      tags$div(class = "form-text mt-2",
               "Fixes apply to a working copy used by the rest of the app; Revert restores the file exactly as uploaded.")
    )
  })

  observeEvent(input$dh_apply, {
    req(rv$data)
    ids <- input$dh_fixes
    if (is.null(ids) || !length(ids)) {
      showNotification("No fixes selected.", type = "warning"); return()
    }
    rv$data <- clean_apply(rv$data, ids)
    showNotification(sprintf("Applied %d fix%s.", length(ids),
                             if (length(ids) == 1) "" else "es"), type = "message")
  })

  observeEvent(input$dh_revert, {
    req(rv$data_raw)
    rv$data <- rv$data_raw
    showNotification("Reverted to the originally uploaded data.", type = "message")
  })

  # ── Data preview ──────────────────────────────────────────

  output$tbl_preview <- renderDT({
    req(rv$data)
    datatable(rv$data, rownames = FALSE, class = "compact stripe hover",
              options = list(scrollX = TRUE, pageLength = 10, dom = "tip"))
  })

  output$tbl_summary <- renderPrint({
    req(rv$data)
    summary(rv$data)
  })

  # ── Reactive column lists ──────────────────────────────────

  cols_all <- reactive({ req(rv$data); names(rv$data) })
  cols_num <- reactive({
    req(rv$data)
    names(rv$data)[vapply(rv$data, is.numeric, logical(1))]
  })
  # Categorical-ish columns (good for faceting): discrete, or low-cardinality
  # numeric like cyl. Capped so faceting can't be offered on an ID column.
  cols_cat <- reactive({
    req(rv$data)
    names(rv$data)[vapply(rv$data, function(x)
      (is_discrete_col(x) || (is.numeric(x) && dplyr::n_distinct(x) <= 10)) &&
        dplyr::n_distinct(x) <= 30, logical(1))]
  })

  # ── Visualize: configuration accordion (1–4 plots) ────────
  #
  # Built once (and again only when the user hits Reset) — crucially it does
  # NOT depend on input$n_plots, so changing the plot count just shows/hides
  # panels client-side instead of rebuilding (and resetting) every control.
  output$plot_config_accordion <- renderUI({
    rv$reset
    tagList(
      accordion(plot_slot_panel(1), open = "panel1"),
      conditionalPanel("input.n_plots >= 2", accordion(plot_slot_panel(2), open = FALSE)),
      conditionalPanel("input.n_plots >= 3", accordion(plot_slot_panel(3), open = FALSE)),
      conditionalPanel("input.n_plots >= 4", accordion(plot_slot_panel(4), open = FALSE))
    )
  })

  # Per-slot, data-aware selectors. They read rv$reset so the Reset button
  # also clears the variable picks back to their defaults.
  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("ui_mp", idx, "_x")]] <- renderUI({
        req(rv$data); rv$reset
        ty <- input[[paste0("mp", idx, "_type")]] %||% "scatter"
        # Histograms can only bin a numeric column, so restrict the choices
        # rather than letting a text column produce a broken/empty plot.
        if (identical(ty, "histogram"))
          return(selectInput(paste0("mp", idx, "_xvar"), "X Variable (numeric)",
                             choices = cols_num()))
        lbl <- if (identical(ty, "pie")) "Category (one slice per value)" else "X Variable"
        selectInput(paste0("mp", idx, "_xvar"), lbl, choices = cols_all())
      })
      output[[paste0("ui_mp", idx, "_y")]] <- renderUI({
        req(rv$data); rv$reset
        ty <- input[[paste0("mp", idx, "_type")]]
        req(ty)
        if (ty == "histogram")
          return(helpText("Histograms use only an X variable."))
        if (ty == "pie")
          return(selectInput(paste0("mp", idx, "_yvar"),
            label = tags$span("Slice Size",
              info_icon("Optional. By default each slice is the COUNT of rows in that category. Pick a numeric variable to size slices by its SUM within each category instead.")),
            choices = c("Count of each category" = "__count__", cols_num())))
        selectInput(paste0("mp", idx, "_yvar"), "Y Variable", choices = cols_num())
      })
      output[[paste0("ui_mp", idx, "_color")]] <- renderUI({
        req(rv$data); rv$reset
        ty <- input[[paste0("mp", idx, "_type")]]
        req(ty)
        if (ty == "pie") return(NULL)
        selectInput(paste0("mp", idx, "_colorvar"), "Color / Group By (optional)",
                    choices = c("None" = "__none__", cols_all()))
      })
      output[[paste0("ui_mp", idx, "_hint")]] <- renderUI({
        req(rv$data, input[[paste0("mp", idx, "_type")]])
        msg <- chart_hint(rv$data, slot_params(idx))
        if (is.null(msg)) return(NULL)
        div(class = "alert alert-warning py-1 px-2 small mb-2", role = "alert",
            icon("triangle-exclamation"), HTML(paste0(" ", msg)))
      })
      output[[paste0("ui_mp", idx, "_facet")]] <- renderUI({
        req(rv$data); rv$reset
        ty <- input[[paste0("mp", idx, "_type")]]
        if (identical(ty, "pie")) return(NULL)
        selectInput(paste0("mp", idx, "_facetvar"),
                    tags$span("Facet By (small multiples)",
                              info_icon("Splits the chart into one panel per category of this variable. Only categorical / low-cardinality columns are offered.")),
                    choices = c("None" = "__none__", cols_cat()))
      })
      # Max-categories slider for bar (bars) and pie (slices). Data-aware: it
      # ranges up to the number of distinct categories so the user can show
      # all of them, and defaults to a readable cap. Categories beyond the cap
      # are grouped into a single "Other".
      output[[paste0("ui_mp", idx, "_catlimit")]] <- renderUI({
        req(rv$data); rv$reset
        ty <- input[[paste0("mp", idx, "_type")]]
        if (!isTRUE(ty %in% c("bar", "pie"))) return(NULL)
        xv <- input[[paste0("mp", idx, "_xvar")]]
        req(xv, xv %in% names(rv$data))
        nx <- dplyr::n_distinct(rv$data[[xv]])
        if (nx <= 2) return(NULL)
        unit <- if (ty == "bar") "bars" else "slices"
        deflt <- min(if (ty == "bar") BAR_MAX else PIE_MAX, nx)
        sliderInput(paste0("mp", idx, "_catlimitv"),
          tags$span(sprintf("Maximum %s", unit),
            info_icon(sprintf("Keeps the largest categories and groups the rest into a single “Other” slice/bar. Defaults to %d for readability — slide up to show more (max %d), or down to simplify.", deflt, nx))),
          min = 2, max = nx, value = deflt, step = 1)
      })
    })
  }

  # Gather one slot's settings into a plain list
  slot_params <- function(i) {
    list(
      type        = input[[paste0("mp", i, "_type")]],
      x           = input[[paste0("mp", i, "_xvar")]],
      y           = input[[paste0("mp", i, "_yvar")]],
      color       = input[[paste0("mp", i, "_colorvar")]],
      title       = input[[paste0("mp", i, "_title")]],
      xlab        = input[[paste0("mp", i, "_xlab")]],
      ylab        = input[[paste0("mp", i, "_ylab")]],
      theme       = input[[paste0("mp", i, "_theme")]],
      color_hex   = input[[paste0("mp", i, "_color")]],
      size        = input[[paste0("mp", i, "_size")]],
      bins        = input[[paste0("mp", i, "_bins")]],
      bar_agg     = input[[paste0("mp", i, "_baragg")]],
      cat_limit   = input[[paste0("mp", i, "_catlimitv")]],
      reg_overlay = isTRUE(input[[paste0("mp", i, "_reg")]]),
      reg_type    = input[[paste0("mp", i, "_regtype")]],
      reg_deg     = input[[paste0("mp", i, "_regdeg")]],
      reg_ci      = isTRUE(input[[paste0("mp", i, "_regci")]]),
      reg_col     = input[[paste0("mp", i, "_regcol")]],
      trend_label = isTRUE(input[[paste0("mp", i, "_trendlab")]]),
      palette     = input[[paste0("mp", i, "_palette")]]  %||% "auto",
      alpha       = input[[paste0("mp", i, "_alpha")]],
      jitter      = isTRUE(input[[paste0("mp", i, "_jitter")]]),
      logscale    = input[[paste0("mp", i, "_logscale")]] %||% "none",
      facet       = input[[paste0("mp", i, "_facetvar")]],
      legend_pos  = input[[paste0("mp", i, "_legendpos")]] %||% "right",
      gridlines   = input[[paste0("mp", i, "_grid")]] %||% TRUE,
      flip        = isTRUE(input[[paste0("mp", i, "_flip")]])
    )
  }

  # All currently-configured plots (used by Export + previews)
  current_plots <- function() {
    if (is.null(rv$data)) return(list())
    n <- as.integer(input$n_plots %||% 1)
    out <- list()
    for (i in seq_len(n)) {
      pr <- slot_params(i)
      if (is.null(pr$x) || !nzchar(pr$x)) next
      pl <- tryCatch(build_full_plot(rv$data, pr), error = function(e) NULL)
      if (!is.null(pl)) out[[length(out) + 1]] <- pl
    }
    out
  }

  # Copy Plot 1's style across the others
  observeEvent(input$copy_style, {
    n <- as.integer(input$n_plots %||% 1)
    if (n < 2) return()
    th <- input$mp1_theme
    co <- input$mp1_color
    sz <- input$mp1_size
    pal <- input$mp1_palette
    al  <- input$mp1_alpha
    lp  <- input$mp1_legendpos
    gr  <- input$mp1_grid
    for (i in 2:n) {
      if (!is.null(th)) updateSelectInput(session, paste0("mp", i, "_theme"), selected = th)
      if (!is.null(co)) colourpicker::updateColourInput(session, paste0("mp", i, "_color"), value = co)
      if (!is.null(sz)) updateSliderInput(session, paste0("mp", i, "_size"), value = sz)
      if (!is.null(pal)) updateSelectInput(session, paste0("mp", i, "_palette"), selected = pal)
      if (!is.null(al))  updateSliderInput(session, paste0("mp", i, "_alpha"), value = al)
      if (!is.null(lp))  updateSelectInput(session, paste0("mp", i, "_legendpos"), selected = lp)
      if (!is.null(gr))  updateCheckboxInput(session, paste0("mp", i, "_grid"), value = gr)
    }
    showNotification("Applied Plot 1's style to the other plots.", type = "message")
  })

  # Reset every plot's settings to default by rebuilding the config UI.
  observeEvent(input$reset_plots, {
    rv$reset <- rv$reset + 1L
    updateRadioButtons(session, "n_plots", selected = 1)
    showNotification("Reset all plot settings to default.", type = "message")
  })

  # ── Visualize: plot area + per-plot R code ────────────────

  output$plots_area <- renderUI({
    req(rv$data)
    n <- as.integer(input$n_plots %||% 1)
    ph <- if (n == 1) "470px" else "330px"
    big <- nrow(rv$data) > BIG_ROWS
    cards <- lapply(seq_len(n), function(i) {
      plot_ui <- if (big) {
        tagList(
          plotOutput(paste0("mp_st", i), height = ph),
          tags$div(class = "form-text",
                   sprintf("Static view (%s rows). Interactive zoom/hover is disabled above %s rows for responsiveness; exports use all rows.",
                           format(nrow(rv$data), big.mark = ","),
                           format(BIG_ROWS, big.mark = ",")))
        )
      } else {
        tagList(
          conditionalPanel(sprintf("input.mp%d_type != 'pie'", i),
                           plotlyOutput(paste0("mp_ly", i), height = ph)),
          conditionalPanel(sprintf("input.mp%d_type == 'pie'", i),
                           plotOutput(paste0("mp_st", i), height = ph))
        )
      }
      card(
        full_screen = TRUE,
        card_header(paste("Plot", i)),
        plot_ui,
        accordion(
          open = FALSE,
          accordion_panel(
            icon("code"), " R code for this plot",
            value = paste0("code_panel", i),
            tags$button("Copy code", class = "btn btn-sm btn-outline-primary mb-2",
                        onclick = sprintf("DEcopy('code_plot%d', this)", i)),
            verbatimTextOutput(paste0("code_plot", i))
          )
        )
      )
    })
    do.call(layout_columns, c(list(col_widths = if (n == 1) 12 else 6), cards))
  })

  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("mp_ly", idx)]] <- renderPlotly({
        req(rv$data)
        pr <- slot_params(idx)
        req(pr$x)
        p <- build_full_plot(rv$data, pr)
        req(p)
        ggplotly(p) |> layout(margin = list(t = 55, b = 55))
      })
      output[[paste0("mp_st", idx)]] <- renderPlot({
        req(rv$data)
        pr <- slot_params(idx)
        req(pr$x)
        build_full_plot(rv$data, pr)
      }, bg = "white")
      output[[paste0("code_plot", idx)]] <- renderText({
        req(rv$data)
        pr <- slot_params(idx)
        req(pr$x)
        generate_code(rv$data, pr)
      })
    })
  }

  # ── Variable selectors — Regression tab ───────────────────

  output$ui_reg_resp <- renderUI({
    selectInput("reg_resp",
      label = tags$span("Response Variable (Y)",
        info_icon("The outcome you want to predict. Must be a numeric variable.")),
      choices = cols_num())
  })

  output$ui_reg_pred <- renderUI({
    is_multi <- !is.null(input$reg_type) && input$reg_type == "multiple"
    lbl <- if (is_multi) "Predictor Variables (X)" else "Predictor Variable (X)"
    tip <- if (is_multi)
      "Select two or more numeric variables. Hold Ctrl/Cmd to select multiple."
    else
      "Select one numeric variable to predict the response."
    selectInput("reg_pred",
      label    = tags$span(lbl, info_icon(tip)),
      choices  = cols_num(),
      multiple = is_multi)
  })

  # ── Regression model ──────────────────────────────────────

  observeEvent(input$btn_fit, {
    req(rv$data, input$reg_resp, input$reg_pred)
    df   <- rv$data
    resp <- input$reg_resp
    pred <- input$reg_pred
    tryCatch({
      fmla <- switch(input$reg_type,
        linear     = paste(resp, "~", paste(pred, collapse = " + ")),
        multiple   = paste(resp, "~", paste(pred, collapse = " + ")),
        polynomial = paste(resp, "~",
                           paste0("poly(", pred[1], ", ", input$poly_deg_reg, ", raw = TRUE)"))
      )
      rv$model <- lm(as.formula(fmla), data = df)
      showNotification("Model fitted successfully.", type = "message")
    }, error = function(e)
      showNotification(paste("Fitting error:", e$message), type = "error", duration = 8))
  })

  # summary() is the expensive call on a big model, so compute it once and
  # share it between the printout and the plain-English interpretation.
  model_summary <- reactive({ req(rv$model); summary(rv$model) })

  output$reg_summary <- renderPrint({
    if (is.null(rv$model)) cat("Fit a model using the panel on the left.\n")
    else                   model_summary()
  })

  output$reg_interpretation <- renderUI({
    if (is.null(rv$model))
      return(tags$p(class = "text-muted fst-italic",
                    "Fit a model to see an interpretation of the results."))

    s      <- model_summary()
    r2     <- round(s$r.squared, 3)
    adj_r2 <- round(s$adj.r.squared, 3)
    fstat  <- s$fstatistic
    overall_p <- if (!is.null(fstat)) pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE) else NA
    p_label   <- if (!is.na(overall_p)) {
      if (overall_p < 0.001) "p < 0.001" else paste0("p = ", round(overall_p, 4))
    } else "p = N/A"

    overall_tag <- if (!is.na(overall_p) && overall_p < 0.05) {
      tags$p(tags$span(style = "color:#2e7d32; font-weight:600;",
        icon("circle-check"), " The overall model is statistically significant (", p_label, ")."))
    } else {
      tags$p(tags$span(style = "color:#c62828; font-weight:600;",
        icon("circle-xmark"), " The overall model is NOT statistically significant (", p_label, ")."))
    }

    coef_df   <- as.data.frame(s$coefficients)
    pred_rows <- coef_df[rownames(coef_df) != "(Intercept)", , drop = FALSE]
    sig    <- rownames(pred_rows)[pred_rows[, 4] < 0.05]
    nonsig <- rownames(pred_rows)[pred_rows[, 4] >= 0.05]

    tagList(
      overall_tag,
      tags$p(tags$b("R² = ", r2), " — explains ",
             tags$b(paste0(round(r2 * 100, 1), "%")), " of the variance. ",
             tags$span(style = "color:#555;", paste0("(Adj. R² = ", adj_r2, ")"))),
      if (length(sig)    > 0) tags$p(tags$b(style = "color:#2e7d32;", "Significant (p < 0.05): "),    paste(sig,    collapse = ", ")),
      if (length(nonsig) > 0) tags$p(tags$b(style = "color:#c62828;", "Not significant (p ≥ 0.05): "), paste(nonsig, collapse = ", ")),
      tags$p(class = "text-muted small mt-2",
             "α = 0.05. Statistical significance does not imply practical importance.")
    )
  })

  # Diagnostic-plot data is thinned to BIG_ROWS points (deterministically, so
  # the view doesn't jump on re-render) to keep ggplotly snappy on large
  # datasets. The model is still fit on every row.
  thin_rows <- function(d) {
    if (nrow(d) <= BIG_ROWS) return(d)
    d[unique(round(seq(1, nrow(d), length.out = BIG_ROWS))), , drop = FALSE]
  }
  thin_note <- function(n)
    if (n > BIG_ROWS)
      sprintf("Showing ~%s of %s points for responsiveness",
              format(BIG_ROWS, big.mark = ","), format(n, big.mark = ","))
    else NULL

  output$reg_plot_fitted <- renderPlotly({
    req(rv$model)
    d <- data.frame(actual = rv$model$model[[1]], fitted = fitted(rv$model))
    note <- thin_note(nrow(d)); d <- thin_rows(d)
    p <- ggplot(d, aes(x = actual, y = fitted)) +
         geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
         geom_abline(color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
         theme_minimal(base_size = 12) +
         labs(title = "Fitted vs Actual", subtitle = note, x = "Actual", y = "Fitted") +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#777"))
    ggplotly(p) |> layout(margin = list(t = 90, b = 40, l = 55, r = 20))
  })

  output$reg_plot_resid <- renderPlotly({
    req(rv$model)
    d <- data.frame(fitted = fitted(rv$model), resid = residuals(rv$model))
    note <- thin_note(nrow(d)); d <- thin_rows(d)
    p <- ggplot(d, aes(x = fitted, y = resid)) +
         geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
         geom_hline(yintercept = 0, color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
         theme_minimal(base_size = 12) +
         labs(title = "Residuals vs Fitted", subtitle = note, x = "Fitted Values", y = "Residuals") +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"),
               plot.subtitle = element_text(hjust = 0.5, size = 9, color = "#777"))
    ggplotly(p) |> layout(margin = list(t = 90, b = 40, l = 55, r = 20))
  })

  output$dl_reg <- downloadHandler(
    filename = function() paste0("model_summary_", Sys.Date(), ".txt"),
    content  = function(f) { req(rv$model); capture.output(summary(rv$model), file = f) }
  )

  # ── Export: plots ─────────────────────────────────────────

  output$exp_preview <- renderPlot({
    draw_plot_grid(current_plots())
  }, bg = "white")

  output$exp_sep_buttons <- renderUI({
    n <- as.integer(input$n_plots %||% 1)
    if (n <= 1) return(NULL)
    tagList(lapply(seq_len(n), function(i)
      tagList(
        downloadButton(paste0("dl_plot", i), paste("Download Plot", i),
                       class = "btn-success w-100"),
        br(), br()
      )))
  })

  output$dl_combined <- downloadHandler(
    filename = function() paste0("plots_", Sys.Date(), ".", input$exp_fmt %||% "png"),
    content  = function(f) {
      plots <- current_plots()
      render_plots_to_file(plots, f, input$exp_fmt %||% "png",
                           input$exp_w %||% 7, input$exp_h %||% 5.5, input$exp_dpi %||% 150)
    }
  )

  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("dl_plot", idx)]] <- downloadHandler(
        filename = function() paste0("plot", idx, "_", Sys.Date(), ".", input$exp_fmt %||% "png"),
        content  = function(f) {
          pr <- slot_params(idx)
          pl <- tryCatch(build_full_plot(rv$data, pr), error = function(e) NULL)
          render_plots_to_file(if (is.null(pl)) list() else list(pl), f,
                               input$exp_fmt %||% "png",
                               input$exp_w %||% 7, input$exp_h %||% 5.5, input$exp_dpi %||% 150)
        }
      )
    })
  }

  # ── Export: data ──────────────────────────────────────────

  output$ui_export_vars <- renderUI({
    req(rv$data)
    selectInput("export_vars", "Select columns to export:",
                choices  = names(rv$data),
                selected = names(rv$data),
                multiple = TRUE)
  })

  export_df <- reactive({
    req(rv$data)
    vars <- input$export_vars
    if (is.null(vars) || length(vars) == 0) return(rv$data)
    rv$data[, intersect(vars, names(rv$data)), drop = FALSE]
  })

  output$exp_data_tbl <- renderDT({
    req(rv$data)
    datatable(export_df(), rownames = FALSE, class = "compact stripe",
              options = list(scrollX = TRUE, pageLength = 8, dom = "tip"))
  })

  output$dl_csv <- downloadHandler(
    filename = function() paste0("data_", Sys.Date(), ".csv"),
    content  = function(f) { req(rv$data); write.csv(export_df(), f, row.names = FALSE) }
  )

  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("data_", Sys.Date(), ".xlsx"),
    content  = function(f) { req(rv$data); write_xlsx(export_df(), f) }
  )

  # ── Export: regression results ────────────────────────────

  output$reg_export_preview <- renderPrint({
    if (is.null(rv$model)) cat("Fit a model on the Regression tab to enable export.\n")
    else                   model_summary()
  })

  output$dl_exp_reg_txt <- downloadHandler(
    filename = function() paste0("model_summary_", Sys.Date(), ".txt"),
    content  = function(f) { req(rv$model); capture.output(model_summary(), file = f) }
  )

  output$dl_exp_reg_csv <- downloadHandler(
    filename = function() paste0("model_coefficients_", Sys.Date(), ".csv"),
    content  = function(f) {
      req(rv$model)
      co <- as.data.frame(model_summary()$coefficients)
      co <- cbind(Term = rownames(co), co)
      write.csv(co, f, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

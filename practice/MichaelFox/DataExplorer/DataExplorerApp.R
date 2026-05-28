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

# ----------------------------------------------------------
# Helper functions
# ----------------------------------------------------------

read_file_data <- function(path, ext, header = TRUE, sep = ",", dec = ".") {
  ext <- tolower(ext)
  if (ext %in% c("xlsx", "xls")) return(as.data.frame(read_excel(path)))
  if (ext == "rds")               return(as.data.frame(readRDS(path)))
  if (ext == "csv")
    return(read.csv(path, header = header, sep = sep, dec = dec, stringsAsFactors = FALSE))
  if (ext %in% c("tsv", "txt"))
    return(read.table(path, header = header,
                      sep = if (ext == "tsv") "\t" else sep,
                      dec = dec, stringsAsFactors = FALSE))
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
# Plot builder — one function used by every plot slot,
# the Visualize previews, and the Export tab.
#
# p is a plain list of settings:
#   type, x, y, color, title, xlab, ylab,
#   theme, color_hex, size, bins, bar_agg,
#   reg_overlay, reg_type, reg_deg, reg_ci, reg_col
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

  size <- p$size %||% 2
  col  <- p$color_hex %||% UF_BLUE
  bins <- p$bins %||% 30

  title <- if (!is.null(p$title) && nzchar(trimws(p$title))) p$title else NULL
  xlab  <- label_or(p$xlab %||% "", xv)
  ylab  <- if (!is.null(yv)) label_or(p$ylab %||% "", yv) else NULL

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
    p_obj <- ggplot(df, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + geom_point(size = size, alpha = 0.75, color = col)
             else
               p_obj + geom_point(size = size, alpha = 0.75)
    p_obj <- p_obj + smooth_layer()

  } else if (pt == "line") {
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
                 p_obj + geom_col(fill = col, width = bw, alpha = 0.85)
               else
                 p_obj + geom_col(width = bw, alpha = 0.85, position = "dodge")
      ylab <- label_or(p$ylab %||% "",
                       paste0(tools::toTitleCase(p$bar_agg %||% "sum"), " of ", yv))
    } else {
      aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], fill = .data[[cv]])
               else               aes(x = .data[[xv]])
      p_obj <- ggplot(df, aes_m)
      p_obj <- if (is.null(cv))
                 p_obj + geom_bar(stat = "count", fill = col, width = bw, alpha = 0.85)
               else
                 p_obj + geom_bar(stat = "count", width = bw, alpha = 0.85, position = "dodge")
      ylab <- "Count"
    }

  } else if (pt == "histogram") {
    if (!is.null(cv)) {
      p_obj <- ggplot(df, aes(x = .data[[xv]], fill = .data[[cv]])) +
               geom_histogram(bins = bins, color = "white", alpha = 0.75, position = "dodge")
    } else {
      p_obj <- ggplot(df, aes(x = .data[[xv]])) +
               geom_histogram(bins = bins, color = "white", fill = col, alpha = 0.85)
    }
    ylab <- "Count"

  } else if (pt == "boxplot") {
    aes_m <- if (!is.null(cv)) aes(x = .data[[xv]], y = .data[[yv]], fill = .data[[cv]])
             else               aes(x = .data[[xv]], y = .data[[yv]])
    p_obj <- ggplot(df, aes_m)
    p_obj <- if (is.null(cv))
               p_obj + geom_boxplot(fill = col, alpha = 0.75,
                                    outlier.size = size * 0.7, outlier.alpha = 0.6)
             else
               p_obj + geom_boxplot(alpha = 0.75,
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
    pie_df$cat_   <- as.factor(pie_df$cat_)
    pie_df$label_ <- paste0(pie_df$cat_, "\n", pct_label(pie_df$val_))
    return(
      ggplot(pie_df, aes(x = "", y = val_, fill = cat_)) +
        geom_col(width = 1, color = "white", linewidth = 0.5) +
        coord_polar("y", start = 0) +
        geom_text(aes(label = label_), position = position_stack(vjust = 0.5),
                  size = 3.5, color = "white", fontface = "bold") +
        scale_fill_brewer(palette = "Set2", name = xv) +
        labs(title = title) +
        theme_void(base_size = 13) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
              legend.position = "right", legend.title = element_text(size = 11))
    )
  }

  if (is.null(p_obj)) return(NULL)

  if (!is.null(cv)) {
    if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) > 10) {
      p_obj <- p_obj + scale_color_viridis_c() + scale_fill_viridis_c()
    } else {
      p_obj <- p_obj + scale_color_brewer(palette = "Set1") +
                       scale_fill_brewer(palette = "Set1")
    }
  }

  uses_color <- pt %in% c("scatter", "line")
  base_theme <- theme_call(p$theme, 13) +
    theme(
      plot.title      = element_text(hjust = 0.5, face = "bold", size = 14,
                                     margin = margin(b = 10)),
      axis.title      = element_text(size = 12),
      legend.title    = element_text(size = 11),
      legend.position = "right"
    )

  p_obj + base_theme + labs(
    title = title, x = xlab, y = ylab,
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

  size  <- p$size %||% 2
  col   <- p$color_hex %||% UF_BLUE
  bins  <- p$bins %||% 30
  agg   <- p$bar_agg %||% "sum"
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
  if (!is.null(cv)) {
    if (is.numeric(df[[cv]]) && dplyr::n_distinct(df[[cv]]) > 10)
      scale_line <- if (uses_color) "scale_color_viridis_c()" else "scale_fill_viridis_c()"
    else
      scale_line <- if (uses_color) 'scale_color_brewer(palette = "Set1")'
                    else            'scale_fill_brewer(palette = "Set1")'
  }

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
    code <- paste0(
      sprintf('ggplot(plot_df, aes(x = "", y = val, fill = %s)) +', bq(xv)),
      '\n  geom_col(width = 1, color = "white") +',
      '\n  coord_polar("y") +',
      '\n  scale_fill_brewer(palette = "Set2") +',
      '\n  theme_void()',
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

  geom_lines <- switch(pt,
    scatter = if (is.null(cv))
                sprintf('geom_point(size = %s, alpha = 0.75, color = %s)', size, qq(col))
              else
                sprintf('geom_point(size = %s, alpha = 0.75)', size),
    line    = if (is.null(cv))
                sprintf('geom_line(linewidth = %s, color = %s) +\n  geom_point(size = %s, color = %s)',
                        size * 0.4, qq(col), size * 0.7, qq(col))
              else
                sprintf('geom_line(linewidth = %s) +\n  geom_point(size = %s)',
                        size * 0.4, size * 0.7),
    bar     = if (!is.null(yv)) {
                if (is.null(cv))
                  sprintf('geom_col(fill = %s, width = %s, alpha = 0.85)', qq(col), round(bar_width(size), 3))
                else
                  sprintf('geom_col(width = %s, alpha = 0.85, position = "dodge")', round(bar_width(size), 3))
              } else {
                if (is.null(cv))
                  sprintf('geom_bar(fill = %s, width = %s, alpha = 0.85)', qq(col), round(bar_width(size), 3))
                else
                  sprintf('geom_bar(width = %s, alpha = 0.85, position = "dodge")', round(bar_width(size), 3))
              },
    histogram = if (is.null(cv))
                  sprintf('geom_histogram(bins = %s, color = "white", fill = %s, alpha = 0.85)', bins, qq(col))
                else
                  sprintf('geom_histogram(bins = %s, color = "white", alpha = 0.75, position = "dodge")', bins),
    boxplot = if (is.null(cv))
                sprintf('geom_boxplot(fill = %s, alpha = 0.75)', qq(col))
              else
                'geom_boxplot(alpha = 0.75)'
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
  lines <- c(lines, theme_str)
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
    tags$hr(),
    tags$h6("Labels"),
    textInput(paste0("mp", i, "_title"), "Title",        placeholder = "(optional)"),
    textInput(paste0("mp", i, "_xlab"),  "X-Axis Label", placeholder = "auto"),
    textInput(paste0("mp", i, "_ylab"),  "Y-Axis Label", placeholder = "auto"),
    tags$hr(),
    tags$h6("Style"),
    selectInput(paste0("mp", i, "_theme"), "Theme",
                choices = c("Minimal" = "minimal", "Classic" = "classic",
                            "Light" = "light", "B&W" = "bw", "Dark" = "dark")),
    colourInput(paste0("mp", i, "_color"), "Default Color",
                value = MP_COLORS[((i - 1) %% length(MP_COLORS)) + 1]),
    sliderInput(paste0("mp", i, "_size"), "Point / Bar Size",
                min = 0.5, max = 5, value = 2, step = 0.5),
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
        colourInput(paste0("mp", i, "_regcol"), "Line Color", value = UF_ORANGE)
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
                     class = "btn-outline-primary w-100", icon = icon("table"))
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

  rv <- reactiveValues(data = NULL, model = NULL)

  # ── Load data ─────────────────────────────────────────────

  observeEvent(input$load_example, {
    d <- as.data.frame(mtcars)
    d$car <- rownames(d)
    rownames(d) <- NULL
    rv$data <- d
    showNotification("Loaded example dataset: mtcars (Motor Trend Cars)", type = "message")
  })

  observeEvent(input$file, {
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    tryCatch({
      rv$data <- read_file_data(
        input$file$datapath, ext,
        header = input$header, sep = input$sep, dec = input$dec
      )
      showNotification(paste("Loaded:", input$file$name), type = "message")
    }, error = function(e)
      showNotification(paste("Read error:", e$message), type = "error", duration = 8))
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

  # ── Visualize: configuration accordion (1–4 plots) ────────

  output$plot_config_accordion <- renderUI({
    n <- as.integer(input$n_plots %||% 1)
    panels <- lapply(seq_len(n), plot_slot_panel)
    do.call(accordion, c(list(open = "panel1"), panels))
  })

  # Per-slot, data-aware selectors
  for (i in 1:4) {
    local({
      idx <- i
      output[[paste0("ui_mp", idx, "_x")]] <- renderUI({
        req(rv$data)
        selectInput(paste0("mp", idx, "_xvar"), "X Variable", choices = cols_all())
      })
      output[[paste0("ui_mp", idx, "_y")]] <- renderUI({
        req(rv$data)
        ty <- input[[paste0("mp", idx, "_type")]]
        req(ty)
        if (ty == "histogram")
          return(helpText("Histograms use only an X variable."))
        ch <- if (ty == "pie") c("(Count categories)" = "__count__", cols_num()) else cols_num()
        selectInput(paste0("mp", idx, "_yvar"), "Y Variable", choices = ch)
      })
      output[[paste0("ui_mp", idx, "_color")]] <- renderUI({
        req(rv$data)
        ty <- input[[paste0("mp", idx, "_type")]]
        req(ty)
        if (ty == "pie") return(NULL)
        selectInput(paste0("mp", idx, "_colorvar"), "Color / Group By (optional)",
                    choices = c("None" = "__none__", cols_all()))
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
      reg_overlay = isTRUE(input[[paste0("mp", i, "_reg")]]),
      reg_type    = input[[paste0("mp", i, "_regtype")]],
      reg_deg     = input[[paste0("mp", i, "_regdeg")]],
      reg_ci      = isTRUE(input[[paste0("mp", i, "_regci")]]),
      reg_col     = input[[paste0("mp", i, "_regcol")]]
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
    for (i in 2:n) {
      if (!is.null(th)) updateSelectInput(session, paste0("mp", i, "_theme"), selected = th)
      if (!is.null(co)) colourpicker::updateColourInput(session, paste0("mp", i, "_color"), value = co)
      if (!is.null(sz)) updateSliderInput(session, paste0("mp", i, "_size"), value = sz)
    }
    showNotification("Applied Plot 1's style to the other plots.", type = "message")
  })

  # ── Visualize: plot area + per-plot R code ────────────────

  output$plots_area <- renderUI({
    req(rv$data)
    n <- as.integer(input$n_plots %||% 1)
    ph <- if (n == 1) "470px" else "330px"
    cards <- lapply(seq_len(n), function(i) {
      card(
        full_screen = TRUE,
        card_header(paste("Plot", i)),
        conditionalPanel(sprintf("input.mp%d_type != 'pie'", i),
                         plotlyOutput(paste0("mp_ly", i), height = ph)),
        conditionalPanel(sprintf("input.mp%d_type == 'pie'", i),
                         plotOutput(paste0("mp_st", i), height = ph)),
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

  output$reg_summary <- renderPrint({
    if (is.null(rv$model)) cat("Fit a model using the panel on the left.\n")
    else                   summary(rv$model)
  })

  output$reg_interpretation <- renderUI({
    if (is.null(rv$model))
      return(tags$p(class = "text-muted fst-italic",
                    "Fit a model to see an interpretation of the results."))

    s      <- summary(rv$model)
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

  output$reg_plot_fitted <- renderPlotly({
    req(rv$model)
    d <- data.frame(actual = rv$model$model[[1]], fitted = fitted(rv$model))
    p <- ggplot(d, aes(x = actual, y = fitted)) +
         geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
         geom_abline(color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
         theme_minimal(base_size = 12) +
         labs(title = "Fitted vs Actual", x = "Actual", y = "Fitted") +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggplotly(p) |> layout(margin = list(t = 90, b = 40, l = 55, r = 20))
  })

  output$reg_plot_resid <- renderPlotly({
    req(rv$model)
    d <- data.frame(fitted = fitted(rv$model), resid = residuals(rv$model))
    p <- ggplot(d, aes(x = fitted, y = resid)) +
         geom_point(color = UF_BLUE, size = 2.5, alpha = 0.7) +
         geom_hline(yintercept = 0, color = UF_ORANGE, linetype = "dashed", linewidth = 1) +
         theme_minimal(base_size = 12) +
         labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals") +
         theme(plot.title = element_text(hjust = 0.5, face = "bold"))
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
}

shinyApp(ui, server)

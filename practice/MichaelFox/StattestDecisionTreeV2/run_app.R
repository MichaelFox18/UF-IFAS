app_dir <- normalizePath(
  dirname(sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE)))
)
if (length(app_dir) == 0) app_dir <- "."
shiny::runApp(appDir = app_dir, launch.browser = TRUE)

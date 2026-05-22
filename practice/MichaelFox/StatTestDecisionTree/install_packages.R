pkgs <- c("shiny", "bslib", "shinyWidgets", "shinyjs")

missing <- pkgs[!pkgs %in% rownames(installed.packages())]

if (length(missing) == 0) {
  message("All packages already installed.")
} else {
  message("Installing: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
  message("Done. You can now run the app.")
}

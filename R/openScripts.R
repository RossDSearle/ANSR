open_script <- function(scriptName = "ANSR_Demo.R", overwrite = T) {
  # 1. Locate the file in the installed package
  pkg_script <- system.file("DemoScripts", scriptName, package = "ANSR")

  if (pkg_script == "") {
    stop("Script file not found in package.", call. = FALSE)
  }

  home_dir <- path.expand("~")
  of <- paste0(home_dir, '/', scriptName)

  # 2. Copy to current working directory
  file.copy(pkg_script, of, overwrite = overwrite)

  # 3. Open in RStudio / default editor
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(of)
  } else {
    file.edit(of)
  }

  invisible(of)
}


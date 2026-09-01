# To reproduce the JSS manuscript PDF and replication HTML:

local({

args <- commandArgs(FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_file <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  tryCatch(sys.frame(1)$ofile, error = function(e) "main.R")
}

candidate_dirs <- unique(c(
  getwd(),
  file.path(getwd(), "paper"),
  dirname(normalizePath(script_file, mustWork = FALSE)),
  dirname(normalizePath(file.path(getwd(), script_file), mustWork = FALSE))
))
script_dir <- candidate_dirs[file.exists(file.path(candidate_dirs, "article.tex"))]
if (length(script_dir) < 1) {
  stop("Run main.R from the paper directory or the package root.", call. = FALSE)
}

old_wd <- setwd(normalizePath(script_dir[[1]]))
on.exit(setwd(old_wd), add = TRUE)

package_root <- normalizePath(file.path(getwd(), ".."), mustWork = FALSE)
description_file <- file.path(package_root, "DESCRIPTION")
if (file.exists(description_file)) {
  package_info <- read.dcf(description_file, fields = c("Package", "Version"))
  if (identical(unname(package_info[, "Package"]), "pxweb")) {
    source_version <- package_version(package_info[, "Version"])
    installed_version <- tryCatch(
      package_version(utils::packageDescription("pxweb", fields = "Version")),
      error = function(e) package_version("0")
    )
    if (installed_version < source_version && requireNamespace("pkgload", quietly = TRUE)) {
      suppressPackageStartupMessages(pkgload::load_all(package_root, quiet = TRUE))
    }
  }
}

source("workflow-figure.R")

if (requireNamespace("knitr", quietly = TRUE) &&
    requireNamespace("rmarkdown", quietly = TRUE)) {
  code_rmd <- knitr::spin("code.R", knit = FALSE, format = "Rmd")
  rmarkdown::render(code_rmd, output_file = "code.html", quiet = TRUE, clean = TRUE)
  unlink(code_rmd)
} else {
  source("case-study.R")
  warning("Install knitr and rmarkdown to regenerate code.html.", call. = FALSE)
}

tools::texi2pdf("article.tex", clean = FALSE)

})

# Run the replication examples and regenerate both manuscript figures.

local({
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  script_file <- if (length(source_file) == 1L) {
    source_file
  } else if (length(file_arg) > 0L) {
    sub("^--file=", "", file_arg[[1L]])
  } else {
    "paperRJ.R"
  }
  paper_dir <- dirname(normalizePath(script_file, mustWork = TRUE))

  scripts_dir <- file.path(paper_dir, "scripts")
  source(file.path(scripts_dir, "workflow-figure.R"), chdir = TRUE, local = TRUE)
  source(file.path(scripts_dir, "code.R"), chdir = TRUE, local = TRUE, print.eval = TRUE)
})

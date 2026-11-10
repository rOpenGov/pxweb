# Generate the package workflow figure for the manuscript.
#
# The workflow is specified as a Graphviz DOT graph. If Graphviz is
# available, the DOT graph is rendered directly. Otherwise, a base R
# fallback draws the same workflow to PDF so the manuscript can still be
# reproduced on systems without Graphviz.

source_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NA_character_)
args <- commandArgs(FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_file <- if (length(source_file) == 1L && !is.na(source_file) && nzchar(source_file)) {
  source_file
} else if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[[1]])
} else {
  "workflow-figure.R"
}

candidate_dirs <- unique(c(
  getwd(),
  dirname(getwd()),
  file.path(getwd(), "paper"),
  dirname(dirname(normalizePath(script_file, mustWork = FALSE)))
))
script_dir <- candidate_dirs[file.exists(file.path(candidate_dirs, "scripts", "workflow-figure.R"))]
if (length(script_dir) < 1) {
  stop("Run paperRJ.R from the submission folder.", call. = FALSE)
}
script_dir <- normalizePath(script_dir[[1]])
dot_path <- tempfile("workflow-model-", fileext = ".dot")
figure_path <- file.path(script_dir, "workflow-model.pdf")

workflow_dot <- '
digraph pxweb_workflow {
  graph [
    rankdir = LR,
    bgcolor = "white",
    margin = 0.05,
    nodesep = 0.42,
    ranksep = 0.52
  ];

  node [
    shape = box,
    style = "rounded,filled",
    color = "#4A5568",
    fillcolor = "#F7FAFC",
    fontname = "Helvetica",
    fontsize = 10,
    margin = "0.08,0.05"
  ];

  edge [
    color = "#4A5568",
    arrowsize = 0.65,
    fontname = "Helvetica",
    fontsize = 9
  ];

  search [label = "Search and\\ndiscovery\\ncatalogue, URL,\\nor API search"];
  metadata [label = "Metadata\\nhierarchy, values, codelists"];
  query [label = "Query\\nselection and validation"];
  retrieval [label = "Retrieval\\nrequest and optional batching"];
  conversion [label = "Conversion\\ndata.frame or matrix"];
  comments [label = "Comments\\nand citation\\nnotes, status,\\nsource"];

  search -> metadata;
  metadata -> query;
  query -> retrieval;
  retrieval -> conversion;
  retrieval -> comments;
  { rank = same; conversion; comments; }
}
'

writeLines(workflow_dot, dot_path)

render_with_graphviz <- function(dot_path, figure_path) {
  dot <- Sys.which("dot")
  if (!nzchar(dot)) {
    return(FALSE)
  }

  status <- system2(
    dot,
    args = c("-Tpdf", shQuote(dot_path), "-o", shQuote(figure_path)),
    stdout = TRUE,
    stderr = TRUE
  )
  status_code <- attr(status, "status") %||% 0L
  identical(status_code, 0L) && file.exists(figure_path)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

draw_node <- function(x, y, label, width = 0.12, height = 0.17,
                      fill = "#F7FAFC", border = "#4A5568") {
  rect(x - width / 2, y - height / 2, x + width / 2, y + height / 2,
       col = fill, border = border, lwd = 1.1)
  text(x, y, label, cex = 0.74, family = "Helvetica", col = "#1A202C")
}

draw_arrow <- function(x0, y0, x1, y1) {
  arrows(x0, y0, x1, y1, length = 0.07, lwd = 1.1, col = "#4A5568")
}

render_with_base_r <- function(figure_path) {
  grDevices::pdf(figure_path, width = 7.2, height = 1.9, paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)

  par(mar = c(0.2, 0.2, 0.2, 0.2), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = NA)

  xs <- c(seq(0.10, 0.90, length.out = 5), 0.90)
  ys <- c(rep(0.50, 4), 0.76, 0.24)
  node_width <- 0.17
  half_width <- node_width / 2
  labels <- c(
    "Search and\ndiscovery\ncatalogue, URL,\nor API search",
    "Metadata\nhierarchy, values,\ncodelists",
    "Query\nselection and\nvalidation",
    "Retrieval\nrequest and\noptional batching",
    "Conversion\ndata.frame\nor matrix",
    "Comments\nand citation\nnotes, status,\nsource"
  )

  for (i in seq_along(xs)) {
    draw_node(xs[i], ys[i], labels[i], width = node_width, height = 0.36,
              fill = if (i >= 5) "#EDF2F7" else "#F7FAFC")
  }
  for (i in 1:3) {
    draw_arrow(xs[i] + half_width, ys[i], xs[i + 1L] - half_width, ys[i + 1L])
  }
  for (i in 5:6) {
    draw_arrow(xs[4] + half_width, ys[4], xs[i] - half_width, ys[i])
  }

  invisible(TRUE)
}

if (!render_with_graphviz(dot_path, figure_path)) {
  render_with_base_r(figure_path)
}

message("Wrote workflow figure: ", figure_path)

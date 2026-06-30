# Generate the package workflow figure for the JSS manuscript.
#
# The workflow is specified as a Graphviz DOT graph. If Graphviz is
# available, the DOT graph is rendered directly. Otherwise, a base R
# fallback draws the same workflow to PDF so the manuscript can still be
# reproduced on systems without Graphviz.

script_path <- tryCatch({
  args <- commandArgs(FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    normalizePath(sub("^--file=", "", file_arg[[1]]))
  } else {
    normalizePath(sys.frame(1)$ofile)
  }
}, error = function(e) NA_character_)

script_dir <- if (!is.na(script_path)) dirname(script_path) else getwd()
dot_path <- file.path(script_dir, "workflow-model.dot")
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
  conversion -> comments;
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
  grDevices::pdf(figure_path, width = 9.4, height = 3.1, paper = "special")
  on.exit(grDevices::dev.off(), add = TRUE)

  par(mar = c(0.2, 0.2, 0.2, 0.2), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = NA)

  xs <- seq(0.10, 0.90, length.out = 6)
  y <- 0.56
  labels <- c(
    "Search and\ndiscovery\ncatalogue, URL,\nor API search",
    "Metadata\nhierarchy, values,\ncodelists",
    "Query\nselection and\nvalidation",
    "Retrieval\nrequest and\noptional batching",
    "Conversion\ndata.frame\nor matrix",
    "Comments\nand citation\nnotes, status,\nsource"
  )

  for (i in seq_along(xs)) {
    draw_node(xs[i], y, labels[i], width = 0.13, height = 0.22,
              fill = if (i >= 5) "#EDF2F7" else "#F7FAFC")
  }
  for (i in seq_len(length(xs) - 1L)) {
    draw_arrow(xs[i] + 0.065, y, xs[i + 1L] - 0.065, y)
  }

  invisible(TRUE)
}

if (!render_with_graphviz(dot_path, figure_path)) {
  render_with_base_r(figure_path)
}

message("Wrote workflow figure: ", figure_path)

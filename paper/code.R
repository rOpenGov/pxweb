#' ---
#' title: "Replication code for Opening Up Official Statistics in R with pxweb"
#' output:
#'   html_document:
#'     toc: true
#'     number_sections: true
#' ---
#'
#' This script reproduces the code-oriented results in the manuscript and
#' regenerates the case-study figure. By default it uses the cached Statistics
#' Sweden metadata, data, comments, and citation files included with the paper
#' sources. To re-download data from the live PxWeb API and refresh the caches,
#' run with `PXWEB_PAPER_REFRESH_DATA=true`.

#+ setup, include=FALSE
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7.2,
  fig.height = 6.8,
  fig.align = "center"
)

script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_file <- if (length(script_arg) > 0) sub("^--file=", "", script_arg[[1]]) else "code.R"
candidate_dirs <- unique(c(
  getwd(),
  file.path(getwd(), "paper"),
  dirname(normalizePath(script_file, mustWork = FALSE))
))
paper_dir <- candidate_dirs[
  file.exists(file.path(candidate_dirs, "article.tex")) &
    file.exists(file.path(candidate_dirs, "case-study.R"))
]
if (length(paper_dir) < 1) {
  stop("Run code.R from the paper directory or the package root.", call. = FALSE)
}
paper_dir <- normalizePath(paper_dir[[1]])

if (!requireNamespace("pxweb", quietly = TRUE)) {
  stop("The pxweb package is required to run this replication script.", call. = FALSE)
}
if (utils::packageVersion("pxweb") < "1.0.0") {
  stop("This replication script requires pxweb >= 1.0.0.", call. = FALSE)
}

refresh_data <- identical(tolower(Sys.getenv("PXWEB_PAPER_REFRESH_DATA")), "true")
cache_file <- function(name) file.path(paper_dir, "data", name)

require_cache <- function(path) {
  if (!file.exists(path)) {
    stop(
      "Missing cached file: ", path,
      ". Re-run with PXWEB_PAPER_REFRESH_DATA=true to create it.",
      call. = FALSE
    )
  }
}

abbreviate_comment <- function(x, width = 58) {
  too_long <- nchar(x, type = "width") > width
  x[too_long] <- paste0(substr(x[too_long], 1, width - 4), " ...")
  x
}

#' # Installation
#'
#' Released versions can be installed from CRAN.

#+ install, eval=FALSE
# install.packages("pxweb")
library("pxweb")

#' # Package setup
#'
#' The manuscript describes the submitted package source. The catalogue count
#' below is the value reported in the manuscript.

#+ package-setup
library("pxweb")

packageVersion("pxweb")
length(pxweb_api_catalogue())

#' # Search and metadata URL
#'
#' The manuscript locates the Statistics Sweden population table through the
#' PxWeb API version 2 search endpoint. The live search is evaluated only when
#' `PXWEB_PAPER_REFRESH_DATA=true`; otherwise the known metadata URL is used so
#' that the replication file is stable without network access.

#+ search-metadata
metadata_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB638/metadata?lang=en"
search_cache <- cache_file("scb-population-search-results.rds")

if (refresh_data) {
  search_results <- pxweb_search(
    query = "Population by region marital status age sex",
    api_url = "https://statistikdatabasen.scb.se/api/v2",
    lang = "en",
    page_size = 5
  )
  saveRDS(search_results, search_cache)
} else if (file.exists(search_cache)) {
  search_results <- readRDS(search_cache)
} else {
  search_results <- data.frame(
    id = "TAB638",
    metadata_url = metadata_url,
    stringsAsFactors = FALSE
  )
}

url <- search_results$metadata_url[search_results$id == "TAB638"][1]
url

#' # Metadata and codelists
#'
#' Metadata are retrieved with `pxweb_get()` and cached. The codelist display
#' shows the provider-supplied `id` values used below.

#+ metadata-codelists
metadata_cache <- cache_file("scb-population-metadata.rds")

if (refresh_data || !file.exists(metadata_cache)) {
  meta <- pxweb_get(url)
  saveRDS(meta, metadata_cache)
} else {
  meta <- readRDS(metadata_cache)
}

age_codelists <- pxweb_codelists(meta, variable = "Alder")
age_codelists_print <- age_codelists[, c("id", "type", "label")]
age_codelists_print

#' # Query construction
#'
#' The short workflow example illustrates helper selections such as latest time
#' period and aggregation codelists.

#+ helper-query
age_5_year_id <- age_codelists$id[age_codelists$label == "5-year intervals"][1]

query <- list(
  Region = "00",
  Civilstand = "OG",
  Alder = pxweb_aggregation(age_5_year_id),
  Kon = c("1", "2"),
  ContentsCode = "BE0101N1",
  Tid = pxweb_latest()
)

px_query <- pxweb_query(query)
px_query

#' # Retrieval, conversion, comments, and citation
#'
#' The small query is cached as a `pxweb_data` object so that conversion,
#' comments, and citation examples can be reproduced without a live API call.

#+ retrieval-conversion-comments
small_data_cache <- cache_file("scb-small-px-data.rds")

if (refresh_data || !file.exists(small_data_cache)) {
  px_data <- pxweb_get(url, query = query, verbose = FALSE)
  saveRDS(px_data, small_data_cache)
} else {
  px_data <- readRDS(small_data_cache)
}

px_df <- as.data.frame(
  px_data,
  column.name.type = "code",
  variable.value.type = "code"
)
head(px_df, 3)

comments <- pxweb_data_comments(px_data)
as.data.frame(comments, stringsAsFactors = FALSE)

pxweb_cite(px_data)

#' # Case-study query
#'
#' The case study uses all Swedish counties, all marital-status categories,
#' single-year ages from 0 to 100+, both sexes, the population count content
#' variable, and all years from 1968 to 2024.

#+ case-study-query
years <- as.character(1968:2024)
ages <- c(as.character(0:99), "100+")
county_codes <- c(
  "01", "03", "04", "05", "06", "07", "08", "09", "10", "12", "13",
  "14", "17", "18", "19", "20", "21", "22", "23", "24", "25"
)

large_query <- list(
  Region = county_codes,
  Civilstand = c("OG", "G", "SK", "\u00c4NKL"),
  Alder = ages,
  Kon = c("1", "2"),
  ContentsCode = "BE0101N1",
  Tid = years
)

prod(lengths(large_query))

#' # Case-study data and figure
#'
#' `case-study.R` runs the large data retrieval when refreshing the live data.
#' Otherwise it reads the cached data file and regenerates the figure.

#+ case-study-source
source(file.path(paper_dir, "case-study.R"), chdir = TRUE)

population <- readRDS(cache_file("scb-population-counties-1968-2024.rds"))
dim(population)
head(population, 3)

comments_cache <- cache_file("scb-population-comments.rds")
require_cache(comments_cache)
comments_df <- readRDS(comments_cache)
comments_display <- comments_df[, c("comment_type", "comment")]
comments_display$comment <- gsub(intToUtf8(8211), "-", comments_display$comment, fixed = TRUE)
comments_display$comment <- abbreviate_comment(comments_display$comment)
head(comments_display, 2)

#' The generated case-study figure is included below.

#+ case-study-figure, echo=FALSE, out.width="100%"
knitr::include_graphics(file.path(paper_dir, "case-study-population.pdf"))

#' # Cached citation text
#'
#' The live-data branch in `case-study.R` records `pxweb_cite()` output. When
#' running from the cached data, the stored citation is printed here.

#+ citation
citation_file <- cache_file("scb-population-citation.txt")
require_cache(citation_file)
writeLines(readLines(citation_file, warn = FALSE))

#' # Session information

#+ session-info
sessionInfo()

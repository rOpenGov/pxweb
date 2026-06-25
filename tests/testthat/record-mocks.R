# Regenerate httptest fixtures used by with_pxweb_mock_api() tests.
#
# Run from the package root:
# PXWEB_RUN_LIVE_TESTS=true Rscript tests/testthat/record-mocks.R
#
# The generated files are stored under tests/testthat using httptest's URL-based
# fixture layout.

args <- commandArgs(FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) > 0) {
  script_path <- normalizePath(sub("^--file=", "", file_arg[[1]]))
  root <- normalizePath(file.path(dirname(script_path), "..", ".."))
  setwd(root)
}

if (!requireNamespace("httptest", quietly = TRUE)) {
  stop("Package 'httptest' is required to record mocks.", call. = FALSE)
}
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required to load pxweb from source.", call. = FALSE)
}

devtools::load_all(".", quiet = TRUE)
httptest::.mockPaths(file.path(getwd(), "tests", "testthat"))
options(httptest.verbose = TRUE)

v1_table_sv <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
v1_table_en <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
v1_node_sv <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
v1_node_en <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A"
v1_root_sv <- "https://api.scb.se/OV0104/v1/doris/sv"
v1_constructor_sv <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"
v1_constructor_en <- "https://api.scb.se/OV0104/v1/doris/en/ssd/START/ME/ME0104/ME0104C/ME0104T24"
v2_table <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974?lang=sv"
v2_metadata <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"
v2_data <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/data?lang=sv"
v2_equivalent <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB638/metadata?lang=sv"

json_px_query <- readLines(file.path("tests", "testthat", "test_data", "test_query_px.json"))
json_sdmx_query <- readLines(file.path("tests", "testthat", "test_data", "test_query_sdmx.json"))
json_single_query <- file.path(
  system.file(package = "pxweb"),
  "extdata", "test_files", "json_queries", "json_single_query_test.json"
)
json_query_example <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json_query_example.json"
)
json_query_variables <- file.path(
  system.file(package = "pxweb"),
  "extdata", "examples", "json_query_variables_example.json"
)

invisible(pxweb:::pxweb_clear_cache())

invisible(httptest::capture_requests({
  pxweb_get(v1_table_sv, query = json_px_query)
  pxweb_get(v1_table_sv, query = json_sdmx_query)
  pxq <- pxweb_query(json_px_query)
  pxq$response$format <- "sdmx"
  pxweb_get(v1_table_sv, pxq)

  pxweb_get(v1_table_sv)
  pxweb_get(v1_node_sv)
  pxweb(v1_root_sv)
  pxweb_get(v1_root_sv)
  suppressWarnings(pxweb_get(url = v1_table_sv, query = json_single_query))

  jqf <- paste(readLines(json_single_query), collapse = " ")
  class(jqf) <- "json"
  pxweb_get(v1_table_sv, query = pxweb_query(gsub("json", "json-stat", jqf)))
  pxweb_get(v1_table_sv, query = pxweb_query(gsub("json", "jsonstat", jqf)))

  pxweb_get(v1_table_sv, query = json_query_example)
  pxweb_get_data(
    url = v1_table_sv,
    query = json_query_example,
    column.name.type = "text",
    variable.value.type = "text"
  )

  invisible(pxweb:::pxweb_clear_cache())
  suppressWarnings(pxweb_get(url = v1_table_en, query = json_query_example))

  pxweb_query_list <- list(
    Region = "00",
    Alder = "tot",
    ContentsCode = "BE0101N1",
    Tid = c("2016", "2017", "2018", "2019")
  )
  px <- pxweb(v1_table_en)
  px$config$max_values_to_download <- 2
  pxweb_get(url = px, query = pxweb_query_list)
  px$config$max_values_to_download <- 4
  pxweb_get(url = px, query = pxweb_query_list)
  pxweb_get(url = v1_table_en)
  pxweb_get(url = v1_node_en)
  pxweb_get(url = v1_table_en, query = pxweb_query_list)
  pxmo <- pxweb_get(url = v1_table_en)
  pxweb_advanced_get(url = v1_table_en, query = pxweb_query_list, pxmdo = pxmo)

  invisible(pxweb:::pxweb_clear_cache())
  suppressWarnings(pxweb_get(url = v1_table_sv, query = json_query_variables))

  invisible(pxweb:::pxweb_clear_cache())
  pxweb(v1_constructor_sv)
  pxweb(v1_constructor_en)
  pxweb("https://api.scb.se/OV0104/v1/doris/sv?config")

  invisible(capture.output({
    pxe <- pxweb:::pxweb_explorer.character(v1_constructor_sv)
    pxweb:::pxweb_interactive_input(pxe, test_input = "1")
    pxweb:::pxweb_interactive_input(pxe, test_input = "b")

    pxe <- pxweb:::pxweb_explorer.character(v1_constructor_sv)
    pxe_star <- pxweb:::pxweb_interactive_input(pxe, test_input = "*")
    pxweb:::pxweb_interactive_input(pxe_star, test_input = "e")

    pxe <- pxweb:::pxweb_explorer.character(v1_constructor_sv)
    pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "e")
    pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1:2")
    pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1")
    pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1")
    pxe <- pxweb:::pxweb_interactive_input(pxe, test_input = "1")
    pxweb:::pxe_interactive_get_data(pxe, test_input = c("n", "y", "n", "n"))
  }))

  pxweb(v2_metadata)
  pxweb_get(v2_table)
  metadata <- pxweb_get(v2_metadata)
  raw_metadata <- attr(metadata, "pxweb_metadata_v2")
  first_year <- names(raw_metadata$dimension$Tid$category$index)[1]
  query <- list(
    InrikesUtrikes = "83",
    Alder = "0",
    Kon = c("1", "2"),
    ContentsCode = "000005NO",
    Tid = first_year
  )
  pxweb_get(v2_metadata, query = query, verbose = FALSE)
  pxweb_get(v2_table, query = query, verbose = FALSE)
  pxweb_get(v2_data, query = query, verbose = FALSE)

  equivalent_query <- list(
    Region = "00",
    Civilstand = "OG",
    Alder = "0",
    Kon = "1",
    ContentsCode = "BE0101N1",
    Tid = "2024"
  )
  pxweb_get(v1_table_sv, query = equivalent_query, verbose = FALSE)
  pxweb_get(v2_equivalent, query = equivalent_query, verbose = FALSE)
}, simplify = TRUE))

invisible(pxweb:::pxweb_clear_cache())

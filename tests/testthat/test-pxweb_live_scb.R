context("pxweb_live_scb")

# These live smoke tests guard against upstream SCB API drift. If they fail
# because the API changed and pxweb still supports the new response shape,
# update the httptest fixtures with tests/testthat/record-mocks.R.

test_that(desc = "PXWEB API v1 live smoke test with Statistics Sweden", {
  skip_on_cran()
  skip_if_not_live_api()
  pxweb_clear_cache()

  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  query <- file.path(
    system.file(package = "pxweb"),
    "extdata", "test_files", "json_queries", "json_single_query_test.json"
  )

  expect_silent(metadata <- pxweb_get(url))
  expect_s3_class(metadata, "pxweb_metadata")
  expect_true(all(c("Civilstand", "ContentsCode", "Tid") %in% names(pxweb_metadata_dim(metadata))))

  expect_silent(data <- suppressWarnings(pxweb_get(url = url, query = query, verbose = FALSE)))
  expect_s3_class(data, "pxweb_data")
  expect_true(all(pxweb_data_dim(data) > 0))
})

test_that(desc = "PXWEB API v2 live smoke test with Statistics Sweden", {
  skip_on_cran()
  skip_if_not_live_api()
  pxweb_clear_cache()

  table_id <- "TAB5974"
  metadata_url <- paste0("https://statistikdatabasen.scb.se/api/v2/tables/", table_id, "/metadata")

  expect_silent(px <- pxweb(metadata_url))
  expect_equal(px$version, "v2")
  expect_equal(px$paths$api_subpath$path, "api/v2")

  expect_silent(metadata <- pxweb_get(metadata_url))
  expect_s3_class(metadata, "pxweb_metadata")
  raw_metadata <- attr(metadata, "pxweb_metadata_v2")
  expect_equal(raw_metadata$extension$px$tableid, table_id)
  expect_true(all(c("InrikesUtrikes", "Alder", "Kon", "ContentsCode", "Tid") %in% names(pxweb_metadata_dim(metadata))))

  first_year <- names(raw_metadata$dimension$Tid$category$index)[1]
  query <- list(
    InrikesUtrikes = "83",
    Alder = "0",
    Kon = "1",
    ContentsCode = "000005NO",
    Tid = first_year
  )

  expect_silent(data <- pxweb_get(metadata_url, query = query, verbose = FALSE))
  expect_s3_class(data, "pxweb_data_v2")
  expect_true(all(pxweb_data_dim(data) > 0))
})

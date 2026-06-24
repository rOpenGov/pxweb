
test_that(desc = "PXWEB API version detection is local and stable", {
  v1_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"
  v2_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"

  expect_equal(pxweb_detect_version(v1_url), "v1")
  expect_equal(pxweb_detect_version(v2_url), "v2")
})

test_that(desc = "PXWEB API v2 URL helpers build endpoint URLs", {
  v2_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"

  expect_equal(pxweb_v2_api_subpath(v2_url), "api/v2")
  expect_equal(
    build_pxweb_v2_tables_url(v2_url),
    "https://statistikdatabasen.scb.se/api/v2/tables"
  )
  expect_equal(
    build_pxweb_v2_table_metadata_url(v2_url, "TAB5974"),
    "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"
  )
  expect_equal(
    build_pxweb_v2_table_data_url(v2_url, "TAB5974"),
    "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/data"
  )
})

test_that(desc = "PXWEB API v2 response parser routes fixture responses", {
  metadata_response <- readRDS(test_path("test_data/pxweb_metadata_response_v2.rds"))
  table_response <- readRDS(test_path("test_data/pxweb_table_response_v2.rds"))

  expect_silent(metadata <- pxweb_parse_response(metadata_response))
  expect_s3_class(metadata, "pxweb_metadata")
  expect_equal(attr(metadata, "pxweb_metadata_v2")$extension$px$tableid, "TAB5974")

  expect_silent(table <- pxweb_parse_response(table_response))
  expect_s3_class(table, "pxweb_table_response_v2")
  expect_equal(table$id, "TAB5974")
})

test_that(desc = "PXWEB API v2 constructor smoke test", {
  # Keep live API coverage small. Response parsing and data queries are fixture
  # tested until v2 metadata/data support is implemented.
  skip_on_cran()
  skip_if_not_live_api()

  metadata_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"
  expect_silent(pxapi_v2 <- pxweb(url = metadata_url))

  expect_equal(pxapi_v2$version, "v2")
  expect_equal(pxapi_v2$paths$api_subpath$path, "api/v2")
  expect_named(pxapi_v2$config, c("calls_per_period", "period_in_seconds", "max_values_to_download", "CORS"))
  expect_true(pxapi_v2$config$max_values_to_download > 0)
})

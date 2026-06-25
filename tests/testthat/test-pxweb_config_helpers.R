context("pxweb_config_helpers")

pxweb_test_response <- function(body, url = "https://example.test/api/v1/en?config",
                                request_url = url, status = 200L,
                                all_headers = list(list(status = status))) {
  json <- jsonlite::toJSON(body, auto_unbox = TRUE)
  structure(
    list(
      url = url,
      status_code = status,
      headers = structure(
        list("content-type" = "application/json; charset=utf-8"),
        class = c("insensitive", "list")
      ),
      content = charToRaw(json),
      request = structure(list(url = request_url), class = "request"),
      all_headers = all_headers
    ),
    class = "response"
  )
}

pxweb_test_api_object <- function(url = "https://example.test/api/v1/en/table") {
  structure(
    list(
      url = parse_url_or_fail(url),
      version = pxweb_detect_version(url),
      config = NULL,
      calls = list(time_stamps = list()),
      paths = list(rda_file_path = tempfile())
    ),
    class = "list"
  )
}

test_that("PXWEB config response detection handles v1, v2, and failures", {
  expect_true(pxweb:::is_pxweb_config_response(pxweb_test_response(list(
    maxCells = 100,
    maxCalls = 10,
    timeWindow = 60,
    CORS = "*"
  )), "v1"))

  expect_true(pxweb:::is_pxweb_config_response(pxweb_test_response(list(
    maxDataCells = 100,
    maxCallsPerTimeWindow = 10,
    timeWindow = 60
  )), "v2"))

  expect_false(pxweb:::is_pxweb_config_response(pxweb_test_response(list(
    maxCells = 100,
    maxCalls = 10
  )), "v1"))

  expect_false(pxweb:::is_pxweb_config_response(pxweb_test_response(list(
    maxDataCells = 100,
    maxCallsPerTimeWindow = 10,
    timeWindow = 60
  ), status = 500L), "v2"))
})

test_that("PXWEB redirect helper strips query details and reports redirects", {
  response <- pxweb_test_response(
    list(maxCells = 100, maxCalls = 10, timeWindow = 60, CORS = "*"),
    url = "https://new.example.test/api/v1/en?config",
    request_url = "http://old.example.test/api/v1/en?config",
    all_headers = list(list(status = 301L), list(status = 200L))
  )

  redirected <- pxweb:::http_was_redirected(response)

  expect_true(redirected$was_redirected)
  expect_equal(redirected$redirected_from, "http://old.example.test/api/v1/en")
  expect_equal(redirected$redirected_to, "https://new.example.test/api/v1/en")
})

test_that("PXWEB config is normalized for v1 and v2 APIs", {
  v1_response <- pxweb_test_response(list(
    maxCells = 100,
    maxCalls = 10,
    timeWindow = 60,
    CORS = "*"
  ))
  v2_response <- pxweb_test_response(
    list(
      maxDataCells = 200,
      maxCallsPerTimeWindow = 20,
      timeWindow = 120,
      CORS = "*"
    ),
    url = "https://example.test/api/v2/config"
  )

  testthat::local_mocked_bindings(
    GET = function(url, ...) {
      if (grepl("/v2/", url, fixed = TRUE)) v2_response else v1_response
    },
    .package = "httr"
  )
  testthat::local_mocked_bindings(
    pxweb_http_log_response = function(r) invisible(NULL),
    http_was_redirected = function(r) list(
      was_redirected = FALSE,
      redirected_from = NULL,
      redirected_to = NULL
    ),
    .package = "pxweb"
  )

  v1 <- pxweb:::pxweb_add_config(pxweb_test_api_object())
  expect_equal(v1$config$calls_per_period, 10)
  expect_equal(v1$config$period_in_seconds, 60)
  expect_equal(v1$config$max_values_to_download, 100)
  expect_equal(v1$config$CORS, "*")
  expect_length(v1$calls$time_stamps, 1)

  v2 <- pxweb:::pxweb_add_config(pxweb_test_api_object("https://example.test/api/v2/tables/TAB1/metadata"))
  expect_equal(v2$config$calls_per_period, 20)
  expect_equal(v2$config$period_in_seconds, 120)
  expect_equal(v2$config$max_values_to_download, 200)
  expect_null(v2$config$CORS)
  expect_length(v2$calls$time_stamps, 1)
})

test_that("PXWEB URL and RDA path S3 helpers support common input classes", {
  url <- "https://example.test:8443/api/v1/en/table"
  parsed <- parse_url_or_fail(url)
  px <- structure(
    list(
      url = parsed,
      paths = list(rda_file_path = tempfile())
    ),
    class = c("pxweb", "list")
  )
  entry <- pxweb:::pxweb_api_catalogue_entry(list(
    description = "Example API",
    url = "https://example.test/api/[version]/[lang]",
    version = c("v1", "v2"),
    lang = c("en", "sv"),
    alias = "example"
  ))

  expect_equal(pxweb:::build_pxweb_url(url), url)
  expect_equal(pxweb:::build_pxweb_url(list(url = parsed)), url)
  expect_equal(pxweb:::build_pxweb_url(px), url)
  expect_equal(pxweb:::build_pxweb_url(entry), "https://example.test/api/v1/en")
  expect_equal(pxweb:::build_pxweb_config_url(px), paste0(url, "?config"))
  expect_equal(
    pxweb:::build_pxweb_config_url(parse_url_or_fail("https://example.test/api/v2/tables/TAB1/metadata")),
    "https://example.test/api/v2/config"
  )

  expect_match(pxweb:::build_pxweb_rda_file_path(url), "example\\.test\\.rda$")
  expect_equal(pxweb:::build_pxweb_rda_file_path(px), px$paths$rda_file_path)
  expect_match(pxweb:::build_pxweb_rda_file_path(list(url = parsed)), "example\\.test\\.rda$")
})

# Test suits for the examples in the documentation

context("pxweb_test_api")

pxweb_test_response <- function(url, status_code = 200L, body = list()) {
  structure(
    list(
      url = url,
      status_code = status_code,
      headers = structure(list("content-type" = "application/json"), class = "insensitive"),
      all_headers = list(list(status = status_code)),
      content = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
    ),
    class = "response"
  )
}

pxweb_test_catalogue_entry <- function(url, version = "v1", lang = "en") {
  structure(
    list(
      description = "Test API",
      url = url,
      version = version,
      lang = lang
    ),
    class = c("pxweb_api_catalogue_entry", "list")
  )
}

test_that(desc = "Catalogue config audit reports endpoint status", {
  get_calls <- character()
  local_mocked_bindings(
    GET = function(url, ...) {
      get_calls <<- c(get_calls, url)
      if (grepl("missing", url)) {
        return(pxweb_test_response(
          url = url,
          body = list(maxCells = 1L, maxCalls = 2L, timeWindow = 3L)
        ))
      }
      if (grepl("bad", url)) {
        return(pxweb_test_response(url = url, status_code = 500L, body = list()))
      }
      pxweb_test_response(
        url = url,
        body = list(maxCells = 1L, maxCalls = 2L, timeWindow = 3L, CORS = TRUE)
      )
    },
    .package = "httr"
  )

  apis <- list(
    ok.example.com = pxweb_test_catalogue_entry("https://ok.example.com/api/[version]/[lang]"),
    missing.example.com = pxweb_test_catalogue_entry("https://missing.example.com/api/[version]/[lang]"),
    bad.example.com = pxweb_test_catalogue_entry("https://bad.example.com/api/[version]/[lang]"),
    old.example.com = pxweb_test_catalogue_entry("https://old.example.com/api/[version]/[lang]", version = "v0")
  )

  audit <- pxweb_audit_api_catalogue_config(apis = apis, verbose = FALSE)

  expect_equal(audit$api, names(apis))
  expect_equal(audit$config_url[1], "https://ok.example.com/api/v1/en?config")
  expect_equal(audit$ok, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(audit$missing_fields[2], "CORS")
  expect_equal(audit$status_code[3], 500L)
  expect_equal(audit$error[3], "HTTP 500")
  expect_equal(audit$error[4], "Unsupported PXWEB API version: v0")
  expect_equal(get_calls, audit$config_url[1:3])
})

test_that(desc = "Mixed node levels object", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  skip_if_not_live_api()

  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0301/"
  expect_output(res <- suppressWarnings(pxweb_test_api(url)), regexp = "node.+and.+table")
  expect_true(all(res$checked))
  # expect_true(all(!res$error)) # FIXME this also fails

  expect_silent(api_paths <- pxweb:::pxweb_test_create_api_paths(apis = pxweb_api_catalogue()))
  expect_true(all(c(
    "https://api.scb.se/OV0104/v1/doris/en",
    "https://api.scb.se/OV0104/v1/doris/sv"
  ) %in% api_paths$paths))
  expect_true(all(c("api.scb.se", "statfin.stat.fi") %in% api_paths$api))
  expect_true(all(c("v1", "en", "sv") %in% c(api_paths$version, api_paths$lang)))
})

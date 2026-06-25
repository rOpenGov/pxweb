test_that(desc = "pxweb_search searches PXWEB API v2 table endpoint", {
  captured_request <- NULL

  testthat::local_mocked_bindings(
    pxweb_search_request = function(url, query, ...) {
      captured_request <<- list(url = url, query = query)
      structure(list(), class = "response")
    },
    pxweb_search_response = function(response) {
      list(
        language = "en",
        tables = list(
          list(
            id = "TAB6473",
            label = "Population statistics by region and sex. Month 2025M01-2026M04",
            description = "",
            updated = "2026-06-08T06:00:00Z",
            firstPeriod = "2025M01",
            lastPeriod = "2026M04",
            source = "Statistics Sweden",
            subjectCode = "BE",
            timeUnit = "Monthly",
            variableNames = c("region", "population changes", "sex", "observations", "month"),
            paths = list(list(
              list(id = "BE", label = "Population", sortCode = "Population")
            )),
            links = list(
              list(rel = "self", href = "https://statistikdatabasen.scb.se/api/v2/tables/TAB6473?lang=en"),
              list(rel = "metadata", href = "https://statistikdatabasen.scb.se/api/v2/tables/TAB6473/metadata?lang=en"),
              list(rel = "data", href = "https://statistikdatabasen.scb.se/api/v2/tables/TAB6473/data?lang=en&outputFormat=json-stat2")
            )
          )
        )
      )
    },
    .package = "pxweb"
  )

  result <- pxweb_search(
    "population",
    api_url = "https://statistikdatabasen.scb.se/api/v2",
    lang = "en",
    page_size = 1
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, "TAB6473")
  expect_equal(
    result$metadata_url,
    "https://statistikdatabasen.scb.se/api/v2/tables/TAB6473/metadata?lang=en"
  )
  expect_equal(captured_request$url, "https://statistikdatabasen.scb.se/api/v2/tables")
  expect_equal(captured_request$query$query, "population")
  expect_equal(captured_request$query$lang, "en")
  expect_equal(captured_request$query$pageSize, 1)
})

test_that(desc = "pxweb_search searches PXWEB API v1 database root", {
  captured_request <- NULL

  testthat::local_mocked_bindings(
    pxweb_search_request = function(url, query, ...) {
      captured_request <<- list(url = url, query = query)
      structure(list(), class = "response")
    },
    pxweb_search_response = function(response) {
      list(
        list(
          id = "139e.px",
          path = "/vaenn",
          title = "139e -- Vital statistics by sex in population projections",
          score = 1.17395484,
          published = "2026-06-06T21:04:00"
        )
      )
    },
    .package = "pxweb"
  )

  result <- pxweb_search(
    "population",
    api_url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin"
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$id, "139e.px")
  expect_equal(
    result$url,
    "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin/vaenn/139e.px"
  )
  expect_equal(captured_request$url, "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin")
  expect_equal(captured_request$query, list(query = "population"))
})

test_that(desc = "pxweb_search reports informative API URL errors", {
  expect_error(
    pxweb_search("population", api_url = "https://statistikdatabasen.scb.se/api/v2/tables"),
    "api_url must be the API root"
  )
  expect_error(
    pxweb_search("population", api_url = "https://pxdata.stat.fi/PxWeb/api/v1/en"),
    "must include the language and database id"
  )
  expect_error(
    pxweb_search("population", api_url = "https://example.test/no-version"),
    "Cannot detect PXWEB API version"
  )
})

test_that(desc = "pxweb_search reports informative v1 response shape errors", {
  testthat::local_mocked_bindings(
    pxweb_search_request = function(url, query, ...) {
      structure(list(), class = "response")
    },
    pxweb_search_response = function(response) {
      list(list(dbid = "StatFin", text = "StatFin"))
    },
    .package = "pxweb"
  )

  expect_error(
    pxweb_search("population", api_url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin"),
    "did not look like search hits"
  )
})

context("pxweb_test_api_endpoint_unit")

pxweb_test_px <- function(url = "https://example.test/api/v1/en") {
  structure(
    list(
      url = parse_url_or_fail(url),
      version = pxweb_detect_version(url),
      paths = list(
        api_subpath = list(path = "api/v1/en", vector = c("api", "v1", "en"))
      )
    ),
    class = c("pxweb", "list")
  )
}

pxweb_test_metadata <- function() {
  pxweb_metadata(list(
    title = "Example table",
    variables = list(
      list(code = "Region", text = "region", values = c("01", "03"), valueTexts = c("Stockholm", "Uppsala")),
      list(code = "Tid", text = "year", values = c("2024", "2025"), valueTexts = c("2024", "2025"), time = TRUE)
    )
  ))
}

pxweb_test_data <- function() {
  pxweb_data(list(
    columns = list(
      list(code = "Region", text = "region", type = "c"),
      list(code = "Tid", text = "year", type = "t"),
      list(code = "value", text = "value", type = "d")
    ),
    comments = list(),
    data = list(
      list(key = list("01", "2024"), values = list("10"))
    )
  ))
}

test_that("PXWEB API endpoint time limit helper reports elapsed time", {
  limit <- pxweb:::pxweb_test_time_limit(1)
  expect_s3_class(limit, "pxweb_test_time_limit")
  expect_false(pxweb:::is_test_time_limit_reached(limit))

  limit$start_time <- Sys.time() - 2
  expect_true(pxweb:::is_test_time_limit_reached(limit))
})

test_that("PXWEB API endpoint helper expands catalogue path templates", {
  apis <- list(pxweb:::pxweb_api_catalogue_entry(list(
    description = "Example API",
    url = "https://example.test/api/[version]/[lang]",
    version = c("v1", "v2"),
    lang = c("en", "sv"),
    alias = "example"
  )))

  paths <- pxweb:::pxweb_test_create_api_paths(apis)

  expect_equal(paths$idx, rep(1, 4))
  expect_equal(paths$paths, c(
    "https://example.test/api/v1/en",
    "https://example.test/api/v1/sv",
    "https://example.test/api/v2/en",
    "https://example.test/api/v2/sv"
  ))
})

test_that("PXWEB API endpoint data frame helper annotates levels", {
  px <- pxweb_test_px()
  testthat::local_mocked_bindings(
    pxweb_get = function(x, ...) pxweb_levels(list(
      list(id = "A", type = "l", text = "Node A"),
      list(id = "T1", type = "t", text = "Table 1")
    )),
    .package = "pxweb"
  )

  df <- pxweb:::pxweb_get_api_test_data_frame(px)

  expect_equal(df$id, c("A", "T1"))
  expect_equal(df$type, c("l", "t"))
  expect_equal(df$path, paste0("https://example.test/api/v1/en/", c("A", "T1")))
  expect_false(any(df$checked))
  expect_false(any(df$error))
  expect_false(any(df$download_error))
  expect_true(all(is.na(df$updated)))
})

test_that("PXWEB API endpoint traversal can touch the first table without live API", {
  level_calls <- 0L
  testthat::local_mocked_bindings(
    pxweb = function(url) pxweb_test_px(url),
    pxweb_get_api_test_data_frame = function(x) {
      level_calls <<- level_calls + 1L
      if (level_calls == 1L) {
        data.frame(
          id = c("A", "T1"),
          type = c("l", "t"),
          text = c("Node A", "Table 1"),
          updated = NA,
          path = c("https://example.test/api/v1/en/A", "https://example.test/api/v1/en/T1"),
          checked = FALSE,
          error = FALSE,
          download_error = FALSE,
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          id = "T2",
          type = "t",
          text = "Table 2",
          updated = NA,
          path = "https://example.test/api/v1/en/A/T2",
          checked = FALSE,
          error = FALSE,
          download_error = FALSE,
          stringsAsFactors = FALSE
        )
      }
    },
    pxweb_get = function(url, query = NULL, ...) {
      if (is.null(query)) pxweb_test_metadata() else pxweb_test_data()
    },
    .package = "pxweb"
  )

  res <- pxweb_test_api("https://example.test/api/v1/en", test_type = "touch", verbose = FALSE)

  expect_true(any(res$type == "t"))
  expect_true(any(res$checked))
  expect_false(any(res$error, na.rm = TRUE))
  expect_false(any(res$download_error, na.rm = TRUE))
  expect_true(any(res$obs > 0, na.rm = TRUE))
})

test_that("PXWEB API endpoint traversal records node and download errors", {
  level_calls <- 0L
  testthat::local_mocked_bindings(
    pxweb = function(url) {
      if (grepl("/bad-node$", url)) stop("node failed")
      pxweb_test_px(url)
    },
    pxweb_get_api_test_data_frame = function(x) {
      level_calls <<- level_calls + 1L
      if (level_calls == 1L) {
        data.frame(
          id = c("bad-node", "T1"),
          type = c("l", "t"),
          text = c("Broken node", "Table 1"),
          updated = NA,
          path = c("https://example.test/api/v1/en/bad-node", "https://example.test/api/v1/en/T1"),
          checked = FALSE,
          error = FALSE,
          download_error = FALSE,
          stringsAsFactors = FALSE
        )
      } else {
        stop("unexpected nested traversal")
      }
    },
    pxweb_get = function(url, query = NULL, ...) {
      if (is.null(query)) pxweb_test_metadata() else stop("download failed")
    },
    .package = "pxweb"
  )

  res <- pxweb_test_api("https://example.test/api/v1/en", test_type = "first", verbose = FALSE)

  expect_true(res$error[res$id == "bad-node"])
  expect_true(res$checked[res$id == "bad-node"])
  expect_true(res$error[res$id == "T1"])
  expect_true(res$download_error[res$id == "T1"])
  expect_true(res$checked[res$id == "T1"])
})

pxweb_v2_data_fixture <- function() {
  list(
    version = "2.0",
    class = "dataset",
    label = "Population by region and year",
    id = list("Region", "Tid", "ContentsCode"),
    size = list(2L, 2L, 1L),
    dimension = list(
      Region = list(
        label = "region",
        category = list(
          index = list("01" = 0L, "03" = 1L),
          label = list("01" = "Stockholm", "03" = "Uppsala")
        ),
        extension = list(elimination = FALSE, show = "value")
      ),
      Tid = list(
        label = "year",
        category = list(
          index = list("2024" = 0L, "2025" = 1L),
          label = list("2024" = "2024", "2025" = "2025")
        ),
        extension = list(elimination = FALSE, show = "code")
      ),
      ContentsCode = list(
        label = "contents",
        category = list(
          index = list("POP" = 0L),
          label = list("POP" = "Population")
        ),
        extension = list(elimination = FALSE, show = "value")
      )
    ),
    value = c(10, 20, 30, 40)
  )
}

pxweb_v2_data_response <- function(x) {
  json <- jsonlite::toJSON(x, auto_unbox = TRUE)
  postfields <- charToRaw(jsonlite::toJSON(
    list(selection = list(list(variableCode = "Region", valueCodes = list("01")))),
    auto_unbox = TRUE
  ))
  structure(
    list(
      url = "https://example.test/api/v2/tables/TAB1/data",
      status_code = 200L,
      headers = structure(
        list("content-type" = "application/json; charset=utf-8"),
        class = c("insensitive", "list")
      ),
      content = charToRaw(json),
      request = structure(
        list(options = list(postfields = postfields)),
        class = "request"
      )
    ),
    class = "response"
  )
}

test_that(desc = "PXWEB API v2 JSON-stat2 data constructor", {
  x <- pxweb_v2_data_fixture()

  expect_silent(data <- pxweb_data_jsonstat2(x))
  expect_s3_class(data, "pxweb_data_jsonstat2")

  expect_silent(data <- pxweb_data_v2(x))
  expect_s3_class(data, "pxweb_data_v2")
  expect_s3_class(data, "pxweb_data_jsonstat2")
})

test_that(desc = "PXWEB API v2 JSON-stat2 data converts to data.frame", {
  data <- pxweb_data_v2(pxweb_v2_data_fixture())

  expect_equal(
    as.data.frame(data, column.name.type = "code", variable.value.type = "code"),
    data.frame(
      Region = c("01", "01", "03", "03"),
      Tid = c("2024", "2025", "2024", "2025"),
      ContentsCode = c("POP", "POP", "POP", "POP"),
      value = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    as.data.frame(data, column.name.type = "text", variable.value.type = "text"),
    data.frame(
      region = c("Stockholm", "Stockholm", "Uppsala", "Uppsala"),
      year = c("2024", "2025", "2024", "2025"),
      contents = c("Population", "Population", "Population", "Population"),
      value = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    )
  )
})

test_that(desc = "PXWEB API v2 response parser routes JSON-stat2 data", {
  response <- pxweb_v2_data_response(pxweb_v2_data_fixture())

  expect_silent(data <- pxweb_parse_response(response))
  expect_s3_class(data, "pxweb_data_v2")
  expect_equal(
    as.data.frame(data, column.name.type = "code", variable.value.type = "code")$value,
    c(10, 20, 30, 40)
  )
})

test_that(desc = "PXWEB API v2 response parser stores non JSON-stat2 output", {
  response <- pxweb_v2_data_response(pxweb_v2_data_fixture())
  response$url <- "https://example.test/api/v2/tables/TAB1/data?outputFormat=csv"
  response$content <- charToRaw("Region,Tid,value\n01,2024,10\n")

  expect_silent(path <- pxweb_parse_response(response))
  expect_true(file.exists(path))
  expect_match(path, "\\.csv$")
})

test_that(desc = "PXWEB API v2 data.frame wrapper requires JSON-stat2", {
  expect_error(
    pxweb_get_data("https://example.test/api/v2/tables/TAB1/metadata", list(Region = "01"), output_format = "csv"),
    "requires output_format = 'json-stat2'"
  )
})

test_that(desc = "PXWEB API v2 JSON-stat2 data batches can be combined", {
  chunk_one <- pxweb_v2_data_fixture()
  chunk_one$id <- list("Region", "Tid", "ContentsCode")
  chunk_one$size <- list(1L, 2L, 1L)
  chunk_one$dimension$Region$category$index <- list("01" = 0L)
  chunk_one$dimension$Region$category$label <- list("01" = "Stockholm")
  chunk_one$value <- c(10, 20)

  chunk_two <- pxweb_v2_data_fixture()
  chunk_two$id <- list("Region", "Tid", "ContentsCode")
  chunk_two$size <- list(1L, 2L, 1L)
  chunk_two$dimension$Region$category$index <- list("03" = 0L)
  chunk_two$dimension$Region$category$label <- list("03" = "Uppsala")
  chunk_two$value <- c(30, 40)

  chunks <- list(pxweb_data_v2(chunk_one), pxweb_data_v2(chunk_two))
  combined <- pxweb_data_v2_c(chunks)

  expect_s3_class(combined, "pxweb_data_v2")
  expect_equal(
    as.data.frame(combined, column.name.type = "code", variable.value.type = "code"),
    data.frame(
      Region = c("01", "01", "03", "03"),
      Tid = c("2024", "2025", "2024", "2025"),
      ContentsCode = c("POP", "POP", "POP", "POP"),
      value = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(pxweb_c(chunks), combined)

  metadata <- pxweb_metadata(list(
    title = "Population by region and year",
    variables = list(
      list(
        code = "Region",
        text = "region",
        values = c("01", "03"),
        valueTexts = c("Stockholm", "Uppsala"),
        elimination = FALSE,
        time = FALSE
      ),
      list(
        code = "Tid",
        text = "year",
        values = c("2024", "2025"),
        valueTexts = c("2024", "2025"),
        elimination = FALSE,
        time = TRUE
      ),
      list(
        code = "ContentsCode",
        text = "contents",
        values = "POP",
        valueTexts = "Population",
        elimination = FALSE,
        time = FALSE
      )
    )
  ))
  chunks[[1]]$pxweb_metadata <- metadata
  chunks[[2]]$pxweb_metadata <- metadata

  expect_equal(
    as.data.frame(pxweb_c(chunks), column.name.type = "code", variable.value.type = "code"),
    as.data.frame(combined, column.name.type = "code", variable.value.type = "code")
  )
})

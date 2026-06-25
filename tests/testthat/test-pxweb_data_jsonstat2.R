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

test_that(desc = "PXWEB API v2 JSON-stat2 null values convert to NA", {
  x <- pxweb_v2_data_fixture()
  x$value <- list(10, NULL, 30, 40)

  expect_silent(data <- pxweb_data_v2(x))
  df <- as.data.frame(data, column.name.type = "code", variable.value.type = "code")

  expect_equal(df$value, c(10, NA, 30, 40))
  expect_true(is.numeric(df$value))
})

test_that(desc = "PXWEB API v2 JSON-stat2 status and notes are preserved", {
  x <- pxweb_v2_data_fixture()
  x$status <- list("1" = "..")
  x$note <- list("Dataset note")
  x$dimension$Region$note <- list("Region note")
  x$dimension$ContentsCode$category$unit <- list(
    POP = list(base = "persons", decimals = 0L)
  )

  expect_silent(data <- pxweb_data_v2(x))
  expect_identical(data$status, x$status)
  expect_identical(data$note, x$note)
  expect_identical(data$dimension$Region$note, x$dimension$Region$note)
  expect_identical(
    data$dimension$ContentsCode$category$unit,
    x$dimension$ContentsCode$category$unit
  )
  expect_equal(nrow(as.data.frame(data)), 4)
})

test_that(desc = "PXWEB API v2 JSON-stat2 falls back to codes when value labels are missing", {
  x <- pxweb_v2_data_fixture()
  x$dimension$Region$category$label <- NULL
  x$dimension$Tid$category$label <- list("2024" = "Year 2024")

  expect_silent(data <- pxweb_data_v2(x))
  df <- as.data.frame(data, column.name.type = "text", variable.value.type = "text")

  expect_equal(df$region, c("01", "01", "03", "03"))
  expect_equal(df$year, c("Year 2024", "2025", "Year 2024", "2025"))
})

test_that(desc = "PXWEB API v2 JSON-stat2 data supports multiple content values", {
  x <- pxweb_v2_data_fixture()
  x$size <- list(2L, 2L, 2L)
  x$dimension$ContentsCode$category$index <- list("POP" = 0L, "AREA" = 1L)
  x$dimension$ContentsCode$category$label <- list(
    "POP" = "Population",
    "AREA" = "Area"
  )
  x$value <- seq_len(8)

  expect_silent(data <- pxweb_data_v2(x))

  expect_equal(
    as.data.frame(data, column.name.type = "code", variable.value.type = "code"),
    data.frame(
      Region = c("01", "01", "01", "01", "03", "03", "03", "03"),
      Tid = c("2024", "2024", "2025", "2025", "2024", "2024", "2025", "2025"),
      ContentsCode = c("POP", "AREA", "POP", "AREA", "POP", "AREA", "POP", "AREA"),
      value = seq_len(8),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    as.data.frame(data, column.name.type = "text", variable.value.type = "text")$contents,
    c("Population", "Area", "Population", "Area", "Population", "Area", "Population", "Area")
  )
})

test_that(desc = "PXWEB API v2 data supports v1-style dimension and column helpers", {
  data <- pxweb_data_v2(pxweb_v2_data_fixture())

  expect_equal(pxweb_data_dim(data), c(4, 4))
  expect_equal(pxweb_data_colnames(data, type = "code"), c("Region", "Tid", "ContentsCode", "value"))
  expect_equal(pxweb_data_colnames(data, type = "text"), c("region", "year", "contents", "value"))
})

test_that(desc = "PXWEB API v2 data supports matrix coercion", {
  data <- pxweb_data_v2(pxweb_v2_data_fixture())

  expect_equal(
    as.matrix(data, column.name.type = "code", variable.value.type = "code"),
    as.matrix(as.data.frame(data, column.name.type = "code", variable.value.type = "code"))
  )
  expect_equal(
    as.matrix(data, column.name.type = "text", variable.value.type = "text"),
    as.matrix(as.data.frame(data, column.name.type = "text", variable.value.type = "text"))
  )
})

test_that(desc = "PXWEB API v2 comments map notes and statuses to v1-style comments", {
  x <- pxweb_v2_data_fixture()
  x$note <- list("Dataset note")
  x$status <- list("1" = "..")
  x$dimension$Region$note <- list("Region note")
  x$dimension$Region$category$note <- list("03" = "Uppsala note")

  expect_silent(comments <- pxweb_data_comments(pxweb_data_v2(x)))
  expect_s3_class(comments, "pxweb_data_comments")
  expect_equal(comments$data_dim, c(4, 4))
  expect_equal(length(comments$pxweb_data_comments), 4)

  comments_df <- as.data.frame(comments, stringsAsFactors = FALSE)
  expect_equal(
    comments_df,
    data.frame(
      row_no = c(NA_integer_, NA_integer_, 3L, 4L, 2L),
      col_no = c(NA_integer_, 1L, 1L, 1L, NA_integer_),
      comment_type = c("obs_comment", "column_comment", "value_comment", "value_comment", "obs_comment"),
      comment = c("Dataset note", "Region note", "Uppsala note", "Uppsala note", "Status: .."),
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

test_that(desc = "PXWEB API v2 large queries are split, downloaded, and combined", {
  px <- structure(
    list(
      url = parse_url_or_fail("https://example.test/api/v2/tables/TAB1/metadata?lang=sv"),
      version = "v2",
      config = list(
        calls_per_period = 100L,
        period_in_seconds = 1L,
        max_values_to_download = 2L,
        CORS = NULL
      ),
      calls = list(time_stamps = list()),
      paths = list(
        rda_file_path = tempfile(fileext = ".rda"),
        api_subpath = list(path = "api/v2", vector = c("api", "v2"))
      )
    ),
    class = c("pxweb", "list")
  )

  metadata <- pxweb_metadata(list(
    title = "Population by region and year",
    variables = list(
      list(
        code = "Region",
        text = "region",
        values = c("01", "03"),
        valueTexts = c("Stockholm", "Uppsala"),
        elimination = TRUE,
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
  attr(metadata, "pxweb_metadata_v2") <- list(
    extension = list(px = list(tableid = "TAB1", language = "sv"))
  )

  post_bodies <- list()
  post_queries <- list()

  data_response <- function(body) {
    selection <- jsonlite::fromJSON(body, simplifyVector = FALSE)$selection
    selected <- lapply(selection, function(x) unlist(x$valueCodes, use.names = FALSE))
    names(selected) <- vapply(selection, function(x) x$variableCode, character(1))
    all_values <- list(
      Region = c("01", "03"),
      Tid = c("2024", "2025"),
      ContentsCode = "POP"
    )
    selected_names <- names(selected)
    selected <- lapply(selected_names, function(variable) {
      values <- selected[[variable]]
      if (identical(values, "*")) {
        all_values[[variable]]
      } else {
        values
      }
    })
    names(selected) <- selected_names

    x <- pxweb_v2_data_fixture()
    x$id <- list("Region", "Tid", "ContentsCode")
    x$size <- list(length(selected$Region), length(selected$Tid), 1L)
    x$dimension$Region$category$index <- as.list(seq_along(selected$Region) - 1L)
    names(x$dimension$Region$category$index) <- selected$Region
    x$dimension$Region$category$label <- as.list(c("01" = "Stockholm", "03" = "Uppsala")[selected$Region])
    names(x$dimension$Region$category$label) <- selected$Region
    x$dimension$Tid$category$index <- as.list(seq_along(selected$Tid) - 1L)
    names(x$dimension$Tid$category$index) <- selected$Tid
    x$dimension$Tid$category$label <- as.list(selected$Tid)
    names(x$dimension$Tid$category$label) <- selected$Tid
    x$dimension$ContentsCode$category$index <- list(POP = 0L)
    x$dimension$ContentsCode$category$label <- list(POP = "Population")

    value_lookup <- c(
      "01\r2024\rPOP" = 10,
      "01\r2025\rPOP" = 20,
      "03\r2024\rPOP" = 30,
      "03\r2025\rPOP" = 40
    )
    grid <- expand.grid(
      ContentsCode = selected$ContentsCode,
      Tid = selected$Tid,
      Region = selected$Region,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    grid <- grid[c("Region", "Tid", "ContentsCode")]
    key <- do.call(paste, c(grid, sep = "\r"))
    x$value <- unname(value_lookup[key])

    json <- jsonlite::toJSON(x, auto_unbox = TRUE)
    structure(
      list(
        url = "https://example.test/api/v2/tables/TAB1/data?lang=sv&outputFormat=json-stat2",
        status_code = 200L,
        headers = structure(
          list("content-type" = "application/json; charset=utf-8"),
          class = c("insensitive", "list")
        ),
        content = charToRaw(json),
        request = structure(
          list(options = list(postfields = charToRaw(body))),
          class = "request"
        )
      ),
      class = "response"
    )
  }

  local_mocked_bindings(
    POST = function(url, body, ..., query = list(), encode = NULL) {
      post_bodies[[length(post_bodies) + 1L]] <<- jsonlite::fromJSON(body, simplifyVector = FALSE)
      post_queries[[length(post_queries) + 1L]] <<- query
      data_response(body)
    },
    .package = "httr"
  )

  query <- list(
    Region = c("01", "03"),
    Tid = c("2024", "2025"),
    ContentsCode = "POP"
  )

  expect_output(
    data <- pxweb_advanced_get(
      url = px,
      query = query,
      verbose = TRUE,
      pxmdo = metadata
    ),
    regexp = "2 batches"
  )
  expect_s3_class(data, "pxweb_data_v2")
  expect_length(post_bodies, 2)
  expect_true(all(vapply(post_queries, function(x) {
    identical(x, list(lang = "sv", outputFormat = "json-stat2"))
  }, logical(1))))
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
})

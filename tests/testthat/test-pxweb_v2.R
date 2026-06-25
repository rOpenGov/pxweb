
test_that(desc = "PXWEB API version detection is local and stable", {
  v1_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24"
  v2_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"

  expect_equal(pxweb_detect_version(v1_url), "v1")
  expect_equal(pxweb_detect_version(v2_url), "v2")
})

test_that(desc = "PXWEB API v2 URL helpers build endpoint URLs", {
  v2_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"
  v2_table_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974?lang=en"
  v2_data_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/data?lang=en"

  expect_equal(pxweb_v2_api_subpath(v2_url), "api/v2")
  expect_equal(pxweb_v2_table_id(v2_url), "TAB5974")
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
  expect_equal(
    pxweb_v2_table_metadata_url(v2_table_url),
    "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata?lang=en"
  )
  expect_equal(
    pxweb_v2_table_metadata_url(v2_data_url),
    "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata?lang=en"
  )
})

test_that(desc = "PXWEB API v2 query conversion creates selection body", {
  pxq <- pxweb_query(list(
    InrikesUtrikes = "83",
    Alder = c("0", "1"),
    Tid = "*"
  ))

  expect_silent(qv2 <- pxweb_query_as_v2(pxq))
  expect_s3_class(qv2, "pxweb_query_v2")
  expect_equal(
    qv2,
    structure(
      list(
        selection = list(
          list(variableCode = "InrikesUtrikes", valueCodes = list("83")),
          list(variableCode = "Alder", valueCodes = list("0", "1")),
          list(variableCode = "Tid", valueCodes = list("*"))
        )
      ),
      class = c("pxweb_query_v2", "list")
    )
  )

  expect_equal(
    jsonlite::fromJSON(pxweb_as_json(qv2), simplifyVector = FALSE),
    list(
      selection = list(
        list(variableCode = "InrikesUtrikes", valueCodes = list("83")),
        list(variableCode = "Alder", valueCodes = list("0", "1")),
        list(variableCode = "Tid", valueCodes = list("*"))
      )
    )
  )
})

test_that(desc = "PXWEB API v2 data request helpers use table id and language", {
  metadata_response <- readRDS(test_path("test_data/pxweb_metadata_response_v2.rds"))
  raw_metadata <- suppressWarnings(httr::content(metadata_response, as = "parsed"))
  pxmd <- pxweb_metadata_v2(raw_metadata)
  px <- structure(
    list(url = parse_url_or_fail("https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata?lang=en")),
    class = c("pxweb", "list")
  )
  pxq <- pxweb_query(list(
    InrikesUtrikes = "83",
    Alder = c("0", "1"),
    Kon = "1",
    ContentsCode = "000005NO",
    Tid = "2025"
  ))

  expect_equal(pxweb_v2_table_id(px, pxmd), "TAB5974")
  expect_equal(
    pxweb_v2_data_query_params(px, pxmd),
    list(lang = "en", outputFormat = "json-stat2")
  )
  expect_equal(
    pxweb_v2_data_query_params(px, pxmd, output_format = "csv"),
    list(lang = "en", outputFormat = "csv")
  )

  request <- pxweb_v2_data_request(px, pxq, pxmd)
  expect_equal(
    request$url,
    "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/data"
  )
  expect_equal(request$query, list(lang = "en", outputFormat = "json-stat2"))
  expect_equal(
    jsonlite::fromJSON(request$body, simplifyVector = FALSE),
    list(
      selection = list(
        list(variableCode = "InrikesUtrikes", valueCodes = list("83")),
        list(variableCode = "Alder", valueCodes = list("0", "1")),
        list(variableCode = "Kon", valueCodes = list("1")),
        list(variableCode = "ContentsCode", valueCodes = list("000005NO")),
        list(variableCode = "Tid", valueCodes = list("2025"))
      )
    )
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
  # Mocks recorded by tests/testthat/record-mocks.R.
  with_pxweb_mock_api({

  metadata_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata"
  expect_silent(pxapi_v2 <- pxweb(url = metadata_url))

  expect_equal(pxapi_v2$version, "v2")
  expect_equal(pxapi_v2$paths$api_subpath$path, "api/v2")
  expect_named(pxapi_v2$config, c("calls_per_period", "period_in_seconds", "max_values_to_download", "CORS"))
  expect_true(pxapi_v2$config$max_values_to_download > 0)
  })
})

test_that(desc = "PXWEB API v2 fixture end-to-end workflow", {
  # Mocks recorded by tests/testthat/record-mocks.R.
  with_pxweb_mock_api({

  table_id <- "TAB5974"
  table_url <- paste0("https://statistikdatabasen.scb.se/api/v2/tables/", table_id, "?lang=sv")
  metadata_url <- paste0("https://statistikdatabasen.scb.se/api/v2/tables/", table_id, "/metadata")
  data_url <- paste0("https://statistikdatabasen.scb.se/api/v2/tables/", table_id, "/data?lang=sv")

  expect_silent(px <- pxweb(metadata_url))
  expect_equal(px$version, "v2")
  expect_equal(px$paths$api_subpath$path, "api/v2")
  expect_true(px$config$max_values_to_download > 0)

  expect_silent(table <- pxweb_get(table_url))
  expect_s3_class(table, "pxweb_table_response_v2")
  expect_equal(table$id, table_id)
  expect_true(any(vapply(table$links, function(x) identical(x$rel, "metadata"), logical(1))))
  expect_true(any(vapply(table$links, function(x) identical(x$rel, "data"), logical(1))))

  expect_silent(metadata <- pxweb_get(metadata_url))
  expect_s3_class(metadata, "pxweb_metadata")
  raw_metadata <- attr(metadata, "pxweb_metadata_v2")
  expect_equal(raw_metadata$extension$px$tableid, table_id)
  expect_named(
    pxweb_metadata_dim(metadata),
    c("InrikesUtrikes", "Alder", "Kon", "ContentsCode", "Tid")
  )

  first_year <- names(raw_metadata$dimension$Tid$category$index)[1]
  query <- list(
    InrikesUtrikes = "83",
    Alder = "0",
    Kon = c("1", "2"),
    ContentsCode = "000005NO",
    Tid = first_year
  )

  expect_silent(data <- pxweb_get(metadata_url, query = query, verbose = FALSE))
  expect_s3_class(data, "pxweb_data_v2")
  expect_silent(data_from_table_url <- pxweb_get(table_url, query = query, verbose = FALSE))
  expect_s3_class(data_from_table_url, "pxweb_data_v2")
  expect_silent(data_from_data_url <- pxweb_get(data_url, query = query, verbose = FALSE))
  expect_s3_class(data_from_data_url, "pxweb_data_v2")

  expect_silent(df_code <- as.data.frame(
    data,
    column.name.type = "code",
    variable.value.type = "code"
  ))

  expect_equal(nrow(df_code), 2)
  expect_named(df_code, c("InrikesUtrikes", "Alder", "Kon", "ContentsCode", "Tid", "value"))
  expect_equal(sort(df_code$Kon), c("1", "2"))
  expect_true(all(df_code$Tid == first_year))
  expect_true(is.numeric(df_code$value))
  expect_true(all(!is.na(df_code$value)))
  expect_equal(
    as.data.frame(data_from_table_url, column.name.type = "code", variable.value.type = "code"),
    df_code
  )
  expect_equal(
    as.data.frame(data_from_data_url, column.name.type = "code", variable.value.type = "code"),
    df_code
  )

  expect_silent(df_text <- as.data.frame(
    data,
    column.name.type = "text",
    variable.value.type = "text"
  ))

  expect_equal(nrow(df_text), 2)
  expect_true("value" %in% names(df_text))
  expect_true(any(df_text$value > 0))
  })
})

test_that(desc = "PXWEB API v1 and v2 equivalent fixtures agree after normalization", {
  content_code <- "BE0101N1"
  v1_data <- pxweb_data(list(
    columns = list(
      list(code = "Region", text = "region", type = "c", comment = "Region note"),
      list(code = "Civilstand", text = "civil status", type = "c"),
      list(code = "Alder", text = "age", type = "c"),
      list(code = "Kon", text = "sex", type = "c"),
      list(code = "Tid", text = "year", type = "t"),
      list(code = content_code, text = "population", type = "d")
    ),
    comments = list(),
    data = list(
      list(
        key = list("00", "OG", "0", "1", "2024"),
        values = list("50937")
      )
    )
  ))

  one_value_category <- function(label, code, text) {
    list(
      label = label,
      category = list(
        index = structure(list(0L), names = code),
        label = structure(list(text), names = code)
      )
    )
  }

  v2_data <- pxweb_data_v2(list(
    version = "2.0",
    class = "dataset",
    id = list("Region", "Civilstand", "Alder", "Kon", "ContentsCode", "Tid"),
    size = as.list(rep(1L, 6)),
    dimension = list(
      Region = c(one_value_category("region", "00", "Sweden"), list(note = list("Region note"))),
      Civilstand = one_value_category("civil status", "OG", "unmarried"),
      Alder = one_value_category("age", "0", "0 years"),
      Kon = one_value_category("sex", "1", "men"),
      ContentsCode = one_value_category("contents", content_code, "population"),
      Tid = one_value_category("year", "2024", "2024")
    ),
    value = 50937
  ))

  v1_df <- as.data.frame(v1_data, column.name.type = "code", variable.value.type = "code")
  v2_df <- as.data.frame(v2_data, column.name.type = "code", variable.value.type = "code")

  common_columns <- c("Region", "Civilstand", "Alder", "Kon", "Tid")
  v1_normalized <- data.frame(
    v1_df[common_columns],
    value = v1_df[[content_code]],
    stringsAsFactors = FALSE
  )
  v2_normalized <- data.frame(
    v2_df[common_columns],
    value = v2_df$value,
    stringsAsFactors = FALSE
  )

  expect_equal(v1_normalized, v2_normalized)
  expect_equal(pxweb_data_dim(v1_data)[1], pxweb_data_dim(v2_data)[1])
  expect_equal(
    setdiff(pxweb_data_colnames(v1_data, "code"), content_code),
    setdiff(pxweb_data_colnames(v2_data, "code"), c("ContentsCode", "value"))
  )
  expect_equal(as.matrix(v1_normalized), as.matrix(v2_normalized))

  normalize_comment <- function(x) {
    gsub("\\s+", " ", trimws(x))
  }
  v1_comments <- normalize_comment(as.data.frame(pxweb_data_comments(v1_data))$comment)
  v2_comments <- normalize_comment(as.data.frame(pxweb_data_comments(v2_data))$comment)

  expect_equal(v1_comments, v2_comments)
})

test_that(desc = "PXWEB API v1 and v2 fixture table helpers agree after normalization", {
  # Mocks recorded by tests/testthat/record-mocks.R.
  with_pxweb_mock_api({

  v1_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  v2_url <- "https://statistikdatabasen.scb.se/api/v2/tables/TAB638/metadata?lang=sv"
  content_code <- "BE0101N1"
  query <- list(
    Region = "00",
    Civilstand = "OG",
    Alder = "0",
    Kon = "1",
    ContentsCode = content_code,
    Tid = "2024"
  )

  expect_silent(v1_data <- pxweb_get(v1_url, query = query, verbose = FALSE))
  expect_silent(v2_data <- pxweb_get(v2_url, query = query, verbose = FALSE))

  expect_s3_class(v1_data, "pxweb_data")
  expect_s3_class(v2_data, "pxweb_data_v2")

  v1_df <- as.data.frame(v1_data, column.name.type = "code", variable.value.type = "code")
  v2_df <- as.data.frame(v2_data, column.name.type = "code", variable.value.type = "code")

  common_columns <- c("Region", "Civilstand", "Alder", "Kon", "Tid")
  v1_normalized <- data.frame(
    v1_df[common_columns],
    value = v1_df[[content_code]],
    stringsAsFactors = FALSE
  )
  v2_normalized <- data.frame(
    v2_df[common_columns],
    value = v2_df$value,
    stringsAsFactors = FALSE
  )

  expect_equal(v1_normalized, v2_normalized)
  expect_equal(pxweb_data_dim(v1_data)[1], pxweb_data_dim(v2_data)[1])
  expect_equal(
    setdiff(pxweb_data_colnames(v1_data, "code"), content_code),
    setdiff(pxweb_data_colnames(v2_data, "code"), c("ContentsCode", "value"))
  )
  expect_equal(
    as.matrix(v1_normalized),
    as.matrix(v2_normalized)
  )

  normalize_comment <- function(x) {
    gsub("\\s+", " ", trimws(x))
  }
  v1_comments <- normalize_comment(as.data.frame(pxweb_data_comments(v1_data))$comment)
  v2_comments <- normalize_comment(as.data.frame(pxweb_data_comments(v2_data))$comment)

  expect_true(length(v1_comments) > 0)
  expect_true(any(v1_comments %in% v2_comments))
  })
})

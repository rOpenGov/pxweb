
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

test_that(desc = "PXWEB API v2 live end-to-end workflow", {
  skip_on_cran()
  skip_if_not_live_api()

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

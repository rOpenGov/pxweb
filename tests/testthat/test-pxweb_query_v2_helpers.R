
test_that("PXWEB API v2 query helper constructors create typed selections", {
  expect_s3_class(pxweb_all(), "pxweb_query_all")
  expect_s3_class(pxweb_latest(), "pxweb_query_latest")
  expect_s3_class(pxweb_aggregation("agg_Ålder5år_1"), "pxweb_query_aggregation")
  expect_s3_class(pxweb_valueset("vs_Ålder1årG"), "pxweb_query_valueset")
  expect_s3_class(pxweb_top(10), "pxweb_query_top")
  expect_s3_class(pxweb_bottom(5), "pxweb_query_bottom")

  expect_equal(pxweb_all()$value_codes, "*")
  expect_equal(pxweb_latest()$value_codes, "9999")
  expect_equal(pxweb_top(10)$value_codes, "top(10)")
  expect_equal(pxweb_bottom(5)$value_codes, "bottom(5)")

  expect_error(pxweb_latest(""))
  expect_error(pxweb_aggregation(""))
  expect_error(pxweb_valueset(""))
  expect_error(pxweb_top(0))
  expect_error(pxweb_bottom(0))
})

test_that("PXWEB API v2 query helpers normalize to body and extra query parameters", {
  request <- pxweb:::pxweb_query_list_as_v2(list(
    Region = pxweb_all(),
    Alder = pxweb_aggregation("agg_Ålder5år_1"),
    Kon = c("1", "2"),
    Tid = pxweb_latest(),
    ContentsCode = pxweb_top(1)
  ))

  expect_s3_class(request, "pxweb_query_v2_request")
  expect_s3_class(request$body, "pxweb_query_v2")
  expect_equal(length(request$body$selection), 5)

  expect_equal(
    request$body$selection[[1]],
    list(variableCode = "Region", valueCodes = list("*"))
  )
  expect_equal(
    request$body$selection[[2]],
    list(variableCode = "Alder", valueCodes = list("*"))
  )
  expect_equal(
    request$body$selection[[3]],
    list(variableCode = "Kon", valueCodes = list("1", "2"))
  )
  expect_equal(
    request$body$selection[[4]],
    list(variableCode = "Tid", valueCodes = list("9999"))
  )
  expect_equal(
    request$body$selection[[5]],
    list(variableCode = "ContentsCode", valueCodes = list("top(1)"))
  )

  expect_equal(request$extra_query[["codelist[Alder]"]], "agg_Ålder5år_1")
  expect_equal(request$extra_query[["outputValues[Alder]"]], "aggregated")
})

test_that("PXWEB API v2 valueset helper supports explicit output values", {
  request <- pxweb:::pxweb_query_list_as_v2(list(
    Alder = pxweb_valueset("vs_Ålder1årG", value_codes = c("0", "1"), output_values = "single")
  ))

  expect_equal(
    request$body$selection[[1]],
    list(variableCode = "Alder", valueCodes = list("0", "1"))
  )
  expect_equal(request$extra_query[["codelist[Alder]"]], "vs_Ålder1årG")
  expect_equal(request$extra_query[["outputValues[Alder]"]], "single")
})

test_that("PXWEB API v2 query helpers work through pxweb_query", {
  pxq <- pxweb_query(list(
    Region = pxweb_all(),
    Alder = pxweb_aggregation("agg_Ålder5år_1"),
    Tid = pxweb_latest(),
    ContentsCode = pxweb_top(1)
  ))

  expect_s3_class(pxq, "pxweb_query")
  expect_equal(pxweb_query_filter(pxq), c(
    Region = "all",
    Alder = "all",
    Tid = "item",
    ContentsCode = "top"
  ))
  expect_equal(pxweb_query_values(pxq)$Alder, "*")
  expect_equal(pxweb_query_values(pxq)$ContentsCode, "1")
  expect_equal(pxweb:::pxweb_query_v2_extra_query(pxq)[["codelist[Alder]"]], "agg_Ålder5år_1")
  expect_equal(pxweb:::pxweb_query_v2_extra_query(pxq)[["outputValues[Alder]"]], "aggregated")
})

test_that("PXWEB API v2 helpers resolve against metadata before request building", {
  metadata_response <- readRDS(test_path("test_data/pxweb_metadata_response_v2.rds"))
  raw_metadata <- suppressWarnings(httr::content(metadata_response, as = "parsed"))
  pxmd <- pxweb_metadata_v2(raw_metadata)
  px <- structure(
    list(url = parse_url_or_fail("https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata?lang=sv")),
    class = c("pxweb", "list")
  )

  pxq <- pxweb_query(list(
    InrikesUtrikes = "83",
    Alder = pxweb_aggregation("agg_Ålder5år_1"),
    Kon = "1",
    ContentsCode = "000005NO",
    Tid = pxweb_latest()
  ))
  pxq <- pxweb_add_mandatory_variables(pxq, pxmd)
  pxq <- pxweb_add_metadata_to_query(pxq, pxmd)
  expect_silent(pxweb_validate_query_with_metadata(pxq, pxmd))

  tid_idx <- which(vapply(pxq$query, function(x) x$code, character(1)) == "Tid")
  expect_equal(
    pxq$query[[tid_idx]]$selection$values,
    utils::tail(names(raw_metadata$dimension$Tid$category$index), 1)
  )

  code_lists <- raw_metadata$dimension$Alder$extension$codeLists
  code_list_ids <- vapply(code_lists, function(x) x$id, character(1))
  expect_true("agg_Ålder5år_1" %in% code_list_ids)
  expect_true("vs_Ålder1årG" %in% code_list_ids)

  request <- pxweb_v2_data_request(px, pxq, pxmd)
  expect_equal(request$query[["codelist[Alder]"]], "agg_Ålder5år_1")
  expect_equal(request$query[["outputValues[Alder]"]], "aggregated")
  expect_equal(
    pxweb:::pxweb_v2_data_value_codes_query(pxq),
    list(
      "valueCodes[InrikesUtrikes]" = "83",
      "valueCodes[Alder]" = "*",
      "valueCodes[Kon]" = "1",
      "valueCodes[ContentsCode]" = "000005NO",
      "valueCodes[Tid]" = pxq$query[[tid_idx]]$selection$values
    )
  )

  body <- jsonlite::fromJSON(request$body, simplifyVector = FALSE)
  expect_equal(body$selection[[2]], list(variableCode = "Alder", valueCodes = list("*")))
  expect_equal(body$selection[[5]], list(variableCode = "Tid", valueCodes = as.list(pxq$query[[tid_idx]]$selection$values)))
})

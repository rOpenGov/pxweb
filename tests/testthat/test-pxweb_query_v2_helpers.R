
test_that("PXWEB API v2 query helper constructors create typed selections", {
  expect_s3_class(pxweb_all(), "pxweb_query_all")
  expect_s3_class(pxweb_latest(), "pxweb_query_latest")
  expect_s3_class(pxweb_aggregation("agg_Alder5ar_1"), "pxweb_query_aggregation")
  expect_s3_class(pxweb_valueset("vs_Alder1ar"), "pxweb_query_valueset")
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
    Alder = pxweb_aggregation("agg_Alder5ar_1"),
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

  expect_equal(request$extra_query[["codelist[Alder]"]], "agg_Alder5ar_1")
  expect_equal(request$extra_query[["outputValues[Alder]"]], "aggregated")
})

test_that("PXWEB API v2 valueset helper supports explicit output values", {
  request <- pxweb:::pxweb_query_list_as_v2(list(
    Alder = pxweb_valueset("vs_Alder1ar", value_codes = c("0", "1"), output_values = "single")
  ))

  expect_equal(
    request$body$selection[[1]],
    list(variableCode = "Alder", valueCodes = list("0", "1"))
  )
  expect_equal(request$extra_query[["codelist[Alder]"]], "vs_Alder1ar")
  expect_equal(request$extra_query[["outputValues[Alder]"]], "single")
})


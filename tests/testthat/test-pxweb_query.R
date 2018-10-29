# Test suits for the examples in the documentation

context("pxweb_query")

test_that(desc="Cache cleaner and print",{

  dims <- list(Alue = c("*"),
               "Asuntokunnan koko" = c("*"),
               Talotyyppi = c("S"),
               Vuosi = c("*"))
  expect_silent(pxq1 <- pxweb_query(dims))
  
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(pxq2 <- pxweb_query(json_query))

  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_agg_example.json")
  expect_silent(pxq3 <- pxweb_query(json_query))
  
})  






# Test suits for the examples in the documentation

context("pxweb_get")

test_that(desc="Constructor works as it should with Statistics Sweden",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  expect_silent(px_meta_data <- pxweb_get(url))
  expect_output(print(px_meta_data), regexp = "PXWEB METADATA")
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
  expect_silent(px_levels <- pxweb_get(url))
  expect_output(print(px_levels), regexp = "PXWEB LEVELS")

  url <- "http://api.scb.se/OV0104/v1/doris/sv"
  expect_silent(px <- pxweb(url))
  expect_silent(px_levels <- pxweb_get(px))
  expect_output(print(px_levels), regexp = "PXWEB LEVELS")

  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  expect_length(pxweb_data_comments(x = px_data), 2)
  
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "test_files", "json_queries", "json_single_query_test.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "test_files", "json_queries", "json_full_test_query.json")
  px <- pxweb(url)
  max_val <- px$config$max_values_to_download
  px$config$max_values_to_download <- 11
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = px, query = json_query, verbose = FALSE)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  expect_output(print(px_data), regexp = "396 observations")
  
  px$config$max_values_to_download <- max_val
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = px, query = json_query)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  expect_output(print(px_data), regexp = "396 observations")
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_big_query_example.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query, verbose = FALSE)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  expect_output(print(px_data), regexp = "255200 observations")
  expect_length(pxweb_data_comments(x = px_data), 2)
})  

test_that(desc="Previous bugs",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  # This is a bug in the previous implementation of pxweb
  url <- "http://bank.stat.gl/api/v1/en/Greenland/BE/BE01"
  tryr <- try(httr::GET(url), silent = TRUE)
  if(!inherits(tryr, "try-error")){
    expect_silent(px_meta_data <- pxweb_get(url))
    expect_output(print(px_meta_data), regexp = "PXWEB LEVELS")
  }
  
  # Missing title
  expect_silent(pxmd <- pxweb_get(url = "http://statistik.linkoping.se/PXWeb/api/v1/sv/Omsorg/Behandlingshem/ombeh01.px"))
})  



test_that(desc="Test to download json-stat objects",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  # Test json-stat
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "test_files", "json_queries", "json_single_query_test.json")
  jqf <- paste(readLines(json_query), collapse = " "); class(jqf) <- "json"
  
  jq <- gsub("json", "json-stat", jqf)
  pxq <- pxweb_query(x = jq)
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = pxq)))
  expect_s3_class(px_data, "json")

  jq <- gsub("json", "jsonstat", jqf)
  pxq <- pxweb_query(x = jq)
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = pxq)))
  expect_s3_class(px_data, "json")
  
})  


test_that(desc="Test pxweb_get_data",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data1 <- pxweb_get(url = url, query = json_query))
  expect_silent(px_data1_df <- as.data.frame(px_data1, column.name.type = "text", variable.value.type = "text"))
  expect_silent(px_data2 <- pxweb_get_data(url = url, query = json_query, column.name.type = "text", variable.value.type = "text"))
  expect_equal(px_data1_df, px_data2)
}) 

test_that(desc="Test http logger",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  expect_silent(px <- pxweb(url))
  pxweb:::pxweb_clear_cache()
  
  url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data <- pxweb_advanced_get(url = url, query = json_query, log_http_calls = TRUE))
  
  expect_true(file.exists(file.path(getwd(), "log_pxweb_api_http_calls.txt")))
  expect_true(file.size(file.path(getwd(), "log_pxweb_api_http_calls.txt")) > 5000)
})  


test_that(desc="No value bug",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://px.hagstofa.is/pxen/api/v1/en/Efnahagur/utanrikisverslun/1_voruvidskipti/02_uttollskra/UTA02801.px"
  expect_silent(px <- pxweb_get(url))
  # pxweb_interactive(url)
  pxweb_query_list <- 
    list("HS-Number" = c("06012031"),
         "Country"=c("AF"),
         "Month"=c("2016M01"),
         "Unit"=c("kg"))
  
  expect_silent(px_data <- pxweb_get(url, query = pxweb_query_list))
  expect_silent(df <- as.data.frame(x = px_data))
  expect_equal(nrow(df), 1)
  
  pxweb_query_list <- 
    list("Country"=c("AF"),
         "Month"=c("2016M01"),
         "Unit"=c("kg"))
  
  expect_error(px_data <- pxweb_get(url, query = pxweb_query_list))
  
  pxweb_query_list <- 
    list("HS-Number" = "*",
         "Country"=c("AF"),
         "Month"=c("2016M01"),
         "Unit"=c("kg"))
  
  expect_error(px_data <- pxweb_get(url, query = pxweb_query_list))
})  



test_that(desc="h level",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://data.ssb.no/api/v0/en/table/pp/pp04/kpi"
  expect_silent(px <- pxweb_get(url))
  
  expect_silent(px2 <- pxweb_levels_remove_headers(px))

  expect_gt(length(px), length(px2))
})  


test_that(desc="large variable call",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  skip("Get 403, bug reported to SCB in december 2018")
  url <- "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0001/BE0001G/BE0001ENamn10"  
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_last_names.json")
  expect_silent(px <- pxweb_get(url, query = pxweb_query(json_query)))
})  


test_that(desc="Cite data",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_output(pxweb_cite(px_data), regexp = "Population by region")
  expect_output(pxweb_cite(px_data), regexp = "Stockholm, Sweden")
})  


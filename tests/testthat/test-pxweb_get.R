# Test suits for the examples in the documentation

context("pxweb_get")

test_that(desc="Test to download px and sdmx",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  json_px_query <- readLines(test_path("test_data/test_query_px.json"))
  
  expect_silent(px_file_path1 <- 
                  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
                            query = json_px_query)
  )
  checkmate::expect_file_exists(px_file_path1)
  
  # data <- pxR::read.px(px_file_path1)
  
  json_sdmx_query <- readLines(test_path("test_data/test_query_sdmx.json"))
  expect_silent(px_file_path2 <- 
                  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
                            query = json_sdmx_query)
  )
  checkmate::expect_file_exists(px_file_path2)
  expect_true(px_file_path1 != px_file_path2)
  
  pxq <- pxweb_query(json_px_query) 
  pxq$response$format <- "sdmx"
  pxfp <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
                    pxq)
  expect_true(px_file_path2 == pxfp)
  
})  

test_that(desc="Constructor works as it should with Statistics Sweden",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  expect_silent(px_meta_data <- pxweb_get(url))
  expect_output(print(px_meta_data), regexp = "PXWEB METADATA")
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
  expect_silent(px_levels <- pxweb_get(url))
  expect_output(print(px_levels), regexp = "PXWEB LEVELS")

  url <- "https://api.scb.se/OV0104/v1/doris/sv"
  expect_silent(px <- pxweb(url))
  expect_silent(px_levels <- pxweb_get(px))
  expect_output(print(px_levels), regexp = "PXWEB LEVELS")

  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  expect_length(pxweb_data_comments(x = px_data), 2)
  
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "test_files", "json_queries", "json_single_query_test.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_output(print(px_data), regexp = "PXWEB DATA")
  
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
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
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
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
  url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01"
  tryr <- try(httr::GET(url), silent = TRUE)
  if(!inherits(tryr, "try-error")){
    expect_silent(px_meta_data <- pxweb_get(url))
    expect_output(print(px_meta_data), regexp = "PXWEB LEVELS")
  }

})  



test_that(desc="Test to download json-stat objects",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  # Test json-stat
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
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
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data1 <- pxweb_get(url = url, query = json_query))
  expect_silent(px_data1_df <- as.data.frame(px_data1, column.name.type = "text", variable.value.type = "text"))
  expect_silent(px_data2 <- pxweb_get_data(url = url, query = json_query, column.name.type = "text", variable.value.type = "text"))
  expect_equal(px_data1_df, px_data2)
}) 

test_that(desc="Test http logger",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  expect_silent(px <- pxweb(url))
  pxweb:::pxweb_clear_cache()
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data <- pxweb_advanced_get(url = url, query = json_query, log_http_calls = TRUE))
  
  expect_true(file.exists(file.path(getwd(), "log_pxweb_api_http_calls.txt")))
  expect_true(file.size(file.path(getwd(), "log_pxweb_api_http_calls.txt")) > 5000)
})  

test_that(desc="large variable call",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0001/BE0001G/BE0001ENamn10"  
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_last_names.json")
  expect_silent(px <- pxweb_get(url, query = pxweb_query(json_query)))
})  


test_that(desc="Cite data",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(px_data <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  expect_output(pxweb_cite(px_data), regexp = "Population by region")
  expect_output(pxweb_cite(px_data), regexp = "Stockholm, Sweden")
})  


test_that(desc="Filter query error bug",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  url <- "http://data.ssb.no/api/v0/en/table/04861"
  json_query <- readLines(test_path("test_data/filter_query.json"))
  expect_silent(px_data1 <- suppressWarnings(pxweb_get(url = url, query = json_query)))
  df1 <- jsonlite::fromJSON(px_data1)
  
  expect_silent(x_httr <- httr::content(httr::POST(url, body = json_query, encode = "json"), "text"))
  df2 <- jsonlite::fromJSON(x_httr)

  expect_identical(df1$dataset$dimension$Region$category$index,
                   df2$dataset$dimension$Region$category$index)
  expect_identical(df1$dataset$dimension$Tid$category$index,
                   df2$dataset$dimension$Tid$category$index)

})  


test_that(desc="a small big query",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()

  pxweb_query_list <- 
    list("Region"=c("00"),
         "Alder"=c("tot"),
         "ContentsCode"=c("BE0101N1"),
         "Tid"=c("2016","2017","2018","2019"))
  
  # Download data 
  px <- pxweb("https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy")
  px$config$max_values_to_download <- 2
  
  expect_output(px_data1 <- pxweb_get(url = px, query = pxweb_query_list), regexp = "2 batches")
  
  px$config$max_values_to_download <- 4
  expect_silent(px_data2 <- pxweb_get(url = px, query = pxweb_query_list))
  
  expect_identical(px_data1$data, px_data2$data)
})  




test_that(desc="manually supplying a pxmdo",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  pxweb_query_list <- 
    list("Region"=c("00"),
         "Alder"=c("tot"),
         "ContentsCode"=c("BE0101N1"),
         "Tid"=c("2016","2017","2018","2019"))
  
  # Download data 
  url_md <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
  expect_silent(pxmo1 <- pxweb_get(url = url_md))
  url_not_md <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A"
  expect_silent(pxmo2 <- pxweb_get(url = url_not_md))  

  expect_silent(px_data1 <- pxweb_get(url = url_md, query = pxweb_query_list))
  expect_silent(px_data2 <- pxweb_advanced_get(url = url_md, query = pxweb_query_list, pxmdo = pxmo1))
  expect_identical(px_data1$data, px_data2$data)
  
})  


test_that(desc="return clear error message when missing values",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()

  pql <- list("Tilltalsnamn"=c("20Agnes"),
              "Tid"=c("2019"))
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0001/BE0001D/BE0001T05AR"
  expect_warning(pd <- pxweb_get(url, query = pql), regexp = "ContentsCode")
  
})  



test_that(desc="Query with non-ascii characters work as well",{
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  pxweb_query_list <-
    list("KOHEZIJSKA REGIJA"=c("0"),
#         "DRŽAVA ROJSTVA"=c("0"),
         "SPOL"=c("0"),
#         "ČETRTLETJE"=c("2008Q1","2020Q3"),
         "MERITVE"=c("2000"))
  
  # Explicit encoding of ČETRTLETJE
  fixed.name <- paste("\U010C", "ETRTLETJE", sep = "")
  years <- c("2008Q1","2008Q2","2008Q3","2008Q4","2009Q1","2009Q2","2009Q3","2009Q4","2010Q1","2010Q2","2010Q3" ,"2010Q4","2011Q1","2011Q2","2011Q3","2011Q4","2012Q1","2012Q2","2012Q3","2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4","2015Q1","2015Q2","2015Q3","2015Q4","2016Q1","2016Q2","2016Q3","2016Q4","2017Q1","2017Q2","2017Q3","2017Q4","2018Q1","2018Q2","2018Q3","2018Q4","2019Q1","2019Q2","2019Q3","2019Q4","2020Q1","2020Q2","2020Q3")
  pxweb_query_list[[fixed.name]] <- years
  
  expect_silent(px_data <-
    pxd <- pxweb_get(url = "https://pxweb.stat.si:443/SiStatData/api/v1/en/Data/0762002S.px",
              query = pxweb_query_list)
  )

})  




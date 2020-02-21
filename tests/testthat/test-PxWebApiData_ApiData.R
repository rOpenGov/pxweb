context("ApiData")

test_that("ApiData - Readymade SSB-data with urlType", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  ssb1066 <- suppressWarnings(ApiData(1066, getDataByGET = TRUE, urlType = "SSB"))
  expect_equal(is.data.frame(ssb1066[[1]]), TRUE)
  expect_equal(names(ssb1066)[2], "dataset")
  expect_equal(names(ssb1066)[1], "07129: Detaljomsetningsindeksen, etter næring, måned og statistikkvariabel", "dataset")
})

test_that("ApiData - SCB-data using TRUE and FALSE", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  urlSCB <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  a1 <- suppressWarnings(ApiData(urlSCB, Region = FALSE, Civilstand = "G", Alder = "19", Kon = "2", ContentsCode = c("Folkmängd", "Folkökning"), Tid = "1969"))
  a2 <- suppressWarnings(ApiData(urlSCB, Region = FALSE, Civilstand = "gifta", Alder = "19 år", Kon = "kvinnor", ContentsCode = c("BE0101N1", "BE0101N2"), Tid = "1969"))
  a3 <- suppressWarnings(ApiData(urlSCB, Region = FALSE, Civilstand = 2, Alder = 20, Kon = 2, ContentsCode = TRUE, Tid = 2))
  expect_equal(is.data.frame(a1[[1]]), TRUE)
  expect_equal(is.integer(a1[[1]][, "value"]), TRUE)
  expect_equal(is.character(a1[[2]][, "ContentsCode"]), TRUE)
  expect_equal(a1[[1]][, "value"], a1[[2]][, "value"])
  expect_equal(a1, a2)
  expect_equal(a1, a3)
})


test_that("ApiData - StatFin-data with special characters", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  urlStatFin <- "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyonv/statfin_tyonv_pxt_001.px"
  a1 <- suppressWarnings(ApiData(urlStatFin, Kuukausi = c("2006M02"), Alue2018 = c("005"), Muuttujat = c("TYOTTOMAT")))
  a2 <- suppressWarnings(ApiData(urlStatFin, Kuukausi = "2006M02", Alue2018 = "Alajärvi Kunta", Muuttujat = "Työttömät"))
  a3 <- suppressWarnings(ApiData(urlStatFin, Kuukausi = 2, Alue2018 = 2, Muuttujat = 2))
  expect_equal(a1[[1]]$Alue2018, "Alajärvi Kunta")
  expect_equal(a1, a2)
  expect_equal(a1, a3)
})



test_that("ApiData - SSB-data advanced use", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  urlSSB <- "http://data.ssb.no/api/v0/en/table/04861"
  a1  <- suppressWarnings(ApiData(urlSSB, Region = "0399", ContentsCode = TRUE, Tid = 2i)) 
  a1q <- suppressWarnings(ApiData(urlSSB, Region = "0399", ContentsCode = TRUE, Tid = 2i, returnApiQuery=TRUE))
  a2  <- suppressWarnings(ApiData(urlSSB, Region = list("039*"), ContentsCode = c("Area of urban settlements (km²)", "Bosatte"), Tid = -(1:2)))
  a3q <- suppressWarnings(ApiData(urlSSB, Region = "Uoppgitt komm. Oslo", ContentsCode = list("all", "*"), Tid = list("top", "2"), returnApiQuery=TRUE))
  expect_equal(a1, a2)
  expect_equal(a1q, a3q)
})


test_that("ApiData - SSB-data with returnMetaFrames", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  
  urlSSB <- "http://data.ssb.no/api/v0/en/table/04861"
  mf <- suppressWarnings(ApiData(urlSSB, returnMetaFrames = TRUE))
  expect_equal(names(mf), c("Region", "ContentsCode", "Tid"))
  expect_equivalent(attr(mf, "text")[c("Region", "ContentsCode", "Tid")], c("region", "contents", "year"))
  expect_equivalent(c(attr(mf, "elimination"), attr(mf, "time")), c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(mf[[1]]$valueTexts[mf[[1]]$values == "0121"], "Rømskog")
  expect_equal(mf[[2]]$valueTexts, c("Area of urban settlements (km²)", "Number of residents"))
  expect_equivalent(sapply(mf, class), rep("data.frame", 3))
  expect_equivalent(sapply(mf[[3]], class), c("character", "character"))
})

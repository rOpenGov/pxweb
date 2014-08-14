# Test large queris (that it is downloaded part by part)

cat("\ntests_big_query.R : ")

test_that(desc="Big queries",{
  ptm <- proc.time()
  expect_that({
    cleanTestData <-
      get_pxweb(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
                dims = list(Region = c('*'), Civilstand = c('*'), Alder = c('*'), Kon = c('*'), ContentsCode = c('*'),Tid = c('*')),
                clean = FALSE)
  }, not(throws_error()))
  diff <- proc.time()-ptm
  Sys.sleep(max(1.1-diff[3],0))
})  
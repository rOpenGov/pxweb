# Test suite for clean_pxweb()

context("clean_pxweb.R")

test_that(desc="clean_pxweb",{
  
  skip_on_cran()
  skip("Skip temporarily (until new version)")
  
  api_tests_clean_pxweb <- list(
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet",
      dims = list(ContentsCode = c('PR0101A1'),
                  Tid = c('*'))),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('00', '01'),
                  Civilstand = c('*'),
                  Alder = c('0', 'tot'),
                  Kon = c('*'),
                  ContentsCode = c('*'),
                  Tid = c('2010', '2011', '2012', '2013'))
    ),
    
    list(
      url = "http://pxwebapi2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/asas/010_asas_tau_101.px",
      dims = list("Alue" = c("*"),
                  "Asuntokunnan koko" = c("*"),
                  "Talotyyppi" = c("S"),
                  "Vuosi" = c("*")
      )
    ),
    
    list(
      url = "http://pxwebapi2.stat.fi/PXWeb/api/v1/fi/StatFin/asu/asas/010_asas_tau_101.px",
      dims = list("Alue" = c("*"),
                  "Asuntokunnan koko" = c("*"),
                  "Talotyyppi" = c("*"),
                  "Vuosi" = c("1985", "1987")
      )
    ),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/Fordon",
      dims = list("Fordonsslag" = c("MCEJMOPED"),
                  "Bestand" = c("AVST"),
                  "ContentsCode" = c("TK1001A1"),
                  "Tid" = c("1997M06")
      )
    ),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('00', '01'),
                  Civilstand = c('*'),
                  Alder = c('0', 'tot'),
                  Kon = c('*'),
                  ContentsCode = c('BE0101N1'),
                  Tid = c('2010'))
    ),
    
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001S/SnabbStatTK1001",
      dims = list("ContentsCode" = c("TK1001AE"),
                  "Tid" = c("2014M02"))
    )
  )
  
  
  for (test in api_tests_clean_pxweb){
    test_data <-
      get_pxweb_data(url = test$url,
                     dims = test$dims,
                     clean = FALSE)
    
    expect_that({
      test_clean <- pxweb:::clean_pxweb(data2clean=test_data, 
                                        url=test$url, 
                                        dims=test$dims,
                                        content_node=get_pxweb_metadata(path=test$url))
    }, not(throws_error()), info = test$url)
    
    expect_is(object=test_clean[[1]][,ncol(test_clean[[1]])], "numeric", info = test$url)
    
    for(j in 1:(ncol(test_clean[[1]])-1)){
      expect_equal(object=sum(is.na(test_clean[[1]][,j])), 0)
      expect_is(object=test_clean[[1]][,j], "factor", 
                info=paste(test$url, " : col ", j, ".", sep=""))
    }
  }
})


test_that(desc="clean_pxweb_numbers",{
  
  skip_on_cran()
  skip("Skip temporarily (until new version)")
  
  api_tests_clean_pxweb_numbers <- list(
    list(
      url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
      dims = list(Region = c('00', '01'),
                  Civilstand = c('*'),
                  Alder = c('0', 'tot'),
                  Kon = c('*'),
                  ContentsCode = c('BE0101N1'),
                  Tid = c('2010'))
    )
  )
  
  for (test in api_tests_clean_pxweb_numbers){
      test_data <-
        get_pxweb_data(url = test$url,
                       dims = test$dims,
                       clean = FALSE)
      
      test_data_clean <-
        pxweb:::clean_pxweb(data2clean=test_data, 
                            url=test$url, 
                            dims=test$dims,
                            content_node=get_pxweb_metadata(path=test$url))
            
      clean_part <- test_data_clean[[1]]$values
      original_part <- 
        suppressWarnings(as.numeric(stringr::str_replace_all(test_data[,ncol(test_data)],"\\s","")))
      expect_true(object=all(clean_part[!is.na(clean_part)] == original_part[!is.na(clean_part)]))
      
  }
})

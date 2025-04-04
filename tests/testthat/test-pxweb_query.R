# Test suits for the examples in the documentation

context("pxweb_query")

test_that(desc = "pxweb_query object", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()

  dims <- list(
    Alue = c("*"),
    "Asuntokunnan koko" = c("*"),
    Talotyyppi = c("S"),
    Vuosi = c("*")
  )
  expect_silent(pxq1 <- pxweb_query(dims))

  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
  expect_silent(pxq2 <- pxweb_query(json_query))

  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_agg_example.json")
  expect_silent(pxq3 <- pxweb_query(json_query))
  
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "sq-api_table_statfin_eot_pxt_132a.px.json")
  expect_silent(pxq4 <- pxweb_query(json_query))

  dims <- list(Alue = c("*", "*"))
  expect_error(pxq1 <- pxweb_query(x = dims))

  expect_output(print(pxq3), "PXWEB QUERY")
})


test_that(desc = "split pxweb_query object", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  skip_if_offline()

  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  dims <- list(
    Region = c("01", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "03", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "04", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "05", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "06", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "07", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "08", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "09", "0980", "10", "1060", "1080", "1081", "1082", "1083", "12", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "13", "1315", "1380", "1381", "1382", "1383", "1384", "14", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "17", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "18", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "19", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "21", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "22", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "23", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "24", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "25", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"),
    Civilstand = c("OG", "G", "ÄNKL", "SK"),
    Alder = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100+"),
    Kon = c("1", "2"),
    ContentsCode = c("BE0101N1"),
    Tid = c("1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
  )
  expect_silent(pxq <- pxweb_query(dims))
  expect_silent(px <- pxweb(url))
  expect_silent(pxmd <- pxweb_get(url))


  px$config$max_values_to_download <- 110000
  expect_silent(pxqs <- pxweb:::pxweb_split_query(pxq, px, pxmd))
  expect_length(pxqs, 120)
  qsize <- 0
  for (i in seq_along(pxqs)) {
    expect_s3_class(pxqs[[i]], "pxweb_query")
    qsize <- qsize + prod(pxweb:::pxweb_query_dim(pxqs[[i]]))
  }
  expect_equal(qsize, 12564400)

  px$config$max_values_to_download <- 10000000
  expect_silent(pxqs <- pxweb:::pxweb_split_query(pxq, px, pxmd))
  expect_length(pxqs, 2)

  px$config$max_values_to_download <- 1000000
  expect_silent(pxqs <- pxweb:::pxweb_split_query(pxq, px, pxmd))
  expect_length(pxqs, 13)

  px$config$max_values_to_download <- 12564400
  expect_silent(pxqs <- pxweb:::pxweb_split_query(pxq, px, pxmd))
  expect_length(pxqs, 1)

  dims <- list(
    Region = c("01", "0114", "0115", "0117", "0120", "0123", "0125", "0126", "0127", "0128", "0136", "0138", "0139", "0140", "0160", "0162", "0163", "0180", "0181", "0182", "0183", "0184", "0186", "0187", "0188", "0191", "0192", "03", "0305", "0319", "0330", "0331", "0360", "0380", "0381", "0382", "04", "0428", "0461", "0480", "0481", "0482", "0483", "0484", "0486", "0488", "05", "0509", "0512", "0513", "0560", "0561", "0562", "0563", "0580", "0581", "0582", "0583", "0584", "0586", "06", "0604", "0617", "0642", "0643", "0662", "0665", "0680", "0682", "0683", "0684", "0685", "0686", "0687", "07", "0760", "0761", "0763", "0764", "0765", "0767", "0780", "0781", "08", "0821", "0834", "0840", "0860", "0861", "0862", "0880", "0881", "0882", "0883", "0884", "0885", "09", "0980", "10", "1060", "1080", "1081", "1082", "1083", "12", "1214", "1230", "1231", "1233", "1256", "1257", "1260", "1261", "1262", "1263", "1264", "1265", "1266", "1267", "1270", "1272", "1273", "1275", "1276", "1277", "1278", "1280", "1281", "1282", "1283", "1284", "1285", "1286", "1287", "1290", "1291", "1292", "1293", "13", "1315", "1380", "1381", "1382", "1383", "1384", "14", "1401", "1402", "1407", "1415", "1419", "1421", "1427", "1430", "1435", "1438", "1439", "1440", "1441", "1442", "1443", "1444", "1445", "1446", "1447", "1452", "1460", "1461", "1462", "1463", "1465", "1466", "1470", "1471", "1472", "1473", "1480", "1481", "1482", "1484", "1485", "1486", "1487", "1488", "1489", "1490", "1491", "1492", "1493", "1494", "1495", "1496", "1497", "1498", "1499", "17", "1715", "1730", "1737", "1760", "1761", "1762", "1763", "1764", "1765", "1766", "1780", "1781", "1782", "1783", "1784", "1785", "18", "1814", "1860", "1861", "1862", "1863", "1864", "1880", "1881", "1882", "1883", "1884", "1885", "19", "1904", "1907", "1960", "1961", "1962", "1980", "1981", "1982", "1983", "1984", "20", "2021", "2023", "2026", "2029", "2031", "2034", "2039", "2061", "2062", "2080", "2081", "2082", "2083", "2084", "2085", "21", "2101", "2104", "2121", "2132", "2161", "2180", "2181", "2182", "2183", "2184", "22", "2260", "2262", "2280", "2281", "2282", "2283", "2284", "23", "2303", "2305", "2309", "2313", "2321", "2326", "2361", "2380", "24", "2401", "2403", "2404", "2409", "2417", "2418", "2421", "2422", "2425", "2460", "2462", "2463", "2480", "2481", "2482", "25", "2505", "2506", "2510", "2513", "2514", "2518", "2521", "2523", "2560", "2580", "2581", "2582", "2583", "2584"),
    Civilstand = c("OG", "G", "ÄNKL", "SK"),
    Kon = c("1", "2"),
    ContentsCode = c("BE0101N1"),
    Tid = c("1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")
  )
  expect_silent(pxq <- pxweb_query(dims))

  px$config$max_values_to_download <- 50
  expect_silent(pxqs <- pxweb:::pxweb_split_query(pxq, px, pxmd))
  expect_length(pxqs, 2488)
})


test_that(desc = "split pxweb_query bug", {
  # CRAN seem to run tests in parallel, hence API tests cannot be run on CRAN.
  skip_on_cran()
  skip_if_offline()

  url <- "http://px.hagstofa.is/pxis/api/v1/is/Efnahagur/utanrikisverslun/1_voruvidskipti/03_inntollskra/UTA03801.px"
  pxweb_query_list <- list(
    "Tollskrárnúmer" = c("*"), # length(vars[[1]]$values) is equal to 4474
    "Land" = c("*"), # length(vars[[2]]$values) is equal to 249
    "Mánuður" = c("*"), # length(vars[[3]]$values) is equal to 36
    "Eining" = c("*")
  ) # length(vars[[4]]$values) is equal to 4
  # pxq <- pxweb_add_metadata_to_query(pxq, pxmd)
  # pxweb_validate_query_with_metadata(pxq, pxmd)
  # pxweb_query_dim(pxq)

  expect_silent(pxq <- pxweb_query(pxweb_query_list))
  expect_silent(px <- pxweb(url))
  expect_silent(pxmd <- pxweb_get(url))

  pxq <- pxweb:::pxweb_add_metadata_to_query(pxq, pxmd)
  px$config$max_values_to_download <- 10000
  expect_error(pxqs <- pxweb_split_query(pxq, px, pxmd))

  px$config$max_values_to_download <- 20000
  expect_silent(pxqs <- pxweb_split_query(pxq, px, pxmd))
  expect_gt(length(pxqs), 1000)
})



test_that(desc = "pxweb_query JSON parse error message", {
  json_query <- file.path(system.file(package = "pxweb"), "extdata", "test_files", "json_queries", "json_single_query_test.json")
  jq <- jsonlite::toJSON(paste(readLines(json_query), collapse = " "))
  expect_error(pxq1 <- pxweb_query(x = jq), regexp = "cannot parse")
})


test_that(desc = "mandatory variables are included automatically", {
  skip_if_offline()
  skip_on_cran()
  fp <- test_path(file.path("test_data", "pxm1_test.rda"))
  url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"

  if (FALSE) {
    px_meta <- pxweb_get(url)
    save(px_meta, file = fp)
  } else {
    load(file = fp)
  }

  pxweb_query_list <-
    list(
      "Civilstand" = c("*"),
      "Kon" = c("1", "2"),
      "ContentsCode" = c("BE0101N1"),
      "Tid" = c("2015", "2016", "2017")
    )
  expect_silent(pxq1 <- pxweb_query(pxweb_query_list))
  expect_silent(pxq2 <- pxweb_add_mandatory_variables(pxq1, px_meta))
  expect_identical(pxq1, pxq2)

  pxweb_query_list <-
    list(
      "Civilstand" = c("*"),
      "Kon" = c("1"),
      #         "ContentsCode"=c("BE0101N1"),
      "Tid" = c("2015")
    )
  expect_silent(pxq3 <- pxweb_query(pxweb_query_list))
  expect_warning(pxq4 <- pxweb_add_mandatory_variables(pxq3, px_meta), regexp = "ContentsCode")
  expect_failure(expect_identical(pxq3, pxq4))

  skip_on_ci()
  skip_on_cran()
  # This gives an error on github action that it creates output
  # I cant reproduce that error as of now.
  expect_silent(out <- capture.output(pxd <- pxweb_get(url, pxq4)))
  if (length(out) > 0) warning(out)
})

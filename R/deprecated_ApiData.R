#' PX-Web Data by API
#' 
#' A function to read PX-Web data into R via API. The example code reads data from the three national statistical institutes, Statistics Norway, Statistics Sweden and Statistics Finland.
#' 
#' @encoding UTF8
#'
#' @param urlToData url to data or id of SSB data
#' @param ... specification of JSON query for each variable
#' @param getDataByGET When TRUE, readymade dataset by GET - works only for Statistics Norway readymade datasets
#' @param returnMetaData When TRUE, metadata returned  
#' @param returnMetaValues When TRUE, values from metadata returned 
#' @param returnMetaFrames When TRUE, values and valueTexts from metadata returned as data frames 
#' @param returnApiQuery When TRUE, JSON query returned 
#' @param defaultJSONquery specification for variables not included in ...
#' @param verbosePrint When TRUE, printing to console
#' @param use_factors Parameter to \code{\link[rjstat]{fromJSONstat}} defining whether dimension categories should be factors or character objects.
#' @param urlType  Parameter defining how url is constructed from id number. Currently two Statistics Norway possibilities: "SSB" (Norwegian) or "SSBen" (English)
#' 
#' @details Each variable is specified by using the variable name as input parameter. The value can be specified as:  
#' TRUE (all), FALSE (eliminated), imaginary value (top), variable indices, 
#' original variable id's (values) or variable labels (valueTexts). 
#' Reversed indices can be specified as negative values. 
#' Indices outside the range are removed. Variables not specified is set to the value of defaultJSONquery 
#' whose default means the first and the two last elements. 
#' 
#' The value can also be specified as a (unnamed) two-element list corresponding to the two 
#' query elements, filter and values. In addition it possible with a single-element list.
#' Then filter is set to 'all'. See examples. 
#'
#' @return list of two data sets (label and id)
#' @keywords internal
#' @export
#' 
#' @importFrom jsonlite unbox read_json
#' @importFrom httr GET POST verbose content
#' @importFrom utils head tail
#'
#' @examples
#' \dontrun{
#' ##### Readymade dataset by GET - works only for Statistics Norway readymade datasets
#' x <- ApiData("http://data.ssb.no/api/v0/dataset/1066.json?lang=en", getDataByGET = TRUE)
#' x[[1]]  # The label version of the data set
#' x[[2]]  # The id version of the data set
#' 
#' ##### Special output
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", returnMetaData = TRUE)   # meta data
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", returnMetaValues = TRUE) # meta data values
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", returnMetaFrames = TRUE) # list of data frames
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", returnApiQuery = TRUE)   # query using defaults
#' 
#' 
#' ##### Ordinary use
#' 
#' # NACE2007 as imaginary value (top 10), ContentsCode as TRUE (all), Tid is default
#' ApiData("http://data.ssb.no/api/v0/en/table/09941", NACE2007 = 10i, ContentsCode = TRUE)
#' 
#' # Two specified and the last is default (as above) – in Norwegian change en to no in url
#' ApiData("http://data.ssb.no/api/v0/no/table/09941", NACE2007 = 10i, ContentsCode = TRUE)
#' 
#' # Number of residents (bosatte) last year, each region
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = TRUE, 
#'         ContentsCode = "Bosatte", Tid = 1i)
#' 
#' # Number of residents (bosatte) each year, total
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = FALSE, 
#'         ContentsCode = "Bosatte", Tid = TRUE)
#' 
#' # Some years
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = FALSE, 
#'         ContentsCode = "Bosatte", Tid = c(1, 5, -1))
#' 
#' # Two selected regions
#' ApiData("http://data.ssb.no/api/v0/en/table/04861", Region = c("0811", "0301"), 
#'         ContentsCode = 1, Tid = c(1, -1))
#' 
#' 
#' ##### Using id instead of url, unnamed input and verbosePrint
#' ApiData(4861, c("0811", "0301"), 1, c(1, -1)) # same as below 
#' ApiData(4861, Region = c("0811", "0301"), ContentsCode=1, Tid=c(1, -1)) 
#' names(ApiData(4861,returnMetaFrames = TRUE))  # these names from metadata assumed two lines above
#' ApiData("4861", c("0811", "0301"), 1, c(1, -1),  urlType="SSBen")
#' ApiData("01222", c("0811", "0301"), c(4, 9:11), 2i, verbosePrint = TRUE)
#' ApiData(1066, getDataByGET = TRUE,  urlType="SSB")
#' ApiData(1066, getDataByGET = TRUE,  urlType="SSBen")
#' 
#' 
#' ##### Advanced use using list. See details above. Try returnApiQuery=TRUE on the same examples. 
#' ApiData(4861, Region = list("03*"), ContentsCode = 1, Tid = 5i) # "all" can be dropped from the list
#' ApiData(4861, Region = list("all", "03*"), ContentsCode = 1, Tid = 5i)  # same as above
#' ApiData(04861, Region = list("item", c("0811", "0301")), ContentsCode = 1, Tid = 5i)
#' 
#' 
#' ##### Using data from SCB to illustrate returnMetaFrames
#' urlSCB <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' mf <- ApiData(urlSCB, returnMetaFrames = TRUE)
#' names(mf)              # All the variable names
#' attr(mf, "text")       # Corresponding text information as attribute
#' mf$ContentsCode        # Data frame for the fifth variable (alternatively  mf[[5]])
#' attr(mf,"elimination") # Finding variables that can be eliminated
#' ApiData(urlSCB,        # Eliminating all variables that can be eliminated (line below)
#'         Region = FALSE, Civilstand = FALSE, Alder = FALSE,  Kon = FALSE,
#'         ContentsCode  = "BE0101N1", # Selecting a single ContentsCode by text input
#'         Tid = TRUE)                 # Choosing all possible values of Tid.
#'  
#'                
#' ##### Using data from Statfi to illustrate use of input by variable labels (valueTexts)
#' urlStatfi <- "http://pxnet2.stat.fi/PXWeb/api/v1/en/StatFin/vrm/kuol/statfin_kuol_pxt_010.px"
#' ApiData(urlStatfi, returnMetaFrames = TRUE)$Tiedot
#' ApiData(urlStatfi, Alue = FALSE, Vuosi = TRUE, Tiedot = "Population")  # same as Tiedot = '15' 
#' }
ApiData <- function(urlToData, ..., getDataByGET = FALSE, returnMetaData = FALSE, returnMetaValues = FALSE, 
                    returnMetaFrames = FALSE, returnApiQuery = FALSE, 
                    defaultJSONquery = c(1,-2, -1), verbosePrint = FALSE,
                    use_factors=FALSE, urlType="SSB") {
  .Deprecated(new = "pxweb_advanced_get")
  integerUrl <- suppressWarnings(as.integer(urlToData))
  if (!is.na(integerUrl)) 
    urlToData <- MakeUrl(integerUrl, urlType = urlType, getDataByGET = getDataByGET) # SSBurl(integerUrl, getDataByGET)
  if (getDataByGET) 
    post <- GET(urlToData) else {
      metaData <- MetaData(urlToData)
      if (returnMetaData) 
        return(metaData)
      if (returnMetaValues) 
        return(VarMetaData(metaData))
      metaFrames <- MetaFrames(metaData)   
      if (returnMetaFrames) 
        return(metaFrames)
      if (verbosePrint){ 
        print(VarMetaData(metaData))
        cat("\n\n")
      }
      # if(returnApiDataCall) # Not in use
      # return(as.call(c(list(as.symbol('ApiData'),urlToData=urlToData),MakeApiQuery(metaFrames,...,returnThezList=TRUE,defaultJSONquery=defaultJSONquery))))
      sporr <- MakeApiQuery(metaFrames, ..., defaultJSONquery = defaultJSONquery)
      if (returnApiQuery) 
        return(sporr)
      if (verbosePrint) 
        post <- POST(urlToData, body = sporr, encode = "json", verbose()) else post <- POST(urlToData, body = sporr, encode = "json")
    }
  depr_check_for_package("rjstat")
  #c(fromJSONstat(content(post, "text"), naming = "label"), fromJSONstat(content(post, "text"), naming = "id"))
  c(rjstat::fromJSONstat(content(post, "text"), naming = "label",use_factors=use_factors), 
    rjstat::fromJSONstat(content(post, "text"), naming = "id",use_factors=use_factors))
}




#' Adding leading zeros
#'
#' @param n  numeric vector
#' @param width width
#'
#' @return Number as string
#' @keywords internal 
#'
Number = function(n,width=3){
  s = "s=sprintf('%0d',n)"
  s = gsub("0",as.character(width),s)
  eval(parse(text=s))
  s = gsub(" ","0",s)
  s
}


MetaData <- function(url) {
  z <- read_json(url)  # Same as fromJSON(content(GET(url),'text'),simplifyVector = FALSE)
  tit <- z[[1]]
  z <- z[[2]]
  for (i in seq_len(length(z))) {
    for (j in seq_len(length(z[[i]]))) z[[i]][[j]] <- unlist(z[[i]][[j]])
  }
  z
}

VarMetaData <- function(metaData) {
  n <- length(metaData)
  nam <- rep("", n)
  elimination <- rep(FALSE, n)
  for (i in 1:length(metaData)) {
    nam[i] <- metaData[[i]]$code
    if (!is.null(metaData[[i]]$elimination)) 
      elimination[i] <- metaData[[i]]$elimination
    metaData[[i]] <- metaData[[i]]$values
  }
  names(metaData) <- nam
  attr(metaData, "elimination") <- elimination
  metaData
}


MetaFrames <- function(metaData) {
  n <- length(metaData)
  nam <- rep("", n)
  text <- rep("", n)
  elimination <- rep(FALSE, n)
  time <- rep(FALSE, n)
  for (i in 1:length(metaData)) {
    nam[i] <- metaData[[i]]$code
    if (!is.null(metaData[[i]]$text)) 
      text[i] <- metaData[[i]]$text
    if (!is.null(metaData[[i]]$elimination)) 
      elimination[i] <- metaData[[i]]$elimination
    if (!is.null(metaData[[i]]$time)) 
      time[i] <- metaData[[i]]$time
    metaData[[i]] <- as.data.frame(metaData[[i]][c("values", "valueTexts")], stringsAsFactors = FALSE)
  }
  names(metaData) <- nam
  names(text) <- nam
  names(elimination) <- nam
  names(time) <- nam
  attr(metaData, "text") <- text
  attr(metaData, "elimination") <- elimination
  attr(metaData, "time") <- time
  metaData
}


# x is one element of metaFrames
MakeApiVar <- function(x, values = c(1, -2, -1)) {
  if (is.list(values)) {
    if (length(values) == 1) {
      filt <- "all"
      valu <- values[[1]]
    } else {
      filt <- values[[1]]
      valu <- values[[2]]
    }
  } else {
    if (is.logical(values)) {
      if (!values) 
        return(NULL) else {
          filt <- "all"
          valu <- "*"
        }
      
    } else if (is.complex(values)) {
      filt <- "top"
      valu <- as.character(Im(values))
    } else if (is.numeric(values)) {
      filt <- "item"
      nx <- length(x[[1]]$values)
      values <- values[abs(values) > 0 & abs(values) <= nx]  # Fix outside range
      values[values < 0] <- rev(seq_len(nx))[-values[values < 0]]  # Fix negative
      valu <- x[[1]]$values[unique(values)]
      if (!length(valu)) 
        stop(paste(names(x), "no indices in valid range"))
    } else {
      filt <- "item"
      noMatch <- !(values %in% x[[1]]$values)
      if (any(noMatch)) values[noMatch] <- x[[1]]$values[match(values[noMatch], x[[1]]$valueTexts)]
      if (any(!(values %in% x[[1]]$values)))
        stop(paste(names(x[1]), ": Text input must be in:", 
                   paste(c(HeadEnd(x[[1]]$values, 20), HeadEnd(x[[1]]$valueTexts, 8)), collapse = ", ")))
      valu <- values
    }
  }
  list(code = jsonlite::unbox(names(x)), selection = list(filter = unbox(filt), values = valu))
}


# Old version where x is one element of varMetaData 
MakeApiVarOld <- function(x, values = c(1, -2, -1)) {
  if (is.list(values)) {
    if (length(values) == 1) {
      filt <- "all"
      valu <- values[[1]]
    } else {
      filt <- values[[1]]
      valu <- values[[2]]
    }
  } else {
    if (is.logical(values)) {
      if (!values) 
        return(NULL) else {
          filt <- "all"
          valu <- "*"
        }
      
    } else if (is.complex(values)) {
      filt <- "top"
      valu <- as.character(Im(values))
    } else if (is.numeric(values)) {
      filt <- "item"
      nx <- length(x[[1]])
      values <- values[abs(values) > 0 & abs(values) <= nx]  # Fix outside range
      values[values < 0] <- rev(seq_len(nx))[-values[values < 0]]  # Fix negative
      valu <- x[[1]][unique(values)]
      if (!length(valu)) 
        stop(paste(names(x), "no indices in valid range"))
    } else {
      filt <- "item"
      if (any(!(values %in% x[[1]])))
        stop(paste(names(x[1]), ": Text input must be in:", paste(x[[1]], collapse = ", ")))
      valu <- values
    }
  }
  list(code = jsonlite::unbox(names(x)), selection = list(filter = unbox(filt), values = valu))
}



Pmatch <- function(x, y, CheckHandling = stop) {
  # as pmatch where NA set to remaing values in y
  a <- pmatch(x, y)
  naa <- is.na(a)
  if (any(naa)) {
    nax <- naa & !is.na(x)
    if (any(nax)) {
      CheckHandling(paste("Non-matching input:", paste(x[nax], collapse = ", "),
                          ",   Valid input parameters in addition to those in the function documentation are: ",
                          paste(y, collapse = ", ")))
    }
    if (!any(!naa)) 
      return(seq_len(length(y))[seq_len(length(x))])
    nna <- sum(naa)
    a[naa] <- seq_len(length(y))[-a[!naa]][seq_len(nna)]
  }
  a
}



SSBurl <- function(id, readyMade = FALSE) {
  if (readyMade) 
    url <- paste("http://data.ssb.no/api/v0/dataset/", Number(id, 1), ".json", sep = "") 
  else url <- paste("http://data.ssb.no/api/v0/no/table/", Number(id, 5), sep = "")
  url
}

SSBurlen <- function(id, readyMade = FALSE) {
  if (readyMade) 
    url <- paste("http://data.ssb.no/api/v0/dataset/", Number(id, 1), ".json?lang=en", sep = "") 
  else 
    url <- paste("http://data.ssb.no/api/v0/en/table/", Number(id, 5), sep = "")
  url
}


#' MakeUrl from id
#' 
#' @encoding UTF8
#'
#' @param id integer
#' @param urlType  Currently two possibilities: "SSB" (Norwegian) or "SSBen" (English)
#' @param getDataByGET As input to ApiData
#'
#' @return url as string
#' @export
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' MakeUrl(4861)
#' MakeUrl(4861, "SSBen")
#' MakeUrl(1066, getDataByGET = TRUE)
#' MakeUrl(1066, "SSBen", getDataByGET = TRUE)
#' }
MakeUrl <- function(id,urlType="SSB",getDataByGET = FALSE){
  .Deprecated(new = "pxweb")
  if(urlType=="SSB")
    return(SSBurl(id,getDataByGET))
  if(urlType=="SSBen")
    return(SSBurlen(id,getDataByGET))
  stop('urlType must be "SSB" or "SSBen"')
}



MakeApiQuery <- function(metaFrames, ..., defaultJSONquery = c(1, -2, -1), returnThezList = FALSE) {
  x <- list(...)
  namesx <- names(x)
  if (is.null(namesx)) 
    namesx <- rep(NA, length(x)) else namesx[namesx == ""] <- NA
  z <- vector("list", length(metaFrames))
  a <- z
  names(z) <- names(metaFrames)
  pm <- Pmatch(namesx, names(metaFrames))
  for (i in seq_len(length(z))) z[[i]] <- defaultJSONquery
  z[pm] <- x
  elim <- attr(metaFrames, "elimination")
  emptya <- rep(FALSE, length(a))
  if (returnThezList) 
    return(z)
  for (i in seq_len(length(a))) {
    apiVar <- MakeApiVar(metaFrames[i], z[[i]])
    if (is.null(apiVar)) {
      if (!elim[i]){ 
        if(sum(elim) == 0) 
          ptext = "(no variables can in this table)"
        else
          ptext = paste("(these variables can:",paste(names(elim)[elim], collapse = ", "),")")
        stop(paste(names(z)[i], "cannot be eliminated", ptext))
      }
      emptya[i] <- TRUE
    } else a[[i]] <- apiVar
  }
  b <- list(query = a[!emptya], response = list(format = unbox("json-stat")))
  jsonlite::toJSON(b, auto_unbox = FALSE, pretty = TRUE)
}


HeadEnd <- function(x, n = 8L) {
  x <- as.character(x)
  if (length(x) > (n + 2))
    x <- c(head(x, n = n), "...", tail(x, n = 1))
  x
}


























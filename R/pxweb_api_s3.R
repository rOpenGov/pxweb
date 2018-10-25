#' S3 constructor for pxweb api
#' 
#' @description 
#' The pxwebapi object contain all information to do calls to the pxweb api and keep count 
#' of the calls. It will not make to big calls (so big queries need to be split up before). 
#' All calls to the api should be done using the object. 
#' This corresponds to the "base url" of the api. 
#' 
#' Garantuees:
#' The base_url has been pinged (by pinging full url)
#' The sub_path has been checked
#' The config has been captured from the API
#' The url has been checked to be a pxweb api (through config)
#'
#' @example 
#' pxapi_1 <- pxweb_s3_api(url ="http://api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
#' pxapi_2 <- pxweb_s3_api(url = "api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
#' pxapi_3 <- pxweb_s3_api(url = "http:/api.scb.se/OV0104/v1/doris/sv/ssd/START/ME/ME0104/ME0104C/ME0104T24")
#' pxapi_4 <- pxweb_s3_api(url = "http://api.scb.se/OV0104/v1/doris/sv?config")
#' pxapi_5 <- pxweb_s3_api(url = "http://api.scb.se/OV0104/v1/dosasasasas")
#' pxapi_5 <- pxweb_s3_api(url = "http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/")
#' url = "http://api.scb.se"
#' url <- "https://sv.wikipedia.org/wiki/ISO_639"
#' @export
pxweb_api_s3_get_url <- function(url){
  checkmate::assert_string(url)
  
  # Check if API exists already, if not construct it
  api <- load_pxweb_api_s3(url)
  
  if(is.null(api)){
    # GET url 
    r <- httr::GET(url)
    httr::stop_for_status(r)
    
    # Construct API object
    api <- pxweb_api_s3(r)
  }
  
  
}




pxweb_api_s3 <- function(url){
  UseMethod("pxweb_api_s3")
}

pxweb_api_s3.character <- function(url){
  checkmate::assert_string(url)
  r <- httr::GET(url)
  httr::stop_for_status(r)
  pxweb_api_s3(url = r)
}



pxweb_api_s3.response <- function(url){
  checkmate::assert_class(url, "response")
  checkmate::assert_false(httr::http_error(url))
  url_string <- url$url
  
  # Split up url to api parts
  url_splt <- strsplit(url, "\\?")[[1]][1]
  url_splt <- strsplit(url_splt, "/")[[1]]
  if(length(url_splt) <= 3) stop(paste0("This is not the full url to a PXWEB API: ", url), call. = FALSE)
  res <- list(api_name = url_splt[3],
              api_base_url = paste(url_splt[1:3], collapse = "/"))
  
  # Make one full config call to asses that it is a pxweb api and setup a pxweb api 
  # object here that then is updated
  
  for(p in 4:length(url_splt)){
    tmp_url <- paste0(res$api_base_url, "/", paste(url_splt[4:p], collapse = "/"), "?config")
    tmp_r <- httr::GET(tmp_url)
    if(http_error(r)) next()
    break()
  }
  
  res$api_subpath <- paste(url_splt[4:p], collapse = "/")
  if(length(url_splt) > p){
    res$api_query_path <- paste(url_splt[p:length(url_splt)], collapse = "/")
  } else {
    res$api_query_path <- NULL
  }

  cfg <- content(tmp_r, "parsed")
  if(!all(c("maxValues", "maxCalls", "timeWindow", "CORS") %in% names(cfg))){
    stop(paste0("This is not a PXWEB API url (version PX-Web 2014 Dec R1 or later): ", url), call. = FALSE)
  }
  
  # Add config to the pxweb api object
  res$cfg <- list(calls_per_period = cfg$maxCalls,
                  period_in_seconds = cfg$timeWindow,
                  max_values_to_download = cfg$maxValues,
                  CORS = cfg$CORS)
  
  # Create object
  class(res) <- c("pxweb_api_s3", "list")
  
  # Add calls made to the API
  res <- add_pxweb_api_calls(res)
  
  res
}


print.pxweb_api_s3 <- function()
 

add_pxweb_api_calls <- function(){
  
}



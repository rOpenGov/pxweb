#' Do a GET call to PXWEB API
#'
#' @param url a \code{pxweb} object or url that can be coherced to a \code{pxweb} object.
#' @param query a json string, json file or list object that can be coherced to a \code{pxweb_query} object.
#' @param verbose should large queries print out progress.
#' 
#' @examples 
#' url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' px_meta_data <- pxweb_get(url)
#' 
#' url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
#' px_levels <- pxweb_get(url)
#' 
#' url <- "http://api.scb.se/OV0104/v1/doris/sv"
#' px_levels <- pxweb_get(url)
#' 
#' url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' query <- file.path(system.file(package = "pxweb"), 
#'                    "extdata", "examples", "json_query_example.json")
#' px_data <- pxweb_get(url = url, query = query)
#' 
#' # Convert to data.frame
#' as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
#' 
#' # Get raw data
#' as.matrix(px_data, column.name.type = "code", variable.value.type = "code")
#' 
#' # Get data comments
#' pxweb_data_comments(px_data)
#' 
#' # Get jsonstat data
#' jstat <- query <- file.path(system.file(package = "pxweb"), 
#'                             "extdata", "examples", "json-stat_query_example.json")
#' jstat_data <- pxweb_get(url = url, query = query)
#' 
#' 
#' \dontrun{
#' # Get very large datasets (multiple downloads needed)
#' big_query <- file.path(system.file(package = "pxweb"), 
#'                        "extdata", "examples", "json_big_query_example.json")
#' px_data <- pxweb_get(url = url, query = big_query)
#' }
#' 
#' @export
pxweb_get <- function(url, query = NULL, verbose = TRUE){
  pxweb_advanced_get(url = url, query = query, verbose = verbose)
}

#' Do a GET call to PXWEB API and return a data.frame
#'
#' @details 
#' The functions use will do a \code{pxweb_query} to a PXWEB \code{url} and return a \code{data.frame}.
#' This is a wrapper for the \code{pxweb_get} function.
#' 
#' @seealso See \code{\link{pxweb_get}} for mor general usage and \code{\link{pxweb_query}} for details on PXWEB queries. 
#'
#' @inheritParams pxweb_get
#' @inheritParams pxweb_as_data_frame
#' @export
pxweb_get_data <- function(url, query, verbose = TRUE, column.name.type = "text", variable.value.type = "text"){
  d <- pxweb_advanced_get(url = url, query = query, verbose = verbose)
  as.data.frame(d, column.name.type = column.name.type, variable.value.type = variable.value.type)
}

#' Do a GET call to PXWEB API for advanced users
#'
#' @details 
#' This function is intended for more advanced users that want to supply specific arguments in
#' \code{httr} calls or what to debug \code{httr} calls. 
#' 
#' \code{pxweb_get()} is a wrapper for standard use.
#'
#' @inheritParams pxweb_get
#' @param log_http_calls Should the http calls to the API be logged (for debugging reasons). 
#'                       If TRUE, all calls and responses are logged and written to "log_pxweb_api_http_calls.txt" in the working directory.
#' @param pxweb_metadata A \code{pxweb_metadata} object to use for query. 
#' @param ... Further arguments sent to \code{httr::POST} (for queries) or \code{httr::GET} (for query = \code{NULL}). 
#'            If used with query, also supply a \code{pxweb_metadata} object. Otherwise the same parameters are sent to
#'            both \code{httr::POST} and \code{httr::GET}.
#' @export
pxweb_advanced_get <- function(url, query = NULL, verbose = TRUE, log_http_calls = FALSE, pxweb_metadata = NULL, ...){
  checkmate::assert_flag(log_http_calls)
  checkmate::assert_class(pxweb_metadata, classes = "pxweb_metadata", null.ok = TRUE)
  if(log_http_calls){
    pxweb_http_log_on()
  }
  
  px <- pxweb(url)
  if(!is.null(query)){
    pxq <- pxweb_query(query)
    pxmd <- pxweb_get(px)
    if(!inherits(pxmd, "pxweb_metadata")) {
      stop("The path is not a PXWEB API table endpoint with data:\n", build_pxweb_url(px), call. = FALSE)
    }
    pxq <- pxweb_add_metadata_to_query(pxq, pxmd)
    pxweb_validate_query_with_metadata(pxq, pxmd)
    pxqs <- pxweb_split_query(pxq, px, pxmd)
  } else {
    pxq <- NULL
  }
  
  if(is.null(pxq)){
    px <- pxweb_add_call(px)  
    r <- httr::GET(build_pxweb_url(px), ...)
    httr::stop_for_status(r)
    pxr <- pxweb_parse_response(x = r)
  } else {
    pxr <- list()
    if(length(pxqs) > 1 & verbose) {
      cat("  Downloading large query (in", length(pxqs), "batches):\n")
      pb <- utils::txtProgressBar(min = 0, max = length(pxqs), style = 3)
    }
    for(i in seq_along(pxqs)){
      px <- pxweb_add_call(px)  
      pxurl <- build_pxweb_url(px)
      r <- httr::POST(pxurl, body = pxweb_as_json(pxqs[[i]]), ...)
      httr::stop_for_status(r)
      pxr[[i]] <- pxweb_parse_response(x = r)
      if(length(pxqs) > 1 & verbose) {
        utils::setTxtProgressBar(pb, i)
      }
    }
    if(length(pxqs) > 1 & verbose) {
      close(pb)
    }
    pxr <- pxweb_c(pxr)
    if(inherits(pxr, "pxweb_data")){
      pxr$pxweb_metadata <- pxmd
    }
  }
  pxr
}

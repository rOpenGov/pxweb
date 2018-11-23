#' Do a GET call to PXWEB API url
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
#' \dontrun{
#' big_query <- file.path(system.file(package = "pxweb"), 
#'                        "extdata", "examples", "json_big_query_example.json")
#' px_data <- pxweb_get(url = url, query = big_query)
#' }
#' 
#' @export
pxweb_get <- function(url, query = NULL, verbose = TRUE){
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
    r <- httr::GET(build_pxweb_url(px))
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
      r <- httr::POST(pxurl, body = pxweb_as_json(pxqs[[i]]))
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

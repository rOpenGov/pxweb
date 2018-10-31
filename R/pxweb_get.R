#' Do a GET call to PXWEB API url
#'
#' @param url an url that can be coherced to a \code{pxweb} object.
#' @param query a json string, json file or list object that can be coherced to a \code{pxweb_query} object.
#' 
#' @examples 
#' url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' px_meta_data <- pxweb_get(url)
#' 
#' url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101"
#' px_levels <- pxweb_get(url)
#' url <- "http://api.scb.se/OV0104/v1/doris/sv"
#' px_levels <- pxweb_get(url)
#' 
#' url <- "http://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
#' json_query <- dir(file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
#' px_data <- pxweb_get(url = url, query = json_query)
#' 
#' @keywords internal
#' @export
pxweb_get <- function(url, query = NULL){
  px <- pxweb(url)
  if(!is.null(query)){
    pxq <- pxweb_query(query)
#    pxq <- pxweb_validate_query_with_api(px, pxq)
#    pxqs <- pxweb_split_query(px, pxq)
  } else {
    pxq <- NULL
  }
    
  if(is.null(pxq)){
    px <- pxweb_add_call(px)  
    r <- httr::GET(build_pxweb_url(px))
    pxr <- pxweb_parse_response(x = r)
  } else {
    pxr <- list()
    for(i in seq_along(pxqs)){
      px <- pxweb_add_call(px)  
      pxurl <- build_pxweb_url(px)
#      r <- httr::POST(pxurl, body = pxqs[[i]], encode = "json")
      r <- httr::POST(pxurl, body = pxq, encode = "json")      
      pxr[[i]] <- pxweb_parse_response(x = r)
    }
    # pxr <- pxweb_combine(pxr)
  }
  pxr
}

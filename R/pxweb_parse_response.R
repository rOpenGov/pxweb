#' Parse the response from a PXWEB API
#' 
#' @param x a \code{httr} response object from a PXWEB call.
#' 
#' @keywords internal
pxweb_parse_response <- function(x){
  checkmate::assert_class(x, "response")
  
  obj <- suppressWarnings(httr::content(x, as = "parsed"))

  try_obj <- try(pxweb_database_list(obj), silent = TRUE)
  if(!inherits(try_obj, "try-error")) {
    try_obj <- as_pxweb_levels(try_obj)
    return(try_obj)
  }
      
  try_obj <- try(pxweb_levels(obj), silent = TRUE)
  if(!inherits(try_obj, "try-error")) return(try_obj)  
  
  try_obj <- try(pxweb_metadata(obj), silent = TRUE)
  if(!inherits(try_obj, "try-error")) return(try_obj)  

  try_obj <- try(pxweb_data(x = obj), silent = TRUE)
  if(!inherits(try_obj, "try-error")) return(try_obj)  
  
  try_obj <- try(pxweb_data_jsonstat(x = obj), silent = TRUE)
  if(!inherits(try_obj, "try-error")) return(try_obj)  
  
  stop("Incorrect return response from PXWEB API url: \n", x$url, call. = FALSE)
}

#' @rdname pxweb_parse_response
#' @keywords internal
is_pxweb_response <- function(x){
  !inherits(try(pxweb_parse_response(x), silent = TRUE), "try-error")
}
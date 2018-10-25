#' Get hostname from an url, httr response or pxweb_api_s3 object.
#' 
#' @details 
#' The hostname is used to
#'
#' @param x object to get hostname for.
#' 
#' @keywords internal
pxweb_api_s3_hostname <- function(x){
  UseMethod("pxweb_api_s3_hostname")
}

#' @rdname pxweb_api_s3_hostname
#' @keywords internal
pxweb_api_s3_hostname.character <- function(x){
  checkmate::assert_string(x)
  parsed_url <- parse_url_or_fail(x)
  return(parsed_url$hostname)
}


#' @rdname pxweb_api_s3_hostname
#' @keywords internal
pxweb_api_s3_hostname.response <- function(x){
  httr::parse_url(x$url)$hostname
}

#' @rdname pxweb_api_s3_hostname
#' @keywords internal
pxweb_api_s3_hostname.pxweb_api_s3 <- function(x){
  x$url$hostname
}

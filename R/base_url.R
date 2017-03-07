#' Return base URL to API
#' 
#' ...
#' 
#' @param api API provider ('api.scb.se' or 'pxwebapi2.stat.fi')
#' @param version The version of PXWEB API to use.
#' @param language The language (two letters) to use in the fetched data.
#' @param ... Additional parameters. These are currently ignored.
#' @export
#' 
#' @keywords internal 
#' 
#' @examples
#' a <- base_url("api.scb.se")
#' print(a)
#' 
base_url <- function(api, version = NULL, language = NULL) {
  api_object <- pxweb_api$new(get_api = api)
  api_object$base_url(version = version, language = language)
}

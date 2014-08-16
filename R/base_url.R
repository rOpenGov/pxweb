#' Return base URL to API
#' 
#' ...
#' 
#' @param database API provider ('sweSCB' or 'statfi')
#' @param version The version of SCB API to use. (Default: \code{v1})
#' @param lang The language (two letters) to use in the fetched data. (Default: \code{en})
#' @param ... Additional parameters. These are currently ignored.
#' @export
#' @examples
#' a <- base_url("sweSCB", "v2", "en")
#' print(a)
#' 
base_url <- function(database, version = "v1", lang = "en", ...) {

  if (database == "sweSCB") {

    url <- paste(sprintf("http://api.scb.se/OV0104/%s/doris/%s/ssd",version,lang))

  } else if (database == "statfi") {

    #Was: 
    url <- paste(sprintf("http://pxwebapi2.stat.fi/PXWeb/api/%s/%s/StatFin",version,lang))

  }
  return(url)
}

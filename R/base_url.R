#' Return base URL to API
#' 
#' ...
#' 
#' @param api API provider ('api.scb.se' or 'pxwebapi2.stat.fi')
#' @param version The version of PXWEB API to use. (Default: \code{v1})
#' @param lang The language (two letters) to use in the fetched data. (Default: \code{en})
#' @param ... Additional parameters. These are currently ignored.
#' @export
#' @examples
#' a <- base_url("api.scb.se", "v1", "sv")
#' print(a)
#' 
base_url <- function(api, version = "v1", lang = "en", ...) {

  if (api %in% c("api.scb.se", "scb")) {

    url <- paste(sprintf("http://api.scb.se/OV0104/%s/doris/%s/ssd",version,lang))

  } else if (api %in% c("pxnet2.stat.fi", "statfi")) {

    #Was: 
    #url <- paste(sprintf("http://pxwebapi2.stat.fi/PXWeb/api/%s/%s/StatFin",version,lang))
    url <- paste(sprintf("http://pxnet2.stat.fi/PXWeb/api/%s/%s/StatFin", version, lang))

  }
  return(url)
}

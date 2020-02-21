#' Cite a PXWEB data object
#' 
#' @details 
#' Functionality to automatic cite PXWEB data objects.
#' 
#' @param x a \code{pxweb_data} object to cite.
#' @param style see \code{\link[utils]{bibentry}}.
#' @export
pxweb_cite <- function(x, style =  "citation"){
  checkmate::assert_class(x, "pxweb_data")
  
  api_info <- pxweb_api_catalogue()[[pxweb_api_name(pxweb(x$url))]]
  
  rref <- utils::bibentry(
    bibtype = "Misc",
    title = x$pxweb_metadata$title,
    author = utils::person(api_info$citation$organization),
    organization = api_info$citation$organization,
    address = api_info$citation$address,
    year = as.POSIXlt(x$time_stamp)$year + 1900L,
    url = x$url,
    note = paste0("[Data accessed ", x$time_stamp, " using pxweb R package ", utils::packageVersion("pxweb"), "]"))
  
  print(rref, style = style)
  
  utils::citation("pxweb")
}

#' Defunct functions
#' 
#' @description 
#' These function has as from version 0.10.0 become defunct.
#' Call the functions to get information on new functions to use.
#' 
#' @param url Defunct argument.
#' @param ... Defunct argument.
#' @param api Defunct argument.
#' @param version Defunct argument.
#' @param language Defunct argument.
#' @param dims Defunct argument.
#' @param clean Defunct argument.
#' @param encoding Defunct argument.
#' @param node Defunct argument.
#' @param verbose Defunct argument.
#' @param baseURL Defunct argument.
#' @param descriptions Defunct argument.
#' @param quiet Defunct argument.
#' @param path Defunct argument.
#' @param topnodes Defunct argument.
#' 
#' @export
api_catalogue <- function(){
  .Defunct("pxweb_api_catalogue")
}

#' @rdname api_catalogue
#' @export
update_pxweb_apis <- function(){
  .Defunct(msg = "update_pxweb_apis() is no longer allowed by CRAN. See vignette(\"pxweb\") on how to update your API catalogue.")
}

#' @rdname api_catalogue
#' @export
api_parameters <- function(url = NULL) {
  .Defunct(new = "pxweb_api_catalogue")
}


#' @rdname api_catalogue
#' @export
base_url <- function(api, version = NULL, language = NULL) {
  .Defunct("pxweb")
}

#' @rdname api_catalogue
#' @export
get_pxweb_data <- function(url, dims, clean = FALSE, encoding = NULL) {
  .Defunct("pxweb_get_data")
}

#' @rdname api_catalogue
#' @export
get_pxweb_dims <- function(node, verbose=TRUE) {
  .Defunct("pxweb_advanced_get")
}

#' @rdname api_catalogue
#' @export
get_pxweb_levels <- function(baseURL, descriptions = FALSE, quiet = FALSE, ...) {
  .Defunct("pxweb_get")
}

#' @rdname api_catalogue
#' @export
get_pxweb_metadata <- function(path = NULL, node = NULL, topnodes = NULL, quiet = TRUE, baseURL = NULL, ...) {
  .Defunct("pxweb_get")
}

#' @rdname api_catalogue
#' @export pxweb_api
pxweb_api <- list(new =function(){
  .Defunct(msg = "'pxweb_api' class is defunct. \nUse 'pxweb' instead.\nSee help(\"Defunct\") ")
})

#' @rdname api_catalogue
#' @export
checkForLevels <- function(url) {
  .Defunct()
}

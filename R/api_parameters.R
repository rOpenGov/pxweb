#' Return options for database, version and language choices
#' 
#' ...
#' 
#' @param url Get parameters for the api with this url (NULL = get all API configs)
#' @export
#' @examples api_parameters()
api_parameters <- function(url=NULL) {
  api.file <- system.file("extdata/api.json", package = "pxweb")
  api.list <- RJSONIO::fromJSON(api.file)
  if(!is.null(url)) api.list <- api.list[str_split(url, "/")[[1]][3]][[1]]
  return(api.list)
}

 
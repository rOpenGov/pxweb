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

  # If API is specified, pick the parameters
  if(!is.null(url)) {
    if (!url %in% names(api.list)) {
      # http://api.scb.se/OV0104 into api.scb.se
      api.name <- str_split(url, "/")[[1]][3]
    } else {
      api.name <- url
    }
    api.list <- api.list[api.name]
  }
  class(api.list) <- "api_parameters"
  return(api.list)
}

#' Print method for api_parameters.
#'
#' @param x api_parameter object
#' @param ... currently not in use
#'
#' @export
print.api_parameters <- function(x, ...){
  api_names <- names(x)[names(x) != "foo.bar"]
  for (api in seq_along(api_names)){
    cat("[[", api ,"]]\napi:         ",api_names[api],"\n", sep="")
    cat("limit(s):    ",x[[api]]$calls_per_period ," calls per ",
        x[[api]]$period_in_seconds, " sec. \n             Max ",
        sprintf("%5.0f", x[[api]]$max_values_to_download), " values per call.\n", sep="")
    cat("version(s): ", paste(x[[api]]$version, collapse=", "), "\n")
    cat("language(s):", paste(x[[api]]$lang, collapse=", "), "\n") 
    cat("base url:\n", paste(x[[api]]$url, collapse=", "), "\n")
    cat("\n")
  }    
}

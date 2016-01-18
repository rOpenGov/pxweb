#' Return options for database, version and language choices
#' 
#' ...
#' 
#' @param url Get parameters for the api with this url (NULL = get all API configs)
#' @export
#'
#' @keywords internal 
#' 
#' @examples api_parameters()
api_parameters <- function(url = NULL) {

  api.list <- get_api_list()
  
  # If API is specified, pick the parameters
  if(!is.null(url)) {
    api_url_base <- unlist(lapply(api.list, function(X) strsplit(X$url, split = "/")[[1]][3]))
    url_base <- strsplit(url, split = "/")[[1]][3]
    if (!url_base %in% api_url_base) {
      stop(paste0(url_base, "api does not exist in api cataloge."))
    } else {
      api.list <- api.list[which(api_url_base %in% url_base)[1]]
    }
  }
  class(api.list) <- "api_parameters"
  return(api.list)
}

#' Print method for api_parameters.
#'
#' @param x api_parameter object
#' @param ... currently not in use
#' 
#' @keywords internal 
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

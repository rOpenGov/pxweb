#' Convert object to json
#' 
#' @param x an object to convert.
#' 
#' @keywords internal
pxweb_as_json <- function(x){
  UseMethod("pxweb_as_json")
}

#' @rdname as_json
#' @keywords internal
pxweb_as_json.pxweb_query <- function(x){
  for(i in seq_along(x$query)){
    x$query[[i]]$selection$values <- as.list(x$query[[i]]$selection$values)
  }
  jsonlite::toJSON(x, auto_unbox = TRUE)
}
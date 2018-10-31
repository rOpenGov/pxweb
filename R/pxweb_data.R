#' Construct a \code{pxweb_data} object.
#' 
#' @description 
#' An object that contain the data for a given PXWEB table.
#' 
#' @param x a list returned from a PXWEB API to convert to a \code{pxweb_data} object.
#' 
#' @return 
#' a \code{pxweb_data} object
#' 
#' @keywords internal
pxweb_data <- function(x){
  checkmate::assert_class(x, "list")
  class(x) <- c("pxweb_data", "list")
  assert_pxweb_data(x)
  x
}

#' Assert that x is a correct \code{pxweb_data} object.
#' @param x an object to check.
#' @keywords internal
assert_pxweb_data <- function(x){
  checkmate::assert_class(x, c("pxweb_data", "list"))
  checkmate::assert_names(names(x), must.include = c("columns", "comments", "data"))
  
  for(i in seq_along(x$columns)){
    checkmate::assert_names(names(x$columns[[i]]), must.include = c("code", "text", "type"))
    checkmate::assert_string(x$columns[[i]]$code)
    checkmate::assert_string(x$columns[[i]]$text)
    checkmate::assert_choice(x$columns[[i]]$type, choices = c("t", "c", "d"))
  }
  
  for(i in seq_along(x$comments)){
    checkmate::assert_names(names(x$comments), must.include = c("variable", "value", "comment"))
    checkmate::assert_string(x$columns[[i]]$variable)
    checkmate::assert_string(x$columns[[i]]$value)
    checkmate::assert_choice(x$columns[[i]]$comment)
  }
  
  for(i in seq_along(x$data)){
    checkmate::assert_names(names(x$data[[i]]), must.include = c("key", "values"))
    checkmate::assert_list(x$data[[i]]$key)
  }
}


#' @export
print.pxweb_data <- function(x, ...){
  cat("PXWEB DATA\n")
  cat("With", length(x$columns), "variables and", length(x$data), "observations.")
}

#' Construct a \code{pxweb_database_list} object.
#' 
#' @description 
#' An object that contain the list of databases for a given PXWEB api.
#' 
#' @param x a list returned from a PXWEB API to convert to a \code{pxweb_database_list} object.
#' 
#' @return 
#' a \code{pxweb_database_list} object
#' 
#' @keywords internal
pxweb_database_list <- function(x){
  checkmate::assert_class(x, "list")
  class(x) <- c("pxweb_database_list", "list")
  assert_pxweb_database_list(x)
  x
}

#' Assert that x is a correct \code{pxweb_database_list} object.
#' @param x an object to check.
#' @keywords internal
assert_pxweb_database_list <- function(x){
  checkmate::assert_class(x, c("pxweb_database_list", "list"))
  for(i in seq_along(x)){
    checkmate::assert_names(names(x[[i]]), must.include = c("dbid", "text"), .var.name = paste0("names(x[[", i, "]])"))
    checkmate::assert_string(x[[i]]$dbid, .var.name = paste0("x[[", i, "]]$dbid"))
    checkmate::assert_string(x[[i]]$text, .var.name = paste0("x[[", i, "]]$text"))    
  }    
}

#' Assert that x is a correct \code{pxweb_database_list} object.
#' @param x an object to check.
#' @keywords internal
as_pxweb_levels <- function(x){
  checkmate::assert_class(x, "pxweb_database_list")
  for(i in seq_along(x)){
    x[[i]]$id <- x[[i]]$dbid
    x[[i]]$type <- "l"
    x[[i]] <- x[[i]][c("id", "type", "text")]
  }
  class(x) <- c("pxweb_levels", "list")
  assert_pxweb_levels(x)
  x
}

#' @export
print.pxweb_database_list <- function(x, ...){
  cat("PXWEB DATABASE LIST\n")
  for(i in seq_along(x)){
    cat("  ", x[[i]]$dbid, ": ", x[[i]]$text, "\n")
  }
}
#' Assert that the url structure is correct
#' 
#' @description 
#' Assert that the \code{url} slot in the \code{pxweb} object is correct.
#' 
#' @param x a object to assert
#' @keywords internal
assert_pxweb <- function(x){
  checkmate::assert_class(x, c("pxweb", "list"))
  assert_pxweb_url(x)
  assert_pxweb_rda_file_path(x)
  assert_pxweb_calls(x)
  assert_pxweb_config(x)
}

#' Assert that the url structure is correct
#' 
#' @description 
#' Assert that the \code{url} slot in the \code{pxweb} object is correct.
#' 
#' @param x a object to assert
#' @keywords internal
assert_pxweb_url <- function(x){
  checkmate::assert_class(x, "list")
  checkmate::assert_names(names(x), must.include = "url")
  checkmate::assert_class(x$url, "url")
  checkmate::assert_false(is.null(x$url$hostname))
  checkmate::assert_false(is.null(x$url$scheme))
  checkmate::assert_false(is.null(x$url$path))
}


#' Assert that the rda_file_path is correct
#' 
#' @description 
#' Assert that the \code{rda_file_path} slot in the \code{pxweb} object is correct.
#' 
#' @param x a object to assert
#' @keywords internal
assert_pxweb_rda_file_path <- function(x){
  checkmate::assert_class(x, "list")
  checkmate::assert_names(names(x), must.include = "paths")
  checkmate::assert_false(is.null(x$paths$rda_file_path))
  checkmate::assert_path_for_output(x$paths$rda_file_path, overwrite = TRUE)
}


#' Assert that the rda_file_path is correct
#' 
#' @description 
#' Assert that the \code{rda_file_path} slot in the \code{pxweb} object is correct.
#' 
#' @param x a object to assert
#' @keywords internal
assert_pxweb_calls <- function(x){
  checkmate::assert_class(x, "list")
  checkmate::assert_names(names(x), must.include = "calls")
  checkmate::assert_false(is.null(x$calls$time_stamps))
  checkmate::assert_class(x$calls$time_stamps, "list")
  if(length(x$calls$time_stamps) > 0){
    for(i in seq_along(x$calls$time_stamps)){
      checkmate::assert_class(x$calls$time_stamps[[i]], "POSIXct")
    }
  }
}


#' Assert that the config slot is correct
#' 
#' @description 
#' Assert that the \code{config} slot in the \code{pxweb} object is correct.
#' 
#' @param x a object to assert
#' @keywords internal
assert_pxweb_config <- function(x){
  checkmate::assert_class(x, "list")
  checkmate::assert_names(names(x), must.include = "config")
  checkmate::assert_names(names(x$config), must.include = c("calls_per_period", "period_in_seconds", "max_values_to_download"))
  checkmate::assert_int(x$config$calls_per_period, lower = 1, null.ok = FALSE, na.ok = FALSE)
  checkmate::assert_int(x$config$period_in_seconds, lower = 1, null.ok = FALSE, na.ok = FALSE)  
  checkmate::assert_int(x$config$max_values_to_download, lower = 1, null.ok = FALSE, na.ok = FALSE)    
}
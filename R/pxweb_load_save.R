#' Save and load \code{pxweb} objects from R temp folder
#'  
#' @param obj a \code{pxweb} object
#' @keywords internal
save_pxweb <- function(obj){
  checkmate::assert_class(obj, "pxweb")
  save(obj, file = obj$paths$rda_file_path, compress = FALSE)
}

#' @keywords internal
#' @rdname save_pxweb
load_pxweb <- function(obj){
  assert_pxweb_rda_file_path(obj)
  load_path <- obj$paths$rda_file_path
  rm(obj)
  load(load_path)
  obj
}

#' @keywords internal
#' @rdname save_pxweb
load_pxweb_calls <- function(obj){
  load_pxweb(obj)$calls
}

#' @keywords internal
#' @rdname save_pxweb
load_pxweb_config <- function(obj){
  load_pxweb(obj)$config
}

#' @keywords internal
#' @rdname save_pxweb
load_pxweb_api_subpath <- function(obj){
  load_pxweb(obj)$paths$api_subpath
}

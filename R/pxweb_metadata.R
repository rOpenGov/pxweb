#' Construct a \code{pxweb_metadata} object.
#' 
#' @description 
#' An object that contain the metadata for a given PXWEB table.
#' 
#' @param x a list returned from a PXWEB API to convert to a \code{pxweb_metadata} object.
#' 
#' @return 
#' a \code{pxweb_metadata} object
#' 
#' @keywords internal
pxweb_metadata <- function(x){
  checkmate::assert_names(names(x), must.include = "variables")
  for(i in seq_along(x$variables)){
    checkmate::assert_names(names(x$variables[[i]]), must.include = c("values", "valueTexts"))
    x$variables[[i]]$values <- unlist(x$variables[[i]]$values)
    x$variables[[i]]$valueTexts <- unlist(x$variables[[i]]$valueTexts)
    if(is.null(x$variables[[i]]$elimination)) x$variables[[i]]$elimination <- FALSE
    if(is.null(x$variables[[i]]$time)) x$variables[[i]]$time <- FALSE
  }
  class(x) <- c("pxweb_metadata", "list")
  assert_pxweb_metadata(x)
  x
}



#' Assert that x is a correct \code{pxweb_metadata} object.
#' @param x an object to check.
#' @keywords internal
assert_pxweb_metadata <- function(x){
  checkmate::assert_class(x, c("pxweb_metadata", "list"))
  checkmate::assert_names(names(x), must.include = c("title", "variables"))
  checkmate::assert_string(x$title)
  
  for(i in seq_along(x$variables)){
    checkmate::assert_names(names(x$variables[[i]]), must.include = c("code", "text", "values", "valueTexts", "elimination", "time"), .var.name = paste0("names(x$variables[[", i, "]])"))
    checkmate::assert_string(x$variables[[i]]$code, .var.name = paste0("x$variables[[", i, "]]$code"))
    checkmate::assert_string(x$variables[[i]]$text, .var.name = paste0("x$variables[[", i, "]]$text"))
    checkmate::assert_character(x$variables[[i]]$values, .var.name = paste0("x$variables[[", i, "]]$values"))
    checkmate::assert_character(x$variables[[i]]$valueTexts, len = length(unlist(x$variables[[i]]$values)) , .var.name = paste0("x$variables[[", i, "]]$valueTexts"))
    checkmate::assert_flag(x$variables[[i]]$time, .var.name = paste0("x$variables[[", i, "]]$time"))
    checkmate::assert_flag(x$variables[[i]]$elimination, .var.name = paste0("x$variables[[", i, "]]$elimination"))
  }
}


#' @export
print.pxweb_metadata <- function(x, ...){
  cat("PXWEB METADATA\n")
  cat("variables:\n")
  for(i in seq_along(x$variables)){
    cat(" [[", i ,"]] ",  x$variables[[i]]$code,": ", x$variables[[i]]$text, "\n", sep = "")
  }
}

#' Get boolean vector
#' 
#' @param pxmd a \code{pxweb_metadata} object.
#' 
#' @return pxweb_metadata eliminations as a named boolean vector.
#' 
#' @keywords internal
pxweb_metadata_elimination <- function(pxmd){
  checkmate::assert_class(pxmd, "pxweb_metadata")
  res <- unlist(lapply(pxmd$variables,function(x) x$elimination))  
  names(res) <- unlist(lapply(pxmd$variables,function(x) x$code))  
  res
}


#' Compue the dimension of a metadata object
#' 
#' @param pxmd a \code{pxweb_metadata} object.
#' 
#' @keywords internal
pxweb_metadata_dim <- function(pxmd){
  checkmate::assert_class(pxmd, "pxweb_metadata")
  dim_res <- numeric(length(pxmd$variables))
  for(i in seq_along(pxmd$variables)){
    names(dim_res)[i] <- pxmd$variables[[i]]$code
    dim_res[i] <- length(pxmd$variables[[i]]$values)
  }
  dim_res
}
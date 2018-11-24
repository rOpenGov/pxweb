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
    checkmate::assert_names(names(x$comments[[i]]), must.include = c("variable", "value", "comment"))
    checkmate::assert_string(x$comments[[i]]$variable)
    checkmate::assert_string(x$comments[[i]]$value)
    checkmate::assert_string(x$comments[[i]]$comment)
  }
  
  for(i in seq_along(x$data)){
    checkmate::assert_names(names(x$data[[i]]), must.include = c("key", "values"))
    checkmate::assert_list(x$data[[i]]$key)
  }
}


#' @export
print.pxweb_data <- function(x, ...){
  cat("PXWEB DATA\n")
  pxdim <- pxweb_data_dim(x)
  cat("With", pxdim[2], "variables and", pxdim[1], "observations.")
}



#' Compute the dimension of the query \code{pxweb_data} object
#' 
#' @param pxd a \code{pxweb_data} object.
#' 
#' @keywords internal
pxweb_data_dim <- function(pxd){
  checkmate::assert_class(pxd, "pxweb_data")
  c(length(pxd$data), length(pxd$columns))
}


#' Get query filter
#' 
#' @param pxd a \code{pxweb_data} object.
#' 
#' @return character vector with column names.
#' 
#' @keywords internal
pxweb_data_colnames <- function(pxd, type = "text"){
  checkmate::assert_class(pxd, "pxweb_data")
  checkmate::assert_choice(type, choices = c("text", "code"))
  unlist(lapply(pxd$columns, function(x) x[[type]]))
}




#' Convert a pxweb data objects values to valuetext
#' 
#' @param x a \code{pxweb_data} object
#' @param variable a variable to convert
#' @param value_levels value_levels to convert
#' @keywords internal
pxd_values_to_valuetexts <- function(x, variable, value_levels){
  checkmate::assert_class(x, classes = "pxweb_data")
  pxmd_dim <- pxweb_metadata_dim(x$pxweb_metadata)
  checkmate::assert_choice(variable, choices = names(pxmd_dim))
  var_id <- which(names(pxmd_dim) %in% variable)
  checkmate::assert_subset(value_levels, choices = x$pxweb_metadata$variables[[var_id]]$values)
  new_value_levels <- value_levels
  for(i in seq_along(value_levels)){
    idx <- which(x$pxweb_metadata$variables[[var_id]]$values %in% value_levels[i])
    new_value_levels[i] <- x$pxweb_metadata$variables[[var_id]]$valueTexts[idx]
  }
  new_value_levels
}

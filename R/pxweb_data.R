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
  no_comments <- length(pxweb_data_comments(x)$pxweb_data_comments)
  if(no_comments > 0){
    cat("With", pxdim[2], "variables,", pxdim[1], "observations and", no_comments, "comments/footnotes.")
  } else {
    cat("With", pxdim[2], "variables and", pxdim[1], "observations.")
  }
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
#' @param variable_code the variable name of the variable to convert. NOTE! Need to be the variable code.
#' @param variable_vector the vector with the variables to convert.
#' @keywords internal
pxd_values_to_valuetexts <- function(x, variable_code, variable_vector){
  checkmate::assert_class(x, classes = "pxweb_data")
  pxmd_dim <- pxweb_metadata_dim(x$pxweb_metadata)
  checkmate::assert_choice(variable_code, choices = names(pxmd_dim))
  var_idx <- which(names(pxmd_dim) %in% variable_code)
  var_labels <- x$pxweb_metadata$variables[[var_idx]]$values
  checkmate::assert_subset(variable_vector, choices = var_labels)
  checkmate::assert_true(is.character(variable_vector) | is.factor(variable_vector))  
  
  input_is_factor <- is.factor(variable_vector)
  
  if(!input_is_factor){
    variable_vector <- as.factor(variable_vector)
  }
  new_variable_levels <- variable_levels <- levels(variable_vector)
  
  for(i in seq_along(variable_levels)){
    idx <- which(x$pxweb_metadata$variables[[var_idx]]$values %in% variable_levels[i])
    new_variable_levels[i] <- x$pxweb_metadata$variables[[var_idx]]$valueTexts[idx]
  }
  levels(variable_vector) <- new_variable_levels
  if(!input_is_factor){
    variable_vector <- as.character(variable_vector)
  }
  variable_vector
}

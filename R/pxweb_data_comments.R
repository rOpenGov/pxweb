#' Construct a \code{pxweb_data_comments} object.
#' 
#' @description 
#' An object that contain the comments for a given PXWEB table.
#' 
#' @param x a \code{pxweb_data} object.
#' 
#' @return 
#' a \code{pxweb_data_comments} object
#' 
#' @export
pxweb_data_comments <- function(x){
  UseMethod("pxweb_data_comments")
}

#' @rdname pxweb_data_comments
#' @export
pxweb_data_comments.pxweb_data <- function(x){
  checkmate::assert_class(x, "pxweb_data")
  obj <- list()
  
  ctxt <- pxweb_data_colnames(x, type = "text")
  ccode <- pxweb_data_colnames(x, type = "code")
  
  for(i in seq_along(x$columns)){
    if(!is.null(x$columns[[i]]$comment)){
      obj[[length(obj) + 1]] <- pxweb_data_column_comment(x$columns[[i]])
    }
  }
  
  for(i in seq_along(x$comments)){
    obj[[length(obj) + 1]] <- pxweb_data_value_comment(x$comments[[i]], ccode, ctxt)
  }
  
  has_comment <- unlist(lapply(x$data, function(x) !is.null(x$comment)))
  comment_idx <- which(has_comment)
  for(i in seq_along(comment_idx)){
    obj[[length(obj) + 1]] <- pxweb_data_obs_comment(x$data[[comment_idx[i]]], ccode, ctxt)
  }

  class(obj) <- c("pxweb_data_comments", "list")
  assert_pxweb_data_comments(obj)
  obj
}


#' Construct a \code{pxweb_data_comment} object
#' @param x an object to convert to a \code{pxweb_data_comment} object
#' @keywords internal
pxweb_data_column_comment <- function(x){
  checkmate::assert_names(names(x), permutation.of = c("code", "text", "type", "comment"))
  obj <- x[c("code", "text")]
  obj$value <- NULL 
  obj$comment <- x$comment
  class(obj) <- c("column_comment", "pxweb_data_comment", "list")
  obj
}

#' @rdname pxweb_data_column_comment
#' @keywords internal
pxweb_data_value_comment <- function(x, column_code, column_text){
  checkmate::assert_names(names(x), permutation.of = c("variable", "value", "comment"))
  checkmate::assert_character(column_code)
  checkmate::assert_subset(x$variable, choices = column_code)
  checkmate::assert_character(column_text)  
  obj <- list(code = x$variable, 
              text = column_text[which(x$variable %in% column_code)], 
              value = x$value,
              comment = x$comment)
  class(obj) <- c("value_comment", "pxweb_data_comment", "list")
  obj
}

#' @rdname pxweb_data_column_comment
#' @keywords internal
pxweb_data_obs_comment <- function(x, column_code, column_text){
  checkmate::assert_names(names(x), permutation.of = c("key", "values", "comment"))
  checkmate::assert_character(column_code)
  checkmate::assert_character(column_text)  
  
  obj <- list(code = column_code, 
              text = column_text, 
              value = unname(unlist(x[c("key", "values")])),
              comment = x$comment)
  class(obj) <- c("obs_comment", "pxweb_data_comment", "list")
  obj
}

#' Assert that x is a correct \code{pxweb_data_comments} object.
#' @param x an object to check.
#' @keywords internal
assert_pxweb_data_comments <- function(x){
  checkmate::assert_class(x, c("pxweb_data_comments", "list"))
  
  for(i in seq_along(x)){
    checkmate::assert_class(x[[i]], "pxweb_data_comment")
    checkmate::assert_choice(class(x[[i]])[1], choices = c("obs_comment", 
                                                      "value_comment",
                                                      "column_comment"))
  }
}

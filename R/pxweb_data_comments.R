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
#' @keywords internal
#' @export
pxweb_data_comments.pxweb_data <- function(x){
  checkmate::assert_class(x, "pxweb_data")
  obj <- list()
  
  for(i in seq_along(x$columns)){
    if(!is.null(x$columns[[i]]$comment)){
      obj[[length(obj) + 1]] <- pxweb_data_column_comment(x, i)
    }
  }
  
  for(i in seq_along(x$comments)){
    obj[[length(obj) + 1]] <- pxweb_data_value_comment(x, i)
  }
  
  has_comment <- unlist(lapply(x$data, function(x) !is.null(x$comment)))
  comment_idx <- which(has_comment)
  for(i in seq_along(comment_idx)){
    obj[[length(obj) + 1]] <- pxweb_data_obs_comment(x, comment_idx[i])
  }

  obj <- list(pxweb_data_comments = obj,
              data_dim = pxweb_data_dim(x))
  class(obj) <- c("pxweb_data_comments", "list")
  assert_pxweb_data_comments(x = obj)
  obj
}


#' Construct a \code{pxweb_data_comment} object
#' @param x an \code{pxweb_data} to extract a \code{pxweb_data_comment} object from.
#' @param column_idx the index of the column to extract.
#' @param comment_idx the index of the comment to extract.
#' @param obs_idx the index of the comment to extract.
#' @keywords internal
pxweb_data_column_comment <- function(x, column_idx){
  checkmate::assert_class(x, "pxweb_data")
  checkmate::assert_int(column_idx, lower = 1, upper = length(x$columns))
  obj <- x$columns[[column_idx]][c("code", "text")]
  obj$value <- NULL 
  obj$comment <- x$columns[[column_idx]]$comment
  obj$idx_data_frame <- data.frame(row_no = NA, col_no = column_idx) 
  class(obj) <- c("column_comment", "pxweb_data_comment", "list")
  obj
}

#' @rdname pxweb_data_column_comment
#' @keywords internal
pxweb_data_value_comment <- function(x, comment_idx){
  checkmate::assert_class(x, "pxweb_data")
  checkmate::assert_int(comment_idx, lower = 1, upper = length(x$comments))

  column_text <- pxweb_data_colnames(x, type = "text")
  column_code <- pxweb_data_colnames(x, type = "code")
  comment_column_idx <- which(x$comments[[comment_idx]]$variable %in% column_code)
  
  obj <- list(code = x$comments[[comment_idx]]$variable, 
              text = column_text[comment_column_idx], 
              value = x$comments[[comment_idx]]$value,
              comment = x$comments[[comment_idx]]$comment)
  
  slot_idx <- c(rep(1, length(x$data[[1]]$key)), rep(2, length(x$data[[1]]$values)))
  slot_pos <- c(1:length(x$data[[1]]$key), 1:length(x$data[[1]]$values))
  has_value <- unlist(lapply(x$data, 
                             function(x, slot_idx, slot_pos, comment_column_idx, obj) x[[slot_idx[comment_column_idx]]][[slot_pos[comment_column_idx]]] == obj$value,
                             slot_idx = slot_idx, slot_pos = slot_pos, comment_column_idx = comment_column_idx, obj = obj))
  obj$idx_data_frame <- data.frame(row_no = which(has_value), col_no = rep(comment_idx, sum(has_value)))
  class(obj) <- c("value_comment", "pxweb_data_comment", "list")
  obj
}

#' @rdname pxweb_data_column_comment
#' @keywords internal
pxweb_data_obs_comment <- function(x, obs_idx){
  checkmate::assert_class(x, "pxweb_data")
  checkmate::assert_int(obs_idx, lower = 1, upper = length(x$data))

  column_text <- pxweb_data_colnames(x, type = "text")
  column_code <- pxweb_data_colnames(x, type = "code")
  
  obj <- list(code = column_code, 
              text = column_text, 
              value = unname(unlist(x$data[[obs_idx]][c("key", "values")])),
              comment = x$data[[obs_idx]]$comment)
  obj$idx_data_frame <- data.frame(row_no = obs_idx, col_no = NA)
  class(obj) <- c("obs_comment", "pxweb_data_comment", "list")
  obj
}

#' Assert that x is a correct \code{pxweb_data_comments} object.
#' @param x an object to check.
#' @keywords internal
assert_pxweb_data_comments <- function(x){
  checkmate::assert_class(x, c("pxweb_data_comments", "list"))
  checkmate::assert_names(names(x), permutation.of = c("pxweb_data_comments", "data_dim"))
  
  for(i in seq_along(x$comments)){
    checkmate::assert_class(x$comments[[i]], "pxweb_data_comment")
    checkmate::assert_choice(class(x$comments[[i]])[1], choices = c("obs_comment", 
                                                           "value_comment",
                                                           "column_comment"))
  }
  checkmate::assert_integerish(x$data_dim, lower = 1)
}


#' @export
print.pxweb_data_comment <- function(x, ...){
  cat(class(x)[[1]], " (", paste(x$text, collapse = ", "), " [", paste(x$code, collapse = ", "), "], ", paste(x$value, collapse = ", "), "):\n  ", x$comment, "\n", sep = "")
}


#' @export
#' @keywords internal
print.pxweb_data_comments <- function(x, ...){
  if(length(x$pxweb_data_comments) <= 0){
    cat("NO PXWEB DATA COMMENTS\n")
  } else {
    cat("PXWEB DATA COMMENTS\n")
    for(i in seq_along(x$pxweb_data_comments)){
      cat("$pxweb_data_comments[[", i, "]]\n", sep = "")
      print(x$pxweb_data_comments[[i]])
    }
  }
}
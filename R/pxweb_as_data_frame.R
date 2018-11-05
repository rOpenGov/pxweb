#' Coerce to a \code{data.frame}
#' 
#' @param x an object to convert to \code{data.frame}.
#' @param row.names See \code{\link[base]{as.data.frame}}.
#' @param optional See \code{\link[base]{as.data.frame}}.
#' @param ... See \code{\link[base]{as.data.frame}}.
#' @param stringsAsFactors See \code{\link[base]{as.data.frame}}.
#' @param column.name.source character: should \code{code} or \code{text} be used as column names?
#' 
#' @seealso \code{\link[base]{as.data.frame}}.
#' 
#' @keywords internal
pxweb_as_data_frame <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = default.stringsAsFactors(), column.name.source = "text"){
  checkmate::assert_choice(column.name.source, c("code", "text"))
  UseMethod("pxweb_as_data_frame")
}

#' @rdname pxweb_as_data_frame
#' @keywords internal
pxweb_as_data_frame.pxweb_data <-  function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = default.stringsAsFactors(), column.name.source = "text"){
  pxdims <- pxweb_data_dim(x)
  checkmate::assert_character(row.names, len = pxdims[1], null.ok = TRUE)
  checkmate::assert_choice(column.name.source, c("code", "text"))
  checkmate::assert_flag(optional)
  checkmate::assert_flag(stringsAsFactors)
  
  # Fill out
  df <- matrix("", ncol = pxdims[2], nrow = pxdims[1])
  slot_idx <- c(rep(1, length(x$data[[1]]$key)), rep(2, length(x$data[[1]]$values)))
  slot_pos <- c(1:length(x$data[[1]]$key), 1:length(x$data[[1]]$values))
  for(i in 1:pxdims[1]){
    for(j in 1:pxdims[2]){
      df[i, j] <- x$data[[i]][[slot_idx[j]]][[slot_pos[j]]]
    }
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = optional, ...)
  colnames(df) <- pxweb_data_colnames(x, column.name.source)
  for(j in 1:pxdims[2]){
    if(slot_idx[j] == 1){
      if(stringsAsFactors){
        df[, j] <- as.factor(df[, j])
      }
    } else {
      df[, j] <- as.numeric(df[, j])
    }
  }
  if(!is.null(row.names)) {
    rownames(df) <- row.names
  }
  df
}


#' @rdname pxweb_as_data_frame
#' @keywords internal
pxweb_as_data_frame.pxweb_data_comments <-  function(x, optional = FALSE, ..., stringsAsFactors = default.stringsAsFactors()){
  checkmate::assert_flag(optional)
  checkmate::assert_flag(stringsAsFactors)
  
  dfs <- list()
  for(i in seq_along(x$comments)){
    dfs[[i]] <- pxweb_as_data_frame(x$comments[[i]], optional = optional, ..., stringsAsFactors = FALSE)
  }
  df <- do.call(rbind, dfs)
  if(stringsAsFactors){
    df$comment_type <- as.factor(df$comment_type)
    df$comment <- as.factor(df$comment)    
  }
  df
}

#' @rdname pxweb_as_data_frame
#' @keywords internal
pxweb_as_data_frame.pxweb_data_comment <- function(x, optional = FALSE, ..., stringsAsFactors = default.stringsAsFactors()){
  checkmate::assert_flag(optional)
  checkmate::assert_flag(stringsAsFactors)
  
  df <- x$idx_data_frame
  if(stringsAsFactors){
    df$comment_type <- as.factor(class(x)[1])
    df$comment <- as.factor(x$comment)
  } else {
    df$comment_type <- class(x)[1]
    df$comment <- x$comment
  }
  df
}



#' @rdname pxweb_as_data_frame
#' @export 
as.data.frame.pxweb_data <- function(x, 
                                     row.names = NULL, 
                                     optional = FALSE, 
                                     ..., 
                                     stringsAsFactors = default.stringsAsFactors(), 
                                     column.name.source = "text"){
  
  pxweb_as_data_frame(x, 
                      row.names = row.names, 
                      optional = optional,
                      ...,
                      stringsAsFactors = stringsAsFactors,
                      column.name.source = column.name.source)
}
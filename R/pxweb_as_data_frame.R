#' Convert object to a \code{data.frame}
#' 
#' @param x an object to convert to \code{data.frame}.
#' @param row.names See \link{\code{as.data.frame}}.
#' @param optional See \link{\code{as.data.frame}}.
#' @param ... See \link{\code{as.data.frame}}.
#' @param stringsAsFactors See \link{\code{as.data.frame}}.
#' @param column.name.source character: should \code{code} or \code{text} be used as column names?
#' 
#' @seealso \link{\code{as.data.frame}}.
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
pxweb_as_data_frame.pxweb_data_comment <-  function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = FALSE){

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
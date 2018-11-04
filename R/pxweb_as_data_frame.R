#' Convert object to a \code{data.frame}
#' 
#' @param x an object to convert.
#' @param column_names should \code{code} or \code{text} be used as column names?
#' 
#' @keywords internal
pxweb_as_data_frame <- function(x, column_names = "text"){
  checkmate::assert_choice(column_names, c("code", "text"))
  UseMethod("pxweb_as_data_frame")
}

#' @rdname pxweb_as_data_frame
#' @keywords internal
pxweb_as_data_frame.pxweb_data <-  function(x, column_names = "text"){
  pxdims <- pxweb_data_dim(x)
  # Fill out
  df <- matrix("", ncol = pxdims[2], nrow = pxdims[1])
  slot_idx <- c(rep(1, length(x$data[[1]]$key)), rep(2, length(x$data[[1]]$values)))
  slot_pos <- c(1:length(x$data[[1]]$key), 1:length(x$data[[1]]$values))
  for(i in 1:pxdims[1]){
    for(j in 1:pxdims[2]){
      df[i, j] <- x$data[[i]][[slot_idx[j]]][[slot_pos[j]]]
    }
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  colnames(df) <- pxweb_data_colnames(x, column_names)
  for(j in 1:pxdims[2]){
    if(slot_idx[j] == 1){
      df[, j] <- as.factor(df[, j])
    } else {
      df[, j] <- as.numeric(df[, j])
    }
  }
  df
}
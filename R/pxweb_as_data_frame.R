#' Coerce a \code{pxweb_data} object to a \code{data.frame}
#'
#' @param x an object to convert to \code{data.frame}.
#' @param row.names See \code{\link[base]{as.data.frame}}.
#' @param optional See \code{\link[base]{as.data.frame}}.
#' @param ... See \code{\link[base]{as.data.frame}}.
#' @param stringsAsFactors See \code{\link[base]{as.data.frame}}.
#' @param column.name.type character: should \code{code} or \code{text} be used as column names?
#' @param variable.value.type character: should \code{code} or \code{text} be used as values in columns?
#'
#' @seealso \code{\link[base]{as.data.frame}}.
#'
#' @export
pxweb_as_data_frame <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = FALSE, column.name.type = "text", variable.value.type = "text") {
  checkmate::assert_choice(column.name.type, c("code", "text"))
  checkmate::assert_choice(column.name.type, c("code", "text"))
  UseMethod("pxweb_as_data_frame")
}

#' @rdname pxweb_as_data_frame
#' @export
pxweb_as_data_frame.pxweb_data <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = FALSE, column.name.type = "text", variable.value.type = "text") {
  pxdims <- pxweb_data_dim(x)
  checkmate::assert_character(row.names, len = pxdims[1], null.ok = TRUE)
  checkmate::assert_choice(column.name.type, c("code", "text"))
  checkmate::assert_choice(variable.value.type, c("code", "text"))
  checkmate::assert_flag(optional)
  checkmate::assert_flag(stringsAsFactors)

  # Fill out
  df <- pxweb_as_matrix(x, row.names = row.names, column.name.type = column.name.type, variable.value.type = variable.value.type)
  slot <- pxweb_pxd_slot_idx_pos(x)
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = optional, ...)
  dat_code_cn <- pxweb_data_colnames(x, "code")
  for (j in 1:pxdims[2]) {
    if (slot$idx[j] == 1) {
      if (stringsAsFactors) {
        df[, j] <- as.factor(df[, j])
      }
    } else {
      df[, j] <- as.numeric(df[, j])
    }
  }
  df
}

#' @rdname pxweb_as_data_frame
#' @export
pxweb_as_data_frame.pxweb_data_comments <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = FALSE) {
  checkmate::assert_flag(optional)
  checkmate::assert_flag(stringsAsFactors)

  if (length(x$pxweb_data_comments) == 0) {
    df <- data.frame(row_no = integer(0), col_no = integer(0), comment_type = character(0), comment = character(0), stringsAsFactors = FALSE)
    if (stringsAsFactors) {
      df$comment_type <- as.factor(df$comment_type)
      df$comment <- as.factor(df$comment)
    }
    return(df)
  }

  dfs <- list()
  for (i in seq_along(x$pxweb_data_comments)) {
    dfs[[i]] <- pxweb_as_data_frame(x$pxweb_data_comments[[i]], optional = optional, ..., stringsAsFactors = FALSE)
  }
  df <- do.call(rbind, dfs)
  if (stringsAsFactors) {
    df$comment_type <- as.factor(df$comment_type)
    df$comment <- as.factor(df$comment)
  }
  df
}

#' @rdname pxweb_as_data_frame
#' @export
pxweb_as_data_frame.pxweb_data_comment <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = FALSE) {
  checkmate::assert_flag(optional)
  checkmate::assert_flag(stringsAsFactors)

  df <- x$idx_data_frame
  df$row_no <- as.integer(df$row_no)
  df$col_no <- as.integer(df$col_no)
  if (stringsAsFactors) {
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
pxweb_as_data_frame.pxweb_levels <- function(x, row.names = NULL, optional = FALSE, ..., stringsAsFactors = FALSE) {
  checkmate::assert_flag(optional)
  checkmate::assert_flag(stringsAsFactors)

  df <- list()
  for (i in seq_along(x)) {
    df[[i]] <- as.data.frame(x[[i]], optional = optional, stringsAsFactors = FALSE)
    if (is.null(df[[i]]$updated)) df[[i]]$updated <- NA
  }
  df <- do.call(rbind, df)

  if (stringsAsFactors) {
    df$id <- as.factor(df$id)
    df$type <- as.factor(df$type)
    df$text <- as.factor(df$text)
  }
  df
}



#' @rdname pxweb_as_data_frame
#' @export
as.data.frame.pxweb_data <- function(x,
                                     row.names = NULL,
                                     optional = FALSE,
                                     ...,
                                     stringsAsFactors = FALSE,
                                     column.name.type = "text",
                                     variable.value.type = "text") {
  pxweb_as_data_frame(x,
    row.names = row.names,
    optional = optional,
    ...,
    stringsAsFactors = stringsAsFactors,
    column.name.type = column.name.type,
    variable.value.type = variable.value.type
  )
}

#' @rdname pxweb_as_data_frame
#' @export
as.data.frame.pxweb_data_comments <- function(x,
                                              row.names = NULL,
                                              optional = FALSE,
                                              ...,
                                              stringsAsFactors = FALSE) {
  pxweb_as_data_frame(x,
    row.names = row.names,
    optional = optional,
    ...,
    stringsAsFactors = stringsAsFactors
  )
}


#' @rdname pxweb_as_data_frame
#' @export
as.data.frame.pxweb_levels <- function(x,
                                       row.names = NULL,
                                       optional = FALSE,
                                       ...,
                                       stringsAsFactors = FALSE) {
  pxweb_as_data_frame(x,
    row.names = row.names,
    optional = optional,
    ...,
    stringsAsFactors = stringsAsFactors
  )
}

#' @rdname pxweb_as_data_frame
#' @export
as.data.frame.pxweb_metadata <- function(x,
                                         row.names = NULL,
                                         optional = FALSE,
                                         ...,
                                         stringsAsFactors = FALSE) {
  stop("A pxweb_metadata object cannot be turned into a data.frame.", call. = FALSE)
}


#' @rdname pxweb_as_data_frame
#' @keywords internal
pxweb_as_matrix <- function(x, row.names = NULL, column.name.type = "text", variable.value.type = "text") {
  checkmate::assert_choice(column.name.type, c("code", "text"))
  checkmate::assert_choice(column.name.type, c("code", "text"))
  UseMethod("pxweb_as_matrix")
}

#' @rdname pxweb_as_data_frame
#' @keywords internal
pxweb_as_matrix.pxweb_data <- function(x, row.names = NULL, column.name.type = "text", variable.value.type = "text") {
  pxdims <- pxweb_data_dim(x)
  checkmate::assert_character(row.names, len = pxdims[1], null.ok = TRUE)
  checkmate::assert_choice(column.name.type, c("code", "text"))
  checkmate::assert_choice(variable.value.type, c("code", "text"))
  # Fill out
  mat <- matrix("", ncol = pxdims[2], nrow = pxdims[1])
  slot <- pxweb_pxd_slot_idx_pos(x)
  for (i in 1:pxdims[1]) {
    for (j in 1:pxdims[2]) {
      mat[i, j] <- x$data[[i]][[slot$idx[j]]][[slot$pos[j]]]
    }
  }
  if (variable.value.type == "text") {
    col_codes <- pxweb_data_colnames(x, "code")
  }
  colnames(mat) <- pxweb_data_colnames(x, column.name.type)
  if (!is.null(row.names)) {
    rownames(mat) <- row.names
  }

  for (j in 1:pxdims[2]) {
    if (slot$idx[j] == 1) {
      if (variable.value.type == "text") {
        mat[, j] <- pxd_values_to_valuetexts(x, variable_code = col_codes[j], variable_vector = mat[, j])
      }
    }
  }

  mat
}


#' @rdname pxweb_as_data_frame
#' @export
as.matrix.pxweb_data <- function(x, ..., row.names = NULL, column.name.type = "text", variable.value.type = "text") {
  pxweb_as_matrix(x,
    row.names = row.names,
    column.name.type = column.name.type,
    variable.value.type = variable.value.type
  )
}


#' @rdname pxweb_as_data_frame
#' @keywords internal
pxweb_pxd_slot_idx_pos <- function(x) {
  checkmate::assert_class(x, "pxweb_data")
  slot_idx <- c(rep(1, length(x$data[[1]]$key)), rep(2, length(x$data[[1]]$values)))
  slot_pos <- c(1:length(x$data[[1]]$key), 1:length(x$data[[1]]$values))
  list(idx = slot_idx, pos = slot_pos)
}

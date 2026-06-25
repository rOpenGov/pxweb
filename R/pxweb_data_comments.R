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
#' @examples
#' \dontrun{
#' url <- "https://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/BE0101A/BefolkningNy"
#' json_query <-
#'   file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
#' pxd <- pxweb_get(url = url, query = json_query)
#' pxdcs <- pxweb_data_comments(x = pxd)
#' pxdc_df <- as.data.frame(pxdcs, stringsAsFactors = TRUE)
#' }
#' @export
pxweb_data_comments <- function(x) {
  UseMethod("pxweb_data_comments")
}

#' @rdname pxweb_data_comments
#' @keywords internal
#' @export
pxweb_data_comments.pxweb_data <- function(x) {
  checkmate::assert_class(x, "pxweb_data")
  obj <- list()

  for (i in seq_along(x$columns)) {
    if (!is.null(x$columns[[i]]$comment)) {
      obj[[length(obj) + 1]] <- pxweb_data_column_comment(x, i)
    }
  }

  for (i in seq_along(x$comments)) {
    obj[[length(obj) + 1]] <- pxweb_data_value_comment(x, i)
  }

  has_comment <- unlist(lapply(x$data, function(x) !is.null(x$comment)))
  comment_idx <- which(has_comment)
  for (i in seq_along(comment_idx)) {
    obj[[length(obj) + 1]] <- pxweb_data_obs_comment(x, comment_idx[i])
  }

  obj <- list(
    pxweb_data_comments = obj,
    data_dim = pxweb_data_dim(x)
  )
  class(obj) <- c("pxweb_data_comments", "list")
  assert_pxweb_data_comments(x = obj)
  obj
}

#' @rdname pxweb_data_comments
#' @keywords internal
#' @export
pxweb_data_comments.pxweb_data_v2 <- function(x) {
  checkmate::assert_class(x, "pxweb_data_v2")

  obj <- list()
  dataset_note <- pxweb_data_v2_note_text(x$note)
  if (!is.null(dataset_note)) {
    obj[[length(obj) + 1L]] <- pxweb_data_v2_dataset_note_comment(x, dataset_note)
  }

  variable_ids <- unlist(x$id, use.names = FALSE)
  for (i in seq_along(variable_ids)) {
    variable_id <- variable_ids[[i]]
    dim_note <- pxweb_data_v2_note_text(x$dimension[[variable_id]]$note)
    if (!is.null(dim_note)) {
      obj[[length(obj) + 1L]] <- pxweb_data_v2_column_comment(x, i, dim_note)
    }

    category_notes <- x$dimension[[variable_id]]$category$note
    if (!is.null(category_notes)) {
      category_notes <- as.list(category_notes)
      for (value_code in names(category_notes)) {
        value_note <- pxweb_data_v2_note_text(category_notes[[value_code]])
        if (!is.null(value_note)) {
          obj[[length(obj) + 1L]] <- pxweb_data_v2_value_comment(x, i, value_code, value_note)
        }
      }
    }
  }

  status <- pxweb_data_v2_status_comments(x)
  if (length(status) > 0) {
    obj <- c(obj, status)
  }

  obj <- list(
    pxweb_data_comments = obj,
    data_dim = pxweb_data_dim(x)
  )
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
pxweb_data_column_comment <- function(x, column_idx) {
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
pxweb_data_value_comment <- function(x, comment_idx) {
  checkmate::assert_class(x, "pxweb_data")
  checkmate::assert_int(comment_idx, lower = 1, upper = length(x$comments))

  column_text <- pxweb_data_colnames(x, type = "text")
  column_code <- pxweb_data_colnames(x, type = "code")
  comment_column_idx <- which(x$comments[[comment_idx]]$variable %in% column_code)

  obj <- list(
    code = x$comments[[comment_idx]]$variable,
    text = column_text[comment_column_idx],
    value = x$comments[[comment_idx]]$value,
    comment = x$comments[[comment_idx]]$comment
  )

  slot_idx <- c(rep(1, length(x$data[[1]]$key)), rep(2, length(x$data[[1]]$values)))
  slot_pos <- c(1:length(x$data[[1]]$key), 1:length(x$data[[1]]$values))
  has_value <- unlist(lapply(x$data,
    function(x, slot_idx, slot_pos, comment_column_idx, obj) x[[slot_idx[comment_column_idx]]][[slot_pos[comment_column_idx]]] == obj$value,
    slot_idx = slot_idx, slot_pos = slot_pos, comment_column_idx = comment_column_idx, obj = obj
  ))
  obj$idx_data_frame <- data.frame(row_no = which(has_value), col_no = rep(comment_idx, sum(has_value)))
  class(obj) <- c("value_comment", "pxweb_data_comment", "list")
  obj
}

#' @rdname pxweb_data_column_comment
#' @keywords internal
pxweb_data_obs_comment <- function(x, obs_idx) {
  checkmate::assert_class(x, "pxweb_data")
  checkmate::assert_int(obs_idx, lower = 1, upper = length(x$data))

  column_text <- pxweb_data_colnames(x, type = "text")
  column_code <- pxweb_data_colnames(x, type = "code")

  obj <- list(
    code = column_code,
    text = column_text,
    value = unname(unlist(x$data[[obs_idx]][c("key", "values")])),
    comment = x$data[[obs_idx]]$comment
  )
  obj$idx_data_frame <- data.frame(row_no = obs_idx, col_no = NA)
  class(obj) <- c("obs_comment", "pxweb_data_comment", "list")
  obj
}

pxweb_data_v2_note_text <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- x[nzchar(x)]
  if (length(x) == 0) {
    return(NULL)
  }
  paste(x, collapse = "\n")
}

pxweb_data_v2_dataset_note_comment <- function(x, comment) {
  obj <- list(
    code = pxweb_data_colnames(x, "code"),
    text = pxweb_data_colnames(x, "text"),
    value = NULL,
    comment = comment
  )
  obj$idx_data_frame <- data.frame(row_no = NA_integer_, col_no = NA_integer_)
  class(obj) <- c("obs_comment", "pxweb_data_comment", "list")
  obj
}

pxweb_data_v2_column_comment <- function(x, column_idx, comment) {
  variable_ids <- unlist(x$id, use.names = FALSE)
  variable_id <- variable_ids[[column_idx]]
  obj <- list(
    code = variable_id,
    text = x$dimension[[variable_id]]$label,
    value = NULL,
    comment = comment
  )
  obj$idx_data_frame <- data.frame(row_no = NA_integer_, col_no = column_idx)
  class(obj) <- c("column_comment", "pxweb_data_comment", "list")
  obj
}

pxweb_data_v2_value_comment <- function(x, column_idx, value_code, comment) {
  variable_ids <- unlist(x$id, use.names = FALSE)
  variable_id <- variable_ids[[column_idx]]
  df <- as.data.frame(x, column.name.type = "code", variable.value.type = "code")
  has_value <- df[[variable_id]] == value_code

  obj <- list(
    code = variable_id,
    text = x$dimension[[variable_id]]$label,
    value = value_code,
    comment = comment
  )
  obj$idx_data_frame <- data.frame(
    row_no = which(has_value),
    col_no = rep(column_idx, sum(has_value))
  )
  class(obj) <- c("value_comment", "pxweb_data_comment", "list")
  obj
}

pxweb_data_v2_obs_comment <- function(x, obs_idx, comment) {
  df <- as.data.frame(x, column.name.type = "code", variable.value.type = "code")
  obj <- list(
    code = pxweb_data_colnames(x, "code"),
    text = pxweb_data_colnames(x, "text"),
    value = unname(unlist(df[obs_idx, , drop = TRUE])),
    comment = comment
  )
  obj$idx_data_frame <- data.frame(row_no = obs_idx, col_no = NA_integer_)
  class(obj) <- c("obs_comment", "pxweb_data_comment", "list")
  obj
}

pxweb_data_v2_status_comments <- function(x) {
  status <- x$status
  if (is.null(status) || length(status) == 0) {
    return(list())
  }

  status <- as.list(status)
  status_names <- names(status)
  if (is.null(status_names)) {
    status_names <- as.character(seq_along(status) - 1L)
  }

  res <- list()
  for (i in seq_along(status)) {
    status_text <- pxweb_data_v2_note_text(status[[i]])
    if (is.null(status_text)) {
      next
    }
    status_idx <- suppressWarnings(as.integer(status_names[[i]]))
    if (is.na(status_idx)) {
      next
    }
    obs_idx <- status_idx + 1L
    if (obs_idx < 1L || obs_idx > pxweb_data_dim(x)[1]) {
      next
    }
    res[[length(res) + 1L]] <- pxweb_data_v2_obs_comment(
      x,
      obs_idx,
      paste("Status:", status_text)
    )
  }
  res
}

#' Assert that x is a correct \code{pxweb_data_comments} object.
#' @param x an object to check.
#' @keywords internal
assert_pxweb_data_comments <- function(x) {
  checkmate::assert_class(x, c("pxweb_data_comments", "list"))
  checkmate::assert_names(names(x), permutation.of = c("pxweb_data_comments", "data_dim"))

  for (i in seq_along(x$comments)) {
    checkmate::assert_class(x$comments[[i]], "pxweb_data_comment")
    checkmate::assert_choice(class(x$comments[[i]])[1], choices = c(
      "obs_comment",
      "value_comment",
      "column_comment"
    ))
  }
  checkmate::assert_integerish(x$data_dim, lower = 1)
}


#' @export
print.pxweb_data_comment <- function(x, ...) {
  cat(class(x)[[1]], " (", paste(x$text, collapse = ", "), " [", paste(x$code, collapse = ", "), "], ", paste(x$value, collapse = ", "), "):\n  ", x$comment, "\n", sep = "")
}


#' @export
#' @keywords internal
print.pxweb_data_comments <- function(x, ...) {
  if (length(x$pxweb_data_comments) <= 0) {
    cat("NO PXWEB DATA COMMENTS\n")
  } else {
    cat("PXWEB DATA COMMENTS\n")
    for (i in seq_along(x$pxweb_data_comments)) {
      cat("$pxweb_data_comments[[", i, "]]\n", sep = "")
      print(x$pxweb_data_comments[[i]])
    }
  }
}

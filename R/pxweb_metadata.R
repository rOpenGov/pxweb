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
pxweb_metadata <- function(x) {
  if (is.null(x$title)) {
    x$title <- NA
  }

  checkmate::assert_names(names(x), must.include = "variables")
  for (i in seq_along(x$variables)) {
    if (all(c("values", "valueTexts") %in% names(x$variables[[i]]))) {
      checkmate::assert_names(names(x$variables[[i]]), must.include = c("values", "valueTexts"))
      x$variables[[i]]$values <- unlist(x$variables[[i]]$values)
      x$variables[[i]]$valueTexts <- unlist(x$variables[[i]]$valueTexts)
    }
    if (is.null(x$variables[[i]]$elimination)) x$variables[[i]]$elimination <- FALSE
    if (is.null(x$variables[[i]]$time)) x$variables[[i]]$time <- FALSE
  }
  class(x) <- c("pxweb_metadata", "list")
  assert_pxweb_metadata(x)
  x
}

pxweb_metadata_v2 <- function(x) {
  checkmate::assert_class(x, "list")
  assert_pxweb_metadata_response_v2(x)

  variable_ids <- unlist(x$id, use.names = FALSE)
  time_ids <- unlist(x$role$time, use.names = FALSE)
  if (is.null(time_ids)) {
    time_ids <- character(0)
  }

  variables <- lapply(variable_ids, function(variable_id) {
    dim <- x$dimension[[variable_id]]
    values <- pxweb_metadata_v2_values(dim)
    value_texts <- pxweb_metadata_v2_value_texts(dim, values)

    list(
      code = variable_id,
      text = dim$label,
      values = values,
      valueTexts = value_texts,
      elimination = isTRUE(dim$extension$elimination),
      time = variable_id %in% time_ids
    )
  })

  res <- pxweb_metadata(list(
    title = x$label,
    variables = variables
  ))
  attr(res, "pxweb_metadata_v2") <- x
  res
}

assert_pxweb_metadata_response_v2 <- function(x) {
  checkmate::assert_class(x, "list")
  checkmate::assert_names(names(x), must.include = c("version", "class", "label", "id", "dimension"))
  checkmate::assert_string(x$version, pattern = "^2\\.")
  checkmate::assert_string(x$class, pattern = "^dataset$")
  checkmate::assert_character(unlist(x$id, use.names = FALSE), min.len = 1)
  checkmate::assert_list(x$dimension, min.len = 1)
  checkmate::assert_true(identical(length(unlist(x$value, use.names = FALSE)), 0L))

  variable_ids <- unlist(x$id, use.names = FALSE)
  checkmate::assert_subset(variable_ids, choices = names(x$dimension))
  for (variable_id in variable_ids) {
    dim <- x$dimension[[variable_id]]
    checkmate::assert_names(names(dim), must.include = c("label", "category", "extension"))
    checkmate::assert_string(dim$label)
  }
}

pxweb_metadata_v2_values <- function(dim) {
  index <- dim$category$index
  if (is.null(index)) {
    return(character(0))
  }

  index <- unlist(index, use.names = TRUE)
  if (is.null(names(index))) {
    return(as.character(index))
  }

  index_order <- suppressWarnings(as.numeric(index))
  if (any(is.na(index_order))) {
    return(names(index))
  }
  names(index)[order(index_order)]
}

pxweb_metadata_v2_value_texts <- function(dim, values) {
  labels <- dim$category$label
  if (is.null(labels) || length(values) == 0) {
    return(values)
  }

  labels <- unlist(labels, use.names = TRUE)
  if (is.null(names(labels))) {
    value_texts <- as.character(labels)
  } else {
    value_texts <- unname(labels[values])
  }
  value_texts[is.na(value_texts)] <- values[is.na(value_texts)]
  as.character(value_texts)
}



#' Assert that x is a correct \code{pxweb_metadata} object.
#' @param x an object to check.
#' @keywords internal
assert_pxweb_metadata <- function(x) {
  checkmate::assert_class(x, c("pxweb_metadata", "list"))
  checkmate::assert_names(names(x), must.include = c("title", "variables"))
  checkmate::assert_string(x$title, na.ok = TRUE)

  for (i in seq_along(x$variables)) {
    checkmate::assert_names(names(x$variables[[i]]), must.include = c("code", "text", "elimination", "time"), .var.name = paste0("names(x$variables[[", i, "]])"))
    checkmate::assert_string(x$variables[[i]]$code, .var.name = paste0("x$variables[[", i, "]]$code"))
    checkmate::assert_string(x$variables[[i]]$text, .var.name = paste0("x$variables[[", i, "]]$text"))
    if (!is.null(x$variables[[i]]$values)) {
      checkmate::assert_character(x$variables[[i]]$values, .var.name = paste0("x$variables[[", i, "]]$values"))
      checkmate::assert_character(x$variables[[i]]$valueTexts, len = length(unlist(x$variables[[i]]$values)), .var.name = paste0("x$variables[[", i, "]]$valueTexts"))
    }
    checkmate::assert_flag(x$variables[[i]]$time, .var.name = paste0("x$variables[[", i, "]]$time"))
    checkmate::assert_flag(x$variables[[i]]$elimination, .var.name = paste0("x$variables[[", i, "]]$elimination"))
  }
}


#' @export
print.pxweb_metadata <- function(x, ...) {
  cat("PXWEB METADATA\n")
  cat(x$title, "\n")
  cat("variables:\n")
  for (i in seq_along(x$variables)) {
    cat(" [[", i, "]] ", x$variables[[i]]$code, ": ", x$variables[[i]]$text, "\n", sep = "")
  }
}

#' Get boolean vector
#'
#' @param pxmd a \code{pxweb_metadata} object.
#'
#' @return pxweb_metadata eliminations as a named boolean vector.
#'
#' @keywords internal
pxweb_metadata_elimination <- function(pxmd) {
  checkmate::assert_class(pxmd, "pxweb_metadata")
  res <- unlist(lapply(pxmd$variables, function(x) x$elimination))
  names(res) <- unlist(lapply(pxmd$variables, function(x) x$code))
  res
}

#' Get boolean vector
#'
#' @param pxmd a \code{pxweb_metadata} object.
#'
#' @return pxweb_metadata eliminations as a named boolean vector.
#'
#' @keywords internal
pxweb_metadata_time <- function(pxmd) {
  checkmate::assert_class(pxmd, "pxweb_metadata")
  res <- unlist(lapply(pxmd$variables, function(x) x$time))
  names(res) <- unlist(lapply(pxmd$variables, function(x) x$code))
  res
}


#' Compue the dimension of a metadata object
#'
#' @param pxmd a \code{pxweb_metadata} object.
#'
#' @keywords internal
pxweb_metadata_dim <- function(pxmd) {
  checkmate::assert_class(pxmd, "pxweb_metadata")
  dim_res <- numeric(length(pxmd$variables))
  for (i in seq_along(pxmd$variables)) {
    names(dim_res)[i] <- pxmd$variables[[i]]$code
    dim_res[i] <- length(pxmd$variables[[i]]$values)
  }
  dim_res
}

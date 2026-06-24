#' Combine pxweb objects
#'
#' @param x a list with \code{pxweb} objects.
#'
#' @keywords internal
pxweb_c <- function(x) {
  checkmate::assert_class(x, "list")
  if (length(x) == 1) {
    return(x[[1]])
  }

  if (inherits(x[[1]], "pxweb_data")) {
    return(pxweb_data_c(x))
  }

  if (inherits(x[[1]], "pxweb_data_v2")) {
    return(pxweb_data_v2_c(x))
  }

  if (inherits(x[[1]], "json")) {
    return(x)
  }

  if (inherits(x[[1]], "character")) {
    fp <- unlist(x)
    fe <- file.exists(fp)
    if (all(fe)) {
      message("PXWEB API did not return JSON. Files has been stored locally (tempdir) and paths has been returned.")
      return(x)
    } else {
      stop("Files doesn't exist:\n", paste(fp[!fe], collapse = "\n"), call. = FALSE)
    }
  }

  stop("pxweb_c() not implemented for class '", class(x[[1]])[1], "'.", call. = FALSE)
}


#' Combine pxweb objects
#'
#' @param x a list with \code{pxweb} objects.
#'
#' @keywords internal
pxweb_data_c <- function(x) {
  for (i in seq_along(x)) {
    checkmate::assert_class(x[[i]], "pxweb_data")
  }
  full_pxd <- x[[1]]

  # Assert equal columns
  for (i in 2:length(x)) {
    for (j in seq_along(full_pxd$columns)) {
      checkmate::assert_set_equal(full_pxd$columns[[j]]$code, x[[i]]$columns[[j]]$code)
      checkmate::assert_set_equal(full_pxd$columns[[j]]$text, x[[i]]$columns[[j]]$text)
      checkmate::assert_set_equal(full_pxd$columns[[j]]$type, x[[i]]$columns[[j]]$type)
    }
  }

  # Add comments if not duplicated
  json_comments <- character(length(full_pxd$comments))
  for (i in seq_along(full_pxd$comments)) {
    json_comments[i] <- jsonlite::toJSON(full_pxd$comments[[i]])
  }
  for (i in 2:length(x)) {
    for (j in seq_along(x[[i]]$comments)) {
      json_new_comment <- jsonlite::toJSON(x[[i]]$comments[[j]])
      if (!(json_new_comment %in% json_comments)) json_comments <- c(json_comments, json_new_comment)
    }
  }
  full_pxd$comments <- list()
  for (i in seq_along(json_comments)) {
    full_pxd$comments[[i]] <- jsonlite::fromJSON(json_comments[i])
  }

  # Combine data
  for (i in 2:length(x)) {
    full_pxd$data <- c(full_pxd$data, x[[i]]$data)
  }

  checkmate::assert_class(full_pxd, "pxweb_data")
  full_pxd
}

pxweb_data_v2_c <- function(x) {
  for (i in seq_along(x)) {
    checkmate::assert_class(x[[i]], "pxweb_data_v2")
  }

  full_pxd <- x[[1]]
  variable_ids <- unlist(full_pxd$id, use.names = FALSE)
  dfs <- lapply(x, as.data.frame, column.name.type = "code", variable.value.type = "code")
  df <- do.call(rbind, dfs)

  pxmd <- full_pxd$pxweb_metadata
  dimension_values <- lapply(variable_ids, function(variable_id) {
    present_values <- unique(df[[variable_id]])
    if (inherits(pxmd, "pxweb_metadata")) {
      metadata_variables <- vapply(pxmd$variables, function(variable) variable$code, character(1))
      idx <- which(metadata_variables == variable_id)
      if (length(idx) == 1) {
        return(pxmd$variables[[idx]]$values[pxmd$variables[[idx]]$values %in% present_values])
      }
    }
    present_values
  })
  names(dimension_values) <- variable_ids

  full_grid <- do.call(
    expand.grid,
    c(rev(dimension_values), list(KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
  )
  full_grid <- full_grid[rev(names(full_grid))]
  names(full_grid) <- variable_ids

  row_key <- function(dat) {
    do.call(paste, c(dat[variable_ids], sep = "\r"))
  }
  full_pxd$value <- df$value[match(row_key(full_grid), row_key(df))]
  full_pxd$size <- as.list(as.integer(vapply(dimension_values, length, integer(1))))

  for (variable_id in variable_ids) {
    values <- dimension_values[[variable_id]]
    labels <- values
    if (inherits(pxmd, "pxweb_metadata")) {
      metadata_variables <- vapply(pxmd$variables, function(variable) variable$code, character(1))
      idx <- which(metadata_variables == variable_id)
      if (length(idx) == 1) {
        label_lookup <- stats::setNames(pxmd$variables[[idx]]$valueTexts, pxmd$variables[[idx]]$values)
        labels <- unname(label_lookup[values])
        labels[is.na(labels)] <- values[is.na(labels)]
      }
    }

    index <- as.list(seq_along(values) - 1L)
    names(index) <- values
    label <- as.list(labels)
    names(label) <- values
    full_pxd$dimension[[variable_id]]$category$index <- index
    full_pxd$dimension[[variable_id]]$category$label <- label
  }

  checkmate::assert_class(full_pxd, "pxweb_data_v2")
  full_pxd
}

#' List PXWEB API v2 codelists in metadata.
#'
#' @description
#' Extracts aggregation and value set codelists from a PXWEB API v2 metadata
#' object. Use the returned \code{id} values with \code{\link{pxweb_aggregation}}
#' or \code{\link{pxweb_valueset}}.
#'
#' @param x a \code{pxweb_metadata} object created from PXWEB API v2 metadata,
#'   or a raw PXWEB API v2 metadata list.
#' @param variable optional variable code or label to filter on.
#' @param type optional codelist type to filter on, for example
#'   \code{"Aggregation"} or \code{"Valueset"}.
#'
#' @return
#' A \code{data.frame} with columns \code{variable_code},
#' \code{variable_text}, \code{id}, \code{label}, \code{type}, and \code{href}.
#'
#' @examples
#' \dontrun{
#' meta <- pxweb_get("https://statistikdatabasen.scb.se/api/v2/tables/TAB5974/metadata?lang=sv")
#' pxweb_codelists(meta, variable = "Alder")
#' }
#'
#' @export
pxweb_codelists <- function(x, variable = NULL, type = NULL) {
  raw_metadata <- pxweb_raw_metadata_v2(x)
  checkmate::assert_string(variable, null.ok = TRUE)
  checkmate::assert_character(type, null.ok = TRUE, min.len = 1, any.missing = FALSE)

  variable_ids <- unlist(raw_metadata$id, use.names = FALSE)
  rows <- list()

  for (variable_id in variable_ids) {
    dim <- raw_metadata$dimension[[variable_id]]
    code_lists <- dim$extension$codeLists
    if (is.null(code_lists)) {
      code_lists <- dim$extension$codelists
    }
    if (is.null(code_lists) || length(code_lists) == 0) {
      next
    }

    for (i in seq_along(code_lists)) {
      code_list <- code_lists[[i]]
      rows[[length(rows) + 1L]] <- data.frame(
        variable_code = variable_id,
        variable_text = dim$label,
        id = pxweb_null_value(code_list$id, NA_character_),
        label = pxweb_null_value(code_list$label, NA_character_),
        type = pxweb_null_value(code_list$type, NA_character_),
        href = pxweb_codelist_href(code_list),
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(rows) == 0) {
    res <- data.frame(
      variable_code = character(0),
      variable_text = character(0),
      id = character(0),
      label = character(0),
      type = character(0),
      href = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    res <- do.call(rbind, rows)
    row.names(res) <- NULL
  }

  if (!is.null(variable)) {
    variable_match <- tolower(variable)
    res <- res[tolower(res$variable_code) == variable_match | tolower(res$variable_text) == variable_match, , drop = FALSE]
    row.names(res) <- NULL
  }

  if (!is.null(type)) {
    type_match <- tolower(type)
    res <- res[tolower(res$type) %in% type_match, , drop = FALSE]
    row.names(res) <- NULL
  }

  res
}

pxweb_raw_metadata_v2 <- function(x) {
  if (inherits(x, "pxweb_metadata")) {
    raw_metadata <- attr(x, "pxweb_metadata_v2")
    if (is.null(raw_metadata)) {
      stop("x is not PXWEB API v2 metadata.", call. = FALSE)
    }
    return(raw_metadata)
  }

  checkmate::assert_class(x, "list")
  assert_pxweb_metadata_response_v2(x)
  x
}

pxweb_codelist_href <- function(x) {
  links <- x$links
  if (is.null(links) || length(links) == 0) {
    return(NA_character_)
  }

  for (i in seq_along(links)) {
    if (!is.null(links[[i]]$href)) {
      return(links[[i]]$href)
    }
  }

  NA_character_
}

pxweb_null_value <- function(x, value) {
  if (is.null(x)) {
    return(value)
  }
  x
}

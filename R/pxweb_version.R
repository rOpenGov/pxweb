#' Backend helpers for PXWEB APIs
#'
#' @keywords internal

pxweb_detect_version <- function(x) {
  if (checkmate::test_class(x, "url")) {
    url <- x
  } else if (checkmate::test_class(x, "pxweb")) {
    url <- x$url
  } else if (checkmate::test_string(x)) {
    url <- parse_url_or_fail(x)
  } else {
    stop("Cannot detect version for input.", call. = FALSE)
  }

  path <- tolower(url$path)
  if (grepl("/v2/", path)) {
    return("v2")
  }
  "v1"
}

assert_pxweb_version <- function(x) {
  checkmate::assert_string(x)
  checkmate::assert_choice(x, choices = c("v1", "v2"))
}

pxweb_v2_api_subpath <- function(x, as_vector = FALSE) {
  if (checkmate::test_class(x, "pxweb")) {
    path <- x$url$path
  } else if (checkmate::test_class(x, "url")) {
    path <- x$path
  } else if (checkmate::test_string(x)) {
    path <- parse_url_or_fail(x)$path
  } else {
    stop("Cannot extract v2 API subpath for input.", call. = FALSE)
  }

  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  api_idx <- which(parts == "api")
  v2_idx <- which(parts == "v2")
  idx <- intersect(api_idx + 1L, v2_idx)
  if (length(idx) == 0) {
    stop("Could not identify '/api/v2' in url path: ", path, call. = FALSE)
  }
  end_idx <- idx[1]
  subpath <- parts[1:end_idx]

  if (as_vector) {
    return(subpath)
  }
  paste(subpath, collapse = "/")
}

build_pxweb_v2_tables_url <- function(x) {
  if (checkmate::test_class(x, "pxweb")) {
    u <- x$url
  } else if (checkmate::test_class(x, "url")) {
    u <- x
  } else if (checkmate::test_string(x)) {
    u <- parse_url_or_fail(x)
  } else if (checkmate::test_class(x, "list")) {
    assert_pxweb_url(x)
    u <- x$url
  } else {
    stop("Cannot build v2 tables url for input.", call. = FALSE)
  }
  u$path <- paste(c(pxweb_v2_api_subpath(u, as_vector = TRUE), "tables"), collapse = "/")
  build_pxweb_url(u)
}

build_pxweb_v2_table_metadata_url <- function(x, table_id) {
  checkmate::assert_string(table_id, min.chars = 1)
  table_id <- gsub("^/+", "", table_id)
  paste0(build_pxweb_v2_tables_url(x), "/", table_id, "/metadata")
}

build_pxweb_v2_table_data_url <- function(x, table_id) {
  checkmate::assert_string(table_id, min.chars = 1)
  table_id <- gsub("^/+", "", table_id)
  paste0(build_pxweb_v2_tables_url(x), "/", table_id, "/data")
}

pxweb_v2_table_id <- function(x, pxmd = NULL) {
  if (!is.null(pxmd)) {
    checkmate::assert_class(pxmd, "pxweb_metadata")
    raw_metadata <- attr(pxmd, "pxweb_metadata_v2")
    table_id <- raw_metadata$extension$px$tableid
    if (!is.null(table_id)) {
      return(table_id)
    }
  }

  if (checkmate::test_class(x, "pxweb")) {
    path <- x$url$path
  } else if (checkmate::test_class(x, "url")) {
    path <- x$path
  } else if (checkmate::test_string(x)) {
    path <- parse_url_or_fail(x)$path
  } else {
    stop("Cannot extract PXWEB API v2 table id for input.", call. = FALSE)
  }

  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  table_idx <- which(parts == "tables")
  if (length(table_idx) == 0 || length(parts) < table_idx[1] + 1L) {
    stop("Could not identify PXWEB API v2 table id in url path: ", path, call. = FALSE)
  }
  parts[[table_idx[1] + 1L]]
}

pxweb_v2_data_query_params <- function(px, pxmd = NULL, output_format = "json-stat2") {
  checkmate::assert_class(px, "pxweb")
  checkmate::assert_string(output_format, min.chars = 1)

  lang <- px$url$query$lang
  if (is.null(lang) && !is.null(pxmd)) {
    raw_metadata <- attr(pxmd, "pxweb_metadata_v2")
    lang <- raw_metadata$extension$px$language
  }

  res <- list(
    lang = lang,
    outputFormat = output_format
  )
  res[!vapply(res, is.null, logical(1))]
}

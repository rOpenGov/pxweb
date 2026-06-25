#' Search PXWEB API tables
#'
#' @description
#' Search for tables in a PXWEB API. PXWEB API v2 search uses the standard
#' \code{/tables?query=} endpoint. PXWEB API v1 search is supported for API
#' database roots that expose search, such as Statistics Finland.
#'
#' @param query a search string.
#' @param api_url a PXWEB API root URL. For v2, use an API root such as
#'   \code{"https://statistikdatabasen.scb.se/api/v2"}. For v1, use a
#'   searchable database root such as
#'   \code{"https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin"}.
#' @param lang language code for PXWEB API v2. For v1, the language is part of
#'   \code{api_url}.
#' @param page_number page number for PXWEB API v2 searches.
#' @param page_size page size for PXWEB API v2 searches.
#' @param past_days if supplied, restrict PXWEB API v2 searches to tables
#'   updated in the last \code{past_days} days.
#' @param include_discontinued include discontinued PXWEB API v2 tables.
#' @param ... further arguments passed to \code{httr::GET}.
#'
#' @return a \code{data.frame} with search hits and table URLs.
#'
#' @examples
#' \dontrun{
#' pxweb_search(
#'   "population",
#'   api_url = "https://statistikdatabasen.scb.se/api/v2",
#'   lang = "en"
#' )
#'
#' pxweb_search(
#'   "population",
#'   api_url = "https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin"
#' )
#' }
#'
#' @export
pxweb_search <- function(query,
                         api_url,
                         lang = NULL,
                         page_number = 1,
                         page_size = NULL,
                         past_days = NULL,
                         include_discontinued = FALSE,
                         ...) {
  checkmate::assert_string(query, min.chars = 1)
  checkmate::assert_string(api_url, min.chars = 1)
  checkmate::assert_string(lang, null.ok = TRUE)
  checkmate::assert_count(page_number, positive = TRUE)
  checkmate::assert_count(page_size, positive = TRUE, null.ok = TRUE)
  checkmate::assert_count(past_days, positive = TRUE, null.ok = TRUE)
  checkmate::assert_flag(include_discontinued)

  version <- pxweb_search_api_version(api_url)
  search_url <- pxweb_search_url(api_url, version)

  if (identical(version, "v2")) {
    request_query <- list(
      lang = lang,
      query = query,
      pageNumber = page_number
    )
    if (!is.null(page_size)) {
      request_query$pageSize <- page_size
    }
    if (!is.null(past_days)) {
      request_query$pastDays <- past_days
    }
    if (include_discontinued) {
      request_query$includeDiscontinued <- include_discontinued
    }
  } else {
    request_query <- list(query = query)
  }

  request_query <- request_query[!vapply(request_query, is.null, logical(1))]
  response <- pxweb_search_request(search_url, request_query, ...)
  content <- pxweb_search_response(response)

  if (identical(version, "v2")) {
    pxweb_search_parse_v2(content)
  } else {
    pxweb_search_parse_v1(content, api_url)
  }
}

pxweb_search_api_version <- function(api_url) {
  parsed <- parse_url_or_fail(api_url)
  path <- pxweb_search_path_parts(parsed)

  if ("v2" %in% path) {
    return("v2")
  }
  if ("v1" %in% path) {
    return("v1")
  }

  stop(
    "Cannot detect PXWEB API version from api_url: '", api_url, "'. ",
    "Use a PXWEB API root containing '/v2' or '/v1', for example ",
    "'https://statistikdatabasen.scb.se/api/v2' for v2 or ",
    "'https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin' for v1.",
    call. = FALSE
  )
}

pxweb_search_url <- function(api_url, version) {
  parsed <- parse_url_or_fail(api_url)
  if (length(parsed$query) > 0) {
    stop(
      "api_url should not include query parameters. ",
      "Supply the PXWEB API root and pass the search text as the first argument.",
      call. = FALSE
    )
  }

  path <- pxweb_search_path_parts(parsed)
  version_idx <- match(version, path)
  if (version_idx < length(path)) {
    trailing_path <- path[seq.int(version_idx + 1L, length(path))]
  } else {
    trailing_path <- character()
  }
  normalized_url <- pxweb_search_trim_slash(build_pxweb_url(parsed))

  if (identical(version, "v2")) {
    if (length(trailing_path) > 0) {
      stop(
        "For PXWEB API v2, api_url must be the API root, not a table, ",
        "metadata, data, or /tables endpoint. Use a URL like ",
        "'https://statistikdatabasen.scb.se/api/v2'.",
        call. = FALSE
      )
    }
    return(paste0(normalized_url, "/tables"))
  }

  if (length(trailing_path) < 2) {
    stop(
      "For PXWEB API v1, api_url must include the language and database id ",
      "because v1 search is exposed by searchable database roots. Use a URL ",
      "like 'https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin'.",
      call. = FALSE
    )
  }
  if (grepl("\\.px$", normalized_url, ignore.case = TRUE)) {
    stop(
      "For PXWEB API v1, api_url must be a searchable database root, not a ",
      "table URL. Use a URL like ",
      "'https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin'.",
      call. = FALSE
    )
  }
  normalized_url
}

pxweb_search_request <- function(url, query, ...) {
  httr::GET(url, pxweb_user_agent(), query = query, ...)
}

pxweb_search_response <- function(response) {
  httr::stop_for_status(response)
  suppressWarnings(httr::content(response, as = "parsed"))
}

pxweb_search_parse_v2 <- function(x) {
  if (!is.list(x) || is.null(x$tables) || !is.list(x$tables)) {
    stop(
      "PXWEB API v2 search response did not contain a 'tables' list. ",
      "Check that api_url is a PXWEB API v2 root such as ",
      "'https://statistikdatabasen.scb.se/api/v2'.",
      call. = FALSE
    )
  }

  data.frame(
    id = vapply(x$tables, pxweb_search_chr, character(1), "id"),
    label = vapply(x$tables, pxweb_search_chr, character(1), "label"),
    description = vapply(x$tables, pxweb_search_chr, character(1), "description"),
    updated = vapply(x$tables, pxweb_search_chr, character(1), "updated"),
    first_period = vapply(x$tables, pxweb_search_chr, character(1), "firstPeriod"),
    last_period = vapply(x$tables, pxweb_search_chr, character(1), "lastPeriod"),
    source = vapply(x$tables, pxweb_search_chr, character(1), "source"),
    subject_code = vapply(x$tables, pxweb_search_chr, character(1), "subjectCode"),
    time_unit = vapply(x$tables, pxweb_search_chr, character(1), "timeUnit"),
    metadata_url = vapply(x$tables, function(table) pxweb_search_link(table$links, "metadata"), character(1)),
    data_url = vapply(x$tables, function(table) pxweb_search_link(table$links, "data"), character(1)),
    variable_names = I(lapply(x$tables, function(table) table$variableNames)),
    paths = I(lapply(x$tables, function(table) table$paths)),
    stringsAsFactors = FALSE
  )
}

pxweb_search_parse_v1 <- function(x, api_url) {
  if (!is.list(x) || !all(vapply(x, is.list, logical(1)))) {
    stop(
      "PXWEB API v1 search response did not contain a list of search hits. ",
      "Check that api_url is a searchable PXWEB API v1 database root such as ",
      "'https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin'.",
      call. = FALSE
    )
  }
  if (length(x) == 0) {
    return(pxweb_search_empty_v1())
  }

  required_names <- c("id", "path", "title")
  if (!all(vapply(x, function(hit) all(required_names %in% names(hit)), logical(1)))) {
    stop(
      "PXWEB API v1 search response did not look like search hits. ",
      "Check that api_url is a searchable PXWEB API v1 database root such as ",
      "'https://pxdata.stat.fi/PxWeb/api/v1/en/StatFin', not a language root ",
      "or table URL.",
      call. = FALSE
    )
  }

  data.frame(
    id = vapply(x, pxweb_search_chr, character(1), "id"),
    path = vapply(x, pxweb_search_chr, character(1), "path"),
    title = vapply(x, pxweb_search_chr, character(1), "title"),
    score = vapply(x, pxweb_search_num, numeric(1), "score"),
    published = vapply(x, pxweb_search_chr, character(1), "published"),
    url = vapply(x, function(hit) pxweb_search_v1_table_url(api_url, hit$path, hit$id), character(1)),
    stringsAsFactors = FALSE
  )
}

pxweb_search_empty_v1 <- function() {
  data.frame(
    id = character(),
    path = character(),
    title = character(),
    score = numeric(),
    published = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
}

pxweb_search_path_parts <- function(parsed_url) {
  path <- parsed_url$path
  if (is.null(path) || !nzchar(path)) {
    return(character())
  }
  strsplit(gsub("^/+|/+$", "", path), "/", fixed = FALSE)[[1]]
}

pxweb_search_trim_slash <- function(x) {
  gsub("/+$", "", x)
}

pxweb_search_chr <- function(x, name) {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0) {
    return(NA_character_)
  }
  as.character(value[[1]])
}

pxweb_search_num <- function(x, name) {
  value <- x[[name]]
  if (is.null(value) || length(value) == 0) {
    return(NA_real_)
  }
  as.numeric(value[[1]])
}

pxweb_search_link <- function(links, rel) {
  if (is.null(links) || !is.list(links)) {
    return(NA_character_)
  }
  rels <- vapply(links, pxweb_search_chr, character(1), "rel")
  idx <- match(rel, rels)
  if (is.na(idx)) {
    return(NA_character_)
  }
  pxweb_search_chr(links[[idx]], "href")
}

pxweb_search_v1_table_url <- function(api_url, path, id) {
  paste(
    pxweb_search_trim_slash(api_url),
    gsub("^/+|/+$", "", path),
    id,
    sep = "/"
  )
}

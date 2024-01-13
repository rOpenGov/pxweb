#' Parse a character string or throws error if it fails
#'
#' @param x a character element to parse
#'
#' @keywords internal
parse_url_or_fail <- function(x) {
  checkmate::assert_string(x)
  x <- pxweb_fix_url(x)
  parsed_url <- httr::parse_url(x)
  if (is.null(parsed_url$hostname)) {
    stop("Cannot parse url (hostname). Please check url for potential errors (?parse_url): '", x, "'", call. = FALSE)
  }
  parsed_url
}

#' Parse a character string or throws error if it fails
#'
#' @param x a character element to parse
#'
#' @keywords internal
parse_url_or_fail <- function(x) {
  pu <- parse_url(x)
  if (!parsed_url_has_hostname(x)) {
    stop("Cannot parse url (hostname). Please check url for potential errors (?parse_url): '", x, "'", call. = FALSE)
  }
  pu
}

parse_url <- function(x) {
  checkmate::assert_string(x)
  x <- pxweb_fix_url(x)
  httr::parse_url(x)
}

parsed_url_has_hostname <- function(x){
  if(!checkmate::test_class(x, "url")){
    x <- parse_url(x)
  }
  !is.null(x$hostname)
}

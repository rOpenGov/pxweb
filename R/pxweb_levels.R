#' Construct a \code{pxweb_levels} object.
#'
#' @description
#' An object that contain the levels for a given PXWEB api position.
#'
#' @param x a list returned from a PXWEB API to convert to a \code{pxweb_levels} object.
#'
#' @return
#' a \code{pxweb_levels} object
#'
#' @keywords internal
pxweb_levels <- function(x) {
  checkmate::assert_class(x, "list")
  class(x) <- c("pxweb_levels", "list")
  assert_pxweb_levels(x)
  x
}


#' Assert that x is a correct \code{pxweb_levels} object.
#' @param x an object to check.
#' @keywords internal
assert_pxweb_levels <- function(x) {
  checkmate::assert_class(x, c("pxweb_levels", "list"))
  for (i in seq_along(x)) {
    checkmate::assert_names(names(x[[i]]), must.include = c("id", "type", "text"), .var.name = paste0("names(x[[", i, "]])"))
    checkmate::assert_string(x[[i]]$id, .var.name = paste0("x[[", i, "]]$id"))
    checkmate::assert_choice(x[[i]]$type, choices = c("l", "t", "h"), .var.name = paste0("x[[", i, "]]$type"))
    checkmate::assert_string(x[[i]]$text, .var.name = paste0("x[[", i, "]]$id"))
  }
}


#' @export
print.pxweb_levels <- function(x, ...) {
  cat("PXWEB LEVELS\n")
  for (i in seq_along(x)) {
    cat("  ", x[[i]]$id, " (", x[[i]]$type, "): ", x[[i]]$text, "\n", sep = "")
  }
}

#' @keywords internal
pxweb_levels_choices_df <- function(x) {
  checkmate::assert_class(x, "pxweb_levels")
  df <- pxweb_as_data_frame.pxweb_levels(x)
  df$is_choice <- ifelse(df$type %in% c("t", "l"), yes = TRUE, no = FALSE)
  df$choice_idx <- cumsum(df$is_choice)
  df$choice_idx[!df$is_choice] <- NA
  df
}

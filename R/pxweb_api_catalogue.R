#' Get the PXWEB API catalogue
#' 
#' @details
#' A list with implemented API:s.
#' 
#' @examples
#' pxweb_api_catalogue()
#' 
#' @export
pxweb_api_catalogue <- function(){
  apis <- jsonlite::fromJSON(pxweb_api_catalogue_path())
  pxweb_api_catalogue <- list()
  for(i in seq_along(apis$apis)){
    pxweb_api_catalogue[[names(apis$apis)[i]]] <- pxweb_api_catalogue_entry(apis$apis[[i]])
  }
  class(pxweb_api_catalogue) <- c("pxweb_api_catalogue", "list")
  assert_pxweb_api_catalogue(pxweb_api_catalogue)
  return(pxweb_api_catalogue)
}

#' @rdname pxweb_api_catalogue
#' @keywords internal
#' @export
pxweb_api_catalogue_path <- function(){
  system.file(file.path("extdata", "api.json"), package = "pxweb")
}

#' Assert a \code{pxweb_api_catalogue} object
#' @param x an object to assert is a \code{pxweb_api_catalogue_entry}.
#' @keywords internal
assert_pxweb_api_catalogue <- function(x){
  checkmate::assert_class(x, "pxweb_api_catalogue")
  for(i in seq_along(x)){
    checkmate::assert_class(x[[i]], "pxweb_api_catalogue_entry")
    checkmate::assert_names(names(x)[i], identical.to = httr::parse_url(x[[i]]$url)$hostname, .var.name = paste0("names(x)[",i,"]"))
  }
}


#' Constructor for \code{pxweb_api_catalogue_entry}.
#' @param x an object to convert to a \code{pxweb_api_catalogue_entry} object.
#' @export 
pxweb_api_catalogue_entry <- function(x){
  UseMethod("pxweb_api_catalogue_entry")
}

#' @rdname pxweb_api_catalogue_entry
#' @export
pxweb_api_catalogue_entry.list <- function(x){
  class(x) <- c("pxweb_api_catalogue_entry", "list")
  assert_pxweb_api_catalogue_entry(x)
  x
}

#' @rdname pxweb_api_catalogue_entry
#' @param x an object to assert is a \code{pxweb_api_catalogue_entry}.
#' @keywords internal
assert_pxweb_api_catalogue_entry <- function(x){
  checkmate::assert_class(x, "pxweb_api_catalogue_entry")
  checkmate::assert_names(names(x), must.include = c("description", "url", "version", "lang"))
  if(!is.null(x$alias)){
    checkmate::assert_character(x$alias, min.chars = 2)
  }
  checkmate::assert_string(x$description, min.chars = 2)
  checkmate::assert_string(x$url, pattern = "\\[version\\]")
  checkmate::assert_string(x$url, pattern = "\\[lang\\]")
  checkmate::assert_string(x$url, pattern = "^http")
  checkmate::assert_character(x$version, min.chars = 1)
  checkmate::assert_character(x$lang, min.chars = 1)
  if(!is.null(x$period_in_seconds)){
    checkmate::assert_int(x$period_in_seconds, lower = 1)
  }
  if(!is.null(x$calls_per_period)){
    checkmate::assert_int(x$calls_per_period, lower = 1)
  }
  if(!is.null(x$max_values_to_download)){
    checkmate::assert_int(x$max_values_to_download, lower = 1)
  }
}


#' Print a catalogue entry
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @export
print.pxweb_api_catalogue_entry <- function(x, ...){
  cat("Api:", httr::parse_url(x$url)$hostname)
  cat("\n    ", x$description, "\n")
  if(length(x$alias) > 0) {
    cat("     ('", paste(x$alias, collapse = "', '"), "')", sep="")
    cat("\n")
    } 
  cat("Version(s)   :", paste(x$version, collapse = ", "), "\n")
  cat("Language(s)  :", paste(x$lang, collapse = ", "), "\n")
  if(!is.null(x$calls_per_period) & !is.null(x$period_in_seconds) & !is.null(x$max_values_to_download)){
    cat("Limit(s)     :", x$calls_per_period ,"calls per", x$period_in_seconds, "sec.\n")
    cat("              ", x$max_values_to_download ," values per call.\n")
  }
  cat("Url template :\n", x$url, "\n")
}



#' @keywords internal
pxweb_api_catalogue_alias_table <- function(){
  pxac <- pxweb_api_catalogue()
  dfs <- list()
  for(i in seq_along(pxac)){
    dfs[[i]] <- data.frame(name = names(pxac)[i], 
                           alias = c(names(pxac)[i], pxac[[i]]$alias),
                           idx = i)
  }
  do.call(rbind, dfs)
}

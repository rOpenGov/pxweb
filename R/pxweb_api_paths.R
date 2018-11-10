#' Get the api name, rootpath, subpath and path
#' 
#' @details 
#' The PXWEB API contain the following path:
#' API-NAME/API-VERSION/LANGUAGE/DATABASE-ID/<LEVELS>/TABLE-ID
#' 
#' The full url is made up by the \code{rootpath}, \code{subpath}, and \code{path}.
#' The \code{rootpath} is made up of the protocol and the API-NAME / hostname and protocol (if any).
#' 
#' The \code{subpath} contain the API-VERSION and LANGUAGE but can contain other parts as well. 
#' The subpath is the shortest the config can be called for. It can be seen as the base for the API.
#'
#' The \code{dbpath}, the data base path, contain DATABASE-ID/<LEVELS>/TABLE-ID.
#' 
#' The \code{path}, is the standar path of an url, i.e. \code{subpath} + \code{dbpath}.
#'
#' No path ends with slash, but \code{subpath} and \code{dbpath} may begin with slash,
#' see the parameters
#' 
#' @param x object to get the name or path for
#' @param init_slash should \code{subpath} and \code{path} start with a \code{/}. Default is \code{TRUE}.
#' @param as_vector should \code{subpath} and \code{path} be a vector split by /. Default is \code{FALSE}.
#' 
#' @keywords internal
pxweb_api_name <- function(x){
  UseMethod("pxweb_api_name")
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_name.url <- function(x){
  checkmate::assert_class(x, "url")
  x <- x$hostname
  assert_path(x)
  x
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_name.pxweb <- function(x){
  checkmate::assert_class(x, "pxweb")
  pxweb_api_name(x$url)
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_name.pxweb_explorer <- function(x){
  checkmate::assert_class(x, "pxweb_explorer")
  pxweb_api_name(x$pxweb)
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_rootpath <- function(x){
  UseMethod("pxweb_api_rootpath")
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_rootpath.url <- function(x){
  checkmate::assert_class(x, "url")
  x$path <- ""
  p <- build_pxweb_url(x)
  assert_path(p)
  p
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_rootpath.pxweb <- function(x){
  checkmate::assert_class(x, "pxweb")
  pxweb_api_rootpath(x$url)
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_rootpath.pxweb_explorer <- function(x){
  checkmate::assert_class(x, "pxweb_explorer")
  pxweb_api_rootpath(x$pxweb)
}



#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_subpath <- function(x, init_slash = TRUE, as_vector = FALSE){
  checkmate::assert_flag(init_slash)
  checkmate::assert_flag(as_vector)
  UseMethod("pxweb_api_subpath")
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_subpath.pxweb <- function(x, init_slash = TRUE, as_vector = FALSE){
  if(as_vector) {
    return(x$paths$api_subpath$vector)
  } 
  if(init_slash) {
    p <- paste0("/", x$paths$api_subpath$path)
  } else {
    p <- x$paths$api_subpath$path
  }
  assert_path(p)
  p
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_subpath.pxweb_explorer <- function(x, init_slash = TRUE, as_vector = FALSE){
  pxweb_api_subpath(x$pxweb, init_slash, as_vector)
}




#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_path <- function(x, init_slash = TRUE, as_vector = FALSE){
  checkmate::assert_flag(init_slash)
  checkmate::assert_flag(as_vector)
  UseMethod("pxweb_api_path")
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_path.url <- function(x, init_slash = TRUE, as_vector = FALSE){
  p <- x$path
  if(as_vector) {
    return(strsplit(p, "/")[[1]])
  } 
  if(init_slash) {
    p <- paste0("/", p)
  } 
  assert_path(p)
  p
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_path.pxweb <- function(x, init_slash = TRUE, as_vector = FALSE){
  pxweb_api_path(x$url, init_slash, as_vector)
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_path.pxweb_explorer <- function(x, init_slash = TRUE, as_vector = FALSE){
  pxweb_api_path(x$pxweb, init_slash, as_vector)
}



#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_dbpath <- function(x, init_slash = TRUE, as_vector = FALSE){
  checkmate::assert_flag(init_slash)
  checkmate::assert_flag(as_vector)
  UseMethod("pxweb_api_dbpath")
}

#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_dbpath.pxweb <- function(x, init_slash = TRUE, as_vector = FALSE){
  p1 <- pxweb_api_subpath(x, as_vector = TRUE)
  p2 <- pxweb_api_path(x, as_vector = TRUE)
  checkmate::assert_set_equal(p1, p2[1:length(p1)])
  pv <- p2[(length(p1)+1):length(p2)]
  
  if(as_vector) {
    return(pv)
  } 
  p <- paste(pv, collapse = "/")
  if(init_slash) {
    p <- paste0("/", p)
  } 
  assert_path(p)
  p
}


#' @rdname pxweb_api_name
#' @keywords internal
pxweb_api_path.pxweb_explorer <- function(x, init_slash = TRUE, as_vector = FALSE){
  pxweb_api_path(x$pxweb, init_slash, as_vector)
}

#' @rdname pxweb_api_name
#' @keywords internal
assert_path <- function(x){
  checkmate::assert_string(x)
}

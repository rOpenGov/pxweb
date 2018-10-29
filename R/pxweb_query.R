#' PXWEB query constructor
#'
#' @description 
#' Creates a pxweb query object from either a list with named values, 
#' json query file or json query string. See examples.
#'
#' @param x an object to cast as a pxweb_query object.
#' 
#' @examples 
#' dims <- list(Alue = c("*"),
#'              "Asuntokunnan koko" = c("*"),
#'              Talotyyppi = c("S"),
#'              Vuosi = c("*"))
#' pxq1 <- pxweb_query(dims)
#' json_query <- file.path(system.file(package = "pxweb"), "extdata", "examples", "json_query_example.json")
#' pxq2 <- pxweb_query(json_query)
#'
#' @export
pxweb_query <- function(x){
  UseMethod("pxweb_query")
}

#' @rdname pxweb_query
#' @keywords internal
pxweb_query.character <- function(x){
  obj <- jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
  class(obj) <- c("pxweb_query", "list")
  obj$response$format <- "json"
  assert_pxweb_query(obj)
  obj
}

#' @rdname pxweb_query
#' @keywords internal
pxweb_query.pxweb_query <- function(x){
  return(x)
}

#' @rdname pxweb_query
#' @keywords internal
pxweb_query.list <- function(x){
  checkmate::assert_named(x)
  obj <- list(query = list(),
              response = list(format = "json"))
  for(i in seq_along(x)){
    obj$query[[i]] <- list(code = names(x)[i],
                           selection = list(filter = "item",
                                            values = x[[i]]))
    if(x[[i]] == "*") {
      obj$query[[i]]$selection$filter <- "all"
    }
  }
  class(obj) <- c("pxweb_query", "list")
  assert_pxweb_query(obj)
  obj
}


#' Assert a pxweb_query object
#' 
#' @param x an object to assert conferms to the structure of an pxweb_query object.
#' 
#' @keywords internal
assert_pxweb_query <- function(x){
  checkmate::assert_class(x, c("pxweb_query", "list"), .var.name = "pxweb_query")
  checkmate::assert_names(names(x), must.include = c("query", "response"), .var.name = "names(pxweb_query)")
  checkmate::assert_names(names(x$response), must.include = c("format"))
  checkmate::assert_choice(x$response$format, c("json"))

  checkmate::assert_named(x$query, "unnamed")
  for(i in seq_along(x$query)){
    checkmate::assert_character(x$query[[i]]$code, any.missing = FALSE, min.len = 1)
    checkmate::assert_names(names(x$query[[i]]$selection), must.include = c("filter", "values"))
    checkmate::assert_character(x$query[[i]]$selection$filter, len = 1, any.missing = FALSE)
    checkmate::assert_character(x$query[[i]]$selection$values)
    checkmate::assert_named(x$query[[i]]$selection$values, type = "unnamed")
  }
  
  # Assert filter values
  for(i in seq_along(x$query$selection$filter)){
    filter_aggr <- FALSE
    if(grepl(x = x$query$selection$filter[i], "^[Vv]s:.*")){
      filter_aggr <- TRUE
    }
    if(grepl(x = x$query$selection$filter[i], "^[Aa]gg:.*")){
      filter_aggr <- TRUE
    }
    if(!filter_aggr) {
      checkmate::assert_choice(x$query$selection$filter[i], choices = c("item", "all", "top", "agg:[aggregated values]", "vs:[other value set]"))
      if(x$query$selection$filter[i] %in% c("all", "top")){
        checkmate::assert_character(x$query$selection$values[[i]], len = 1, .var.name = paste0("x$query$selection$values[[", i, "]]"))

      }
    }
  }
}


#pxweb_query_toJSON <- function(x){
#  checkmate::assert_class(x, "pxweb_query")
#  jsonlite::toJSON(x, auto_unbox = TRUE)
#}


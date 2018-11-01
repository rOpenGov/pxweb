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
#' json_query <- file.path(system.file(package = "pxweb"), 
#'                         "extdata", "examples", "json_query_example.json")
#' pxq2 <- pxweb_query(json_query)
#' 
#' @keywords internal
#' @export
pxweb_query <- function(x){
  UseMethod("pxweb_query")
}

#' @rdname pxweb_query
#' @keywords internal
#' @export
pxweb_query.character <- function(x){
  obj <- jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
  class(obj) <- c("pxweb_query", "list")
  obj$response$format <- "json"
  assert_pxweb_query(obj)
  obj
}

#' @rdname pxweb_query
#' @keywords internal
#' @export
pxweb_query.pxweb_query <- function(x){
  return(x)
}

#' @rdname pxweb_query
#' @keywords internal
#' @export
pxweb_query.list <- function(x){
  checkmate::assert_named(x)
  obj <- list(query = list(),
              response = list(format = "json"))
  for(i in seq_along(x)){
    obj$query[[i]] <- list(code = names(x)[i],
                           selection = list(filter = "item",
                                            values = x[[i]]))
    if(x[[i]][1] == "*") {
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
  for(i in seq_along(x$query)){
    filter_aggr <- FALSE
    if(grepl(x = x$query[[i]]$selection$filter, "^[Vv]s:.*")){
      filter_aggr <- TRUE
    }
    if(grepl(x = x$query[[i]]$selection$filter, "^[Aa]gg:.*")){
      filter_aggr <- TRUE
    }
    if(!filter_aggr) {
      checkmate::assert_choice(x$query[[i]]$selection$filter, choices = c("item", "all", "top", "agg:[aggregated values]", "vs:[other value set]"))
      if(x$query[[i]]$selection$filter %in% c("all", "top")){
        checkmate::assert_character(x$query[[i]]$selection$values, len = 1, .var.name = paste0("x$query[[", i, "]]$selection$values"))
      }
    }
  }
}



#' @export
print.pxweb_query <- function(x, ...){
  cat("PXWEB QUERY\n")
  cat("query:\n")
  for(i in seq_along(x$query)){
    cat(" [[", i ,"]] ",  x$query[[i]]$code," (", x$query[[i]]$selection$filter, "):\n", sep = "")
    cat("   ", paste(x$query[[i]]$selection$values, collapse = ", "), "\n", sep = "")    
  }
}



#' Validate a pxweb_query with pxweb_metadata
#' 
#' @details 
#' Assert that all query values exist in metadata.
#' 
#' @param pxq a \code{pxweb_query} object.
#' @param pxmd a \code{pxweb_metadata} object.
#' 
#' @keywords internal
pxweb_validate_query_with_metadata <- function(pxq, pxmd){
  checkmate::assert_class(pxq, "pxweb_query")
  checkmate::assert_class(pxmd, "pxweb_metadata")
  
  pxweb_metadata_variables <- unlist(lapply(pxmd$variables, function(x) x$code))
  for(i in seq_along(pxq$query)){
    pxweb_query_variable_code <- pxq$query[[i]]$code
    checkmate::assert_choice(pxweb_query_variable, choices = pxweb_metadata_variables)
    if(tolower(pxq$query[[i]]$selection$filter) == "item"){
      pxweb_query_variable_values <- pxq$query[[i]]$selection$values
      meta_idx <- which(pxweb_metadata_variables %in% pxweb_query_variable_code)
      pxweb_metadata_variable_values <- pxmd$variables[[meta_idx]]$values
      checkmate::assert_subset(pxweb_query_variable_values, choices = pxweb_metadata_variable_values)
    }
  }
}


#' Add metadata to query
#' 
#' @details 
#' Add metadata values to variables with a query with filter "all".
#' 
#' @param pxq a \code{pxweb_query} object.
#' @param pxmd a \code{pxweb_metadata} object.
#' 
#' @keywords internal
pxweb_add_metadata_to_query <- function(pxq, pxmd){
  checkmate::assert_class(pxq, "pxweb_query")
  checkmate::assert_class(pxmd, "pxweb_metadata")

  # Set values to filter "all"
  pxweb_metadata_variables <- unlist(lapply(pxmd$variables, function(x) x$code))
  for(i in seq_along(pxq$query)){
    pxweb_query_variable_code <- pxq$query[[i]]$code
    checkmate::assert_choice(pxweb_query_variable_code, choices = pxweb_metadata_variables)
    if(tolower(pxq$query[[i]]$selection$filter) == "all"){
      px_pattern <- pxq$query[[i]]$selection$values
      px_pattern <- gsub(pattern = "\\*", replacement = "\\.\\+", px_pattern)
      meta_data_values <- pxmd$variables[[which(pxweb_metadata_variables %in% pxweb_query_variable_code)]]$values
      meta_data_values[grepl(x = meta_data_values, pattern = px_pattern)]
      pxq$query[[i]]$selection$values <- meta_data_values
      pxq$query[[i]]$selection$filter <- "item"
    }
  }
  
  assert_pxweb_query(pxq)
  pxq
}
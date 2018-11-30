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
pxweb_query.json <- function(x){
  pxweb_query(as.character(x))
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

#' @rdname pxweb_query
#' @keywords internal
#' @export
pxweb_query.pxweb_explorer <- function(x){
  checkmate::assert_true(pxe_position_is_full_query(x))
  md_ch <- pxe_metadata_choices(x)
  mdo <- pxe_pxobj_at_position(x)
  mdo_vnm <- pxweb_metadata_dim(mdo)
  
  obj <- list()
  for(i in seq_along(mdo$variables)){
    var_nm <- mdo$variables[[i]]$code
    if(md_ch[[var_nm]][1] == "eliminate"){
      next
    }
    obj[[var_nm]] <- mdo$variables[[i]]$values[md_ch[[var_nm]]]
  }
  pxq <- pxweb_query(obj)
  pxweb_validate_query_with_metadata(pxq, mdo)
  pxq
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
    checkmate::assert_choice(pxweb_query_variable_code, choices = pxweb_metadata_variables)
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


#' Compue the dimension of the query
#' 
#' @param pxq a \code{pxweb_query} object.
#' 
#' @keywords internal
pxweb_query_dim <- function(pxq){
  checkmate::assert_class(pxq, "pxweb_query")
  dim_res <- numeric(length(pxq$query))
  for(i in seq_along(pxq$query)){
    names(dim_res)[i] <- pxq$query[[i]]$code
    if(tolower(pxq$query[[i]]$selection$filter) == "top"){
      dim_res[i] <- as.numeric(pxq$query[[i]]$selection$values)
    } else if(tolower(pxq$query[[i]]$selection$filter) == "all"){
      stop("Cannot compute the dimension for a variable with filter 'all'.", call. = FALSE)
    } else {
      dim_res[i] <- length(pxq$query[[i]]$selection$values)
    }
  }
  dim_res
}


#' Get query filter
#' 
#' @param pxq a \code{pxweb_query} object.
#' 
#' @return query variable selection values as a named list of character vectors.
#' 
#' @keywords internal
pxweb_query_values <- function(pxq){
  checkmate::assert_class(pxq, "pxweb_query")
  res <- lapply(pxq$query,function(x) x$selection$values)   
  names(res) <- unlist(lapply(pxq$query,function(x) x$code))   
  res
}


#' Get query values
#' 
#' @param pxq a \code{pxweb_query} object.
#' 
#' @return query variable selection filters as a named character vector.
#' 
#' @keywords internal
pxweb_query_filter <- function(pxq){
  checkmate::assert_class(pxq, "pxweb_query")
  res <- unlist(lapply(pxq$query,function(x) x$selection$filter)) 
  names(res) <- unlist(lapply(pxq$query,function(x) x$code))   
  res
}


#' Convert a \code{pxweb_query} object to a json object
#' @param pxq a \code{pxweb_query} object.
#' @param ... further argument to \code{jsonlite::toJSON()}.
#' @export
pxweb_query_as_json <- function(pxq, ...){
  checkmate::assert_class(pxq, "pxweb_query")
  pxq$response$format <- jsonlite::unbox(pxq$response$format)
  for(i in seq_along(pxq$query)){
    pxq$query[[i]]$code <- jsonlite::unbox(pxq$query[[i]]$code)
    pxq$query[[i]]$selection$filter <- jsonlite::unbox(pxq$query[[i]]$selection$filter)
  }
  jsonlite::toJSON(pxq, ...)
}

#' Convert a \code{pxweb_query} object to R code
#' @details One element per row is returned.
#' @param pxq a \code{pxweb_query} object.
#' @keywords internal
pxweb_query_as_rcode <- function(pxq){
  checkmate::assert_class(pxq, "pxweb_query")
  
  res <- character(length(pxq$query))
  for(i in seq_along(pxq$query)){
    res[i] <- paste0("\"", pxq$query[[i]]$code, "\"=c(", paste(paste0("\"", pxq$query[[i]]$selection$values, "\""), collapse = ","), ")")
    if(i == 1){
      res[i] <- paste0("  list(", res[i])
    } else {
      res[i] <- paste0("       ", res[i])
    }
  }
  res[length(res)] <- paste0(res[length(res)], ")")
  if(length(res) > 1){
    res[-length(res)] <- paste0(res[-length(res)], ",")
  }
  res <- c("pxweb_query_list <- ", res)

  res
}

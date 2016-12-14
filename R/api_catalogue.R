#' Get the api catalogue
#' 
#' @details
#' Get the catalogue of api and store it as a list.
#' 
#' @examples
#' api_catalogue()
#' 
#' @export
api_catalogue <- function(){
  apis <- names(get_api_list())
  api_catalogue <- list()
  for(api in seq_along(apis)){
    api_catalogue[[api]] <- pxweb_api$new(apis[api])
  }
  return(api_catalogue)
}



#' Get the api catalogue from json file
#' 
#' @param raw Get the raw list from the api json file. 
#' 
#' @keywords internal 
#' 
#' @return api_list object
#' 
get_api_list <- function(raw = FALSE){
  api_file <- system.file("extdata/api.json", package = "pxweb")
  api_raw <- RJSONIO::fromJSON(api_file)
  if(raw) return(api_raw)
  api_list <- c(api_raw$apis, api_raw$local_apis)
  api_list
}



#' Write api catalogue to json file
#' 
#' @keywords internal 
#' 
#' @param api_list api list in raw format to write to json file.
write_api_list <- function(api_list){
  dest <- system.file("extdata/api.json", package = "pxweb")
  writeLines(RJSONIO::toJSON(api_list), con = dest)
}

#' Get index of which api has the name or the alias
#'
#' @param api_name pxweb api name or alias to lookup.
#' @param api_list pxweb api list created with \code{get_api_list()}
#' 
#' @keywords internal 
#' 
get_api_index<- function(api_name, api_list){
  list_to_check <- 
    mapply(c, 
           as.list(names(api_list)),  
           lapply(api_list, function(X) unlist(X$alias)))  
  in_list <- unlist(lapply(list_to_check, function(X) any(X %in% api_name)))
  if(sum(in_list) > 1) warning("Alias exists in multiple pxweb apis.")
  if(sum(in_list) == 0) stop("API do not exist in api catalogue.")
  which(in_list)[1]
}

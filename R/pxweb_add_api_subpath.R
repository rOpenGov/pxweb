#' Add the subpath slot to a pxweb path slot
#' 
#' @details 
#' Queries the path from pos 1 and up until a config is returned.
#' 
#' @param obj an object to add subpath to
#' 
#' @keywords internal
pxweb_add_api_subpath <- function(obj){
  assert_pxweb_url(obj)
  
  if(inherits(obj, "pxweb")){
    return(obj)
  }
  
  if(file.exists(obj$paths$rda_file_path)){
    obj$paths$api_subpath <- load_pxweb_api_subpath(obj)
    return(obj)
  }
  
  path_splt <- strsplit(obj$url$path, "/")[[1]]  
  tmp_url <- obj$url
  
  # Split up url to api parts
  for(p in 1:length(path_splt)){
    obj <- pxweb_add_call(obj)
    tmp_url$path <- paste(path_splt[1:p], collapse = "/")
    tmp_cfg_url <- build_pxweb_config_url(tmp_url)
    tmp_r <- httr::GET(tmp_cfg_url)
    if(is_pxweb_config_response(tmp_r)) break()
  }
  
  # Add the subpath
  obj$paths$api_subpath <- tmp_url$path
  
  obj
}
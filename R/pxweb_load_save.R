save_pxweb <- function(obj){
  checkmate::assert_class(obj, "pxweb")
  save(obj, file = obj$paths$rda_file_path, compress = FALSE)
}

load_pxweb <- function(obj){
  checkmate::assert_class(obj, "pxweb")
  load_path <- obj$paths$rda_file_path
  rm(obj)
  load(load_path)
  obj
}

load_pxweb_calls <- function(obj){
  checkmate::assert_class(obj, "pxweb")
  load_pxweb(obj)$calls
}

load_pxweb_config <- function(obj){
  checkmate::assert_class(obj, "pxweb")
  load_pxweb(obj)$config
}

#' Clear cache of all (or one) \code{pxweb} object
#'
#' @details 
#' Clean the cache and sleep to restore api timing limit.
#'
#' @param x a \code{pxweb} object to clear cache for. Default is NULL (clear everything).
#' 
#' @keywords internal
pxweb_clear_cache<- function(x = NULL){
  if(is.null(x)){
    files <- dir(pxweb_tempdir(), full.names = TRUE)
    if(length(files) == 0) {
      return(FALSE)
    }
    sleep_s <- numeric(length(files))
    for(f in seq_along(files)){
      obj <- NULL # To capture a builde NOTE and safeguard for global variable handling
      load(files[f])
      time_diff_s <- as.numeric(Sys.time()) - as.numeric(obj$calls$time_stamps[[1]])
      sleep_s[f] <- max(obj$config$period_in_seconds - time_diff_s, 0)
    }
    Sys.sleep(max(sleep_s))
    file.remove(files)
    return(TRUE)
  } else {
    checkmate::assert_class(x, "pxweb")
    time_diff_s <- as.numeric(Sys.time()) - as.numeric(x$calls$time_stamps[[1]])
    sleep_s <- max(x$config$period_in_seconds - time_diff_s, 0)
    Sys.sleep(sleep_s)
    if(file.exists(x$paths$rda_file_path)) file.remove(x$paths$rda_file_path)
    return(TRUE)
  }
}
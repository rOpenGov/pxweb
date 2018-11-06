#' Test a full or a part of an pxweb api.
#' 
#' @description
#' The function can be used to test a whole pxweb api by using the api base url.
#' By using a branch in a tree the api is tested below this branch.
#' 
#' @param url The base url to the pxweb api (or a branch of the metadata tree)
#' @param test_type What type of test should be done. 
#'                  The \code{first} observation of each table.
#'                  A random \code{sample} of size \code{n}.
#'                  Download all \code{full} tables. 
#' @param n sample size if \code{test_type} is \code{sample}.
#' 
#' @return 
#' Function returns a data.frame with information on each node
#' Two variables are added: 
#' \code{checked} : The node has been checked
#' \code{error} : Whether there were errors encountered with the call
#' \code{download_error} : Whether there were errors encountered during download
#' 
#' @examples 
#' \dontrun{
#'   url <- "http://bank.stat.gl/api/v1/en/Greenland/BE/BE01"
#'   res <- pxweb_test_api_endpoint(url)
#' }
#' 
#' @export
pxweb_test_api_endpoint <- function(url, test_type="first", n = 1, verbose = TRUE){
  px <- pxweb(url)
  checkmate::assert_choice(test_type, c("first", "sample", "full"))
  checkmate::assert_int(n, lower = 1)
  
  # Build treestructure
  api_tree_df <- pxweb_get_api_test_data_frame(px)
  i <- 0
  while (i < nrow(api_tree_df)) {
    i <- i + 1
    if(api_tree_df$type[i] == "t") {
      next
    }
    
    px <- try(pxweb(api_tree_df$path[i]), silent = TRUE)
    if(inherits(px, "try-error")) {
      api_tree_df$error[i] <- TRUE
      api_tree_df$checked[i] <- TRUE
    }
    
    tmp_df <- try(pxweb_get_api_test_data_frame(px), silent = TRUE)
    if(inherits(tmp_df, "try-error")) {
      api_tree_df$error[i] <- TRUE
      api_tree_df$checked[i] <- TRUE
    }
    
    api_tree_df <- rbind(api_tree_df, tmp_df)
    api_tree_df$checked[i] <- TRUE
  }
  if(verbose){
    cat("PXWEB API CONTAIN:\n")
    cat(sum(api_tree_df$type == "l"), "node(s) and", sum(api_tree_df$type == "t"), "table(s).\n")
    cat("Testing to download data...")
    pb <- utils::txtProgressBar(min = 0, max = nrow(api_tree_df), style = 3)
  }

  # Get metadata and test download
  api_tree_df$obs <- 0
  for(i in 1:nrow(api_tree_df)){
    if(verbose) {
      utils::setTxtProgressBar(pb, i)
    }
    if(api_tree_df$checked[i]) {
      next
    }
    px_obj <- try(pxweb_get(api_tree_df$path[i], verbose = FALSE))
    if(inherits(px_obj, "try-error")) {
      api_tree_df$error[i] <- TRUE
      api_tree_df$checked[i] <- TRUE
    }
    
    api_tree_df$obs[i] <- prod(pxweb_metadata_dim(px_obj))
    
    pxq <- list()
    for(j in seq_along(px_obj$variables)){
      values <- px_obj$variables[[j]]$values
      if(test_type == "first"){
        values <- px_obj$variables[[j]]$values[1]
      }
      if(test_type == "sample"){
        values <- sample(values, size = min(n, length(values)))
      }
      pxq[[px_obj$variables[[j]]$code]] <- values
    }
    px_dat <- try(pxweb_get(api_tree_df$path[i], pxweb_query(pxq), verbose = FALSE))
    if(inherits(px_dat, "try-error")) {
      api_tree_df$error[i] <- TRUE
      api_tree_df$checked[i] <- TRUE
      api_tree_df$download_error[i] <- TRUE
    }
    api_tree_df$checked[i] <- TRUE    
  }
  
  if(verbose) {
    close(pb)
  }
  
  return(api_tree_df)
}

#' Build api test data.frame
#' 
#' @param x a pxweb object
#' @keywords internal
pxweb_get_api_test_data_frame <- function(x){
  checkmate::assert_class(x, "pxweb")
  df <- as.data.frame(pxweb_get(x))
  df$path <- paste0(build_pxweb_url(x), "/", df$id)
  df$checked <- FALSE
  df$error <- FALSE
  df$download_error <- FALSE
  df
}


#' Test a full or a part of a PXWEB api.
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
#'                  \code{touch} the api by only downloading the first table metadata.
#'                  This is minimal testing of the API.
#' @param n sample size if \code{test_type} is \code{sample}.
#' @param verbose The function will print information.
#' @param time_limit Time limit in second the API is allowed to be tested.
#'
#' @return
#' Function returns a data.frame with information on each node
#' Two variables are added:
#' \code{checked} : The node has been checked
#' \code{error} : Whether there were errors encountered with the call
#' \code{download_error} : Whether there were errors encountered during download
#'
#'
#' @examples
#' \dontrun{
#' url <- "https://bank.stat.gl/api/v1/en/Greenland/BE/BE01"
#' res <- pxweb_test_api(url)
#' res <- pxweb_test_api(url, test_type = "touch")
#' }
#' @export
pxweb_test_api <- function(url, test_type = "first", n = 1, verbose = TRUE, time_limit = Inf) {
  px <- pxweb(url)
  checkmate::assert_choice(test_type, c("first", "sample", "full", "touch"))
  checkmate::assert_int(n, lower = 1)
  # time_limit_obj <- pxweb:::pxweb_test_time_limit(time_limit = time_limit)
  time_limit_obj <- pxweb_test_time_limit(time_limit = time_limit)

  # Build treestructure
  # api_tree_df <- pxweb:::pxweb_get_api_test_data_frame(px)
  api_tree_df <- pxweb_get_api_test_data_frame(px)
  if (verbose) {
    cat("\nA PXWEB API IS IDENTIFIED:\n")
    cat(url, "\n")
    cat("Exploring nodes...\n")
  }

  i <- 0
  while (i < nrow(api_tree_df)) {
    i <- i + 1
    if (is_test_time_limit_reached(time_limit_obj)) {
      break
    }

    types_idx_to_check <- i:nrow(api_tree_df)
    is_table <- api_tree_df$type[types_idx_to_check] == "t"
    if (test_type == "touch" && any(api_tree_df$type[i:nrow(api_tree_df)] == "t")) {
      if (verbose) {
        cat("Table identified.\n")
      }
      touch_idx <- types_idx_to_check[is_table][1]
      break
    }

    if (verbose) {
      cat(sum(api_tree_df$type == "l"), "node(s) and", sum(api_tree_df$type == "t"), "table(s). Checking entry", i, "of", nrow(api_tree_df), "...\n")
    }

    if (api_tree_df$type[i] == "t") {
      next
    }

    px <- try(pxweb(url = api_tree_df$path[i]), silent = TRUE)
    if (inherits(px, "try-error")) {
      api_tree_df$error[i] <- TRUE
      api_tree_df$checked[i] <- TRUE
    }

    tmp_df <- try(pxweb_get_api_test_data_frame(x = px), silent = TRUE)
    if (inherits(tmp_df, "try-error")) {
      api_tree_df$error[i] <- TRUE
      api_tree_df$checked[i] <- TRUE
    }

    api_tree_df$checked[i] <- TRUE

    if (test_type == "touch" & !inherits(tmp_df, "try-error")) {
      # Jump to next level
      i <- nrow(api_tree_df)
    }

    if (!inherits(tmp_df, "try-error")) {
      api_tree_df <- rbind(api_tree_df, tmp_df)
    }
  }

  if (verbose & !test_type == "touch") {
    cat("PXWEB API CONTAIN:\n")
    cat(sum(api_tree_df$type == "l"), "node(s) and", sum(api_tree_df$type == "t"), "table(s) in total.\n")
    cat("Downloading data...\n")
    pb <- utils::txtProgressBar(min = 0, max = nrow(api_tree_df), style = 3)
  }

  # Get metadata and test download
  api_tree_df$obs <- 0
  for (i in 1:nrow(api_tree_df)) {
    if (is_test_time_limit_reached(time_limit_obj)) {
      if (verbose) {
        cat("\nTime limit reached. Aborting.\n")
      }
      break
    }

    if (verbose & !test_type == "touch") {
      utils::setTxtProgressBar(pb, i)
    } else if (test_type == "touch") {
      i <- touch_idx # Handle that only few idx has been checked in touch
    }
    if (api_tree_df$checked[i]) {
      next
    }

    px_obj <- try(pxweb_get(api_tree_df$path[i], verbose = FALSE), silent = TRUE)
    if (inherits(px_obj, "try-error")) {
      api_tree_df$error[i] <- TRUE
      api_tree_df$checked[i] <- TRUE
      api_tree_df$obs[i] <- NA
      next
    }

    api_tree_df$obs[i] <- prod(pxweb_metadata_dim(px_obj))

    pxq <- list()
    for (j in seq_along(px_obj$variables)) {
      values <- px_obj$variables[[j]]$values
      if (test_type == "first" | test_type == "touch") {
        values <- px_obj$variables[[j]]$values[1]
      }
      if (test_type == "sample") {
        values <- sample(values, size = min(n, length(values)))
      }
      pxq[[px_obj$variables[[j]]$code]] <- values
    }
    px_dat <- try(pxweb_get(api_tree_df$path[i], pxweb_query(pxq), verbose = FALSE), silent = TRUE)
    if (inherits(px_dat, "try-error")) {
      api_tree_df$error[i] <- TRUE
      api_tree_df$checked[i] <- TRUE
      api_tree_df$download_error[i] <- TRUE
      next
    }
    api_tree_df$checked[i] <- TRUE
    if (test_type == "touch") {
      if (verbose) {
        cat("Table touched.\n")
      }
      break
    }
  }

  if (verbose & !test_type == "touch") {
    close(pb)
  }

  return(api_tree_df)
}

#' Build api test data.frame
#'
#' @param x a pxweb object
#' @keywords internal
pxweb_get_api_test_data_frame <- function(x) {
  checkmate::assert_class(x, "pxweb")
  df <- as.data.frame(pxweb_get(x))
  if (is.null(df$updated)) {
    df$updated <- NA
  }
  df$path <- paste0(build_pxweb_url(x), "/", df$id)
  df$checked <- FALSE
  df$error <- FALSE
  df$download_error <- FALSE
  df
}



#' Test time limit object
#'
#' @details
#' Used to limit testing of API:s.
#' @param time_limit the number of seconds the API will be tested.
#' @keywords internal
pxweb_test_time_limit <- function(time_limit) {
  checkmate::assert_number(time_limit, lower = 1)
  x <- list(start_time = Sys.time(), time_limit = time_limit)
  class(x) <- c("pxweb_test_time_limit", "list")
  return(x)
}

#' @rdname pxweb_test_time_limit
#' @keywords internal
is_test_time_limit_reached <- function(x) {
  checkmate::assert_class(x, "pxweb_test_time_limit")
  time_diff <- as.numeric(Sys.time() - x$start_time, units = "secs")
  time_diff > x$time_limit
}


#' Create all paths from a list of pxweb_api_catalogue entries
#' @param apis a list of pxweb_api_catalogue_entry elements
#' @keywords internal
pxweb_test_create_api_paths <- function(apis) {
  checkmate::assert_class(apis, "list")
  for (i in seq_along(apis)) {
    checkmate::assert_class(apis[[i]], "pxweb_api_catalogue_entry")
  }
  api_idx <- numeric(0)
  api_paths <- character(0)
  for (i in seq_along(apis)) {
    for (j in seq_along(apis[[i]]$version)) {
      for (k in seq_along(apis[[i]]$lang)) {
        api_idx[length(api_idx) + 1] <- i
        base_url <- apis[[i]]$url
        base_url <- gsub("\\[version\\]", base_url, replacement = apis[[i]]$version[j])
        base_url <- gsub("\\[lang\\]", base_url, replacement = apis[[i]]$lang[k])
        api_paths[length(api_paths) + 1] <- base_url
      }
    }
  }
  list(idx = api_idx, paths = api_paths)
}

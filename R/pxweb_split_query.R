#' Split query in optimal sub-queries
#' 
#' @details 
#' Computes (brute-force) the optimal split of a query to
#' match the api maximum value limit. It also take into account 
#' that time variables and content variables should not be split.
#' Also variables with filter "top" should not be split, since
#' the top filter does not supply the individual levels, just a 
#' number. This can probably be improved further.
#' 
#' @param pxq a \code{pxweb_query} object.
#' @param px a \code{pxweb} object.
#' @param pxweb_metadata a \code{pxweb_metadata} object.
#' 
#' @return a list with \code{pxweb_query} objects.
#' 
#' @keywords internal
pxweb_split_query <- function(pxq, px, pxmd){
  checkmate::assert_class(pxq, "pxweb_query")
  checkmate::assert_class(px, "pxweb")
  checkmate::assert_class(pxmd, "pxweb_metadata")
  
  pxqd <- pxweb_query_dim(pxq)
  # Get variables that can be split
  pxqds <- pxweb_query_dim_splittable(pxq, pxmd)
  mxv <- px$config$max_values_to_download
  
  # If able to download in one batch
  if(prod(pxqd) <= mxv) return(list(pxq))
  
  # Search through optimal combination
  comb <- generate_permutations(which(pxqds))
  no_comb <- matrix(which(!pxqds), nrow = nrow(comb), ncol = sum(!pxqds), byrow = TRUE)
  comb <- cbind(comb, no_comb)
  batches <- numeric(nrow(comb))
  for(i in 1:nrow(comb)){
    batches[i] <- split_dimensions_left_right(x = pxqd[comb[i,]], bool = pxqds[comb[i,]], max_size = mxv)$total_batches
  }
  min_comb <- which.min(batches)
  batch_structure <- split_dimensions_left_right(x = pxqd[comb[min_comb,]], bool = pxqds[comb[min_comb,]], max_size = mxv)


  # Create grid of possible variable permutations
  pxq_vals <- pxweb_query_values(pxq)
  value_idx_list <- list()
  value_labels_list <- list()
  for(i in seq_along(pxqd)){
    if(batch_structure$no_of_splits[i] > 1){
      var_name <- names(batch_structure$no_of_splits)[i]
      value_idx_list[[var_name]] <- 1:batch_structure$no_of_splits[i]
      
      value_labels_list[[var_name]] <- list()
      for(j in 1:batch_structure$no_of_splits[i]){
        bs <- batch_structure$max_batch_size[i]
        value_labels_list[[var_name]][[j]] <- pxq_vals[[var_name]][((j-1)*bs + 1):min(j*bs, length(pxq_vals[[var_name]]))]
      }
    }
  }
  
  # Create (and check) batches
  batch_idx <- expand.grid(value_idx_list)
  pxq_list <- list()
  pxq_names <- names(pxq_vals)
  for(i in 1:nrow(batch_idx)){
    pxq_tmp <- pxq
    for(j in 1:ncol(batch_idx)){
      query_no <- which(pxq_names %in% colnames(batch_idx)[j])
      query_nm <- pxq_names[query_no]
      pxq_tmp$query[[query_no]]$selection$values <- value_labels_list[[query_nm]][[batch_idx[i,j]]]
    }
    pxq_list[[i]] <- pxq_tmp
    assert_pxweb_query(pxq_list[[i]])
  }
  pxq_list
}


#' Get vector indicating splittable variables
#' 
#' @details 
#' Splitable variables are variables that can be split. Content variables cannot be split,
#' nor variables with filter == "top".
#' 
#' Currently, we can only be sure that time variables and eliminated variables can be split.
#' Hopefully the next API makes this more clear.
#' 
#' @param pxq a \code{pxweb_query} object.
#' 
#' @return a named logical vector.
#' 
#' @keywords internal
pxweb_query_dim_splittable <- function(pxq, pxmd){
  checkmate::assert_class(pxmd, "pxweb_metadata")
  checkmate::assert_class(pxq, "pxweb_query")
  
  can_be_eliminated <- pxweb_metadata_elimination(pxmd)
  is_time_variable <- pxweb_metadata_time(pxmd)
  can_be_eliminated[is_time_variable] <- TRUE
  
  filter <- pxweb_query_filter(pxq)
  # can_be_eliminated <- can_be_eliminated[sample(1:length(can_be_eliminated))]
  spltable <- can_be_eliminated[names(filter)]
  spltable[tolower(filter) == "top"] <- FALSE
  spltable
}



#' Split variables into chunks
#' 
#' @details 
#' Splitable variables are variables that can be split. Content variables cannot be split,
#' not variables with filter == "top"
#' 
#' @param pxq a \code{pxweb_query} object.
#' 
#' @return a \code{pxweb_split_dimensions}
#' 
#' @keywords internal
split_dimensions_left_right <- function(x, bool, max_size){
  checkmate::assert_integerish(x, lower = 1)
  checkmate::assert_named(x)
  checkmate::assert_logical(bool)
  checkmate::assert_names(names(bool), identical.to = names(x))
  checkmate::assert_int(max_size, lower = 1)
  
  call_dims <- c(prod(x[!bool]), x[bool])
  for(i in seq_along(call_dims)){
    prod_value <- prod(call_dims[1:i])/max_size
    if(prod_value > 1){
      if(i == 1) {
        stop("Too large query. Variable(s) ", paste(names(x[!bool]), collapse = ", "), " cannot be split into batches (eliminate is FALSE).", call. = FALSE)
      }
      for(j in 1:call_dims[i]){
        if(prod(call_dims[1:(i-1)]) * j > max_size) break
      }
      call_dims[i] <- j - 1
    }
  }
  
  max_batch_size <- x
  max_batch_size[names(call_dims[-1])] <- call_dims[-1]
  res <- list(total_dim = x, 
              max_batch_size = max_batch_size,
              no_of_splits = ceiling(x / max_batch_size))
  res$total_batches <- prod(res$no_of_splits)
  class(res) <- c("pxweb_split_dimensions", "list")
  res
}



#' Generate batch permutations
#'
#' @details 
#' Generates permutations of dim. If more than 6 dim (highly unlikely) a sample of 1000 combinations
#' is drawn. Otherwise all possible permutations are returned.
#'
#' @param x a vector with elements to permute
#' 
#' @keywords internal
generate_permutations <- function(x){
  checkmate::assert_integerish(x, lower = 1)
  n <- length(x)
  if(n < 7){
    res <- permutations(n = n, v = x, r = n)
  } else {
    res <- matrix(0, ncol = n, nrow = 1000)
    for(i in 1:1000){
      res[i,] <- sample(x, n)
    }
  }
  res
}



#' Generate permutations of dimensions to find optimal no of batches
#'
#' @details 
#' Taken from gtools to minimize dependencies. See permutations 
#' of the gtools packages for details
#'
#' @param n See \code{gtools::permutations}.
#' @param r See \code{gtools::permutations}.
#' @param v See \code{gtools::permutations}.
#' @param nset See \code{gtools::permutations}.
#' @param repeats.allowed See \code{gtools::permutations}.
#'
#' @keywords internal
permutations <- function (n, r, v = 1:n, set = TRUE, repeats.allowed = FALSE) 
{
  if (mode(n) != "numeric" || length(n) != 1 || n < 1 || (n%%1) != 
      0) 
    stop("bad value of n")
  if (mode(r) != "numeric" || length(r) != 1 || r < 1 || (r%%1) != 
      0) 
    stop("bad value of r")
  if (!is.atomic(v) || length(v) < n) 
    stop("v is either non-atomic or too short")
  if ((r > n) & repeats.allowed == FALSE) 
    stop("r > n and repeats.allowed=FALSE")
  if (set) {
    v <- unique(sort(v))
    if (length(v) < n) 
      stop("too few different elements")
  }
  v0 <- vector(mode(v), 0)
  if (repeats.allowed) 
    sub <- function(n, r, v) {
      if (r == 1) 
        matrix(v, n, 1)
      else if (n == 1) 
        matrix(v, 1, r)
      else {
        inner <- Recall(n, r - 1, v)
        cbind(rep(v, rep(nrow(inner), n)), matrix(t(inner), 
                                                  ncol = ncol(inner), nrow = nrow(inner) * n, 
                                                  byrow = TRUE))
      }
    }
  else sub <- function(n, r, v) {
    if (r == 1) 
      matrix(v, n, 1)
    else if (n == 1) 
      matrix(v, 1, r)
    else {
      X <- NULL
      for (i in 1:n) X <- rbind(X, cbind(v[i], Recall(n - 
                                                        1, r - 1, v[-i])))
      X
    }
  }
  sub(n, r, v[1:n])
}
#' Clean raw data from PX-WEB
#' 
#' This function clean the raw data from PX-WEB to R tall format. 
#' 
#' @param data2clean Data to clean.
#' @param url url to the bottom nod (to get meta data)
#' @param dims dimension of the api-call 
#' @param content_node node with content downloaded with \link{get_pxweb_metadata}. 
#' If NULL, meta data is downloaded with \link{get_pxweb_metadata}.
#' 
#' @keywords internal 
#' 
#' @return data frame melted and in R (numeric) format
#' 

clean_pxweb <- function(data2clean, url, dims, content_node=NULL) {  

  # Assertions
  stopifnot(class(data2clean) == "data.frame")
  stopifnot(class(url) == "character")
  
  # Convert to data table
  # Store and change name to fix bug with varnames in data.table (cant have comma in varname)
  colnames(data2clean) <- str_replace_all(colnames(data2clean), pattern = ",", ";")
  head <- colnames(data2clean)
  #colnames(data2clean) <- as.character(1:length(head))
  data2clean <- as.data.table(data2clean)
    
  # Get metadata to use in creating factors of Tid and contentCode
  if(is.null(content_node)){
    contentNode <- get_pxweb_metadata(url)
  } else {
    contentNode <- content_node
  }
   
  # Collect factor labels for tid and contentCode and convert
  # other variables to factor variables
  dim_size <- get_dim_size(url=url, dims=dims, content_node=contentNode)[[1]]
  dim_var_type <- 
    calc_dim_type(dim_data2clean = dim(data2clean), 
                          dim_size = dim_size)

  # Melt the data to long format idvars 
  meltData <- data2clean[, list(variable = names(.SD), value = unlist(.SD, use.names = F)), 
                         by = eval(names(data2clean)[dim_var_type$row_variables])]
  meltData <- as.data.frame(meltData)

  # Convert to factors
  for (idvar in names(data2clean)[dim_var_type$row_variables]){
    meltData[,idvar] <- as.factor(meltData[,idvar])
  }

  # Get labels for col_variables
  col_var_lab <- list() #i <- content_time_name_index[2]
  for (i in dim_var_type$col_variables){
    if(length(dims[[i]])==1 && dims[[i]]=="*"){
      dims[[i]] <- contentNode$variables$variables[[i]]$values
    }
    lab_index <- which(contentNode$variables$variables[[i]]$values %in% dims[[i]])
    col_var_lab[[contentNode$variables$variables[[i]]$code]] <-
      contentNode$variables$variables[[i]]$valueTexts[lab_index]
  }
  
  # Calculate col_variables
  for (k in seq_along(dim_var_type$col_variables)){
    i <- rev(seq_along(dim_var_type$col_variables))[k] # Col variables reverse order
    j <- dim_var_type$col_variables[i] # Col variable column index
    if(k == 1) {
      each_no <- 1
    } else {
      each_no <- prod(dim_size[(dim_var_type$col_variables[i]+1):length(dim_size)])
    }
    meltData[, contentNode$variables$variables[[j]]$text] <- 
      factor(rep(col_var_lab[[i]], each=each_no))
  }
  meltData[, "values"] <- suppressWarnings(as.numeric(str_replace_all(meltData$value,"\\s","")))
   
  # Remove variables wiyhout any use
  meltData$value <- NULL
  meltData$variable <- NULL

  return(list(data=meltData, content_node = contentNode))

}


#' Calculates which dimensions that are used as row and col variables in data
#' 
#' @param dim_data2clean The dimension of the data from PXWEB
#' @param dim_size for the call from \link{get_dim_size}
#' 
#' @keywords internal 
#' 
#' @return list with index (row and col dim)
#' 

calc_dim_type <- function(dim_data2clean, dim_size){
  
  if(prod(dim_size) == 1){    
    if(all(dim_data2clean == 1)){
      row_col_variables <- list("row_variables" = numeric(0), 
                                "col_variables" = dim_data2clean[2]:length(dim_size))
      return(row_col_variables)
    } else {
      row_col_variables <- list("row_variables" = 1:(dim_data2clean[2]-1), 
                                "col_variables" = dim_data2clean[2]:length(dim_size))
      return(row_col_variables)
    }

  } else if (prod(dim_size) == dim_data2clean[2]){
    
    row_col_variables <- list("row_variables" = numeric(0), 
                              "col_variables" = 1:length(dim_size))
    return(row_col_variables)
  
  } else {
    for(i in seq_along(dim_size)){
      if(prod(dim_size[1:i]) == dim_data2clean[1]) {
        break()
      }
    }
    for(j in (i+1):length(dim_size)){ # j <- i+1
      cols <- prod(dim_size[j:length(dim_size)])
      if(cols + j - 1 == dim_data2clean[2]) break() # Kolla detta!
    }
    row_col_variables <- list("row_variables" = 1:(j-1), 
                              "col_variables" = j:length(dim_size))        
    return(row_col_variables)
  }
  stop("Can't calculate row and col variables.\nPlease send you API-call to ", maintainer("pxweb"),".") 
}



#' Reorders the dimensions of a \code{get_pxweb_data()} call and checks that all dimensions are correct.
#' 
#' @param url Url to check against
#' @param dims dims object to check
#' 
#' @keywords internal 
#' 
#' @return a correct (and checked) dims object
#' 
reorder_and_check_dims <- function(url, dims){
  metadata <- get_pxweb_metadata(path = url)
  dim_names <- unlist(lapply(X = metadata$variables$variables, FUN=function(X) X$code))
  if(!all(names(dims) %in% dim_names)) stop("Wrong dims names in call!")
  dims[dim_names]
}


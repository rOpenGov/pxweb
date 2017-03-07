#' Get the whole node tree from a given pxweb api
#' 
#' @param url The base url to the pxweb api
#' 
#' @return 
#' Function returns a data.frame with information on each node
#' Two variables are added: 
#' \code{checked} : The node has been checked
#' \code{error} : Whether there were errors encountered with the call
#' 
#' @keywords internal 
#' 
#' @import data.table 

test_pxweb_api_get_nodes <- function(url){

  nodes <- as.data.table(get_pxweb_metadata(path=url))
  nodes$level <- 1
  nodes$checked <- FALSE
  nodes$error <- FALSE

  i <- 1 
  while(i <= nrow(nodes)){
    if(nodes$type[i]=="l" & !nodes$checked[i]){
      message("Check node (level ", nodes$level[i] ,") : ", nodes$id[i])
      tempDF <- suppressWarnings(try(get_pxweb_metadata(path=nodes$URL[i]), silent=TRUE))
      if(class(tempDF)=="try-error"){
        if(str_detect(tempDF[1],pattern="Error : No internet connection to")){
          next()
        } else {
          nodes$error[i] <- TRUE        
        }
      } else {
        tempDF$level <- nodes$level[i] + 1
        tempDF$checked <- FALSE
        tempDF$error <- FALSE
        nodes <- rbind(nodes, tempDF[,names(nodes)])
      }
      nodes$checked[i] <- TRUE
    }
    i <- i + 1
  }
  return(nodes)
}
  
  
#' Get metadata from all bottom nodes identified with \link{test_pxweb_api_get_nodes}.
#' 
#' @param nodes A data.frame returned by \link{test_pxweb_api_get_nodes}
#' 
#' @keywords internal 
#' 
#' @return 
#' List with one list per node object containing meta data
#' 
test_pxweb_api_get_node_metadata <- function(nodes){
  bottomNodes <- nodes[nodes$type=="t",]
  nodesList <- list()
  
  i <- 1
  while(i <= nrow(bottomNodes)){
    tempObj <- suppressWarnings(try(get_pxweb_metadata(path=bottomNodes$URL[i])))
    if(class(tempObj)=="try-error"){
      bottomNodes$error[i] <- TRUE
      message("Error in", bottomNodes$id[i])
    } else {
      nodesList[[i]] <- list(dims=tempObj$variables$variables,URL=tempObj$URL)
    }
    message("Download metadata (", i, "/", nrow(bottomNodes),"): ", bottomNodes$id[i])
    i <- i + 1
  }
  return(nodesList)
}


#' Download data for each bottom node in the pxweb api
#' 
#' @param nodes Nodes downloaded with \link{test_pxweb_api_get_nodes}
#' @param nodesList Node metadata downloaded with \link{test_pxweb_api_get_node_metadata}
#' @param download_all Should all data be downloaded (TRUE) or sample (FALSE)
#' @param seed Seed to use if download_all is FALSE and a sample is downloaded
#' 
#' @keywords internal 
#' 
#' @return 
#' data.frame with information \code{download_error} containing download errors
#' @import data.table 
test_pxweb_api_get_data <- function(nodes, nodesList, download_all=FALSE, seed=as.integer(Sys.time())){
  
  bottomNodes <- nodes[nodes$type=="t",]  
  set.seed(seed)
  bottomNodes$download_error <- FALSE
  i <- 1
  calls <- list()
  while(i <= nrow(bottomNodes)){
    message("Download (", i, "/", nrow(bottomNodes),"): ", bottomNodes$id[i])

    dims <- list()
    for (dim in nodesList[[i]]$dims){
      if(download_all){
        dims[[length(dims)+1]] <- '*'
      } else {
        dims[[length(dims)+1]] <- sample(dim$values, size=1)
      }
      names(dims)[length(dims)] <- dim$code
    }
    tempObj <- suppressWarnings(try(get_pxweb_data(url=bottomNodes$URL[i],clean=TRUE,
                                                   dims=dims)))
    bottomNodes$checked[i] <- TRUE
    if(class(tempObj)=="try-error") bottomNodes$download_error[i] <- TRUE
    
    calls[[bottomNodes$id[i]]] <- 
      list(url=bottomNodes$URL[i],
           dims = dims)    
    i <- i + 1
  }
  return(list(data=bottomNodes, calls=calls))
}


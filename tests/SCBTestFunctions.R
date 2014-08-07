#' Test to get all nodes from SCB
#' 
#' Download and search one node per \code{delay_s} in the SCB API.
#' 
#' @param delay_s The time (in second for each call)
#' 
#' @return 
#' Function returns a data.fram with information on each node
#' 
testGetSCBnodes <- function(delay_s=1){
  require(data.table)
  nodes <- as.data.table(scbGetMetadata(path=baseURL()))
  nodes$level <- 1
  nodes$checked <- FALSE
  nodes$error <- FALSE
  
  i <- 1 
  while(i < nrow(nodes)){
    ptm <- proc.time()    
    if(nodes$type[i]=="l" & !nodes$checked[i]){
      cat("Check node:",nodes$id[i],"level",nodes$level[i],"\n")
      tempDF <- suppressWarnings(try(scbGetMetadata(path=nodes$URL[i]),silent=TRUE))
      if(class(tempDF)=="try-error"){
        if(str_detect(tempDF[1],pattern="Error : No internet connection to")){
          diff <- proc.time()-ptm
          Sys.sleep(max(delay_s-diff[3],0))
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
    i <- i +1
    diff <- proc.time()-ptm
    Sys.sleep(max(delay_s-diff[3],0))
  }
  return(nodes)
}



#' Test to get metadata from all bottom nodes in the SCB API
#' 
#' Tests to download metadata for each bottom node in the SCB API.
#' 
#' @param nodes A data.frame downloaded with \link{testGetSCBnodes}
#' 
#' @return 
#' List with one list per node object containing meta data
#' 
testNodeMetadata <- function(nodes){
  bottomNodes <- nodes[nodes$type=="t",]
  nodesList <- list()
  
  i <- 1
  while(i < nrow(bottomNodes)){
    ptm <- proc.time()    
    tempObj <- suppressWarnings(try(scbGetMetadata(path=bottomNodes$URL[i])))
    if(class(tempObj)=="try-error"){
      bottomNodes$error[i] <- TRUE
      Sys.sleep(2)
      cat("Error in", bottomNodes$id[i],"\n")
    } else {
      nodesList[[i]] <- list(dims=tempObj$variables$variables,URL=tempObj$URL)
    }
    cat(i,"/",nrow(bottomNodes),"\n")
    i <- i + 1
    
    diff <- proc.time()-ptm
    Sys.sleep(max(1.1-diff[3],0))
  }
  return(nodesList)
}


#' Test to download data from all bottom nodes in the SCB API
#' 
#' Tests to download data for each bottom node in the SCB API.
#' 
#' @param DLAll Should all data in the node be downloaded or just a sample
#' @param seed Seed to use if DLAll is FALSE and a sample is downloaded
#' @param nodes Nodes downloaded with \link{testGetSCBnodes}
#' @param nodesList Node metadata downloaded with \link{testNodeMetadata}
#' 
#' @return 
#' Data.frame with information \code{DLerror} containing download errors
#' 
testDownloadSCBdata <- function(DLAll=FALSE, seed=as.integer(Sys.time()), nodes, nodesList){
  require(data.table)
  
  bottomNodes <- nodes[nodes$type=="t",]  
  set.seed(seed)
  bottomNodes$DLerror <- FALSE
  i <- 1
  while(i <= nrow(bottomNodes)){
    cat("Downloading:", bottomNodes$id[i], "\n")
    ptm <- proc.time()    

    dims <- list()
    for (dim in nodesList[[i]]$dims){
      if(DLAll){
        dims[[length(dims)+1]] <- '*'
      } else {
        dims[[length(dims)+1]] <- sample(dim$values,size=1)
      }
      names(dims)[length(dims)] <- dim$code
    }
    tempObj <- suppressWarnings(try(scbGetData(url=bottomNodes$URL[i],clean=TRUE,
                                               dims=dims)))
    if(class(tempObj)=="try-error") bottomNodes$DLerror[i] <- TRUE
    i <- i + 1
    
    diff <- proc.time()-ptm
    Sys.sleep(max(1.1-diff[3],0))
  }
  return(bottomNodes)
}

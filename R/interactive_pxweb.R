#' @title Find and download data interactively from PX-WEB API
#'
#' @description Wrapper function (for \link{get_pxweb_data} and \link{get_pxweb_metadata}) to simply find and download data to the current R session. 
#' 
#' @param baseURL The base URL to use, depending on the web service. 
#' @param history keep the history when the function is running.
#' @param ... further parameters. These are currently ignored.
#' 
#' 
#' @seealso
#' \code{\link{get_pxweb_metadata}}, \code{\link{get_pxweb_data}}
#' @export
#' @examples
#' api_parameters() # List options
#' baseURL <- base_url("sweSCB", "v1", "sv")
#' \donttest{
#' d <- interactive_pxweb(baseURL)
#' }

interactive_pxweb <- function(baseURL, history = FALSE, ...){

  # Get top node
  Node <- get_pxweb_metadata(baseURL = baseURL) 
  
  # List to store nodes
  allNodes <- list()
  
  # Parameter indicating when to jump out of while loop
  quit <- FALSE 

  # The main program
  while(!quit) { 
    # Generate header
    if (!history) { cat("\014") }
    cat("Content of '", str_split(baseURL,pattern="/")[[1]][3], "' at current (", length(allNodes)+1, ") node level:\n", sep="") 
    cat(rep("=", round(getOption("width")*0.9)), "\n",sep="") 
    
    # Print information in node and ask for choice
    findData.printNode(Node)
    inputValue <- findData.input(type = "node", input = Node)

    if (inputValue == "q") { quit <- TRUE; next() }

    # Traverse to the previous node
    if (inputValue == "b") {
      if (length(allNodes) == 0) { next() }
      Node <- allNodes[[length(allNodes)]]
      allNodes[[length(allNodes)]] <- NULL
    }
    
    # If node choice is selected, download the next node  
    if (str_detect(inputValue, pattern = "[0-9]+")) {
       
      # Check if it is the botton node and if so, ask to download data
      if (Node$type[as.numeric(inputValue)] == "t") {
        downloadedData<-
          download_pxweb(dataNode=
            list(get_pxweb_metadata(
              Node$URL[as.numeric(inputValue)]),
              Node$URL[as.numeric(inputValue)]
            ))
        return(downloadedData)
      }

      # If not the bottom node, traverse to the next node (and save the current node)
      # to be able to traverse back up in the node tree
      allNodes[[length(allNodes) + 1]] <- Node
      Node <- get_pxweb_metadata(Node$URL[as.numeric(inputValue)])

    }
  }
}



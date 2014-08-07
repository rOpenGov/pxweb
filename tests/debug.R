skip <- TRUE

if (!skip) {
library(pxweb)
api_parameters() # List options
baseURL <- base_url("statfi", "v1", "fi")
history <- FALSE

library(data.table)
library(stringr)
library(RJSONIO)
library(httr)

fs <- list.files("../R/", full.names = T)
for (f in fs) {source(f)}

findData <- function(baseURL, history = FALSE, ...){

  print("Get top node")
  Node <- scbGetMetadata(baseURL = baseURL) 
  
  print("List to store nodes")
  allNodes <- list()
  
  print("Parameter indicating when to jump out of while loop")
  quit <- FALSE 

  print("The main program")
  while(!quit) { 

    print("Generate header")
    if (!history) { message("\014") }
    message("CONTENT OF SCB API AT CURRENT (", length(allNodes)+1, ") NODE LEVEL:\n", sep="") 
    message(rep("=", round(getOption("width")*0.9)), "\n",sep="") 
    
    print("Print information in node and ask for choice")
    .findData.printNode(Node)
    inputValue <- .findData.input(type = "node", input = Node)

    if (inputValue == "q") { quit <- TRUE; next() }

    print("Traverse to the previous node")
    if (inputValue == "b") {
      if (length(allNodes) == 0) { next() }
      Node <- allNodes[[length(allNodes)]]
      allNodes[[length(allNodes)]] <- NULL
    }
    
    print("If node choice is selected, download the next node")
    if (str_detect(inputValue, pattern = "[0-9]+")) {
       
      print("Check if it is the botton node and if so, ask to download data")
      if (Node$type[as.numeric(inputValue)] == "t") {
        downloadedData<-
          .findData.Download(dataNode=
            list(scbGetMetadata(
              Node$URL[as.numeric(inputValue)]),
              Node$URL[as.numeric(inputValue)]
            ))
        return(downloadedData)
      }

      print("If not the bottom node, traverse to the next node (and save the current node)")
      # to be able to traverse back up in the node tree
      allNodes[[length(allNodes) + 1]] <- Node
      Node <- scbGetMetadata(Node$URL[as.numeric(inputValue)])
    }
  }
}

d <- findData(baseURL)
}
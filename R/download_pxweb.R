#' @title Traverse node for query alternatives and download data.
#'
#' @description Goes through the dataNode and ask user for input for all 
#' variables and then put this together to a query for \link{get_pxweb_data}.
#' 
#' @param dataNode Botton node in node tree.
#' @param test_input Vector of length 4 to test inputs to the first 4 questions in the query.
#' @param ... further parameters. These are currently ignored.
#' @export
download_pxweb <- function(dataNode, test_input = NULL, ...) {
  # Assertions
  stopifnot(length(test_input) == 0 | length(test_input) == 3 )
  
  # Define tests
  if(length(test_input) == 0){
    testInputDown <- testInputClean <- character(0)
    testInputCode <- testInputVarAlt <- character(0)
  } else {
    testInputDown <- test_input[1]
    testInputClean <- test_input[2]
    testInputCode <- test_input[3]
    testInputVarAlt <- "1"
  }
  
  dataNodeName <- dataNode[[2]]
  dataNode <- dataNode[[1]]
  
  # Ask if the file should be downloaded
  inputDown <- findData.input(
    type = "yesno",
    input = str_c("Do you want to download '", dataNodeName, "'?", sep=""),
    test_input = testInputDown)
  download <- inputDown == "y"
  
  inputClean <- findData.input(
    type = "yesno",
    input = "Do you want to clean and melt this file (to wide R format)?",
    test_input = testInputClean)
  cleanBool <- inputClean == "y"
  
  inputCode <- findData.input(
    type="yesno",
    input="Do you want to print the code for downloading this data?",
    test_input = testInputCode)
  
  # Choose variables values
  varList <- list()
  varListText <- character(0)
  
  # Print the alternatives (for data to download) and choose alternatives to download i<-2
  for(i in 1:length(dataNode$variables$variables)) {
    # Print the alternatives to download
    listElem <- dataNode$variables$variables[[i]]
    if(is.null(listElem$values) | is.null(listElem$valueTexts)) {
      next()
    }
    varDF <- data.frame(id = listElem$values,
                        text = listElem$valueTexts,
                        stringsAsFactors = FALSE)
    # Ask for input from user
    varAlt <- findData.input(
      type="alt", 
      input=list(varDF, listElem$text),
      test_input = testInputVarAlt)
    
    # Convert the alternatives from the user to the PX-WEB API format
    if (varAlt[1] != "*") {
      tempAlt <- character(0)
      tempAlt <- listElem$values[as.numeric(varAlt)]
    } else {
      tempAlt <- "*"
    }
    
    # Save the alternative to use to download data 
    varList[[listElem$code]] <- tempAlt    
    varListText <- c(varListText,
                     str_c(listElem$code,
                           " = c('",
                           str_c(tempAlt, collapse="', '"),
                           "')", 
                           collapse=""))
  }
  
  if(download){
    cat("Downloading... ")
    tempData <- get_pxweb_data(dataNode$URL, varList, clean = cleanBool)
    cat("Done.\n")
  }
  
  # Print the code to repeat the downloading 
  if (inputCode == "y") {
    findData.printCode(dataNode$URL,
                           varListText,
                           clean = cleanBool)
  }
  
  if(download){ return(tempData) } else {return(invisible(NULL))}
}



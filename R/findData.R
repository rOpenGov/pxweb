#' @title Find and download data from PX-WEB API
#'
#' @description Wrapper function (for \link{scbGetData} and \link{scbGetMetadata}) to simply find and download data from SCB to the current R session. 
#' 
#' @param baseURL The base URL to use, depending on the web service. 
#' @param history keep the history when the function is running.
#' @param ... further parameters. These are currently ignored.
#' 
#' 
#' @seealso
#' \code{\link{scbGetMetadata}}, \code{\link{scbGetData}}
#' @export
#' @examples
#' \dontrun{
#' api_parameters() # List options
#' baseURL <- base_url("sweSCB", "v1", "sv")
#' d <- findData(baseURL)
#' }
#' 

findData <- function(baseURL, history = FALSE, ...){

  # Get top node
  Node <- scbGetMetadata(baseURL = baseURL) 
  
  # List to store nodes
  allNodes <- list()
  
  # Parameter indicating when to jump out of while loop
  quit <- FALSE 

  # The main program
  while(!quit) { 
    # Generate header
    if (!history) { message("\014") }
    message("CONTENT OF SCB API AT CURRENT (", length(allNodes)+1, ") NODE LEVEL:\n", sep="") 
    message(rep("=", round(getOption("width")*0.9)), "\n",sep="") 
    
    # Print information in node and ask for choice
    .findData.printNode(Node)
    inputValue <- .findData.input(type = "node", input = Node)

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
          .findData.Download(dataNode=
            list(scbGetMetadata(
              Node$URL[as.numeric(inputValue)]),
              Node$URL[as.numeric(inputValue)]
            ))
        return(downloadedData)
      }

      # If not the bottom node, traverse to the next node (and save the current node)
      # to be able to traverse back up in the node tree
      allNodes[[length(allNodes) + 1]] <- Node
      Node <- scbGetMetadata(Node$URL[as.numeric(inputValue)])
    }
  }
}


#' @title Traverse node for query alternatives and download data.
#'
#' @description Goes through the dataNode and ask user for input for all 
#' variables and then put this together to a query for \link{scbGetData}.
#' 
#' @param dataNode Botton node in SCB node tree.
#' @param test_input Vector of length 4 to test inputs to the first 4 questions in the query.
#' @param ... further parameters. These are currently ignored.
#' 
.findData.Download <- function(dataNode, test_input = NULL, ...) {
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
  inputDown <- .findData.input(
    type = "yesno",
    input = str_c("Do you want to download '", dataNodeName, "'?", sep=""),
    test_input = testInputDown)
  download <- inputDown == "y"
  
  inputClean <- .findData.input(
    type = "yesno",
    input = "Do you want to clean and melt this file (to wide R format)?",
    test_input = testInputClean)
  cleanBool <- inputClean == "y"
  
  inputCode <- .findData.input(
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
    varAlt <- .findData.input(
      type="alt", 
      input=list(varDF, listElem$text),
      test_input = testInputVarAlt)
    
    # Convert the alternatives from the user to the SCB api format
    if (varAlt[1] != "*") {
      tempAlt <- character(0)
      tempAlt <- listElem$values[as.numeric(varAlt)]
    } else {
      tempAlt <- "*"
    }
    
    # Save the alternative to use to download data from SCB api
    varList[[listElem$code]] <- tempAlt    
    varListText <- c(varListText,
                     str_c(listElem$code,
                           " = c('",
                           str_c(tempAlt, collapse="', '"),
                           "')", 
                           collapse=""))
  }
  
  if(download){
    message("Downloading... ")
save(dataNode, varList, cleanBool, file = "tmp.RData")
    tempData <- scbGetData(dataNode$URL, varList, clean = cleanBool)
    message("Done.\n")
  }
  
  # Print the code to repeat the downloading from SCB
  if (inputCode == "y") {
    .findData.printCode(dataNode$URL,
                           varListText,
                           clean = cleanBool)
  }
  
  if(download){ return(tempData) } else {return(invisible(NULL))}
}

.findData.inputBaseCat <- function(alt, codedAlt) {
  # The function prints the 'alt' rows in 'codedAlt'.
  # The purpose is to print alternatives for each input from the user
  output<-"\n("
  for (i in 1:length(alt)){
    if (i != 1){
      output <- str_join(output, ", ", sep="")
    }
    output <- str_join(output, 
                       "'", 
                       codedAlt[alt[i], 1], 
                       "' = ",
                       codedAlt[alt[i],2], sep="")
  }
  return(str_join(output,")", sep=""))
}

.findData.input <- function(type, input = NULL, test_input = character(0), silent = FALSE){
  # If silent sink output
  if(silent){
    temp <- tempfile()
    sink(file=temp)
  }
  
  # Define the possible alternatives that the user can do (except alternatives)
  codedAlt <- data.frame(abbr=c("esc", "b", "*", "y", "n", "a"),
                         name=c("Quit", "Back", "Select all", "Yes", "No", "Show all"),
                         stringsAsFactors = FALSE)
  textTitle <- alt <- character(0)
  baseCat <- numeric(0)
  
  # Define the different types of input
  if (type == "node") {
    baseCat<-1:2
    alt <- rownames(input)
    textHead <- "\nEnter the data (number) you want to explore:"
  }
  
  if (type == "yesno") {
    baseCat <- c(1,4:5)
    textHead <- input
  }
  
  if (type == "text") {
    textHead <- input
  }
  
  if (type == "alt") {
    baseCat <- c(1,3,6)
    varDF <- input[[1]]
    alt <- rownames(varDF)
    
    # Calculate a short list of alternatives
    if (nrow(varDF) > 11) {
      varDFshort <- varDF[c(1:6, (nrow(varDF)-4):nrow(varDF)), ]
      rownames(varDFshort)[6] <- "."
    } else {
      varDFshort <- varDF }

    textTitle <- str_join("\nALTERNATIVES FOR VARIABLE: ",
                          toupper(input[[2]]),
                          " \n",
                          str_join(
                            rep("=", round(getOption("width")*0.9)), collapse = ""), 
                          "\n", sep="")
    textHead <-
      str_join("\nChoose your alternative(s) by number:",
               "\nSeparate multiple choices by ',' and intervals by ':'", sep="")
  }

  inputOK <- FALSE
  inputScan <- ""
  
  
  while(!inputOK) {
    # Print title, alternatives and so forth
    message(textTitle)
    if (type == "alt") {
      if (inputScan == "a") {
        toprint <- varDF
      } else {
        toprint <- varDFshort
      }
      .findData.printNode(xscb = toprint, print = TRUE)
    }
    message(textHead)
    if (type != "text") {
      message(.findData.inputBaseCat(baseCat, codedAlt), "\n")
    }
    
    # Get input from the user (if not test run)
    if (length(test_input)==0) {
      inputScanRaw <- scan(what=character(), multi.line = FALSE, quiet=TRUE, nlines=1 , sep=",")
    } else {
      inputScanRaw <- scan(what=character(), quiet=TRUE, sep=",", text=test_input)
    }
    
    # If just an enter is entered -> start over
    if (length(inputScanRaw) == 0) { next() }
    
    # Format the input data (to lowercase and without whitespaces) and as char vector
    inputScan <- tolower(str_trim(inputScanRaw))
    # If a = "Show all", restart, but show all alternatives
    
    if (inputScan[1] == "a") { next() }
    
    # Case sensitive text input
    if (type == "text") inputScan <- inputScanRaw
    
    # Scan for duplicates and do corrections
    inputScan <- .findData.inputConvert(inputScan)
    
    # Test if the input are OK (valid)
    inputOK <- 
      (length(inputScan) == 1 && inputScan %in% tolower(codedAlt$abbr[baseCat])) |
      all(inputScan %in% tolower(alt)) | 
      type == "text"

    if(type != "alt" & length(inputScan) > 1) inputOK <- FALSE
    if(type == "text") {
      if(make.names(inputScan) != inputScan) {
        inputOK <- FALSE
        message("This is not a valid name of a data.frame object in R.\n")
        message("You could change the name to '", 
            make.names(inputScan),
            "'.\n", sep="")
      }
    }
        
    if(!inputOK){
      message("Sorry, no such entry allowed. Please try again!\n\n")
    }
  } 

  # Stop sink and remove output
  if(silent){
    sink()
    unlink(temp)
  }
  
  return(inputScan)
}

.findData.printNode <- function(xscb, print=TRUE) {
  # Preparations of for printing the node
  xscb$text <- as.character(xscb$text) 
  nSCBidlen <- max(str_length(as.character(xscb$id))) # Get max str length of id
  nSCBpos <- max(str_length(rownames(xscb))) # Get max str length of row number 
  nSCBconsole <- round(getOption("width")*0.9)
  
  # Calculates where the different output should be printed
  startPos <- nSCBpos+nSCBidlen+5
  scbTextSpace <- nSCBconsole-startPos
  finalText <- character(0) 
  
  for (i in 1:nrow(xscb)) {
    # Corrections if there is an shortened list of alternatives
    if (rownames(xscb)[i] == "."){
      finalText <- str_join(finalText,"\n")
      next()
    }
    
    # The text that should be printed
    finalText <- str_join(
      finalText,
      rownames(xscb)[i],
      ".",
      str_join(
        rep(" ", nSCBpos - str_length(rownames(xscb)[i])), collapse=""),
      " [",
      xscb$id[i],
      "]",
      str_join(rep(" ", nSCBidlen - str_length(as.character(xscb$id[i]))), collapse=""),
      " ",collapse="")
    
    # Convert if there is console is too narrow for the text
    first <- rerun <- TRUE
    tempText <- xscb$text[i]
    while(first | rerun){
      # Cut upp the alternative text to pieces that fit the console width
      tempTextSpaces <- str_locate_all(tempText,pattern=" ")[[1]][ , 1]
      if (str_length(tempText) > scbTextSpace){
        tempTextCut <- max(tempTextSpaces[tempTextSpaces < scbTextSpace]) - 1
      } else {
        tempTextCut <- str_length(tempText)
        rerun <- FALSE
      }
            
      finalText <-
        str_join(finalText,
                 str_join(rep(" ", startPos*(1-as.numeric(first))), collapse=""),
                 str_sub(tempText, 1, tempTextCut), "\n", collapse="")
      
      if (rerun) {
        tempText <- str_sub(tempText, tempTextCut + 2)
      }

      first <- FALSE
    }
  }
  # Print node text or save it as a character value
  if (print) {
    message(finalText)
  } else {
    return(finalText)
  }
}

.findData.printCode <- function(url, varListText, clean) {
  # Print the code used to download the data
  
  message("To download the same data from SCB again, use the following code:\n\n")
  message("myDataSetName",
      " <- \n  scbGetData(url = \"", 
      url,
      "\",\n",
      rep(" ",13),
      "dims = list(", sep="")

  # Print the chosen alternatives for each data dimension
  for (i in 1:length(varListText)){
    if(i != 1){
      message(rep(" ", 25), sep="")
    }
    message(varListText[i], sep="")
    if (i != length(varListText)) {
      message(",\n",sep="")
    }
  }

  message("),\n")
  
  # Print if the data should be cleaned or not
  message(rep(" ",13), 
      "clean = ",
      as.character(clean), sep="")
  message(")\n\n")
}


.findData.inputConvert <- function(input) {
  # Set the output (for input of length == 1)
  output <- input  

  # Do conversions for 
  if (length(input) > 1 || str_detect(input, ":")) {
    output <- character(0)
    for(i in 1 : length(input)) {
      # Split input values on the format [0-9]+:[0-9]+
      if (str_detect(input[i], ":")){
        index <- as.numeric(unlist(str_split(input[i], pattern = ":")))
        output <- c(output, as.character(index[1]:index[2]))
      } else {
        # Otherwise just add the value
        output <- c(output, input[i])
      }
    }
    # Sort and remove duplicates
    output <- unique(output)
    output <- output[order(as.numeric(output))]
  }
  return(output)
}






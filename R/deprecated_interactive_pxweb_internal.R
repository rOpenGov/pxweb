#' @title Traverse node for query alternatives and download data.
#'
#' @description Goes through the dataNode and ask user for input for all 
#' variables and then put this together to a query for \link{get_pxweb_data}.
#' 
#' @param dataNode Botton node in node tree.
#' @param test_input Vector of length 4 to test inputs to the first 4 questions in the query.
#' @param ... further parameters. These are currently ignored.
#' 
#' @keywords internal 
#' 
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
                     str_c(ifelse(make.names(listElem$code) == listElem$code, 
                                  listElem$code,
                                  str_c("\"", listElem$code, "\"", collapse="")),
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

findData.inputBaseCat <- function(alt, codedAlt) {
  # The function prints the 'alt' rows in 'codedAlt'.
  # The purpose is to print alternatives for each input from the user
  output<-"\n("
  for (i in 1:length(alt)){
    if (i != 1){
      output <- str_c(output, ", ", sep="")
    }
    output <- str_c(output, 
                       "'", 
                       codedAlt[alt[i], 1], 
                       "' = ",
                       codedAlt[alt[i],2], sep="")
  }
  return(str_c(output,")", sep=""))
}



#' Get input that is consistent with 
#'
#' @param type type of input to get.
#' @param input data.frame with input data to use with 
#' @param test_input input for test cases
#' @param silent no output
#' 
#' @keywords internal 
#'
findData.input <- function(type, input = NULL, test_input = character(0), silent = FALSE){
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
  max_cat <- NA
  
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
    max_cat <- length(alt)
    
    # Calculate a short list of alternatives
    if (nrow(varDF) > 11) {
      varDFshort <- varDF[c(1:6, (nrow(varDF)-4):nrow(varDF)), ]
      rownames(varDFshort)[6] <- "."
    } else {
      varDFshort <- varDF }

    textTitle <- str_c("\nALTERNATIVES FOR VARIABLE: ",
                          toupper(input[[2]]),
                          " \n",
                          str_c(
                            rep("=", round(getOption("width")*0.9)), collapse = ""), 
                          "\n", sep="")
    textHead <-
      str_c("\nChoose your alternative(s) by number:",
               "\nSeparate multiple choices by ',' and intervals by ':'", sep="")
  }
  
  if (type == "db") {
    baseCat <- c(1)    
    toprint <- data.frame(id=1:nrow(input), text = input$text)
    alt <- rownames(toprint)
    max_cat <- 1
    
    textTitle <- str_c("\nCHOOSE DATABASE:\n",
                          str_c(
                            rep("=", round(getOption("width")*0.9)), collapse = ""), 
                          "\n", sep="")
    textHead <-
      str_c("\nChoose database by number:", sep="")
  }

  if (type == "api") {
    baseCat <- c(1)    
    toprint <- data.frame(id=input[,1], text = input[,2])
    alt <- rownames(toprint)
    max_cat <- 1
    
    textTitle <- str_c("\nCHOOSE API:\n",
                          str_c(
                            rep("=", round(getOption("width")*0.9)), collapse = ""), 
                          "\n", sep="")
    textHead <-
      str_c("\nChoose api by number:", sep="")
  }
  

  inputOK <- FALSE
  inputScan <- ""

  
  while(!inputOK) {
    # Print title, alternatives and so forth
    cat(textTitle)
    if (type == "alt") {
      if (inputScan == "a") {
        toprint <- varDF
      } else {
        toprint <- varDFshort
      }
      findData.printNode(xscb = toprint, print = TRUE)
    }
    if (type == "db" | type == "api") {
      findData.printNode(xscb = toprint, print = TRUE)
    }    
    cat(textHead)
    if (type != "text") {
      cat(findData.inputBaseCat(baseCat, codedAlt), "\n")
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
    inputScan <- findData.inputConvert(inputScan, max_value=max_cat)
    
    # Test if the input are OK (valid)
    inputOK <- 
      (length(inputScan) == 1 && inputScan %in% tolower(codedAlt$abbr[baseCat])) |
      all(inputScan %in% tolower(alt)) | 
      type == "text"

    if(type != "alt" & length(inputScan) > 1) inputOK <- FALSE
    if(type == "text") {
      if(make.names(inputScan) != inputScan) {
        inputOK <- FALSE
        cat("This is not a valid name of a data.frame object in R.\n")
        cat("You could change the name to '", 
            make.names(inputScan),
            "'.\n", sep="")
      }
    }
        
    if(!inputOK){
      cat("Sorry, no such entry allowed. Please try again!\n\n")
    }
  } 

  # Stop sink and remove output
  if(silent){
    sink()
    unlink(temp)
  }
  
  return(inputScan)
}

findData.printNode <- function(xscb, print=TRUE) {
  # Preparations of for printing the node
  xscb$text <- as.character(xscb$text) 
  nSCBidlen <- max(str_length(as.character(xscb$id))) # Get max str length of id
  nSCBpos <- max(str_length(rownames(xscb))) # Get max str length of row number 
  nSCBconsole <- round(getOption("width")*0.9)
  
  # Calculates where the different output should be printed
  startPos <- nSCBpos+nSCBidlen+5
  scbTextSpace <- max(nSCBconsole-startPos, 5)  # Minimum 5 to avoid endless loop
  finalText <- character(0) 
  
  for (i in 1:nrow(xscb)) {
    # Corrections if there is an shortened list of alternatives
    if (rownames(xscb)[i] == "."){
      finalText <- str_c(finalText,"\n")
      next()
    }
    
    # The text that should be printed
    finalText <- str_c(
      finalText,
      rownames(xscb)[i],
      ".",
      str_c(
        rep(" ", nSCBpos - str_length(rownames(xscb)[i])), collapse=""),
      " [",
      xscb$id[i],
      "]",
      str_c(rep(" ", nSCBidlen - str_length(as.character(xscb$id[i]))), collapse=""),
      " ",collapse="")
    
    # Convert if there is console is too narrow for the text
    first <- rerun <- TRUE
    tempText <- xscb$text[i]
    while(first | rerun){
      # Cut upp the alternative text to pieces that fit the console width
      tempTextSpaces <- str_locate_all(tempText,pattern=" ")[[1]][ , 1]
      if (str_length(tempText) > scbTextSpace){
        tempTextCut <- max(tempTextSpaces, scbTextSpace) - 1
      } else {
        tempTextCut <- str_length(tempText)
        rerun <- FALSE
      }
            
      finalText <-
        str_c(finalText,
                 str_c(rep(" ", startPos*(1-as.numeric(first))), collapse=""),
                 str_sub(tempText, 1, tempTextCut), "\n", collapse="")
      
      if (rerun) {
        tempText <- str_sub(tempText, tempTextCut + 2)
      }

      first <- FALSE
    }
  }
  # Print node text or save it as a character value
  if (print) {
    cat(finalText)
  } else {
    return(finalText)
  }
}

findData.printCode <- function(url, varListText, clean) {
  # Print the code used to download the data
  
  cat("To download the same data again, use the following code:\n(save code using UTF-8 encoding)\n\n")
  cat("myDataSetName",
      " <- \n  get_pxweb_data(url = \"", 
      url,
      "\",\n",
      rep(" ",13),
      "dims = list(", sep="")

  # Print the chosen alternatives for each data dimension
  for (i in 1:length(varListText)){
    if(i != 1){
      cat(rep(" ", 25), sep="")
    }
    cat(varListText[i], sep="")
    if (i != length(varListText)) {
      cat(",\n",sep="")
    }
  }

  cat("),\n")
  
  # Print if the data should be cleaned or not
  cat(rep(" ",13), 
      "clean = ",
      as.character(clean), sep="")
  cat(")\n\n")
}


findData.inputConvert <- function(input, max_value=NA) {
  # Set the output (for input of length == 1)
  output <- input  

  # Do conversions for  i<-1
  if (length(input) > 1 || str_detect(input, ":")) {
    output <- character(0)
    for(i in 1 : length(input)) { # i <- 2
      # Split input values on the format [0-9]+:[0-9]+
      if (str_detect(input[i], ":")){
        index <- as.numeric(unlist(str_split(input[i], pattern = ":")))
        if(is.na(index[1])) index[1] <- 1
        if(is.na(index[2])) {
          index[2] <- max_value
          if(is.na(max_value)) index[2] <- index[1]
        }
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



#' Calculate a specific database to get data from
#' 
#' @param baseURL The basic url to the pxweb api
#' @param pre_choice Predifined choice of database
#' 
#' @return base url to the specific data base
#' 
#' @keywords internal 
#' 
choose_pxweb_database_url <- function(baseURL, pre_choice = NULL){
  data_bases <- get_pxweb_metadata(baseURL = baseURL) 
  if(nrow(data_bases) == 1){
    return(paste0(baseURL, "/", text_to_url(data_bases$dbid)))
  } else if(is.null(pre_choice)) {
    db_choice <- as.numeric(findData.input(type = "db", input = data_bases))
    return(paste0(baseURL, "/", text_to_url(data_bases$dbid[db_choice])))
  } else if(!is.null(pre_choice)){
    return(paste0(baseURL, "/", text_to_url(data_bases$dbid[pre_choice])))
  }
} 


#' Choose an api from api_catalogue
#' 
#' @keywords internal 
#' 
#' @return base url to the specific data base
#' 
choose_pxweb_api <- function(){
  res <- character(3)
  apis <- api_catalogue()
  api_df <- data.frame(api_names = unlist(lapply(apis, FUN=function(X) X$api)),
                       text = unlist(lapply(apis, FUN=function(X) X$description)))

  api_choice <- as.numeric(findData.input(type = "api", input = api_df))
  res[1] <- apis[[api_choice]]$api
  i <- 1
  for(type in c("languages", "versions")){
    i <- i + 1
    if(type == "languages") vec <- apis[[api_choice]]$languages
    if(type == "versions") vec <- apis[[api_choice]]$versions

    if(length(vec) > 1) {
      choice <- 
        as.numeric(findData.input(type = "api",
                                  input = data.frame(id = 1:length(vec),
                                                     text = vec)))
      choice <- vec[choice]
    } else {
      choice <- vec
    }
    res[i] <- choice
  }
  return(res)
} 


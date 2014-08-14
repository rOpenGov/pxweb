findData.inputBaseCat <- function(alt, codedAlt) {
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
    cat(textTitle)
    if (type == "alt") {
      if (inputScan == "a") {
        toprint <- varDF
      } else {
        toprint <- varDFshort
      }
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
    inputScan <- findData.inputConvert(inputScan)
    
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
    cat(finalText)
  } else {
    return(finalText)
  }
}

findData.printCode <- function(url, varListText, clean) {
  # Print the code used to download the data
  
  cat("To download the same data again, use the following code:\n\n")
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


findData.inputConvert <- function(input) {
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






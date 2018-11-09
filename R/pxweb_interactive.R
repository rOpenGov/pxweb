#' @title Find and download data interactively from PX-WEB API
#'
#' @description Wrapper function (for \link{get_pxweb_data} and \link{get_pxweb_metadata}) to simply find and download data to the current R session. 
#' 
#' @param x The name or alias of the pxweb api to connect to, a \code{pxweb} object or an url. Use \link{api_catalogue} to get a list of apis.
#' 
#' 
#' @seealso
#' \code{\link{get_pxweb_metadata}}, \code{\link{get_pxweb_data}}
#' @export
#' @examples
#'  pxweb_api_catalogue() # List apis
#' \dontrun{
#'  d <- pxweb_interactive()
#'  d <- pxweb_interactive(x = "api.scb.se")
#'  d <- pxweb_interactive(x = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/")
#' }

pxweb_interactive <- function(x = NULL){
  
  pxexp <- pxweb_explorer(x)
  

  
  
  

  # Choose DB and get top node
  baseURL <- api_obj$base_url(version = version, language = language)
  dbURL <- choose_pxweb_database_url(baseURL)
  Node <- get_pxweb_metadata(baseURL = dbURL) 
  
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
      Node <- get_pxweb_metadata(path = Node$URL[as.numeric(inputValue)])

    }
  }
}


#' Create a \code{pxweb_explorer} object.
#' @param x a \code{pxweb} object, a PXWEB url, \code{NULL} or an api in the api catalogue.
#' 
#' \dontrun{
#'  d <- pxweb_explorer()
#'  d <- pxweb_explorer(x = "api.scb.se")
#'  d <- pxweb_explorer(x = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/")
#' }
#' @keywords internal
pxweb_explorer <- function(x = NULL){
  UseMethod("pxweb_explorer")
}


#' @rdname pxweb_explorer
#' @keywords internal
pxweb_explorer.NULL <- function(x){
  apis <- pxweb_api_catalogue()
  pxe <- list(pxweb = NULL,
              root = "/")
  pxe$position <- ""
  pxalist <- list()
  txt <- unname(unlist(lapply(apis, function(x) x$description)))
  for(i in seq_along(names(apis))){
    pxalist[[i]] <- list(id=names(apis)[i], type = "l", text = txt[i])
  }
  pxl <- pxweb:::pxweb_levels(pxalist)
  pxe$pxobjs <- list("apis" = list(pxobj = pxl))
  class(pxe) <- c("pxweb_explorer", "list")
  assert_pxweb_explorer(pxe)
  pxe
}

#' @rdname pxweb_explorer
#' @keywords internal
pxweb_explorer.character <- function(x){
  apis <- pxweb_api_catalogue()
  api_alias_tbl <- pxweb_api_catalogue_alias_table()
  px <- try(pxweb(x), silent = TRUE)
  if(inherits(px, "try-error")){
    pos_idx <- which(x %in% api_alias_tbl$alias)
    if(length(pos_idx) == 0){
      stop("'", x, "' is not a PXWEB API. See pxweb_api_catalogue() for available PXWEB APIs.", call. = FALSE)
    }
    px <- apis[[api_alias_tbl$idx[pos_idx]]]
  }
  pxweb_explorer(px)
}

#' @rdname pxweb_explorer
#' @keywords internal
pxweb_explorer.pxweb <- function(x){
  pxe <- list(pxweb = x)
  tot_pos <- strsplit(httr::parse_url(pxe$pxweb$url)$path, split = "/")[[1]]
  pxe$root <- paste0("/", pxe$pxweb$paths$api_subpath$path)
  pxe$position <- tot_pos[(length(pxe$pxweb$paths$api_subpath$vector)+1):length(tot_pos)]
  
  sub_path <- paste0("/", paste(pxe$position, collapse = "/"))
  pxe$pxobjs <- list(list(pxobj = pxweb_get(x)))
  names(pxe$pxobjs) <- sub_path
  class(pxe) <- c("pxweb_explorer", "list")
  assert_pxweb_explorer(x = pxe)
  pxe
}

#' @rdname pxweb_explorer
#' @keywords internal
pxweb_explorer.pxweb_api_catalogue_entry <- function(x){
  pxe <- list(pxweb = pxweb(build_pxweb_url(x)))
  tot_pos <- strsplit(httr::parse_url(pxe$pxweb$url)$path, split = "/")[[1]]
  pxe$root <- "/"
  pxe$position <- ""
  
  tot_pos <- strsplit(httr::parse_url(x$url)$path, split = "/")[[1]]
  ver_pos <- which(tot_pos == "[version]")
  lan_pos <- which(tot_pos == "[lang]")
  version_list <- list()
  for(i in seq_along(x$version)){
    version_list[[i]] <- list(id=gsub("\\[version\\]", paste(tot_pos[1:ver_pos], collapse = "/"), replacement = x$version[i]), 
                              type = "l", 
                              text = x$version[i])
  }
  pxe$pxobjs <- list("/" = list(pxobj = pxweb:::pxweb_levels(version_list)))

  language_list <- list()
  for(i in seq_along(x$version)){
    language_list[[i]] <- list()
    for(j in seq_along(x$lang)){
      lan_part <- gsub("\\[lang\\]", paste(tot_pos[(ver_pos+1):length(tot_pos)], collapse = "/"), replacement = x$lang[j])
      language_list[[i]][[j]] <- list(id=lan_part, 
                                      type = "l", 
                                      text = x$lang[j])
    }
    pxe$pxobjs[[version_list[[i]]$id]] <- list(pxobj = pxweb:::pxweb_levels(language_list[[i]]), parent = "/")
  }
  class(pxe) <- c("pxweb_explorer", "list")
  assert_pxweb_explorer(x = pxe)
  pxe
}

#' @rdname pxweb_explorer
#' @keywords internal
assert_pxweb_explorer <- function(x){
  checkmate::assert_class(x, "pxweb_explorer")
  checkmate::assert_string(x$root, pattern = "^/")
  for(i in seq_along(x$pxobjs)){
    checkmate::assert_names(names(x$pxobjs[[i]]), must.include = "pxobj")
    is_pxlev <- inherits(x$pxobjs[[i]]$pxobj, "pxweb_levels")
    is_pxmd <- inherits(x$pxobjs[[i]]$pxobj, "pxweb_metadata")
    checkmate::assert_true(is_pxlev | is_pxmd)
    if(!is.null(x$pxobjs[[i]]$parent)){
      checkmate::assert_string(x$pxobjs[[i]]$parent)
      checkmate::assert_choice(x$pxobjs[[i]]$parent, names(x$pxobjs))
    }
  }
}


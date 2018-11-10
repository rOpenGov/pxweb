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
#'  x <- pxweb_interactive()
#'  x <- pxweb_interactive(x = "api.scb.se")
#'  x <- pxweb_interactive(x = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/")
#' }
pxweb_interactive <- function(x = NULL){
  # Setup structure
  pxexp <- pxweb_explorer(x)
  
  # The main program
  while(!pxexp$quit) { 
    # Generate header
    if (!pxexp$show_history) { 
      cat("\014") 
    }
    
    print(pxexp)
    pxexp <- pxweb_interactive_input(pxexp)
    
  }
}


#' Create a \code{pxweb_explorer} object.
#' @param x a \code{pxweb} object, a PXWEB url, \code{NULL} or an api in the api catalogue.
#' 
#' @description 
#' \code{position} the current position in the api, as a character vector from the root.
#' Note position is not alway a correct url. Metadata and other choices are part of position
#' 
#' \code{root} is the bottom path (as position) that the user can go. If length 0, user can essentially go to hostname.
#' 
#' paste(root_path + position, collapse = "/")  is used to construct the path to the position
#' in case of url.
#' 
#' \dontrun{
#'  x <- pxweb_explorer()
#'  x <- pxweb_explorer(x = "api.scb.se")
#'  x <- pxweb_explorer(x = "http://api.scb.se/OV0104/v1/doris/en/ssd/BE/BE0101/")
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
              root = character(0))
  pxe$position <- character(0)
  pxalist <- list()
  txt <- unname(unlist(lapply(apis, function(x) x$description)))
  for(i in seq_along(names(apis))){
    pxalist[[i]] <- list(id=names(apis)[i], type = "l", text = txt[i])
  }
  pxl <- pxweb:::pxweb_levels(pxalist)
  pxe$pxobjs <- list("api_names" = list(pxobj = pxl))
  class(pxe) <- c("pxweb_explorer", "list")
  pxe <- add_pxweb_explorer_defaults(pxe)
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
  pxe$root <- pxweb_api_subpath(pxe$pxweb, as_vector = TRUE)
  pxe$position <- pxweb_api_path(pxe$pxweb, as_vector = TRUE)
  pxe$pxobjs <- list(list(pxobj = pxweb_get(x)))
  names(pxe$pxobjs) <- pxweb_api_path(pxe$pxweb)
  class(pxe) <- c("pxweb_explorer", "list")
  pxe <- add_pxweb_explorer_defaults(pxe)
  assert_pxweb_explorer(x = pxe)
  pxe
}

#' @rdname pxweb_explorer
#' @keywords internal
pxweb_explorer.pxweb_api_catalogue_entry <- function(x){
  pxe <- list(pxweb = pxweb(build_pxweb_url(x)))
  tot_pos <- pxweb_api_path(pxe$pxweb, as_vector = TRUE)
  pxe$root <- character(0)
  pxe$position <- character(0)
  
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
  pxe <- add_pxweb_explorer_defaults(pxe)
  assert_pxweb_explorer(x = pxe)
  pxe
}

#' Add default values to pxe
#' @param pxe a \code{pxweb_explorer} object
#' @keywords internal
add_pxweb_explorer_defaults <- function(pxe){
  checkmate::assert_class(pxe, "pxweb_explorer")
  pxe$show_history <- FALSE
  pxe$quit <- FALSE
  pxe$print_all_choices <- FALSE
  pxe
}


#' @rdname pxweb_explorer
#' @keywords internal
assert_pxweb_explorer <- function(x){
  checkmate::assert_class(x, "pxweb_explorer")
  checkmate::assert_character(x$root)
  checkmate::assert_character(x$position)
  checkmate::assert_subset(x = x$root, x$position)
  checkmate::assert_flag(x$show_history)
  checkmate::assert_flag(x$print_all_choices)
  checkmate::assert_flag(x$quit)
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


#' @param include_rootpath Should the rootpath be included? Default is FALSE
#' @rdname pxweb_api_name
#' @keywords internal
pxweb_explorer_position_path <- function(x, init_slash = TRUE, as_vector = FALSE, include_rootpath = FALSE){
  checkmate::assert_class(pxe, "pxweb_explorer")
  if(is.null(x$pxweb)){
    if(init_slash){
      return("/")
    } else {
      return("")
    }
  } 
  if(as_vector){
    if(include_rootpath){
      return(c(pxweb_api_rootpath(x), x$position))
    } else {
      return(x$position)
    }
  }
  p <- paste(x$position, collapse = "/")
  if(include_rootpath){
    return(paste(pxweb_api_rootpath(x), p, sep = "/"))
  } else {
    if(init_slash){
      return(paste("/", p, sep = ""))
    }
  }
  return(p)
}



#' @rdname pxweb_explorer
#' @keywords internal
print.pxweb_explorer <- function(x, ...){
  print_bar()  
  cat("R PXWEB: Content of '", pxweb_api_name(x), "'\n", sep="") 
  sp <- pxweb_explorer_position_path(x, init_slash = TRUE, include_rootpath = FALSE)
  if(nchar(sp) > 0) cat("         at '", sp, "'\n", sep="") 
  print_bar()  
  pxweb_explorer_print_choices(x)
  print_bar()  
}

print_bar <- function(){
  cat(rep("=", round(getOption("width")*0.95)), "\n",sep="")
}


pxweb_explorer_print_choices <- function(x){
  obj <- x$pxobjs[[pxweb_explorer_position(x)]]$pxobj
  show_no <- 4
  if(x$print_all_choices | length(obj) <= show_no * 2){
    print_idx <- 1:length(obj) 
  } else {
    print_idx <- c(1:show_no, NA, (length(obj) - show_no + 1):length(obj))
  }

  print_idx_char <- as.character(print_idx)
  print_idx_char_nmax <- max(nchar(print_idx_char), na.rm = TRUE)
  print_idx_char <- str_pad(print_idx_char, print_idx_char_nmax )
  
  cat(str_pad(txt = " ", n = print_idx_char_nmax + 6), "DESCRIPTION\n")
  
  for(i in seq_along(print_idx)){
    if(is.na(print_idx[i])) {
      cat("\n")
      next
    }
    cat(" [", print_idx_char[i], " ] : ", obj[[print_idx[i]]]$text, "\n", sep = "")
  }
}


#' Pad a string to a fixed size
#' @param x a character vector to pad
#' @param n final char width
#' @param pad pad symbol
#' @param type pad from 'left' or 'right'.
#' @keywords internal
str_pad <- function(txt, n = 5, pad = " ", type = "left"){
  checkmate::assert_character(txt)
  checkmate::assert_string(pad)
  checkmate::assert_true(nchar(pad)==1)
  checkmate::assert_int(n)
  checkmate::assert_choice(type, c("left", "right"))
  
  nch <- pmax((n - nchar(txt)), rep(0, length(txt)))
  nch[is.na(nch)] <- 2
  pads <- unlist(lapply(nch, function(x, pad) {paste(rep(pad, x), collapse="")}, pad))
  if(type == "left"){
    return(paste(pads, txt))
  } else {
    return(paste(txt, pads))
  }
}



#' Get input from user
pxweb_interactive_input <- function(pxe){
  checkmate::assert_class(pxe, "pxweb_explorer")
  user_input <- pxweb_explorer_get_input(pxe)
  pxe <- pxweb_explorer_handle_input(user_input, pxe)
  pxe
}

pxweb_explorer_get_input <- function(pxe){
  checkmate::assert_class(pxe, "pxweb_explorer")
  
  input_ok <- FALSE
  allowed_input <- pxweb_explorer_allowed_input(pxe)
  
  while(!input_ok){
    user_input <- scan(what=character(), multi.line = FALSE, quiet=TRUE, nlines=1)
    input_ok <- input_is_ok(user_input, all_inp)
  }

  class(user_input) <- c("pxweb_user_input", "list")
  user_input
}
  

#' DEBUG
#' rm(list = ls())
#' source("R/pxweb_api_catalogue.R")
#' source("R/pxweb_build_pxweb_urls.R")
#' source("R/pxweb_api_paths.R")

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
  pxe <- pxweb_explorer(x)
  
  # The main program
  while(!pxe$quit) { 
    # Generate header
    if (!pxe$show_history) { 
      cat("\014") 
    }
    
    print(pxe)
    pxe <- pxweb_interactive_input(pxe)
  }
  
  
  
  if(pxe$get_data){
    
  } else {
    return(invisble(NULL))
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
  pxe$variable_choice <- list()
  pxalist <- list()
  txt <- unname(unlist(lapply(apis, function(x) x$description)))
  for(i in seq_along(names(apis))){
    pxalist[[i]] <- list(id=names(apis)[i], type = "l", text = txt[i])
  }
  pxl <- pxweb:::pxweb_levels(pxalist)
  pxe$pxobjs <- list("/" = list(pxobj = pxl))
  class(pxe) <- c("pxweb_explorer", "list")
  pxe <- add_pxe_defaults(pxe)
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
  pxe$variable_choice <- list()
  pxe$pxobjs <- list(list(pxobj = pxweb_get(x)))
  names(pxe$pxobjs) <- pxweb_api_path(pxe$pxweb)
  class(pxe) <- c("pxweb_explorer", "list")
  pxe <- add_pxe_defaults(pxe)
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
  pxe$variable_choice <- list()
  
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
  pxe <- add_pxe_defaults(pxe)
  assert_pxweb_explorer(x = pxe)
  pxe
}

#' Add default values to pxe
#' @param pxe a \code{pxweb_explorer} object
#' @keywords internal
add_pxe_defaults <- function(pxe){
  checkmate::assert_class(pxe, "pxweb_explorer")
  pxe$show_history <- FALSE
  pxe$quit <- FALSE
  pxe$print_all_choices <- FALSE
  pxe$print_no_of_choices <- 4
  pxe$show_id <- FALSE
  pxe
}


#' @rdname pxweb_explorer
#' @keywords internal
assert_pxweb_explorer <- function(x){
  checkmate::assert_class(x, "pxweb_explorer")
  checkmate::assert_character(x$root)
  checkmate::assert_character(x$position)
  checkmate::assert_subset(x = x$root, x$position)
  checkmate::assert_list(x$variable_choice)
  checkmate::assert_flag(x$show_history)
  checkmate::assert_flag(x$print_all_choices)
  checkmate::assert_flag(x$quit)
  checkmate::assert_flag(x$show_id)
  checkmate::assert_int(x$print_no_of_choices, lower = 1)
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
pxe_position_path <- function(x, init_slash = TRUE, as_vector = FALSE, include_rootpath = FALSE){
  checkmate::assert_class(x, "pxweb_explorer")
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
  sp <- pxe_position_path(x, init_slash = TRUE, include_rootpath = FALSE)
  if(nchar(sp) > 1) cat("         at '", sp, "'\n", sep="") 
  titl <- pxe_position_title(x)
  if(nchar(titl) > 1) cat("   INFO: ", titl, "\n", sep="") 
  print_bar()  
  pxe_print_choices(x)
  print_bar()  
}


pxe_position_title <- function(x){
  ""
}

#' @rdname pxweb_explorer
#' @keywords internal
print_bar <- function(){
  cat(rep("=", round(getOption("width")*0.95)), "\n",sep="")
}

#' @rdname pxweb_explorer
#' @keywords internal
pxe_print_choices <- function(x){
  obj <- pxe_pxobj_at_position(x)
  show_no <- x$print_no_of_choices
  if(x$print_all_choices | length(obj) <= show_no * 2){
    print_idx <- 1:length(obj) 
  } else {
    print_idx <- c(1:show_no, NA, (length(obj) - show_no + 1):length(obj))
  }

  print_idx_char <- as.character(print_idx)
  print_idx_char_nmax <- max(nchar(print_idx_char), na.rm = TRUE)
  print_idx_char <- str_pad(print_idx_char, print_idx_char_nmax )
  
#  cat(str_pad(txt = " ", n = print_idx_char_nmax + 6), "CHOICES\n")
  
  for(i in seq_along(print_idx)){
    if(is.na(print_idx[i])) {
      cat("\n")
      next
    }
    if(x$show_id){
      cat(" [", print_idx_char[i], " ] : ", obj[[print_idx[i]]]$text, " (", obj[[print_idx[i]]]$id ,")", "\n", sep = "")    
    } else {
      cat(" [", print_idx_char[i], " ] : ", obj[[print_idx[i]]]$text, " \n", sep = "")
    }
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
  user_input <- pxe_input(pxe)
  pxe <- pxe_handle_input(user_input, pxe)
  pxe
}

pxe_handle_input <- function(user_input, pxe){
  checkmate::assert_class(pxe, "pxweb_explorer")
  UseMethod("pxe_handle_input")
}

pxe_handle_input.numeric <- function(user_input, pxe){
  obj <- pxe_pxobj_at_position(pxe)
  if(pxe_position_is_variable(pxe)){
    pxe$print_all_choices <- FALSE
    stop("not done yet")
  } else {
    new_pos <- obj[[user_input]]$id
    pxe <- pxe_add_position(pxe, new_pos)
  }
  assert_pxweb_explorer(pxe)
  pxe
}

pxe_handle_input.character <- function(user_input, pxe){
  
  user_input_ok <- FALSE
  if(user_input == "b"){
    pxe <- pxe_back_position(pxe)
    user_input_ok <- TRUE
  }
  
  if(user_input == "i"){
    pxe$show_id <- !pxe$show_id
    user_input_ok <- TRUE    
  }
  
  if(user_input == "a"){
    pxe$print_all_choices <- !pxe$print_all_choices
    user_input_ok <- TRUE    
  }
  
  if(!user_input_ok) stop("Not implemented part!")
  
  assert_pxweb_explorer(pxe)
  pxe
}


#' Move in the \code{pxweb_explorer} position
#' 
#' @param pxe a \code{pxweb_explorer} object.
#' @param new_pos add a new position.
#'
pxe_back_position <- function(pxe){
  checkmate::assert_class(pxe, "pxweb_explorer")
  pxe$position <- pxe$position[-length(pxe$position)]
  obj <- pxe_pxobj_at_position(pxe)
  if(is.null(obj)){
    pxe_pxobj_at_position(pxe) <- 
      pxweb_get(pxe_position_path(pxe, include_rootpath = TRUE))
  }
  pxe$print_all_choices <- FALSE
  assert_pxweb_explorer(pxe)
  pxe
}
#' @rdname pxe_back_position
#' @keywords internal
pxe_add_position <- function(pxe, new_pos){
  checkmate::assert_class(pxe, "pxweb_explorer")
  checkmate::assert_string(new_pos)
  pxe$position[length(pxe$position) + 1] <- new_pos
  obj <- pxe_pxobj_at_position(pxe)
  if(is.null(obj)){
    pxe_pxobj_at_position(pxe) <- 
      pxweb_get(pxe_position_path(pxe, include_rootpath = TRUE))
  }
  pxe$print_all_choices <- FALSE
  assert_pxweb_explorer(pxe)
  pxe
}


#' Get (allowed) inputs for a \code{pxweb_explorer} object.
#' 
#' @param pxe a \code{pxweb_explorer_object}
#' @keywords internal
pxe_input <- function(pxe){
  checkmate::assert_class(pxe, "pxweb_explorer")
  
  input_ok <- FALSE
  allowed_input <- pxe_allowed_input(pxe)
  
  while(!input_ok){
    print(allowed_input)
    user_input <- scan(what=character(), multi.line = FALSE, quiet=TRUE, nlines=1, sep = "\n")
    user_input <- pxe_parse_input(user_input, allowed_input)
    input_ok <- user_input$ok
  }
  class(user_input) <- c("pxweb_user_input", "list")
  user_input$input
}
  
pxe_parse_input <- function(user_input, allowed_input){
  checkmate::assert_string(user_input)
  checkmate::assert_class(allowed_input, "pxweb_input_allowed")
  
  ui <- str_trim(user_input)
  if(ui %in% allowed_input$keys$code[allowed_input$keys$allowed]){
    return(list(ok = TRUE, input = ui))
  }
  if(grepl(x = ui, pattern = "^[:,0-9 ]*$")){
    ui <- eval(parse(text=paste("c(", ui, ")")))
    ui <- ui[!duplicated(ui)]
    if(!all(ui %in% 1:allowed_input$max_choice)){
      return(list(ok = FALSE))
    }
    if(allowed_input$multiple_choice | length(ui) == 1){
      return(list(ok = TRUE, input = ui))
    }
  }
  return(list(ok = FALSE))
}


pxe_allowed_input <- function(pxe){
  input_df <- data.frame(code=c("esc", "b", "*", "a", "e", "i", "i"),
                         text=c("Quit", "Back", "Select all", "Show all", "Eliminate", "Show id", "Hide id"),
                         stringsAsFactors = FALSE)
  input_df$allowed <- FALSE

  input_df$allowed[input_df$code == "esc"] <- TRUE
    
  if(length(pxe$position) > length(pxe$root)){
    input_df$allowed[input_df$code == "b"] <- TRUE
  }
  
  if(pxe_position_is_variable(pxe)){
    input_df$allowed[input_df$code == "*"] <- TRUE
    if(pxe_position_variable_can_be_eliminated(pxe)){
      input_df$allowed[input_df$code == "e"] <- TRUE
    }
  }
  
  if(!pxe$print_all_choices & pxe_position_choice_size(pxe) > pxe$print_no_of_choices*2){
    input_df$allowed[input_df$code == "a"] <- TRUE
  }
  
  if(pxe$show_id){
    input_df$allowed[input_df$text == "Hide id"] <- TRUE
  } else {
    input_df$allowed[input_df$text == "Show id"] <- TRUE
  }
  
  res <- list(keys = input_df,
              multiple_choice = pxe_position_multiple_choice_allowed(pxe),
              max_choice = pxe_position_choice_size(pxe))
  class(res) <- c("pxweb_input_allowed", "list")
  assert_pxweb_input_allowed(res)
  res
}

assert_pxweb_input_allowed <- function(x){
  checkmate::assert_class(x, "pxweb_input_allowed")
  checkmate::assert_names(names(x), permutation.of = c("keys", "multiple_choice", "max_choice"))
  checkmate::assert_flag(x$multiple_choice)
  checkmate::assert_int(x$max_choice, lower = 1)
  checkmate::assert_class(x$key, "data.frame")
  checkmate::assert_character(x$key$text)
  checkmate::assert_character(x$key$code)
  checkmate::assert_logical(x$key$allowed)
}

print.pxweb_input_allowed <- function(x, ...){
  if(!x$multiple_choice){
    cat("Enter the number you want to choose:\n")
  } else {
    cat("Enter one or more number(s) to choose:\n")
    cat("Separate multiple choices by ',' and intervals of choices by ':'\n")
  }
  txt <- paste("(", paste(paste(paste("'", x$keys$code[x$keys$allowed], "'", sep = ""), "=", x$keys$text[x$keys$allowed]), collapse = ", "), ")", sep= "")
  cat(txt, "\n")
}

pxe_pxobj_at_position <- function(x){
  checkmate::assert_class(x, "pxweb_explorer")
  x$pxobjs[[pxe_position_path(x)]]$pxobj
}

`pxe_pxobj_at_position<-` <- function(x, value){
  checkmate::assert_class(x, "pxweb_explorer")
  checkmate::assert_true(inherits(value, "pxweb_levels") | inherits(value, "pxweb_levels"))
  x$pxobjs[[pxe_position_path(x)]]$pxobj <- value
  assert_pxweb_explorer(x)
  x
}


pxe_position_is_variable <- function(x) {
  FALSE
}

pxe_position_choice_size <- function(x) {
  length(pxe_pxobj_at_position(x))
}

pxe_position_variable_can_be_eliminated <- function(x) {
  FALSE
}

pxe_position_multiple_choice_allowed <- function(x){
  FALSE
}

#' Taken from \code{trimws} for reasons of compatibility with previous R versios.
#' @keywords internal
#' @seealso trimws
#' @param x a string to trim.
#' @param which how to trim the string.
str_trim <- function (x, which = c("both", "left", "right")) 
{
  which <- match.arg(which)
  mysub <- function(re, x) sub(re, "", x, perl = TRUE)
  if (which == "left") 
    return(mysub("^[ \t\r\n]+", x))
  if (which == "right") 
    return(mysub("[ \t\r\n]+$", x))
  mysub("[ \t\r\n]+$", mysub("^[ \t\r\n]+", x))
}

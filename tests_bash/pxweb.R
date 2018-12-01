#install.packages("devtools")
devtools::install_local(getwd())
library(pxweb)
rm(list = ls())

print(sessionInfo())

# Create list of API paths
apis <- pxweb_api_catalogue()
api_paths <- pxweb:::pxweb_test_create_api_paths(apis)
api_idx <- api_paths$idx
api_paths <- api_paths$paths

cat("\n\nPING APIS:\n")

# Ping all APIs shallowly
errored <- rep(FALSE, length(api_paths))
config_diff <- rep(FALSE, length(api_paths))
ping_results <- list()
for(i in seq_along(api_paths)){
  cat(api_paths[i], "\n")
  ping_results[[i]] <- try(pxweb(api_paths[i]), silent = TRUE)
  if(inherits(ping_results[[i]], "try-error")){
    errored[i] <- TRUE
  }
  
  # Check if config and api cataloge has the same settings.
  if(ping_results[[i]]$config$calls_per_period != apis[[api_idx[i]]]$calls_per_period){
    config_diff[i] <- TRUE
  }
  if(ping_results[[i]]$config$period_in_seconds != apis[[api_idx[i]]]$period_in_seconds){
    config_diff[i] <- TRUE
  }
  if(ping_results[[i]]$config$max_values_to_download != apis[[api_idx[i]]]$max_values_to_download){
    config_diff[i] <- TRUE
  }
}



if(any(errored)){
  cat("\n\nERRONEOUS PATHS:\n")
  for(i in seq_along(which(errored))){
    cat("\n", api_paths[which(errored)[i]], "\n", sep ="")
    cat(ping_results[[which(errored)[i]]][1])
  }
}

if(any(config_diff)){
  cat("\n\nAPI DIFF BETWEEN API CONFIG AND THE R API CATALOGUE:\n")
  for(i in seq_along(which(config_diff))){
    cat("\n", api_paths[which(config_diff)[i]], ":\n", sep ="")
    cat("  Calls per period: R API catalogue (", apis[[api_idx[which(config_diff)[i]]]]$calls_per_period ,") and API config (", ping_results[[which(config_diff)[i]]]$config$calls_per_period,").\n",  sep = "")
    cat("  Periods in seconds: R API catalogue (", apis[[api_idx[which(config_diff)[i]]]]$period_in_seconds ,") and API config (", ping_results[[which(config_diff)[i]]]$config$period_in_seconds,").\n",  sep = "")
    cat("  Max values: R API catalogue (", apis[[api_idx[which(config_diff)[i]]]]$max_values_to_download ,") and API config (", ping_results[[which(config_diff)[i]]]$config$max_values_to_download,").\n",  sep = "")
  }
}

warns <- warnings()
if(length(warns) > 0){
  print(warns)
}

# Touch all APIs
touch_results <- list()
for(i in seq_along(api_paths)){
  if(errored[i]) {
    next
  }
  cat(api_paths[i], "\n")
  touch_results[[i]] <- try(pxweb_test_api_endpoint(url = api_paths[i], test_type = "touch", verbose = FALSE), silent = TRUE)
  if(inherits(touch_results[[i]], "try-error")){
    errored[i] <- TRUE
  }
}

if(any(errored)){
  cat("\n\nERRORS:\n")
  for(i in seq_along(which(errored))){
    cat("\n", api_paths[which(errored)[i]], "\n", sep ="")
    cat(touch_results[[which(errored)[i]]][1])
  }
}

warns <- warnings()
if(length(warns) > 0){
  print(warns)
}


# Check new APIs
apis <- pxweb_api_catalogue()
# Change to master branch later on
gh_apis <- pxweb:::pxweb_api_catalogue_from_github(branch = "test")
new_api_idx <- which(!names(apis) %in% names(gh_apis))
new_api_errored <- FALSE

if(length(new_api_idx) > 0){
  new_api_paths <- pxweb:::pxweb_test_create_api_paths(apis[new_api_idx])
  new_api_index <- new_api_paths$idx
  new_api_paths <- new_api_paths$paths
  first_results <- list()
  new_api_errored <- rep(FALSE, length(new_api_paths))
  
  for(i in seq_along(new_api_idx)){
    cat(new_api_paths[new_api_index[i]], "\n")
    first_results[[i]] <- try(pxweb_test_api_endpoint(url = new_api_paths[new_api_index[i]], test_type = "first", verbose = TRUE, time_limit = 10*60), silent = TRUE)
    if(inherits(first_results[[i]], "try-error")){
      new_api_errored[i] <- TRUE
    }
  }
}

if(any(new_api_errored)){
  cat("\n\nNEW API ERRORS:\n")
  for(i in seq_along(which(new_api_errored))){
    cat("\n", new_api_paths[which(new_api_errored)[i]], "\n", sep ="")
    cat(first_results[[which(new_api_errored)[i]]][1])
  }
}


# Check for duplicates in Alias
api_alias_table <- pxweb:::pxweb_api_catalogue_alias_table()
duplicated_alias <- duplicated(api_alias_table$alias)
if(any(duplicated_alias)){
  cat("\nDUPLICATE ALIAS IDENTIFIED:\n\n")
  print(api_alias_table[duplicated_alias,])
}

# Check for duplicates in names
api_cat <- pxweb_api_catalogue()
duplicated_names <- duplicated(names(api_cat))
if(any(duplicated_names)){
  cat("\nDUPLICATE NAMES IDENTIFIED:\n\n")
  print(names(api_cat)[duplicated_names])
}

# Check API parameters has en as default
apis <- pxweb_api_catalogue()
parameter_error <- rep(FALSE, length(apis))
for (i in seq_along(apis)) {
  if("en" %in% apis[[i]]$lang) {
    if(apis[[i]]$lang[1] != "en"){
      cat("LANGUAGE PARAMETERS ERROR ('en' should be default):", names(apis[i]), "\n")
      parameter_error[i] <- TRUE
    }
  }
}


if(any(errored) | length(warns) > 0 | any(config_diff) | any(new_api_errored) | any(duplicated_alias) | any(parameter_error) | any(duplicated_names)){
  quit(save = "no", status = 1)
}


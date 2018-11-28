#install.packages("devtools")
devtools::install_local(getwd())
library(pxweb)

print(sessionInfo())

# Create list of API paths
apis <- pxweb_api_catalogue()
api_idx <- numeric(0)
api_paths <- character(0)
for(i in seq_along(apis)){
  for(j in seq_along(apis[[i]]$version)){
    for(k in seq_along(apis[[i]]$lang)){
      api_idx[length(api_idx) + 1] <- i
      base_url <- apis[[i]]$url
      base_url <- gsub("\\[version\\]", base_url, replacement = apis[[i]]$version[j])
      base_url <- gsub("\\[lang\\]", base_url, replacement = apis[[i]]$lang[k])
      api_paths[length(api_paths) + 1] <- base_url
    }
  }
}

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


if(any(errored) | length(warns) > 0 | any(config_diff)){
  quit(save = "no", status = 1)
}


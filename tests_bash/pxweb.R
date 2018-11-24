#install.packages("devtools")
devtools::install_local(getwd())
library(pxweb)

print(sessionInfo())

# Create list of API paths
apis <- pxweb_api_catalogue()
api_paths <- character(0)
for(i in seq_along(apis)){
  for(j in seq_along(apis[[i]]$version)){
    for(k in seq_along(apis[[i]]$lang)){
      base_url <- apis[[i]]$url
      base_url <- gsub("\\[version\\]", base_url, replacement = apis[[i]]$version[j])
      base_url <- gsub("\\[lang\\]", base_url, replacement = apis[[i]]$lang[k])
      api_paths[length(api_paths) + 1] <- base_url
    }
  }
}

# Ping all APIs shallowly
errored <- rep(FALSE, length(api_paths))
ping_results <- list()
for(i in seq_along(api_paths)){
  cat(api_paths[i], "\n")
  ping_results[[i]] <- try(pxweb(api_paths[i]), silent = TRUE)
  if(inherits(ping_results[[i]], "try-error")){
    errored[i] <- TRUE
  }
}

if(any(errored)){
  cat("\n\nERRONEOUS PATHS:\n")
  for(i in seq_along(which(errored))){
    cat("\n", api_paths[which(errored)[i]], "\n", sep ="")
    cat(ping_results[[which(errored)[i]]][1])
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

if(any(errored) | length(warns) > 0){
  quit(save = "no", status = 1)
}

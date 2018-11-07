library(pxweb)

print(sessionInfo())

# Create list of API paths
apis <- api_catalogue()
api_paths <- character(0)
for(i in seq_along(apis)){
  for(j in seq_along(apis[[i]]$versions)){
    for(k in seq_along(apis[[i]]$languages)){
      base_url <- apis[[i]]$url
      base_url <- gsub("\\[version\\]", base_url, replacement = apis[[i]]$versions[j])
      base_url <- gsub("\\[lang\\]", base_url, replacement = apis[[i]]$languages[k])
      api_paths[length(api_paths) + 1] <- base_url
    }
  }
}

# Check all APIs
for(i in seq_along(api_paths)){
  cat(api_paths[i], "\n")
  res <- pxweb_test_api_endpoint(api_paths[i])
  cat(api_paths[i], "\n\n")
  if(any(res$error)){
    cat("ERRONEOUS PATHS:\n")
    cat(paste(paste(res$path[res$error], "\n"), collapse = ""))
    cat("\n")
  }
}

# Signal errors to operating system
if(FALSE){
  quit(save = "no", status = 1)
}



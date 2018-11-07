#install.packages("devtools")
devtools::install_local(getwd())
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
errored <- rep(FALSE, length(api_paths))
for(i in seq_along(api_paths)){
  cat(api_paths[i], "\n")
  res <- try(pxweb(api_paths[i]), silent = TRUE)
  if(inherits(res, "try-error")){
    errored[i] <- TRUE
  }
}

if(any(errored)){
  print(api_paths[which(errored)])
  quit(save = "no", status = 1)
}

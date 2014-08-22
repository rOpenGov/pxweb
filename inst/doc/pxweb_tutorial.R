
## ----install, eval=FALSE-------------------------------------------------
## install.packages("pxweb")


## ----test, message=FALSE, warning=FALSE, eval=TRUE-----------------------
library(pxweb)


## ----locale, eval=FALSE--------------------------------------------------
## Sys.setlocale(locale="UTF-8")


## ----apiparameters, message=FALSE, eval=TRUE-----------------------------
library(pxweb)
print(api_parameters())


## ----standardquery, message=FALSE, eval=FALSE----------------------------
## baseURL <- base_url("statfi", "v1", "fi")
## d <- interactive_pxweb(baseURL)


## ----citation, message=FALSE, eval=TRUE----------------------------------
citation("pxweb")


## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()



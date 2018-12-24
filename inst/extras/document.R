library(devtools)
document("../../")

#library(knitr)
#knit("../../vignettes/pxweb_tutorial.Rmd", "../../vignettes/pxweb_tutorial.md")

library(knitr)
knit("../../README.Rmd", "../../README.md")

library(pkgdown)
setwd("../../"); build_site()





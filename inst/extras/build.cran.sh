# !/bin/sh
#/home/lemila/bin/R CMD BATCH document.R
#/home/lemila/bin/R CMD build ../../ --no-build-vignettes
/home/lemila/bin/R-3.6.2/bin/R CMD build ../../ 
/home/lemila/bin/R-3.6.2/bin/R CMD check --as-cran pxweb_0.9.14.tar.gz
/home/lemila/bin/R-3.6.2/bin/R CMD INSTALL pxweb_0.9.14.tar.gz


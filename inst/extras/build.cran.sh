/usr/local/bin/R CMD BATCH document.R
#/usr/local/bin/R CMD build ../../ --no-build-vignettes
/usr/local/bin/R CMD build ../../ 
/usr/local/bin/R CMD check --as-cran pxweb_0.5.53.tar.gz
/usr/local/bin/R CMD INSTALL pxweb_0.5.53.tar.gz


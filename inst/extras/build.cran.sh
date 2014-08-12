/usr/bin/R CMD BATCH document.R
/usr/bin/R CMD build ../../
/usr/bin/R CMD check --as-cran pxweb_0.3.45.tar.gz
/usr/bin/R CMD INSTALL pxweb_0.3.45.tar.gz


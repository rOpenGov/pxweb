# !/bin/sh
#/home/lei/bin/R CMD BATCH document.R
#/home/lei/bin/R CMD build ../../ --no-build-vignettes
/home/lei/bin/R-3.5.1/bin/R CMD build ../../ 
/home/lei/bin/R-3.5.1/bin/R CMD check --as-cran pxweb_0.7.1.tar.gz
/home/lei/bin/R-3.5.1/bin/R CMD INSTALL pxweb_0.7.1.tar.gz


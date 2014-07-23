/usr/bin/R CMD BATCH document.R
/usr/bin/R CMD build ../../
/usr/bin/R CMD check --as-cran finSCB_0.3.4.tar.gz
/usr/bin/R CMD INSTALL finSCB_0.3.4.tar.gz


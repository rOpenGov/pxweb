To reproduce the JSS manuscript PDF and replication HTML, run in R from this
directory:

```r
source("main.R")
```

This regenerates `case-study-population.pdf`, `workflow-model.pdf`,
`code.html`, and `article.pdf`. The file `code.html` is generated from
`code.R` with `knitr::spin()`, following the JSS recommendation for replication
code. By default the paper uses the cached Statistics Sweden metadata, data,
comments, and citation files in `data/`; set `PXWEB_PAPER_REFRESH_DATA=true` to
re-download the live data and refresh the caches.

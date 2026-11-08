## The R Journal manuscript

`paperRJ.tex` is the article body. Build it with the journal's wrapper by
running this in R from this directory:

```r
tools::texi2pdf("RJwrapper.tex", clean = TRUE)
```

This produces `RJwrapper.pdf` using `paperRJ.bib` and the two figure PDFs.
The local `RJournal.sty` comes from the journal's official template and is
needed to compile here. The journal's LaTeX author guide excludes it from the
submission archive.

Run the R Journal replication entry point from this directory with
`Rscript --vanilla paperRJ.R`. It runs the examples and regenerates both
figures using the snapshots in `data/`. Install `pxweb` 1.0.0 or later and
`knitr` first. Graphviz is optional.

The generated `rj-submission/` folder contains the staged manuscript,
replication files, figures, PDF, and motivating letter.
From inside that folder, this command uses the style in the parent directory
on macOS or Linux:

```sh
TEXINPUTS=..: Rscript --vanilla -e 'tools::texi2pdf("RJwrapper.tex", clean = TRUE)'
```

## Replication and standard LaTeX build

To regenerate the figures, replication HTML, and standard article PDF, run in
R from this directory:

```r
source("main.R")
```

This regenerates `case-study-population.pdf`, `workflow-model.pdf`,
`code.html`, and `article.pdf`. The file `code.html` is generated from
`code.R` with `knitr::spin()`, following the JSS recommendation for replication
code. By default the paper uses the cached Statistics Sweden metadata, data,
comments, and citation files in `data/`; set `PXWEB_PAPER_REFRESH_DATA=true` to
re-download the live data and refresh the caches.

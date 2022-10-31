#!/bin/bash
# Need to be in Project root

git log -n 1
R --version
Rscript --vanilla tests_bash/pxweb.R

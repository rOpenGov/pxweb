#!/bin/bash
# Need to be in Project root
#

git log -n 1
R --version
script_start_time=$(date +"%T")
# Set Pxweb Error to 1 (to break Travis)
echo "PXWEB_ERROR=1" > "PXWEB_ERROR.sh"

echo $(date +"%T")

echo " " && echo -en "travis_fold:start:test-pxweb\n"
Rscript --vanilla bash_tests/pxweb.R
r_exit=$?; echo $r_exit; if [[ $r_exit != 0 ]]; then exit $r_exit; fi
echo $r_exit
echo "travis_fold:end:test-pxweb"

echo $(date +"%T")

script_end_time=$(date +"%T")
echo "Start: $script_start_time Stop: $script_end_time"
# Set PXWEB Error to 0
echo "PXWEB_ERROR=0" > "PXWEB_ERROR.sh"

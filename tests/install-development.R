#!/usr/bin/env Rscript

#
# Install packages from github using devtools
#

# Make sure devtools and opal dependencies are installed
if (!require('devtools', character.only=TRUE)) {
  install.packages('devtools', repos=c('http://cran.rstudio.com'), dependencies=TRUE)
}
if (!require('RCurl', character.only=TRUE)) {
  install.packages('RCurl', repos=c('http://cran.rstudio.com'), dependencies=TRUE)
}
if (!require('rjson', character.only=TRUE)) {
  install.packages('rjson', repos=c('http://cran.rstudio.com'), dependencies=TRUE)
}
# Install opal packages from github
library(devtools)
devtools::install_github('opal', username='datashield', ref='master')
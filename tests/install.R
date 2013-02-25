#!/usr/bin/env Rscript

#
# Install opal and dependencies from repositories
#

install.packages('opal', repos=c('http://cran.obiba.org','http://cran.rstudio.com'), dependencies=TRUE)
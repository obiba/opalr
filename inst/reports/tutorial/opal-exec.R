#!/usr/bin/Rscript

library(opalr)

# change report input path to fit your environment
input <- "inst/reports/tutorial/opal-prod.Rmd"

# change the output directory to fit your environment
output <- "/tmp"

# set report options
options(opal.url="http://demo.obiba.org:8080",
        opal.username="administrator",
        opal.password="password",
        opal.report.style="Flatly")

# expected report path is the html file in the output directory
opal.report(input, output=output, progress=TRUE)

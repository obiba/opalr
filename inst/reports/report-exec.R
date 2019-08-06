#!/usr/bin/Rscript

library(opalr)

input <- commandArgs(TRUE)[1]
output <- commandArgs(TRUE)[2]

options(opal.url="http://opal-demo.obiba.org",
        opal.username="administrator",
        opal.password="password",
        opal.datasource="datashield",
        opal.table="CNSIM1",
        opal.withStatistics=TRUE,
        opal.withGraphs=TRUE,
        opal.report.style="Flatly")

opal.report(input, output, progress=TRUE)

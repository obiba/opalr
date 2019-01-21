#!/usr/bin/Rscript

library(opalr)

input <- commandArgs(TRUE)[1]
output <- commandArgs(TRUE)[2]

options(opal.url="http://demo.obiba.org:8080",
        opal.username="administrator",
        opal.password="password",
        opal.datasource="onyx",
        opal.table="StandingHeight",
        opal.withStatistics=TRUE,
        opal.withGraphs=TRUE,
        opal.report.style="Flatly")

opal.report(input, output, progress=TRUE)

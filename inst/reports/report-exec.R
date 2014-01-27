#!/usr/bin/Rscript

library(opal)

input <- commandArgs(TRUE)[1]
output <- commandArgs(TRUE)[2]

options(opal.url="http://demo.obiba.org:8080",
        opal.username="administrator",
        opal.password="password",
        opal.datasource="onyx",
        opal.table="StandingHeight",
        opal.withStatistics=TRUE,
        opal.withGraphs=TRUE)

opal.report(input, output, boot_style="flatly",progress=TRUE)

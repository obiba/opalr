#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(opal)

message("**** datashield logins and assignments:")
server <- c("study1", "study2")
url <- c("https://some.opal.host:8443","https://another.opal.host")
user <- c("user1", "datashield-certificate.pem")
password <- c("user1pwd", "datashield-private.pem")
table <- c("store.Dataset","foo.DS")
logindata <- data.frame(server,url,user,password,table)
opals<-datashield.login(logindata, symbol="D", variables=list("GENDER","BMI"))

message("**** check assigned variables:")
datashield.symbols(opals)

message("**** execute some summary (if these methods are available in the opals):")
datashield.aggregate(opals,'summary.ds(D)')
datashield.aggregate(opals,'length(D$GENDER)')

message("**** clean symbols:")
datashield.rm(opals,'D')
datashield.symbols(opals)

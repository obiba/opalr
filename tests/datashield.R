#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(opal)

message("**** datashield logins and assignments:")
server <- c("demo")
url <- c("http://demo.obiba.org:8080")
user <- c("administrator")
password <- c("password")
table <- c("mica_demo.FNAC")
logindata <- data.frame(server,url,user,password,table)
opals<-datashield.login(logindata, assign=TRUE, variables=c("SUKUP","PITUUS"))

message("**** check assigned variables:")
datashield.symbols(opals)

message("**** execute some aggregate calls (if these methods are available in the opals):")
datashield.aggregate(opals,'colnames(D)')
datashield.aggregate(opals,'length(D$SUKUP)')

message("**** clean symbols:")
datashield.rm(opals,'D')
datashield.symbols(opals)

datashield.logout(opals)

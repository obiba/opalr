#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(opal)

server <- c("demo")
url <- c("http://localhost:8080")
user <- c("administrator")
password <- c("password")
table <- c("datashield.CNSIM1")
logindata <- data.frame(server,url,user,password,table)
opals<-datashield.login(logindata, assign=TRUE, variables=c("GENDER","PM_BMI_CONTINUOUS"))
datashield.symbols(opals)
datashield.aggregate(opals,'colnames(D)')
datashield.aggregate(opals,'length(D$GENDER)')
datashield.logout(opals, save='ilovedatashield')

opals<-datashield.login(logindata, restore='ilovedatashield')
datashield.symbols(opals)
datashield.aggregate(opals,'colnames(D)')
datashield.aggregate(opals,'length(D$GENDER)')
datashield.logout(opals, save='ilovedatashield')

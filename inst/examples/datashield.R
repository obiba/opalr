#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(opal)

message("**** datashield logins and assignments:")
server <- c("demo")
url <- c("http://localhost:8080")
user <- c("administrator")
password <- c("password")
table <- c("datashield.CNSIM1")
logindata <- data.frame(server,url,user,password,table)
opals<-datashield.login(logindata, assign=TRUE, variables=c("GENDER","PM_BMI_CONTINUOUS"))
print(opals)
message("**** check assigned variables:")
datashield.symbols(opals)

message("**** table assignment can also happen later:")
datashield.assign(opals, "D", "datashield.CNSIM1", variables=c("GENDER","PM_BMI_CONTINUOUS"), tibble=TRUE)
datashield.aggregate(opals,'class(D)')

message("**** execute some aggregate calls (if these methods are available in the opals):")
datashield.aggregate(opals,'colnames(D)')
datashield.aggregate(opals,'length(D$GENDER)')

message("**** clean symbols:")
datashield.rm(opals,'D')
datashield.symbols(opals)

datashield.logout(opals)

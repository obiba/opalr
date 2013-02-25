#!/usr/bin/env Rscript

#
# Opal Datashield client
#

library(opal)
# http login
#o<-opal.login('dsuser', 'password', 'http://localhost:8080')
# https login with ssl options
o1<-opal.login('dsuser1', 'password', 'https://localhost:8443',opts=list(ssl.verifyhost=0,ssl.verifypeer=0,sslversion=3))
o2<-opal.login('dsuser2', 'password', 'https://localhost:8443',opts=list(ssl.verifyhost=0,ssl.verifypeer=0,sslversion=3))
opals<-list(o1, o2)

message("**** assign some variables:")
datashield.assign(opals,'SEX','opal-data.HOP:GENDER')
datashield.assign(opals,'BMI','opal-data.HOP:PM_BMI_CONTINUOUS')
datashield.symbols(opals)

message("**** execute some summary (if these methods are available in the opals):")
datashield.aggregate(opals,'length(SEX)')
datashield.aggregate(opals,'summary(SEX)')
datashield.aggregate(opals,'summary(BMI)')

message("**** clean symbols:")
datashield.rm(opals,'SEX')
datashield.rm(opals,'BMI')
datashield.symbols(opals)

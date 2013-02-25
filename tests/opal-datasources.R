#!/usr/bin/env Rscript

#
# Opal R client
#

library(opal)
# http login
#o<-opal.login('ruser', 'password', 'http://localhost:8080')
# https login with ssl options
o<-opal.login('ruser', 'password', 'https://localhost:8443',opts=list(ssl.verifyhost=0,ssl.verifypeer=0,sslversion=3))

message("**** datasources:")
opal.datasources(o)
message("**** opal-data datasource:")
opal.datasource(o,'opal-data')
message("**** opal-data datasource tables:")
opal.tables(o,'opal-data')
message("**** opal-data datasource HOP table:")
opal.table(o,'opal-data','HOP')
message("**** opal-data datasource HOP table GENDER variables:")
opal.variables(o,'opal-data','HOP')
message("**** opal-data datasource HOP table GENDER variable:")
opal.variable(o,'opal-data','HOP','GENDER')
message("**** missing variable:")
opal.variable(o,'opal-data','HOP','XXXX')

message("**** assign some variables:")
opal.assign(o,'SEX','opal-data.HOP:GENDER')
opal.assign(o,'BMI','opal-data.HOP:PM_BMI_CONTINUOUS')
opal.symbols(o)

message("**** execute some summary:")
opal.execute(o,'length(SEX)')
opal.execute(o,'summary(SEX)')
opal.execute(o,'summary(BMI)')

message("**** clean symbols:")
opal.rm(o,'SEX')
opal.rm(o,'BMI')
opal.symbols(o)

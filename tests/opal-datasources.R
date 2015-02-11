#!/usr/bin/env Rscript

#
# Opal R client
#

library(opal)

# https login
o<-opal.login('administrator', 'password', 'https://demo.obiba.org:8443')

message("**** datasources:")
opal.datasources(o)
message("**** mica_demo datasource:")
opal.datasource(o,'mica_demo')
message("**** mica_demo datasource tables:")
opal.tables(o,'mica_demo')
message("**** mica_demo datasource HOP-FNAC table:")
opal.table(o,'mica_demo','HOP-FNAC')
message("**** mica_demo datasource HOP-FNAC table variables:")
opal.variables(o,'mica_demo','HOP-FNAC')
message("**** mica_demo datasource HOP-FNAC table GENDER variable:")
opal.variable(o,'mica_demo','HOP-FNAC','GENDER')
message("**** missing variable:")
opal.variable(o,'mica_demo','HOP-FNAC','XXXX')

message("**** assign some variables:")
opal.assign(o,'SEX','mica_demo.HOP-FNAC:GENDER')
opal.assign(o,'BMI','mica_demo.HOP-FNAC:PM_BMI_CONTINUOUS')
opal.symbols(o)

message("**** execute some summary:")
opal.execute(o,'length(SEX)')
opal.execute(o,'summary(SEX)')
opal.execute(o,'summary(BMI)')

message("**** clean symbols:")
opal.rm(o,'SEX')
opal.rm(o,'BMI')
opal.symbols(o)

message("**** assign enumerated variables in a data.frame:")
opal.assign(o,'HOP','mica_demo.HOP-FNAC',variables=list('GENDER','PM_BMI_CONTINUOUS'))
opal.symbols(o)

message("**** execute some operations on the data.frame:")
opal.execute(o,'head(HOP)')
opal.execute(o,'colnames(HOP)')
opal.execute(o,'summary(HOP)')
opal.execute(o,'summary(HOP$GENDER)')
opal.execute(o,'summary(HOP$PM_BMI_CONTINUOUS)')

message("**** assign variables filtered by Magma javascript in a data.frame:")
opal.assign(o,'HOP','mica_demo.HOP-FNAC',variables='name().matches("DIAB")')

message("**** execute some operations on the data.frame:")
opal.execute(o,'head(HOP)')
vars <- opal.execute(o,'colnames(HOP)')
vars
lapply(vars, function(v) { opal.execute(o, paste0('summary(HOP$', v, ')'))  })

message("**** clean symbols and logout:")
opal.rm(o,'HOP')
opal.logout(o)

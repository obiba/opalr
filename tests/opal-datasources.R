#!/usr/bin/env Rscript

#
# Opal R client
#

library(opal)

# https login
o<-opal.login('administrator', 'password', 'https://opal-demo.obiba.org')

message("**** datasources:")
opal.datasources(o)
message("**** FNAC datasource:")
opal.datasource(o,'FNAC')
message("**** FNAC datasource tables:")
opal.tables(o,'FNAC')
message("**** FNAC datasource HOP table:")
opal.table(o,'FNAC','HOP')
message("**** FNAC datasource HOP table variables:")
opal.variables(o,'FNAC','HOP')
message("**** FNAC datasource HOP table GENDER variable:")
opal.variable(o,'FNAC','HOP','GENDER')
message("**** missing variable:")
opal.variable(o,'FNAC','HOP','XXXX')

message("**** assign some variables:")
opal.assign(o,'SEX','FNAC.HOP:GENDER')
opal.assign(o,'BMI','FNAC.HOP:PM_BMI_CONTINUOUS')
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
opal.assign(o,'HOP','FNAC.HOP',variables=list('GENDER','PM_BMI_CONTINUOUS'))
opal.symbols(o)

message("**** execute some operations on the data.frame:")
opal.execute(o,'head(HOP)')
opal.execute(o,'colnames(HOP)')
opal.execute(o,'summary(HOP)')
opal.execute(o,'summary(HOP$GENDER)')
opal.execute(o,'summary(HOP$PM_BMI_CONTINUOUS)')

message("**** assign variables filtered by Magma javascript in a data.frame:")
opal.assign(o,'HOP','FNAC.HOP',variables='name().matches("DIAB")')

message("**** execute some operations on the data.frame:")
opal.execute(o,'head(HOP)')
vars <- opal.execute(o,'colnames(HOP)')
vars
lapply(vars, function(v) { opal.execute(o, paste0('summary(HOP$', v, ')'))  })

message("**** clean symbols and logout:")
opal.rm(o,'HOP')
opal.logout(o)

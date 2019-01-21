#!/usr/bin/env Rscript

#
# Opal R client
#

library(opalr)

# https login
o<-opal.login('administrator', 'password', 'https://opal-demo.obiba.org')

datasource <- 'CPTP'
table <- 'Cag_coreqx'

message("**** all datasources:")
opal.datasources(o)
message("**** a datasource:")
opal.datasource(o,datasource)
message("**** all datasource tables:")
opal.tables(o, datasource)
message("**** a datasource table:")
opal.table(o, datasource, table)
message("**** all variables of a datasource table:")
opal.variables(o, datasource, table)
message("**** a variable of a datasource table:")
opal.variable(o, datasource, table, 'A_SDC_GENDER')
message("**** a missing variable:")
opal.variable(o,datasource, table,'XXXX')
message("**** a valueset of a datasource table:")
opal.valueset(o, datasource, table, '1346991013469910')

message("**** assign some variables:")
opal.assign(o,'SEX','CPTP.Cag_coreqx:A_SDC_GENDER')
opal.assign(o,'AGE','CPTP.Cag_coreqx:A_SDC_RECRUITMENT_AGE_CALC')
opal.symbols(o)

message("**** execute some summary:")
opal.execute(o,'length(SEX)')
opal.execute(o,'summary(SEX)')
opal.execute(o,'summary(AGE)')

message("**** clean symbols:")
opal.rm(o,'SEX')
opal.rm(o,'AGE')
opal.symbols(o)

message("**** assign enumerated variables in a data.frame:")
opal.assign(o,'D','CPTP.Cag_coreqx',variables=list('A_SDC_GENDER','A_SDC_RECRUITMENT_AGE_CALC'))
opal.symbols(o)

message("**** execute some operations on the data.frame:")
opal.execute(o,'head(D)')
opal.execute(o,'colnames(D)')
opal.execute(o,'summary(D)')
opal.execute(o,'summary(D$A_SDC_GENDER)')
opal.execute(o,'summary(D$A_SDC_RECRUITMENT_AGE_CALC)')

message("**** assign variables filtered by Magma javascript in a data.frame:")
opal.assign(o,'D','CPTP.Cag_coreqx',variables='name().matches("CHILD")')

message("**** execute some operations on the data.frame:")
opal.execute(o,'head(D)')
vars <- opal.execute(o,'colnames(D)')
vars
lapply(vars, function(v) { opal.execute(o, paste0('summary(D$', v, ')'))  })

message("**** clean symbols and logout:")
opal.rm(o,'D')
opal.logout(o)

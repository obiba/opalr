#!/usr/bin/env Rscript

#
# Opal R client
#

library(opal)

# Save ID is session ID
o<-opal.login('administrator', 'password', 'http://localhost:8080')
opal.assign(o,'HOP','datashield.CNSIM1',variables=list('GENDER','PM_BMI_CONTINUOUS'))
opal.assign(o,'SEX','datashield.CNSIM1:GENDER')
opal.assign(o,'BMI','datashield.CNSIM1:PM_BMI_CONTINUOUS')
opal.symbols(o)
opal.execute(o,'save(BMI, SEX, file = "BS.RData")')
opal.execute(o, 'dir.create("test")')
opal.execute(o,'save(HOP, file = "test/H.RData")')
opal.execute(o,'list.files(recursive=T)')
wsid <- opal.logout(o, save=TRUE)
wsid

o<-opal.login('administrator', 'password', 'http://localhost:8080', restore=wsid)
opal.symbols(o)
opal.execute(o,'list.files(recursive=T)')
opal.execute(o,'summary(HOP)')
opal.execute(o,'summary(SEX)')
opal.execute(o,'summary(BMI)')
opal.logout(o)

# Arbitrary save ID
o<-opal.login('administrator', 'password', 'http://localhost:8080')
opal.assign(o,'HOP','datashield.CNSIM1',variables=list('GENDER','PM_BMI_CONTINUOUS'))
opal.logout(o, save='toto')

o<-opal.login('administrator', 'password', 'http://localhost:8080', restore='toto')
opal.symbols(o)
opal.execute(o,'summary(HOP)')
opal.logout(o)

#!/usr/bin/env Rscript

#
# Opal R client
#

library(opalr)

# Save ID is session ID
o<-opal.login('administrator', 'password', url = 'http://localhost:8080')
opal.assign(o,'HOP','CNSIM.CNSIM1',variables=list('GENDER','PM_BMI_CONTINUOUS'))
opal.execute(o, 'head(HOP)')
opal.workspace_save(o, 'step1')
opal.assign(o,'SEX','CNSIM.CNSIM1:GENDER')
opal.workspace_save(o, 'step2')
opal.assign(o,'BMI','CNSIM.CNSIM1:PM_BMI_CONTINUOUS')
opal.workspace_save(o, 'step3')
opal.symbols(o)
opal.execute(o,'save(BMI, SEX, file = "BS.RData")')
opal.execute(o, 'dir.create("test")')
opal.execute(o,'save(HOP, file = "test/H.RData")')
opal.execute(o,'list.files(recursive=T)')
wsid <- opal.logout(o, save=TRUE)
wsid

o<-opal.login('administrator', 'password', url = 'http://localhost:8080', restore=wsid)
opal.symbols(o)
opal.execute(o,'list.files(recursive=T)')
opal.execute(o,'summary(HOP)')
opal.execute(o,'summary(SEX)')
opal.execute(o,'summary(BMI)')
opal.logout(o)

# Arbitrary save ID
o<-opal.login('administrator', 'password', url = 'http://localhost:8080')
opal.assign(o,'HOP','CNSIM.CNSIM1',variables=list('GENDER','PM_BMI_CONTINUOUS'))
opal.logout(o, save='toto')

o<-opal.login('administrator', 'password', url = 'http://localhost:8080', restore='toto')
opal.symbols(o)
opal.execute(o,'summary(HOP)')
opal.logout(o)

# user specific workspaces
o<-opal.login('user1', 'password', url = 'http://localhost:8080')
opal.assign(o,'SEX','FNAC.FNAC:SUKUP')
opal.logout(o, save='xxx')

# list user workspaces and remove some
o<-opal.login('user1', 'password', url = 'http://localhost:8080')
opal.workspaces(o)
opal.workspace_rm(o,'xxx')
opal.logout(o)

# administrator has access to all user workspaces
o<-opal.login('administrator', 'password', url = 'http://localhost:8080')
opal.workspaces(o)
opal.logout(o)


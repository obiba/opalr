#!/usr/bin/env Rscript

library(opal)

options(opal.username='administrator', 
        opal.password='password', 
        opal.url='http://localhost:8080') 
drugs <- "test.Drugs"
fnac <- "large.FNAC"

options(verbose=FALSE)

#
# start a Datashield session
#
o <- opal.login()

rid <- datashield.assign(o,"D",drugs, async=TRUE)
datashield.command(o, rid, wait=TRUE)
rid <- datashield.aggregate(o, quote(colnames(D)), async=TRUE)
datashield.command(o, rid, wait=TRUE)
datashield.command_result(o, rid)
datashield.commands(o)
datashield.commands_rm(o)

datashield.logout(o)

#
# start 2 Datashield sessions in parallel
#
o1 <- opal.login()
o2 <- opal.login()
os <- list(o1,o2)

# assign a large table asynchronously in each Datashield session (=in parallel) and wait for them to complete
rid1 <- datashield.assign(o1, "D", drugs, async=TRUE)
rid2 <- datashield.assign(o2, "D", drugs, async=TRUE)
rids <- list(rid1,rid2)
datashield.commands(os)
datashield.command(os, rids, wait=TRUE)
datashield.symbols(os)


# assign a large variable asynchronously in each Datashield session (=in parallel) and wait for them to complete
rid1 <- datashield.assign(o1, "V", paste0(fnac, ":SUKUP"), async=TRUE)
rid2 <- datashield.assign(o2, "V", paste0(fnac, ":SUKUP"), async=TRUE)
rids <- list(rid1,rid2)
datashield.commands(os)
datashield.command(os, rids, wait=TRUE)
datashield.symbols(os)
rids <- datashield.aggregate(os,quote(table1d.ds(V)), async=TRUE)
res <- datashield.command_result(os, rids, wait=TRUE)
print(res)

# assign a large table asynchronously in each Datashield session (=in parallel) and wait for them to complete
rid1 <- datashield.assign(o1, "D", fnac, variables=list("SUKUP","PITUUS"), async=TRUE)
rid2 <- datashield.assign(o2, "D", fnac, variables=list("SUKUP","PITUUS"), async=TRUE)
rids <- list(rid1,rid2)
datashield.command(os, rids, wait=TRUE)
rids <- datashield.aggregate(os, quote(table2d.ds(D$SUKUP,D$PITUUS)), async=TRUE)
res <- datashield.command_result(os, rids, wait=TRUE)
print(res)

datashield.logout(os)

o1 <- opal.login()
o2 <- opal.login(url="http://demo.obiba.org:8080")
os <- list(o1,o2)
print(os)

rid1 <- datashield.assign(o1, "D", fnac, variables=list("SUKUP","PITUUS"), async=TRUE)
rid2 <- datashield.assign(o2, "D", "mica_demo.FNAC", variables=list("SUKUP","PITUUS"), async=TRUE)
rids <- list(rid1,rid2)
datashield.commands(os)
datashield.command(os, rids, wait=TRUE)

rids <- datashield.aggregate(os,quote(table1d.ds(D$SUKUP)), async=TRUE)
res <- datashield.command_result(os, rids, wait=TRUE)
print(res)

datashield.logout(os)
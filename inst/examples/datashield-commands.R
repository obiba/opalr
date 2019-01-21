#!/usr/bin/env Rscript

library(opalr)

options(opal.username='administrator', 
        opal.password='password', 
        opal.url='https://opal-demo.obiba.org') 
fnac <- "FNAC.FNAC"

options(verbose=FALSE)

#
# start a Datashield session
#
o <- opal.login()

rid <- datashield.assign(o,"D",fnac, async=TRUE, wait=FALSE)
datashield.command(o, rid, wait=TRUE)
rid <- datashield.aggregate(o, quote(colnames(D)), async=TRUE, wait=FALSE)
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
rid1 <- datashield.assign(o1, "D", fnac, async=TRUE, wait=FALSE)
rid2 <- datashield.assign(o2, "D", fnac, async=TRUE, wait=FALSE)
rids <- list(rid1,rid2)
datashield.commands(os)
datashield.command(os, rids, wait=TRUE)
datashield.symbols(os)


# assign a large variable asynchronously in each Datashield session (=in parallel) and wait for them to complete
rid1 <- datashield.assign(o1, "V", paste0(fnac, ":SUKUP"), async=TRUE, wait=FALSE)
rid2 <- datashield.assign(o2, "V", paste0(fnac, ":SUKUP"), async=TRUE, wait=FALSE)
rids <- list(rid1,rid2)
datashield.commands(os)
datashield.command(os, rids, wait=TRUE)
datashield.symbols(os)
rids <- datashield.aggregate(os,quote(table1dDS(V)), async=TRUE, wait=FALSE)
res <- datashield.command_result(os, rids, wait=TRUE)
print(res)

# assign a large table asynchronously in each Datashield session (=in parallel) and wait for them to complete
rid1 <- datashield.assign(o1, "D", fnac, variables=list("SUKUP","PITUUS"), async=TRUE, wait=FALSE)
rid2 <- datashield.assign(o2, "D", fnac, variables=list("SUKUP","PITUUS"), async=TRUE, wait=FALSE)
rids <- list(rid1,rid2)
datashield.command(os, rids, wait=TRUE)
#rids <- datashield.aggregate(os, quote(table2dDS(D$SUKUP,D$PITUUS)), async=TRUE)
#res <- datashield.command_result(os, rids, wait=TRUE)
#print(res)

datashield.logout(os)

o1 <- opal.login()
o2 <- opal.login(url="https://opal-demo.obiba.org")
os <- list(o1,o2)
print(os)

rid1 <- datashield.assign(o1, "D", fnac, variables=list("SUKUP","PITUUS"), async=TRUE, wait=FALSE)
rid2 <- datashield.assign(o2, "D", "FNAC.FNAC", variables=list("SUKUP","PITUUS"), async=TRUE, wait=FALSE)
rids <- list(rid1,rid2)
datashield.commands(os)
datashield.command(os, rids, wait=TRUE)

rids <- datashield.aggregate(os,quote(table1dDS(D$SUKUP)), async=TRUE, wait=FALSE)
res <- datashield.command_result(os, rids, wait=TRUE)
print(res)

datashield.logout(os)

server <- c("local", "demo")
url <- c(getOption("opal.url"),"https://opal-demo.obiba.org")
user <- c("administrator", "administrator")
password <- c("password", "password")
table <- c(fnac,"FNAC.FNAC")
logindata <- data.frame(server,url,user,password,table)

os <- datashield.login(logins=logindata, assign=TRUE, variables=list("SUKUP","PITUUS"))
print(os)

datashield.logout(os)


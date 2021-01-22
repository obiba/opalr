#!/usr/bin/env Rscript

library(opalr)

options(opal.username='administrator', 
        opal.password='password', 
        opal.url='http://localhost:8080') 
table <- "CNSIM.CNSIM1"

#
# start a R session
#
o <- opal.login()

# execute a command synchronously
opal.execute(o, ".libPaths()")
opal.commands(o)

# execute a command asynchronously
rid <- opal.execute(o, ".libPaths()", async=TRUE)
opal.commands(o)
# get the result
opal.command_result(o, rid)
opal.commands(o)

# assign a large table asynchronously and wait for it to complete
opal.assign(o, "V", paste0(table,':GENDER'))
opal.execute(o, "head(V)")
V <- opal.execute(o, "V")
rid <- opal.assign(o, "D", table, async=TRUE)
opal.command_result(o, rid, wait=TRUE)
opal.commands(o)
opal.symbols(o)
opal.execute(o, "head(D)")
opal.symbol_rm(o, "D")
rid <- opal.assign(o, "D", table, async=TRUE)
opal.command(o, rid, wait=TRUE)
opal.commands(o)
opal.symbols(o)
opal.execute(o, "head(D)")
opal.assign(o, "LG", as.symbol("D$LAB_GLUC"), async=FALSE)
rid <- opal.assign(o, "LG", as.symbol("D$LAB_GLUC"), async=TRUE)
opal.command_result(o, rid, wait=TRUE)
opal.execute(o, "summary(LG)")
opal.logout(o)

#
# start 2 R sessions in parallel
#
o1 <- opal.login()
o2 <- opal.login()
os <- list(o1,o2)

opal.assign(o1, "V", paste0(table,':GENDER'))
opal.assign(o2, "V", paste0(table,':GENDER'))

# assign a large table asynchronously in each R session (=in parallel) and wait for them to complete
rid1 <- opal.assign(o1, "D", table, async=TRUE)
rid2 <- opal.assign(o2, "D", table, async=TRUE)
rids <- list(rid1,rid2)
res <- lapply(1:length(os), function(i) {
  opal.command_result(os[[i]], rids[[i]], wait=TRUE)
})
opal.symbols(o1)
opal.symbols(o2)
opal.logout(os)

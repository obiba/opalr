#!/usr/bin/env Rscript

#
# Opal R client pushes arbitrary data to the R server
#

library(opalr)

# https login
o<-opal.login('administrator', 'password', 'http://localhost:8080')

# Push an arbitrary data frame to the R server
opal.assign.data(o, "D", mtcars)
opal.symbols(o)
opal.execute(o, "class(D)")
opal.execute(o, "head(D)")
opal.execute(o, "summary(D)")
# get it back
summary(opal.execute(o, "D"))
# compare with original
summary(mtcars)

# Push an arbitrary vector to the R server
opal.assign(o, "CYL", mtcars$cyl)
opal.symbols(o)
opal.execute(o, "class(CYL)")
opal.execute(o, "summary(CYL)")
# get it back
summary(opal.execute(o, "CYL"))
# compare with original
summary(mtcars$cyl)

# Push a string
opal.assign.data(o,"str", "Hello!")
opal.symbols(o)
opal.execute(o, "str")

opal.logout(o)

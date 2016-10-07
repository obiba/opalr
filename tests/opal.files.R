#!/usr/bin/env Rscript

#
# Opal R client
#

library(opal)

# https login
o<-opal.login('administrator', 'password', 'http://localhost:8080')
opal.file(o, '/home/administrator/datashield/CNSIM1.zip')
opal.file(o, '/home/administrator/datashield/hop.R')
opal.file(o, '/home/administrator/nasa/mars.csv')
opal.logout(o)

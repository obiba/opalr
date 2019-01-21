#!/usr/bin/env Rscript

#
# Opal R client
#

library(opalr)

# https login
o<-opal.login('administrator', 'password', 'http://localhost:8080')
opal.execute(o,'list.files(recursive=T)')
opal.file_download(o, '/home/administrator/datashield/CNSIM1.zip')
opal.file_download(o, '/home/administrator/datashield/hop.R')
opal.file_download(o, '/home/administrator/nasa/mars.csv')
list.files()
file.exists('CNSIM1.zip')
file.exists('hop.R')
file.exists('mars.csv')
unlink('CNSIM1.zip')
unlink('hop.R')
unlink('mars.csv')
opal.file_write(o, '/home/administrator/nasa/mars.csv')
opal.execute(o,'file.exists("mars.csv")')
opal.execute(o,'read.csv("mars.csv")')
opal.file_write(o, '/home/administrator/nasa/mars.csv', 'test/mars.csv')
opal.execute(o,'file.exists("test/mars.csv")')
opal.execute(o,'list.files(recursive=T)')
opal.execute(o,'read.csv("test/mars.csv")')
opal.file_read(o,'mars.csv','/tmp')
opal.file_download(o, '/home/administrator/nasa/mars.csv')
file.exists('mars.csv')
unlink('mars.csv')
opal.logout(o)

# Opal R

## Opal R Client

Usage:

    library('opal')
    o <- opal.login('username', 'passwd', 'http://localhost:8080')
    opal.assign(o, 'VAR1', 'datasource.table:VAR1')
    opal.execute(o, 'summary(VAR1)')

## DataSHIELD Client

Usage:

    library('opal')
    os <- opal.login('username', 'passwd', list('http://opal1.org', 'http://opal2.org'))
    datashield.assign(os, 'VAR1', 'datasource.table:VAR1')
    datashield.symbols(os)
    datashield.rm(os, 'VAR1')
    datashield.methods(os,type='assign')

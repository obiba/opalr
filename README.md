# Opal R

## Opal R Client

Usage:

    library('opal')
    o <- opal.login('username', 'passwd', 'http://localhost:8080')
    # Assign one variable to a R symbol
    opal.assign(o, 'VAR1', 'datasource.table:VAR1')
    opal.execute(o, 'summary(VAR1)')
    # Assign some variables as a data.frame to a R symbol
    opal.assign(o, 'TABLE', 'datasource.table', variables=list('VAR1','VAR2'))
    opal.execute(o, 'summary(TABLE$VAR1)')
    opal.execute(o, 'summary(TABLE$VAR2)')

## DataSHIELD Client

Usage:

    library('opal')
    os <- opal.login('username', 'passwd', list('http://opal1.org', 'http://opal2.org'))
    # Assign one variable to a R symbol
    datashield.assign(os, 'VAR1', 'datasource.table:VAR1')
    # Assign some variables as a data.frame to a R symbol
    datashield.assign(o, 'TABLE', 'datasource.table', variables=list('VAR1','VAR2'))
    datashield.symbols(os)
    datashield.rm(os, 'VAR1')
    datashield.rm(os, 'TABLE')
    datashield.methods(os,type='assign')

# Opal R

[![Build Status](https://travis-ci.com/obiba/opalr.svg?branch=master)](https://travis-ci.com/obiba/opalr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/opalr)](https://cran.r-project.org/package=opalr)

## Opal R Client

Usage:

    library(opalr)
    o <- opal.login('username', 'passwd', 'http://localhost:8080')
    # Assign one variable to a R symbol
    opal.assign(o, 'VAR1', 'datasource.table:VAR1')
    opal.execute(o, 'summary(VAR1)')
    # Assign some variables as a data.frame to a R symbol
    opal.assign(o, 'TABLE', 'datasource.table', variables=list('VAR1','VAR2'))
    opal.execute(o, 'summary(TABLE$VAR1)')
    opal.execute(o, 'summary(TABLE$VAR2)')
    opal.logout(o)

## DataSHIELD Client

See [DSOpal](https://github.com/datashield/DSOpal).
    

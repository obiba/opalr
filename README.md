# Opal R

[![Build Status](https://travis-ci.com/obiba/opalr.svg?branch=master)](https://travis-ci.com/obiba/opalr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/opalr)](https://cran.r-project.org/package=opalr)

Opal R Client for the [Opal](https://www.obiba.org/pages/products/opal/) data warehouse. Most of the web services
of Opal can be reached by an `opalr` function: import/export, data dictionaries, projects, tables, resources, 
permissions, users, DataSHIELD profiles etc.

See [opalr documentation](https://obiba.github.io/opalr/).

Basic usage:

    #install.packages("opalr")
    library(opalr)
    o <- opal.login('administrator', 'password', url = 'https://opal-demo.obiba.org')
    # Assign some variables as a data.frame to a R symbol
    opal.assign(o, 'D', 'CNSIM.CNSIM1')
    opal.execute(o, 'summary(D$GENDER)')
    opal.execute(o, 'summary(D$LAB_GLUC)')
    opal.logout(o)

## DataSHIELD Client

See [DSOpal](https://datashield.github.io/DSOpal/).
    

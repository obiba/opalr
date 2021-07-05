# Opal R

[![Build Status](https://travis-ci.com/obiba/opalr.svg?branch=master)](https://travis-ci.com/obiba/opalr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/opalr)](https://cran.r-project.org/package=opalr)

Opal R Client for the [Opal](https://www.obiba.org/pages/products/opal/) data warehouse. Most of the web services
of Opal can be reached by an `opalr` function: import/export, data dictionaries, projects, tables, resources, 
permissions, users, DataSHIELD profiles etc.

See [opalr documentation](https://www.obiba.org/opalr/).

Installation:

```
# install from CRAN
install.packages("opalr")

# or install latest development version
remotes::install_github("obiba/opalr")
```

Basic usage:

```
library(opalr)
o <- opal.login('administrator', 'password', url = 'https://opal-demo.obiba.org')

# assign a table (as a data.frame) to a R symbol
opal.assign(o, 'D', 'CNSIM.CNSIM1')

# perform R operations on the server side
opal.execute(o, 'summary(D$GENDER)')
opal.execute(o, 'summary(D$LAB_GLUC)')

opal.logout(o)
```

## Cookbooks

* How to perform [DataSHIELD Administration](https://www.obiba.org/opalr/articles/datashield-admin.html)
* How to manage [Opal Files](https://www.obiba.org/opalr/articles/opal-files.html)
* How to manage [Opal Projects](https://www.obiba.org/opalr/articles/opal-projects.html)
* How to interact with an [Opal R Session](https://www.obiba.org/opalr/articles/opal-rsession.html)

## DataSHIELD Client

See the [DSOpal](https://datashield.github.io/DSOpal/) documentation.

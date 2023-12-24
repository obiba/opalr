# Opal R

[![Build Status](https://app.travis-ci.com/obiba/opalr.svg?branch=master)](https://app.travis-ci.com/github/obiba/opalr)
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

##  Options

Some helper options can be defined to control default values and behavior:

* `opal.username`, the login user name (default is `NULL`).
* `opal.password`, the login user password (default is `NULL`).
* `opal.token`, the login personal access token (default is `NULL`).
* `opal.url`, the login Opal URL (default is `NULL`).
* `opal.opts`, the curl options (default is `NULL`), see [httr::httr_options](https://httr.r-lib.org/reference/httr_options.html).
* `opal.profile`, the login profile (R servers cluster name) (default is `NULL`).
* `opal.progress`, whether to show progress bar (default is `TRUE`).
* `opal.progress.clear`, whether to clear progress bar after it is completed (default is `FALSE`).
* `opal.retry.times`, the maximum number of GET requests to attempt (default is `3`, no retry when value is `1`).
* `opal.retry.quiet`, whether to print a message displaying how long until the next request (default is `FALSE`).

## Cookbooks

* How to perform [DataSHIELD Administration](https://www.obiba.org/opalr/articles/datashield-admin.html)
* How to manage [Opal Files](https://www.obiba.org/opalr/articles/opal-files.html)
* How to manage [Opal Projects](https://www.obiba.org/opalr/articles/opal-projects.html)
* How to interact with an [Opal R Session](https://www.obiba.org/opalr/articles/opal-rsession.html)

## DataSHIELD Client

See the [DSOpal](https://datashield.github.io/DSOpal/) documentation.

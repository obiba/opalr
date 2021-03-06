% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datashield.admin.R
\name{dsadmin.perm_delete}
\alias{dsadmin.perm_delete}
\title{Delete a DataSHIELD permission}
\usage{
dsadmin.perm_delete(opal, subject, type = "user")
}
\arguments{
\item{opal}{Opal connection object.}

\item{subject}{A vector of subject identifiers: user names or group names (depending on the type).}

\item{type}{The type of subject: user (default) or group.}
}
\description{
Delete a permission that was applied to the DataSHIELD service. Silently returns when there is no such permission.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
dsadmin.perm_add(o, c('andrei', 'valentina'), 'user', 'use')
dsadmin.perm(o)
dsadmin.perm_delete(o, c('andrei', 'valentina'), 'user')
opal.logout(o)
}
}

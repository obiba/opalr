% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.admin.R
\name{oadmin.system_perm_add}
\alias{oadmin.system_perm_add}
\title{Add or update a System permission}
\usage{
oadmin.system_perm_add(opal, subject, type = "user", permission)
}
\arguments{
\item{opal}{Opal connection object.}

\item{subject}{A vector of subject identifiers: user names or group names (depending on the type).}

\item{type}{The type of subject: user (default) or group.}

\item{permission}{The permission to apply: project_add or administrate.}
}
\description{
Add or update a permission on the whole system.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
oadmin.system_perm_add(o, c('andrei', 'valentina'), 'user', 'project_add')
oadmin.system_perm(o)
oadmin.system_perm_delete(o, c('andrei', 'valentina'), 'user')
opal.logout(o)
}
}

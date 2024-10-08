% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datashield.admin.R
\name{dsadmin.profile_rparser}
\alias{dsadmin.profile_rparser}
\title{Set or remove the R parser version of a DataSHIELD profile}
\usage{
dsadmin.profile_rparser(opal, name, rParser = NULL)
}
\arguments{
\item{opal}{Opal object.}

\item{name}{Name of the profile.}

\item{rParser}{Version of the DataSHIELD R parser that applies to this profile. If not
specified, the system's default one will be used. A valid version would be one of 'v1' or 'v2'.}
}
\description{
Set or remove the R parser version of a DataSHIELD profile
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
dsadmin.profile_create(o, name = 'survival', cluster = 'demo')
# apply R parser version v2
dsadmin.profile_rparser(o, name = 'survival', rParser = 'v2')
# apply system's default R parser version
dsadmin.profile_rparser(o, name = 'survival')
opal.logout(o)
}
}
\seealso{
Other DataSHIELD profiles: 
\code{\link{dsadmin.profile}()},
\code{\link{dsadmin.profile_access}()},
\code{\link{dsadmin.profile_create}()},
\code{\link{dsadmin.profile_delete}()},
\code{\link{dsadmin.profile_enable}()},
\code{\link{dsadmin.profile_exists}()},
\code{\link{dsadmin.profile_init}()},
\code{\link{dsadmin.profile_perm}()},
\code{\link{dsadmin.profile_perm_add}()},
\code{\link{dsadmin.profile_perm_delete}()},
\code{\link{dsadmin.profiles}()}
}
\concept{DataSHIELD profiles}

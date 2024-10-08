% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datashield.admin.R
\name{dsadmin.profile}
\alias{dsadmin.profile}
\title{Get a DataSHIELD profile}
\usage{
dsadmin.profile(opal, name)
}
\arguments{
\item{opal}{Opal object.}

\item{name}{Name of the profile.}
}
\description{
Note that getting a specific DataSHIELD profile details is not allowed for regular DataSHIELD
users when the profile has no restricted access. This function is for profiles
editors only (system administrators or DataSHIELD administrators).
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
dsadmin.profile(o, name = 'default')
opal.logout(o)
}
}
\seealso{
Other DataSHIELD profiles: 
\code{\link{dsadmin.profile_access}()},
\code{\link{dsadmin.profile_create}()},
\code{\link{dsadmin.profile_delete}()},
\code{\link{dsadmin.profile_enable}()},
\code{\link{dsadmin.profile_exists}()},
\code{\link{dsadmin.profile_init}()},
\code{\link{dsadmin.profile_perm}()},
\code{\link{dsadmin.profile_perm_add}()},
\code{\link{dsadmin.profile_perm_delete}()},
\code{\link{dsadmin.profile_rparser}()},
\code{\link{dsadmin.profiles}()}
}
\concept{DataSHIELD profiles}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datashield.admin.R
\name{dsadmin.rm_options}
\alias{dsadmin.rm_options}
\title{Remove all DataSHIELD options}
\usage{
dsadmin.rm_options(opal, profile = NULL)
}
\arguments{
\item{opal}{Opal object or list of opal objects.}

\item{profile}{The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.}
}
\description{
Remove all DataSHIELD options
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
dsadmin.rm_options(o, 'foo')
opal.logout(o)
}
}
\seealso{
Other DataSHIELD functions: 
\code{\link{dsadmin.get_method}()},
\code{\link{dsadmin.get_methods}()},
\code{\link{dsadmin.get_options}()},
\code{\link{dsadmin.install_github_package}()},
\code{\link{dsadmin.install_local_package}()},
\code{\link{dsadmin.install_package}()},
\code{\link{dsadmin.installed_package}()},
\code{\link{dsadmin.package_description}()},
\code{\link{dsadmin.package_descriptions}()},
\code{\link{dsadmin.publish_package}()},
\code{\link{dsadmin.remove_package}()},
\code{\link{dsadmin.rm_method}()},
\code{\link{dsadmin.rm_methods}()},
\code{\link{dsadmin.rm_option}()},
\code{\link{dsadmin.rm_package_methods}()},
\code{\link{dsadmin.set_method}()},
\code{\link{dsadmin.set_option}()},
\code{\link{dsadmin.set_package_methods}()},
\code{\link{dsadmin.unpublish_package}()}
}
\concept{DataSHIELD functions}

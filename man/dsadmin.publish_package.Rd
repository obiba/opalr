% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datashield.admin.R
\name{dsadmin.publish_package}
\alias{dsadmin.publish_package}
\title{Publish DataSHIELD package settings}
\usage{
dsadmin.publish_package(opal, pkg, profile = NULL)
}
\arguments{
\item{opal}{Opal object or list of opal objects.}

\item{pkg}{Package name.}

\item{profile}{The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.}
}
\value{
TRUE if successfull
}
\description{
Declare DataSHIELD aggregate/assign methods and options as defined by the package.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
dsadmin.publish_package(o, 'dsBase')
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
\code{\link{dsadmin.remove_package}()},
\code{\link{dsadmin.rm_method}()},
\code{\link{dsadmin.rm_methods}()},
\code{\link{dsadmin.rm_option}()},
\code{\link{dsadmin.rm_options}()},
\code{\link{dsadmin.rm_package_methods}()},
\code{\link{dsadmin.set_method}()},
\code{\link{dsadmin.set_option}()},
\code{\link{dsadmin.set_package_methods}()},
\code{\link{dsadmin.unpublish_package}()}
}
\concept{DataSHIELD functions}

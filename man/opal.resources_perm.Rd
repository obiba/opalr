% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.resource.R
\name{opal.resources_perm}
\alias{opal.resources_perm}
\title{Get the permissions on any resource}
\usage{
opal.resources_perm(opal, project)
}
\arguments{
\item{opal}{Opal connection object.}

\item{project}{The project name.}
}
\value{
A data.frame with columns: subject, type, permission
}
\description{
Get the permissions that were applied globally on the project's resources.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
opal.resources_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'view')
opal.resources_perm(o, 'CNSIM')
opal.resources_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
opal.logout(o)
}
}
\seealso{
Other resource functions: 
\code{\link{opal.resource}()},
\code{\link{opal.resource_create}()},
\code{\link{opal.resource_delete}()},
\code{\link{opal.resource_exists}()},
\code{\link{opal.resource_extension_create}()},
\code{\link{opal.resource_get}()},
\code{\link{opal.resource_perm}()},
\code{\link{opal.resource_perm_add}()},
\code{\link{opal.resource_perm_delete}()},
\code{\link{opal.resources}()},
\code{\link{opal.resources_perm_add}()},
\code{\link{opal.resources_perm_delete}()}
}
\concept{resource functions}

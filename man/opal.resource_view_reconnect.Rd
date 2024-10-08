% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.resource.R
\name{opal.resource_view_reconnect}
\alias{opal.resource_view_reconnect}
\title{Reconnect an Opal view to its underlying resource}
\usage{
opal.resource_view_reconnect(opal, project, table)
}
\arguments{
\item{opal}{Opal connection object.}

\item{project}{Project name where the view is located.}

\item{table}{View name to be reconnected.}
}
\description{
A view over a resource handles a connection to this resource. When the resource 
changes (data update, broken connection etc.), the connection to this resource
can be re-initialized.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
# make a view over a resource
opal.resource_view_create(o, "CNSIM", "CNSIM4", resource = "RSRC.CNSIM1")
# re-initialize the view's connection to the resource
opal.resource_view_reconnect(o, "CNSIM", "CNSIM4")
opal.logout(o)
}
}
\seealso{
Other table functions: 
\code{\link{opal.perms}()},
\code{\link{opal.resource_view_create}()},
\code{\link{opal.table_create}()},
\code{\link{opal.table_delete}()},
\code{\link{opal.table_dictionary_get}()},
\code{\link{opal.table_dictionary_update}()},
\code{\link{opal.table_exists}()},
\code{\link{opal.table_export}()},
\code{\link{opal.table_get}()},
\code{\link{opal.table_import}()},
\code{\link{opal.table_perm}()},
\code{\link{opal.table_perm_add}()},
\code{\link{opal.table_perm_delete}()},
\code{\link{opal.table_save}()},
\code{\link{opal.table_truncate}()},
\code{\link{opal.table_view_create}()},
\code{\link{opal.table_view_update}()}
}
\concept{table functions}

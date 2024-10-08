% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.table.R
\name{opal.table_delete}
\alias{opal.table_delete}
\title{Delete a Opal table}
\usage{
opal.table_delete(opal, project, table, silent = TRUE)
}
\arguments{
\item{opal}{Opal connection object.}

\item{project}{Project name where the table is located.}

\item{table}{Table name to be deleted.}

\item{silent}{Warn if table does not exist, default is TRUE.}
}
\description{
Removes both values and data dictionary of a table, or remove the table's logic if the table is a view.
Fails if the table does not exist. See also \link{opal.table_truncate}.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
opal.table_delete(o, "CNSIM", "CNSIM1")
opal.logout(o)
}
}
\seealso{
Other table functions: 
\code{\link{opal.perms}()},
\code{\link{opal.resource_view_create}()},
\code{\link{opal.resource_view_reconnect}()},
\code{\link{opal.table_create}()},
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

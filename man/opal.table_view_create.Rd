% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.table.R
\name{opal.table_view_create}
\alias{opal.table_view_create}
\title{Create an Opal view over tables}
\usage{
opal.table_view_create(opal, project, table, tables, type = "Participant")
}
\arguments{
\item{opal}{Opal connection object.}

\item{project}{Project name where the table will be located.}

\item{table}{Table name to be created}

\item{tables}{List of the fully qualified table names that are referred by the view.}

\item{type}{Entity type, default is "Participant". Ignored if some table references are 
provided.}
}
\description{
Create an Opal view if a table with same name does not already exist. The view
created will have no dictionary, use \link{opal.table_dictionary_update} to 
apply a dictionary.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
# make a view
opal.table_view_create(o, "CNSIM", "CNSIM123", 
                       c("CNSIM.CNSIM1", "CNSIM.CNSIM2", "CNSIM.CNSIM3"))
opal.logout(o)
}
}
\seealso{
Other table functions: 
\code{\link{opal.perms}()},
\code{\link{opal.resource_view_create}()},
\code{\link{opal.resource_view_reconnect}()},
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
\code{\link{opal.table_view_update}()}
}
\concept{table functions}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.workspace.R
\name{opal.workspace_save}
\alias{opal.workspace_save}
\title{Save the current session in a opal R workspace.}
\usage{
opal.workspace_save(opal, save = TRUE)
}
\arguments{
\item{opal}{Opal object.}

\item{save}{Save the workspace with given identifier (default is TRUE, current session ID if TRUE).}
}
\value{
The workspace ID (invisible)
}
\description{
Save the current session in a opal R workspace.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
# provide a workspace ID
opal.workspace_save(o, 'test')
# or use default one
id <- opal.workspace_save(o)
opal.logout(o)
}
}
\seealso{
Other workspace functions: 
\code{\link{opal.workspace_restore}()},
\code{\link{opal.workspace_rm}()},
\code{\link{opal.workspaces}()}
}
\concept{workspace functions}

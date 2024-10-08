% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.project.R
\name{opal.project_delete}
\alias{opal.project_delete}
\title{Delete a project}
\usage{
opal.project_delete(opal, project, archive = FALSE, silent = TRUE)
}
\arguments{
\item{opal}{Opal object.}

\item{project}{Name of the project}

\item{archive}{Logical that is TRUE if the complete removal of the project is requested.}

\item{silent}{Warn if project does not exist, default is TRUE.}
}
\description{
Delete a project and every data what could have been associated to it.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
opal.project_delete(o, 'test')
opal.logout(o)
}
}
\seealso{
Other project functions: 
\code{\link{opal.project}()},
\code{\link{opal.project_create}()},
\code{\link{opal.project_exists}()},
\code{\link{opal.projects}()},
\code{\link{opal.projects_databases}()}
}
\concept{project functions}

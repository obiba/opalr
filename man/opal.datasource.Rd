% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.datasource.R
\name{opal.datasource}
\alias{opal.datasource}
\title{Get a datasource}
\usage{
opal.datasource(opal, datasource)
}
\arguments{
\item{opal}{Opal object.}

\item{datasource}{Name of the datasource.}
}
\description{
Get a datasource
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
opal.datasource(o, 'CNSIM')
opal.logout(o)
}
}
\seealso{
Other datasource functions: 
\code{\link{opal.annotate}()},
\code{\link{opal.annotations}()},
\code{\link{opal.attribute_values}()},
\code{\link{opal.datasources}()},
\code{\link{opal.table}()},
\code{\link{opal.tables}()},
\code{\link{opal.valueset}()},
\code{\link{opal.variable}()},
\code{\link{opal.variable_summary}()},
\code{\link{opal.variables}()}
}
\concept{datasource functions}

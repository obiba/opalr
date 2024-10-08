% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.datasource.R
\name{opal.attribute_values}
\alias{opal.attribute_values}
\title{Get a vector of attribute values}
\usage{
opal.attribute_values(attributes, namespace = NULL, name = "label")
}
\arguments{
\item{attributes}{A list of attributes, usually variable or category attributes.}

\item{namespace}{Optional attribute namespace.}

\item{name}{Required attribute name.}
}
\description{
Get a vector of attribute values (for each locale) matching the given attribute namespace and name. Vector is null if no such attribute is found.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
var <- opal.variable(o, 'CNSIM', 'CNSIM1', 'GENDER')
opal.attribute_values(var$attributes)
opal.logout(o)
}
}
\seealso{
Other datasource functions: 
\code{\link{opal.annotate}()},
\code{\link{opal.annotations}()},
\code{\link{opal.datasource}()},
\code{\link{opal.datasources}()},
\code{\link{opal.table}()},
\code{\link{opal.tables}()},
\code{\link{opal.valueset}()},
\code{\link{opal.variable}()},
\code{\link{opal.variable_summary}()},
\code{\link{opal.variables}()}
}
\concept{datasource functions}

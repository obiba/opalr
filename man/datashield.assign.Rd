\name{datashield.assign}
\alias{datashield.assign}
\alias{datashield.assign.list}
\alias{datashield.assign.opal}
\title{Data assignment}
\usage{
datashield.assign(opals, symbol, value, variables = NULL, missings = FALSE)

\method{datashield.assign}{opal}(opal, symbol, value, variables = NULL,
  missings = FALSE)

\method{datashield.assign}{list}(opals, symbol, value, variables = NULL,
  missings = FALSE)
}
\arguments{
  \item{opals}{Opal object or list of opal objects.}

  \item{symbol}{Name of the R symbol.}

  \item{value}{Fully qualified name of a variable or a
  table in Opal (must be the same in each Opal) or a R
  expression with allowed assign functions calls.}

  \item{variables}{List of variable names or Javascript
  expression that selects the variables of a table (ignored
  if value does not refere to a table). See javascript
  documentation:
  http://wiki.obiba.org/display/OPALDOC/Variable+Methods}

  \item{missings}{If TRUE, missing values will be pushed
  from Opal to R, default is FALSE. Ignored if value is an
  R expression.}
}
\description{
Assign a Opal value to a R symbol in the current Datashield
session.
}
\examples{
{
# assign a list of variables from table HOP of opal object o
datashield.assign(o, symbol="D", value"demo.HOP", variables=list("GENDER","LAB_GLUC"))

# assign all the variables matching 'LAB' from table HOP of opal object o
datashield.assign(o, symbol="D", value"demo.HOP", variables="name().matches('LAB_')")
}
}


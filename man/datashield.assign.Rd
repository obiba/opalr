\name{datashield.assign}
\alias{datashield.assign}
\alias{datashield.assign.list}
\alias{datashield.assign.opal}
\title{Data assignment}
\usage{
  datashield.assign(opals, symbol, value, missings = FALSE)

  \method{datashield.assign}{opal} (opal, symbol, value,
    missings = FALSE)

  \method{datashield.assign}{list} (opals, ...)
}
\arguments{
  \item{opals}{Opal object or list of opal objects.}

  \item{symbol}{Name of the R symbol.}

  \item{value}{Fully qualified name of a variable or a
  table in Opal (must be the same in each Opal) or a R
  expression with allowed assign functions calls.}

  \item{missings}{If TRUE, missing values will be pushed
  from Opal to R, default is FALSE. Ignored if value is an
  R expression.}
}
\description{
  Assign a Opal value to a R symbol in the current
  Datashield session.
}


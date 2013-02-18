\name{datashield.assign}
\alias{datashield.assign}
\alias{datashield.assign.list}
\alias{datashield.assign.opal}
\title{Data assignment}
\usage{
  datashield.assign(opals, symbol, value)

  \method{datashield.assign}{opal} (opal, symbol, value)

  \method{datashield.assign}{list} (opals, ...)
}
\arguments{
  \item{opals}{Opal object or list of opal objects.}

  \item{symbol}{Name of the R symbol.}

  \item{value}{Fully qualified name of a variable or a
  table in Opal (must be the same in each Opal).}
}
\description{
  Assign a Opal value to a R symbol in the current
  Datashield session.
}


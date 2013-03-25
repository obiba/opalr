\name{datashield.methods}
\alias{datashield.methods}
\alias{datashield.methods.list}
\alias{datashield.methods.opal}
\title{List Datashield methods}
\usage{
  datashield.methods(opals, type = "aggregate")

  \method{datashield.methods}{opal} (opal,
    type = "aggregate")

  \method{datashield.methods}{list} (opals,
    type = "aggregate")
}
\arguments{
  \item{opals}{Opal object or list of opal objects.}

  \item{type}{Type of the method: "aggregate" (default) or
  "assign".}
}
\description{
  Get available Datashield methods of a given type.
}


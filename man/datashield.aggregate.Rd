\name{datashield.aggregate}
\alias{datashield.aggregate}
\alias{datashield.aggregate.list}
\alias{datashield.aggregate.opal}
\title{Data aggregation}
\usage{
  datashield.aggregate(opals, expr)

  \method{datashield.aggregate}{opal} (opal, expr)

  \method{datashield.aggregate}{list} (opals, expr)
}
\arguments{
  \item{opals}{Opal object or list of opal objects.}

  \item{expr}{Expression to evaluate.}
}
\description{
  Aggregates the expression result using the specified
  aggregation method in the current Datashield session.
}


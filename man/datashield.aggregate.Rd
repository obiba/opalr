\name{datashield.aggregate}
\alias{datashield.aggregate}
\alias{datashield.aggregate.list}
\alias{datashield.aggregate.opal}
\title{Data aggregation}
\usage{
datashield.aggregate(opals, expr, async = FALSE, wait = TRUE)

\method{datashield.aggregate}{opal}(opal, expr, async = FALSE, wait = TRUE)

\method{datashield.aggregate}{list}(opals, expr, async = FALSE, wait = TRUE)
}
\arguments{
  \item{opals}{Opal object or list of opal objects.}

  \item{expr}{Expression to evaluate.}

  \item{async}{R script is executed asynchronously within
  the session (default is FALSE). If TRUE, the value
  returned is the ID of the command to look for (from Opal
  2.1, older versions will return the result).}

  \item{wait}{Wait for the R script asynchronously executed
  to complete (makes sense only with async=TRUE).}
}
\value{
The result of the aggregation or the R command ID if the
async flag is TRUE and if the wait flag is FALSE and if
Opal version is at least 2.1.
}
\description{
Aggregates the expression result using the specified
aggregation method in the current Datashield session.
}


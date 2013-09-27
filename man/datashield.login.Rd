\name{datashield.login}
\alias{datashield.login}
\title{Logs in and assigns variables to R}
\usage{
  datashield.login(logins = NULL, assign = NULL,
    variables = NULL, dir = NULL)
}
\arguments{
  \item{logins}{a dataframe table that holds login details.
  This table holds five elements required to login to the
  servers where the data to analyse is stored. See the
  documentation of the examplar input table
  \code{logindata} for details of the login elements.}

  \item{assign}{name of the dataframe to which the opal's
  dataset will be assigned after login into the server(s).}

  \item{variables}{specific variables to assign. If
  \code{assign} is set to FALSE this argument is ignored
  otherwise the specified variables are assigned to R. If
  no variables are specified (default) the whole dataset is
  assigned.}

  \item{dir}{directory where to look for key pairs files.
  If key file path is relative, default is to look in user
  .ssh directory, then in current working directory.}
}
\value{
  object(s) of class opal
}
\description{
  This function allows for clients to login to opal servers
  and (optionaly) assign all the data or specific variables
  from Opal datasources to R. The assigned dataframes (one
  for each opal server) are named 'D'.
}
\examples{
{

#### The below examples illustrate an analysises that use test/simulated data ####

# load that contains the login details
data(logindata)

# Example 1: just login (default)
opals <- datashield.login(logins=logindata)

# Example 2: login and assign the whole dataset
opals <- datashield.login(logins=logindata,assign=TRUE)

# Example 3: login and assign specific variable(s)
myvar <- list("LAB_TSC")
opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
}
}
\author{
  Gaye, A.
}


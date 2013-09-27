\name{datashield.login}
\alias{datashield.login}
\title{Logs in and assigns variables to R}
\usage{
  datashield.login(logins = NULL, symbol = NULL,
    variables = NULL, dir = "~/.ssh")
}
\arguments{
  \item{logins}{A dataframe table that holds login details.
  This table holds five elements required to login to the
  servers where the data to analyse is stored. The expected
  column names are 'server' (the server name), 'url' (the
  opal url), 'user' (the user name or the certificate file
  path), 'password' (the user password or the private key
  file path), 'table' (the fully qualified name of the
  dataset in opal). See the documentation of the examplar
  input table \code{logindata} for details of the login
  elements.}

  \item{symbol}{Name of the dataframe to which the opal's
  dataset will be assigned after login into the server(s).
  By default no assignment is performed.}

  \item{variables}{Specific variables to assign. If
  \code{symbol} is not set this argument is ignored,
  otherwise the specified variables are assigned to R. If
  no variables are specified (default) the whole dataset is
  assigned.}

  \item{dir}{Directory where to look for key pairs files
  (certificate and private key). If key file path is
  relative, default is to look in user '.ssh' directory,
  then in current working directory.}
}
\value{
  Object(s) of class opal
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

# or build your data.frame
server <- c("study1", "study2")
url <- c("https://some.opal.host:8443","https://another.opal.host")
user <- c("user1", "datashield-certificate.pem")
password <- c("user1pwd", "datashield-private.pem")
table <- c("store.Dataset","foo.DS")
logindata <- data.frame(server,url,user,password,table)

# Example 1: just login (default)
opals <- datashield.login(logins=logindata)

# Example 2: login and assign the whole dataset
opals <- datashield.login(logins=logindata,symbol="D")

# Example 3: login and assign specific variable(s)
opals <- datashield.login(logins=logindata,symbol="D",variables=list("LAB_TSC"))
}
}
\author{
  Gaye, A.
}


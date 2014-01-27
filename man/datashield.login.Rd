\name{datashield.login}
\alias{datashield.login}
\title{Logs in and assigns variables to R}
\usage{
datashield.login(logins = NULL, assign = FALSE, variables = NULL,
  symbol = "D", directory = "~/.ssh")
}
\arguments{
  \item{logins}{A dataframe table that holds login details.
  This table holds five elements required to login to the
  servers where the data to analyse is stored. The expected
  column names are 'server' (the server name), 'url' (the
  opal url), 'user' (the user name or the certificate file
  path), 'password' (the user password or the private key
  file path), 'table' (the fully qualified name of the
  table in opal). An additional column 'identifiers' can be
  specified for identifiers mapping (from Opal 2.0). See
  also the documentation of the examplar input table
  \code{logindata} for details of the login elements.}

  \item{assign}{A boolean which tells whether or not data
  should be assigned from the opal table to R after login
  into the server(s).}

  \item{variables}{Specific variables to assign. If
  \code{assign} is set to FALSE this argument is ignored
  otherwise the specified variables are assigned to R. If
  no variables are specified (default) the whole opal's
  table is assigned.}

  \item{symbol}{A character, the name of the dataframe to
  which the opal's table will be assigned after login into
  the server(s).}

  \item{directory}{A character that indicates the location
  of the key pairs files (certificate and private key). If
  the default location is the users '.ssh' directory.}
}
\value{
object(s) of class opal
}
\description{
This function allows for clients to login to opal servers
and (optionaly) assign all the data or specific variables
from Opal tables to R dataframes. The assigned dataframes
(one for each opal server) are named 'D' (by default).
}
\details{

}
\examples{
{

#### The below examples illustrate an analysises that use test/simulated data ####

# build your data.frame
server <- c("study1", "study2")
url <- c("https://some.opal.host:8443","https://another.opal.host")
user <- c("user1", "datashield-certificate.pem")
password <- c("user1pwd", "datashield-private.pem")
table <- c("store.Dataset","foo.DS")
logindata <- data.frame(server,url,user,password,table)

# or load the data.frame that contains the login details
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


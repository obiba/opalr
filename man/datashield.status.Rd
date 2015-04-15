\name{datashield.status}
\alias{datashield.status}
\title{Check Datashield configuration status}
\usage{
  datashield.status(logins = NULL, study = NULL,
    directory = "~/.ssh")
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
}
\value{
  TODO
}
\description{
  TODO
}
\details{
  Get the status of the table(s), method(s), and
  packgage(s) in differents Opals servers.
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
  Mbatchou, S.
}


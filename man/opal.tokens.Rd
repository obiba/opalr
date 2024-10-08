% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.token.R
\name{opal.tokens}
\alias{opal.tokens}
\title{Get the personal access tokens}
\usage{
opal.tokens(opal, df = TRUE)
}
\arguments{
\item{opal}{Opal object.}

\item{df}{Return a data.frame (default is TRUE)}
}
\description{
Get the list of personal access tokens. Like for the other token functions, 
this operation requires the user to authenticate with username/password credentials.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
opal.tokens(o)
opal.logout(o)
}
}
\seealso{
Other token functions: 
\code{\link{opal.token}()},
\code{\link{opal.token_datashield_create}()},
\code{\link{opal.token_delete}()},
\code{\link{opal.token_r_create}()},
\code{\link{opal.token_renew}()},
\code{\link{opal.token_sql_create}()}
}
\concept{token functions}

\name{opal.command_result}
\alias{opal.command_result}
\title{Get result of an asynchronous command}
\usage{
opal.command_result(opal, id)
}
\arguments{
  \item{id}{R command ID}

  \item{opal}{Opal object.}
}
\description{
Get the result of an asynchronous R commands in the remote
R session. The command is removed from the remote R session
after this call.
}


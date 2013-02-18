\name{opal.login}
\alias{opal.login}
\title{Opal login}
\usage{
  opal.login(username = NULL, password = NULL, url,
    opts = list())
}
\arguments{
  \item{username}{User name in opal(s).}

  \item{password}{User password in opal(s).}

  \item{url}{Opal url or list of opal urls.}

  \item{opts}{Curl options}
}
\value{
  A opal object or a list of opal objects.
}
\description{
  Log in Opal(s).
}


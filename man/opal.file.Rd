% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/opal.file.R
\name{opal.file}
\alias{opal.file}
\title{Get file content}
\usage{
opal.file(opal, path, key = NULL)
}
\arguments{
\item{opal}{Opal object.}

\item{path}{Path to the file in the Opal file system.}

\item{key}{File encryption key: downloaded file will be a zip file with content encrypted (use 7zip to decrypt).}
}
\description{
Get file content from the Opal file system.
}
\examples{
\dontrun{
o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
opal.file(o, '/home/administrator/joins/join-src-3.csv')
opal.logout(o)
}
}
\seealso{
Other file functions: 
\code{\link{opal.file_cp}()},
\code{\link{opal.file_download}()},
\code{\link{opal.file_ls}()},
\code{\link{opal.file_mkdir}()},
\code{\link{opal.file_mkdir_tmp}()},
\code{\link{opal.file_mv}()},
\code{\link{opal.file_read}()},
\code{\link{opal.file_rm}()},
\code{\link{opal.file_unzip}()},
\code{\link{opal.file_upload}()},
\code{\link{opal.file_write}()}
}
\concept{file functions}

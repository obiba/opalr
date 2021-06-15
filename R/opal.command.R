#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' List the asynchronous commands
#' 
#' Get the list of asynchronous R commands in the remote R session.
#' 
#' @family command functions
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.commands(o)
#' opal.logout(o)
#' }
#' @export
opal.commands <- function(opal, df=TRUE) {
  if (opal.version_compare(opal,"2.1")<0) return(NULL)
  res <- opal.get(opal, "r", "session", .getRSessionId(opal), "commands")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    id <- rep(NA, n)
    script <- rep(NA, n)
    status <- rep(NA, n)
    withResult <- rep(NA, n)
    createDate <- rep(NA, n)
    startDate <- rep(NA, n)
    endDate <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      id[i] <- item$id
      script[i] <- item$script
      status[i] <- item$status
      withResult[i] <- item$withResult
      createDate[i] <- item$createDate
      if (!is.null(item$startDate)) {
        startDate[i] <- item$startDate  
      }
      if (!is.null(item$endDate)) {
        endDate[i] <- item$endDate 
      }
    }
    data.frame(id, script, status, withResult, createDate, startDate, endDate)
  } else {
    data.frame()
  }
}

#' Get an asynchronous command
#' 
#' Get an asynchronous R commands in the remote R session.
#' 
#' @family command functions
#' @param opal Opal object.
#' @param id R command ID.
#' @param wait Wait for the command to complete.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.command(o, '1234')
#' opal.logout(o)
#' }
#' @export
opal.command <- function(opal, id, wait=FALSE) {
  if (is.null(id) || opal.version_compare(opal,"2.1")<0) return(NULL)
  query <- list()
  if (wait) {
    query["wait"] <- "true"
  }
  opal.get(opal, "r", "session", .getRSessionId(opal), "command", id, query=query)
}

#' Remove an asynchronous command
#' 
#' Remove an asynchronous R commands in the remote R session.
#' 
#' @family command functions
#' @param opal Opal object.
#' @param id R command ID.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.command_rm(o, '1234')
#' opal.logout(o)
#' }
#' @export
opal.command_rm <- function(opal, id) {
  if (is.null(id) || opal.version_compare(opal,"2.1")<0) return()
  ignore <- tryCatch(opal.delete(opal, "r", "session", .getRSessionId(opal), "command", id), error=function(e){})
}

#' Remove all asynchronous commands
#' 
#' Remove all asynchronous R commands in the remote R session.
#' 
#' @family command functions
#' @param opal Opal object.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.commands_rm(o)
#' opal.logout(o)
#' }
#' @export
opal.commands_rm <- function(opal) {
  if (opal.version_compare(opal,"2.1")<0) return()
  res <- lapply(opal.commands(opal), function(cmd) {
    opal.command_rm(opal, cmd$id)
  })
}

#' Get result of an asynchronous command
#' 
#' Get the result of an asynchronous R commands in the remote R session. The command is removed from the
#' remote R session after this call.
#' 
#' @family command functions
#' @param opal Opal object.
#' @param id R command ID.
#' @param wait Wait for the command to complete.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.command_result(o, '1234')
#' opal.logout(o)
#' }
#' @export
opal.command_result <- function(opal, id, wait=FALSE) {
  if (is.null(id) || opal.version_compare(opal,"2.1")<0) return(id)
  if (wait) {
    cmd <- opal.command(opal, id, wait=TRUE)
    if (cmd$status == "FAILED") {
      msg <- cmd$error
      if (is.null(cmd$error)) {
        msg <- "<no message>"
      }
      stop("Command '", cmd$script, "' failed on '", opal$name,"': ", msg, call.=FALSE)
    }
  }
  opal.get(opal, "r", "session", .getRSessionId(opal), "command", id, "result", acceptType = "application/octet-stream")
}
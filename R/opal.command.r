#-------------------------------------------------------------------------------
# Copyright (c) 2016 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the list of asynchronous R commands in the remote R session.
#' 
#' @title List the asynchronous commands
#' 
#' @param opal Opal object.
#' @export
opal.commands <- function(opal) {
  if (opal.version_compare(opal,"2.1")<0) return(NULL)
  .get(opal, "r", "session", .getRSessionId(opal), "commands")
}

#' Get an asynchronous R commands in the remote R session.
#' 
#' @title Get an asynchronous command
#' 
#' @param opal Opal object.
#' @param id R command ID.
#' @param wait Wait for the command to complete.
#' @export
opal.command <- function(opal, id, wait=FALSE) {
  if (is.null(id) || opal.version_compare(opal,"2.1")<0) return(NULL)
  query <- list()
  if (wait) {
    query["wait"] <- "true"
  }
  .get(opal, "r", "session", .getRSessionId(opal), "command", id, query=query)
}

#' Remove an asynchronous R commands in the remote R session.
#' 
#' @title Remove an asynchronous command
#' 
#' @param opal Opal object.
#' @param id R command ID.
#' @export
opal.command_rm <- function(opal, id) {
  if (is.null(id) || opal.version_compare(opal,"2.1")<0) return()
  tryCatch(.delete(opal, "r", "session", .getRSessionId(opal), "command", id), error=function(e){})
}

#' Remove all asynchronous R commands in the remote R session.
#' 
#' @title Remove all asynchronous commands
#' 
#' @param opal Opal object.
#' @export
opal.commands_rm <- function(opal) {
  if (opal.version_compare(opal,"2.1")<0) return()
  res <- lapply(opal.commands(opal), function(cmd) {
    opal.command_rm(opal, cmd$id)
  })
}

#' Get the result of an asynchronous R commands in the remote R session. The command is removed from the
#' remote R session after this call.
#' 
#' @title Get result of an asynchronous command
#' 
#' @param opal Opal object.
#' @param id R command ID.
#' @param wait Wait for the command to complete.
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
  .get(opal, "r", "session", .getRSessionId(opal), "command", id, "result")
}
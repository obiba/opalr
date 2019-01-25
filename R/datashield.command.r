#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the list of asynchronous R commands in the remote Datashield session.
#' 
#' @title List the asynchronous commands
#' 
#' @param opal Opal object or list of opal objects.
#' @rdname datashield.commands
#' @export
datashield.commands <- function(opal) {
  UseMethod('datashield.commands')
}

#' @rdname datashield.commands
#' @export
datashield.commands.opal <- function(opal) {
  if (opal.version_compare(opal,"2.1")<0) return(NULL)
  opal.get(opal, "datashield", "session", .getDatashieldSessionId(opal), "commands")
}

#' @rdname datashield.commands
#' @export
datashield.commands.list <- function(opal) {
  lapply(opal, FUN=datashield.commands)
}

#' Get an asynchronous R commands in the remote Datashield session.
#' 
#' @title Get an asynchronous command
#' 
#' @param opal Opal object or list of opal objects.
#' @param id R command ID or list of R command IDs (one for each opal object).
#' @param wait Wait for the command to complete.
#' @rdname datashield.command
#' @export
datashield.command <- function(opal, id, wait=FALSE) {
  UseMethod('datashield.command')
}

#' @rdname datashield.command
#' @export
datashield.command.opal <- function(opal, id, wait=FALSE) {
  if (is.null(id) || opal.version_compare(opal,"2.1")<0) return(NULL)
  query <- list()
  if (wait) {
    query["wait"] <- "true"
  }
  opal.get(opal, "datashield", "session", .getDatashieldSessionId(opal), "command", id, query=query)
}

#' @rdname datashield.command
#' @export
datashield.command.list <- function(opal, id, wait=FALSE) {
  res <- lapply(1:length(opal), function(i) {
    datashield.command(opal[[i]], id[[i]], wait=wait)
  })
  names(res) <- names(opal)
  res
}

#' Remove an asynchronous R commands in the remote Datashield session.
#' 
#' @title Remove an asynchronous command
#' 
#' @param opal Opal object or list of opal objects.
#' @param id R command ID or list of R command IDs (one for each opal object).
#' @rdname datashield.command_rm
#' @export
datashield.command_rm <- function(opal, id) {
  UseMethod('datashield.command_rm')
}

#' @rdname datashield.command_rm
#' @export
datashield.command_rm.opal <- function(opal, id) {
  if (is.null(id) || opal.version_compare(opal,"2.1")<0) return()
  tryCatch(opal.delete(opal, "datashield", "session", .getDatashieldSessionId(opal), "command", id), error=function(e){})
}

#' @rdname datashield.command_rm
#' @export
datashield.command_rm.list <- function(opal, id) {
  res <- lapply(1:length(opal), function(i) {
    datashield.command_rm(opal[[i]], id[[i]])
  })
}

#' Remove all asynchronous R commands in the remote Datashield session.
#' 
#' @title Remove all asynchronous commands
#' 
#' @param opal Opal object or list of opal objects.
#' @rdname datashield.commands_rm
#' @export
datashield.commands_rm <- function(opal) {
  UseMethod('datashield.commands_rm')
}

#' @rdname datashield.commands_rm
#' @export
datashield.commands_rm.opal <- function(opal) {
  if (opal.version_compare(opal,"2.1")<0) return()
  res <- lapply(datashield.commands(opal), function(cmd) {
    datashield.command_rm(opal, cmd$id)
  })
}

#' @rdname datashield.commands_rm
#' @export
datashield.commands_rm.list <- function(opal) {
  lapply(opal, FUN=datashield.commands_rm.opal)
}

#' Get the result of an asynchronous R commands in the remote Datashield session. The command is removed from the
#' remote Datashield session after this call.
#' 
#' @title Get result of an asynchronous command
#' 
#' @param opal Opal object or list of opal objects.
#' @param id R command ID or list of R command IDs (one for each opal object).
#' @param wait Wait for the command to complete.
#' @rdname datashield.command_result
#' @export
datashield.command_result <- function(opal, id, wait=FALSE) {
  UseMethod('datashield.command_result')
}

#' @rdname datashield.command_result
#' @export
datashield.command_result.opal <- function(opal, id, wait=FALSE) {
  if (is.null(id) || opal.version_compare(opal,"2.1")<0) return(id)
  if (wait) {
    cmd <- datashield.command(opal, id, wait=TRUE)
    if (cmd$status == "FAILED") {
      msg <- cmd$error
      if (is.null(cmd$error)) {
        msg <- "<no message>"
      }
      stop("Command '", cmd$script, "' failed on '", opal$name,"': ", msg, call.=FALSE)
    }
  }
  opal.get(opal, "datashield", "session", .getDatashieldSessionId(opal), "command", id, "result")
}

#' @rdname datashield.command_result
#' @export
datashield.command_result.list <- function(opal, id, wait=FALSE) {
  res <- lapply(1:length(opal), function(i) {
    datashield.command_result(opal[[i]], id[[i]], wait=wait)
  })
  names(res) <- names(opal)
  res
}

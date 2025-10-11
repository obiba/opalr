#-------------------------------------------------------------------------------
# Copyright (c) 2025 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Create the R session
#' 
#' Create the R session if it does not exist and returns its identifier.
#' 
#' @family session functions
#' @param opal Opal object.
#' @param wait Wait for R session to be operational (default is TRUE)
#' @return The R session identifier.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.session(o)
#' opal.logout(o)
#' }
#' @export
opal.session <- function(opal, wait = TRUE) {
  if(is.null(opal$rid)) {
    opal$rid <- .newSession(opal, restore = opal$restore, profile = opal$profile, wait = wait)
  }
  if(is.null(opal$rid)) {
    stop("Remote R session not available")
  }
  invisible(opal$rid)
}

#' Get the R session
#' 
#' Get the R session details if it exists
#' 
#' @family session functions
#' @param opal Opal object.
#' @return The R session object.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.session(o, wait = FALSE)
#' opal.session_get(o)
#' opal.logout(o)
#' }
#' @export
opal.session_get <- function(opal) {
  if(is.null(opal$rid)) {
    stop("Remote R session not available")
  }
  opal.get(opal, opal$context, "session", opal$rid)
}

#' Check if the R session exists
#' 
#' Check if the remote R session exists
#' 
#' @family session functions
#' @param opal Opal object.
#' @return A logical value.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.session_exists(o)
#' opal.logout(o)
#' }
#' @export
opal.session_exists <- function(opal) {
  if(is.null(opal$rid)) {
    return(FALSE)
  }
  tryCatch({
    opal.get(opal, opal$context, "session", opal$rid)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#' Check if the R session is running
#' 
#' Check if the remote R session is running and ready to receive R commands. 
#' Fails if the session does not exist.
#' 
#' @family session functions
#' @param opal Opal object.
#' @return A logical value.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.session(o, wait = FALSE)
#' ready <- opal.session_running(o)
#' while(!ready) {
#'   Sys.sleep(1)
#'   ready <- opal.session_running(o)
#'   cat(".")
#' }
#' opal.session_get(o)
#' opal.logout(o)
#' }
#' @export
opal.session_running <- function(opal) {
  if(is.null(opal$rid)) {
    stop("Remote R session not available")
  }
  res <- opal.get(opal, opal$context, "session", opal$rid)
  if (is.null(res$state)) {
    # older opal servers do not have state for the R session
    TRUE
  } else {
    tolower(res$state) == "running"
  }
}

#' Get the events associated to the R session
#' 
#' The remote R session logs events in its process of creation. Each event has a 
#' the current state of the R session, a date and a message reported by the R server.
#' Fails if the session does not exist.
#' 
#' @family session functions
#' @param opal Opal object.
#' @return A data frame.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.session(o, wait = TRUE)
#' opal.session_events(o)
#' opal.logout(o)
#' }
#' @export
opal.session_events <- function(opal) {
  if(is.null(opal$rid)) {
    stop("Remote R session not available")
  }
  res <- opal.get(opal, opal$context, "session", opal$rid)
  events <- res$events
  eventsMatrix <- sapply(events, function(x) {
    # split each element by =
    parts <- strsplit(x, ";")
    # create a named list
    list(state = parts[[1]][[1]],
         timestamp = parts[[1]][[2]],
         message = parts[[1]][[3]])
  })
  as.data.frame(t(eventsMatrix))
}

#' Delete the R session
#' 
#' Delete the remote R session. Ignored if the session does not exist.
#' 
#' @family session functions
#' @param opal Opal object.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.session(o)
#' opal.session_exists(o)
#' opal.session_delete(o)
#' opal.logout(o)
#' }
#' @export
opal.session_delete <- function(opal) {
  if(!is.null(opal$rid)) {
    tryCatch(opal.delete(opal, opal$context, "session", opal$rid), error = function(e) {})
    opal$rid <- NULL
  }
}

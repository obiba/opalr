#-------------------------------------------------------------------------------
# Copyright (c) 2016 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the R workspaces from a opal.
#' 
#' @param opal Opal object.
#' @export
opal.workspaces=function(opal) {
  if (!is.na(opal$version) && opal.version_compare(opal,"2.6")<0) {
    warning("Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    query <- list(context='R')
    wss <- .extractJsonField(.get(opal, "service", "r", "workspaces", query=query))
    if (length(wss)) {
      name <- c()
      user <- c()
      context <- c()
      lastAccessDate <- c()
      size <- c()
      for (i in 1:length(wss)) {
        ws <- wss[i]
        name <- c(name, ws[[1]]$name)
        user <- c(user, ws[[1]]$user)
        context <- c(context, ws[[1]]$context)
        lastAccessDate <- c(lastAccessDate, ws[[1]]$lastAccessDate)
        size <- c(size, ws[[1]]$size)
      }
      data.frame(name=name, user=user, context=context, lastAccessDate=lastAccessDate, size=size)
    }  
  }
}

#' Remove a R workspace from a opal.
#' 
#' @param opal Opal object.
#' @param ws The workspace name
#' @param user The user name associated to the worskpace. If not provided, the current user is applied.
#' @export
opal.workspace_rm=function(opal, ws, user=NULL) {
  if (!is.na(opal$version) && opal.version_compare(opal,"2.6")<0) {
    warning("Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    u <- user
    if (is.null(user)) {
      u <- opal$username
    }
    if (is.null(u) || length(u) == 0) {
      stop("User name is missing or empty.")
    }
    if (length(ws) == 0) {
      stop("Workspace name is missing or empty.")
    }
    query <- list(context='R', name=ws, user=u)
    ignore <- .extractJsonField(.delete(opal, "service", "r", "workspaces", query=query))  
  }
}

#' Save the current session in a opal R workspace.
#' 
#' @param opal Opal object.
#' @param save Save the workspace with given identifier (default is TRUE, current session ID if TRUE).
#' @export
opal.workspace_save=function(opal, save=TRUE) {
  if (!is.na(opal$version) && opal.version_compare(opal,"2.6")<0) {
    warning("Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    saveId <- save
    if(is.logical(save) && save) {
      saveId <- opal$rid
    }
    res <- .post(opal, "r", "session", opal$rid, "workspaces", query=list(save=saveId))
  }
}
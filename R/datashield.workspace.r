#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the DataSHIELD workspaces.
#' 
#' @param opal Opal object or list of opal objects.
#' @rdname datashield.workspaces
#' @export
datashield.workspaces=function(opal) {
  UseMethod('datashield.workspaces');
}

#' @rdname datashield.workspaces
#' @export
datashield.workspaces.opal=function(opal) {
  if (opal.version_compare(opal,"2.6")<0) {
    warning(opal$name, ": Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    query <- list(context='DataSHIELD')
    prefix <- paste0('^', opal$name, ':')
    res <- lapply(.extractJsonField(.get(opal, "service", "r", "workspaces", query=query)),
           function(ws) {
             if (grepl(prefix, ws$name)) {
               return(ws)
             }
           })
    wss <- res[lapply(res, is.null) != TRUE]
    if (length(wss)) {
      server <- c()
      name <- c()
      user <- c()
      context <- c()
      lastAccessDate <- c()
      size <- c()
      for (i in 1:length(wss)) {
        ws <- wss[i]
        server <- c(server, opal$name)
        name <- c(name, ws[[1]]$name)
        user <- c(user, ws[[1]]$user)
        context <- c(context, ws[[1]]$context)
        lastAccessDate <- c(lastAccessDate, ws[[1]]$lastAccessDate)
        size <- c(size, ws[[1]]$size)
      }
      data.frame(server=server, name=name, user=user, context=context, lastAccessDate=lastAccessDate, size=size)
    }
  }
}

#' @rdname datashield.workspaces
#' @export
datashield.workspaces.list=function(opal) {
  res <- lapply(opal, FUN=datashield.workspaces.opal)
  server <- c()
  name <- c()
  user <- c()
  context <- c()
  lastAccessDate <- c()
  size <- c()
  for (n in names(res)) {
    wss <- res[[n]]
    if (!is.character(wss)) {
      server <- c(server, as.vector(wss$server))
      name <- c(name, as.vector(wss$name))
      user <- c(user, as.vector(wss$user))
      context <- c(context, as.vector(wss$context))
      lastAccessDate <- c(lastAccessDate, as.vector(wss$lastAccessDate))
      size <- c(size, as.vector(wss$size))  
    }
  }
  if (length(server)) {
    data.frame(server=server, name=name, user=user, context=context, lastAccessDate=lastAccessDate, size=size) 
  }
}

#' Remove a DataSHIELD workspace from a opal.
#' 
#' @param opal Opal object or list of opal objects.
#' @param ws The workspace name
#' @rdname datashield.workspace_rm
#' @export
datashield.workspace_rm=function(opal, ws) {
  UseMethod('datashield.workspace_rm');
}

#' @rdname datashield.workspace_rm
#' @export
datashield.workspace_rm.opal=function(opal, ws) {
  u <- opal$username
  if (is.null(u) || length(u) == 0) {
    stop("User name is missing or empty.")
  }
  if (length(ws) == 0) {
    stop("Workspace name is missing or empty.")
  }
  query <- list(context='DataSHIELD', name=ws, user=u)
  if (opal.version_compare(opal,"2.6")<0) {
    warning(opal$name, ": Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    ignore <- .extractJsonField(.delete(opal, "service", "r", "workspaces", query=query))
  }
}

#' @rdname datashield.workspace_rm
#' @export
datashield.workspace_rm.list=function(opal, ws) {
  if (length(ws) == 0) {
    stop("Workspace name is missing or empty.")
  }
  res <- lapply(1:length(opal), function(i) {
    o <- opal[[i]]
    wsname <- paste0(o$name, ':', ws)
    datashield.workspace_rm.opal(o, ws=wsname)
  })
}

#' Save current session in a DataSHIELD workspace.
#' 
#' @param opal Opal object or list of opal objects.
#' @param save The workspace name
#' @rdname datashield.workspace_save
#' @export
datashield.workspace_save=function(opal, save) {
  UseMethod('datashield.workspace_save');
}

#' @rdname datashield.workspace_save
#' @export
datashield.workspace_save.opal=function(opal, save) {
  u <- opal$username
  if (is.null(u) || length(u) == 0) {
    stop("User name is missing or empty.")
  }
  if (length(save) == 0) {
    stop("Workspace name is missing or empty.")
  }
  query <- list(save=save)
  if (opal.version_compare(opal,"2.6")<0) {
    warning(opal$name, ": Workspaces are not available for opal ", opal$version, " (2.6.0 or higher is required)")
  } else {
    ignore <- .post(opal, "datashield", "session", opal$rid, "workspaces", query=query) 
  }
}

#' @rdname datashield.workspace_save
#' @export
datashield.workspace_save.list=function(opal, save) {
  if (length(save) == 0) {
    stop("Workspace name is missing or empty.")
  }
  res <- lapply(1:length(opal), function(i) {
    o <- opal[[i]]
    wsname <- paste0(o$name, ':', save)
    datashield.workspace_save.opal(o, save=wsname)
  })
}
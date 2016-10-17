#-------------------------------------------------------------------------------
# Copyright (c) 2016 OBiBa. All rights reserved.
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
datashield.workspaces=function(opal, ...) {
  UseMethod('datashield.workspaces');
}

#' @rdname datashield.workspaces
#' @method datashield.workspaces opal
#' @S3method datashield.workspaces opal
datashield.workspaces.opal=function(opal) {
  query <- list(context='DataSHIELD')
  suffix <- paste0('-', opal$name, '$')
  res <- lapply(opal:::.extractJsonField(opal:::.get(opal, "service", "r", "workspaces", query=query)),
         function(ws) {
           if (grepl(suffix, ws$name)) {
             return(ws)
           }
         })
  res[lapply(res, is.null) != TRUE]
}

#' @rdname datashield.workspaces
#' @method datashield.workspaces list
#' @S3method datashield.workspaces list
datashield.workspaces.list=function(opals) {
  lapply(opals, FUN=datashield.workspaces.opal)
}

#' Remove a DataSHIELD workspace from a opal.
#' 
#' @param opal Opal object or list of opal objects.
#' @param ws The workspace name
#' @rdname datashield.workspace_rm
#' @export
datashield.workspace_rm=function(opal, ...) {
  UseMethod('datashield.workspace_rm');
}

#' @rdname datashield.workspace_rm
#' @method datashield.workspace_rm opal
#' @S3method datashield.workspace_rm opal
datashield.workspace_rm.opal=function(opal, ws) {
  u <- opal$username
  if (is.null(u) || length(u) == 0) {
    stop("User name is missing or empty.")
  }
  if (length(ws) == 0) {
    stop("Workspace name is missing or empty.")
  }
  query <- list(context='DataSHIELD', name=ws, user=u)
  ignore <- opal:::.extractJsonField(opal:::.delete(opal, "service", "r", "workspaces", query=query))
}

#' @rdname datashield.workspace_rm
#' @method datashield.workspace_rm list
#' @S3method datashield.workspace_rm list
datashield.workspace_rm.list=function(opals, ws) {
  if (length(ws) == 0) {
    stop("Workspace name is missing or empty.")
  }
  res <- lapply(1:length(opals), function(i) {
    o <- opals[[i]]
    wsname <- paste0(ws, '-', o$name)
    datashield.workspace_rm.opal(o, ws=wsname)
  })
}

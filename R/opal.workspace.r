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
  query <- list(context='R')
  opal:::.extractJsonField(opal:::.get(opal, "service", "r", "workspaces", query=query))
}

#' Remove a R workspace from a opal.
#' 
#' @param opal Opal object.
#' @param ws The workspace name
#' @param user The user name associated to the worskpace. If not provided, the current user is applied.
#' @export
opal.workspace_rm=function(opal, ws, user=NULL) {
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
  ignore <- opal:::.extractJsonField(opal:::.delete(opal, "service", "r", "workspaces", query=query))
}
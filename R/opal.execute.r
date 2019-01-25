#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Execute a R script on Opal(s).
#' 
#' @title Execute a R script
#'
#' @param opal Opal object or list of opal objects.
#' @param script R script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @param session Execute in current R session (default is TRUE).
#' @export
opal.execute <- function(opal, script, async=FALSE, session=TRUE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.execute(o, script, async=async, session=session)})
  } else {
    if (session) {
      query <- list()
      if (async) query <- list(async="true")
      ignore <- .getRSessionId(opal)
      opal.post(opal, "r", "session", opal$rid, "execute", query=query, body=script, contentType="application/x-rscript")
    } else {
      opal.post(opal, "r", "execute", body=script, contentType="application/x-rscript")
    }
  }
}

#' Load package in the current session.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
opal.load_package <- function(opal, pkg) {
  opal.execute(opal, paste('library("', pkg, '")', sep=''), TRUE)
}

#' Unload package from the current session.
#'
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @export
opal.unload_package <- function(opal, pkg) {
  resp <- opal.execute(opal, paste('detach("package:', pkg, '", character.only=TRUE, unload=TRUE)', sep=''), TRUE)
}

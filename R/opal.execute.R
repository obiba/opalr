#-------------------------------------------------------------------------------
# Copyright (c) 2020 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Execute a R script
#' 
#' Execute a R script in the remote R session.
#' 
#' @family execution functions
#' @param opal Opal object or list of opal objects.
#' @param script R script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE). 
#'   If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.execute(o, "x <- 'foo'")
#' opal.execute(o, "ls()")
#' opal.logout(o)
#' }
#' @export
opal.execute <- function(opal, script, async=FALSE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.execute(o, script, async=async)})
  } else {
    query <- list()
    if (async) query <- list(async="true")
    ignore <- .getRSessionId(opal)
    opal.post(opal, "r", "session", opal$rid, "execute", query=query, body=script, contentType="application/x-rscript", acceptType = "application/octet-stream")
  }
}

#' Execute a R file script
#' 
#' Upload a R file script and execute it in the remote R session with source().
#' 
#' @family execution functions
#' @param opal Opal object or list of opal objects.
#' @param path Path to the R file script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE). 
#'   If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.execute.source(o, "myscript.R")
#' opal.logout(o)
#' }
#' @export
opal.execute.source <- function(opal, path, async=FALSE) {
  if (!file.exists(path)) {
    stop("No file at path: ", path)
  }
  if (file.info(path)$isdir) {
    stop("Cannot source a directory: ", path)
  }
  if(is.list(opal)){
    lapply(opal, function(o){opal.execute.source(o, path, async=async)})
  } else {
    tmp <- opal.file_mkdir_tmp(opal)
    opal.file_upload(opal, path, tmp)
    filename <- basename(path)
    opal.file_write(opal, paste0(tmp, filename))
    opal.file_rm(opal, tmp)
    script <- paste0("source('", filename, "')")
    query <- list()
    if (async) query <- list(async="true")
    ignore <- .getRSessionId(opal)
    opal.post(opal, "r", "session", opal$rid, "execute", query=query, body=script, contentType="application/x-rscript", acceptType = "application/octet-stream")
  }
}

#' Load package
#' 
#' Load package in the remote R session.
#' 
#' @family execution functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.load_package(o, 'stats')
#' opal.logout(o)
#' }
#' @export
opal.load_package <- function(opal, pkg) {
  resp <- opal.execute(opal, paste('library("', pkg, '")', sep=''))
}

#' Unload package
#' 
#' Unload package from the remote R session.
#'
#' @family execution functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.unload_package(o, 'stats')
#' opal.logout(o)
#' }
#' @export
opal.unload_package <- function(opal, pkg) {
  resp <- opal.execute(opal, paste('detach("package:', pkg, '", character.only=TRUE, unload=TRUE)', sep=''))
}

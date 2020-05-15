#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the resource references of a project
#' 
#' @family project functions
#' @param opal Opal object.
#' @param project Name of the project.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \donttest{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' opal.resources(o, 'datashield')
#' opal.logout(o)
#' }
#' @export
opal.resources <- function(opal, project, df=TRUE) {
  if (!is.na(opal$version) && opal.version_compare(opal,"2.17")<0) {
    stop("Resources are not available in opal ", opal$version, " (2.17.0 or higher is required)")
  }
  res <- opal.get(opal, "project", project, "resources")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    prj <- rep(project, n)
    url <- rep(NA, n)
    format <- rep(NA, n)
    created <- rep(NA, n)
    updated <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      url[i] <- item$resource$url
      if (!is.null(item$resource$format)) {
        format[i] <- item$resource$format  
      }
      created[i] <- item$created
      updated[i] <- item$updated
    }
    data.frame(name, project=prj, url=url, format=format, created, updated) 
  } else {
    data.frame()
  }
}

#' Get a resource of a project
#' 
#' @family project functions
#' @param opal Opal object.
#' @param project Name of the project.
#' @param resource Name of the resource in the project.
#' @examples 
#' \donttest{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' opal.resource(o, 'datashield', 'CNSIM1r')
#' opal.logout(o)
#' }
#' @export
opal.resource <- function(opal, project, resource) {
  if (!is.na(opal$version) && opal.version_compare(opal,"2.17")<0) {
    stop("Resources are not available in opal ", opal$version, " (2.17.0 or higher is required)")
  }
  opal.get(opal, "project", project, "resource", resource);
}
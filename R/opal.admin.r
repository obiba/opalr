#-------------------------------------------------------------------------------
# Copyright (c) 2020 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Install package
#' 
#' Install package if not already available in Opal(s). To install the latest version of a package, it has to be removed first.
#' 
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param repos Character vector, the base URLs of the repositories to use.
#' @return TRUE if successfully installed
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' oadmin.install_package(o, 'xxx')
#' opal.logout(o)
#' }
#' @export
oadmin.install_package <- function(opal, pkg, repos=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){oadmin.install_package(o, pkg, repos)})
  } else {
    if (!oadmin.installed_package(opal, pkg)) {
      # default repos
      defaultrepos <- c(getOption("repos"), "http://cran.obiba.org", "http://cloud.r-project.org")
      if (getOption("repos") != "@CRAN@") {
        defaultrepos <- append(defaultrepos, getOption("repos"))
      }
      # append user provided ones
      repostr <- paste('"', append(defaultrepos, repos),'"',collapse=',',sep='')
      cmd <- paste('install.packages("', pkg, '", repos=c(', repostr ,'), dependencies=TRUE)', sep='')
      resp <- opal.execute(opal, cmd, FALSE)
      oadmin.installed_package(opal, pkg)
    } else {
      TRUE
    }
  }
}

#' Remove package
#' 
#' Remove package permanently.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' oadmin.remove_package(o, 'xxx')
#' opal.logout(o)
#' }
#' @export
oadmin.remove_package <- function(opal, pkg) {
  ignore <- tryCatch(opal.execute(opal, paste('remove.packages("', pkg, '")', sep=''), FALSE), error=function(e){})
}

#' Check package is installed
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @return TRUE if installed
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' oadmin.installed_package(o, 'xxx')
#' oadmin.installed_package(o, 'stats')
#' opal.logout(o)
#' }
#' @export
oadmin.installed_package <- function(opal, pkg) {
  opal.execute(opal, paste('require("', pkg, '", character.only=TRUE)', sep=''), FALSE)
}

#' List installed packages
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @return The result of the installed.packages() call
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' oadmin.installed_packages(o)
#' opal.logout(o)
#' }
#' @export
oadmin.installed_packages <- function(opal) {
  opal.execute(opal, "installed.packages()")
}

#' Get package description
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param fields A character vector giving the fields to extract from each package's DESCRIPTION file in addition to the default ones, or NULL (default). Unavailable fields result in NA values.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' oadmin.package_description(o, 'stats')
#' opal.logout(o)
#' }
#' @export
oadmin.package_description <- function(opal, pkg, fields=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){oadmin.package_description(o, pkg, fields=fields)})
  } else {
    # always query for Datashield fields
    fields <- append(c("Title","Description","Author","Maintainer","Date/Publication","AggregateMethods","AssignMethods"), fields)
    inst <- opal.execute(opal, paste('installed.packages(fields=c("', paste(fields, collapse='","') ,'"))', sep=''), FALSE)
    desc <- NULL
    for (i in 1:nrow(inst)) {
      if(inst[i]==pkg) { 
        desc <- strsplit(inst[i,],"\n")
        break
      }
    }
    return(desc)
  }
}

#' Install devtools package
#' 
#' Install devtools package if not already available.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' oadmin.install_devtools(o)
#' opal.logout(o)
#' }
#' @export
oadmin.install_devtools <- function(opal) {
  oadmin.install_package(opal,'devtools')
}

#' Check devtools package
#' 
#' Check if devtools package is installed.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' oadmin.installed_devtools(o)
#' opal.logout(o)
#' }
#' @export
oadmin.installed_devtools <- function(opal) {
  oadmin.installed_package(opal,'devtools')
}

#' Install a package form GitHub
#' 
#' Install a package from a source repository on GitHub. Makes sure devtools package is available.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param username GitHub user name.
#' @param ref Desired git reference. Could be a commit, tag, or branch name. Defaults to "master".
#' @param auth_user Your github username if you're attempting to install a package hosted in a private repository (and your username is different to username).
#' @param password Your github password
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' oadmin.install_github(o, 'opalr', 'obiba')
#' opal.logout(o)
#' }
#' @export
oadmin.install_github <- function(opal, pkg , username=getOption("github.user"), ref="master", auth_user=NULL, password=NULL) {
  oadmin.install_devtools(opal)
  cmd <- paste('devtools::install_github("', paste0(username, "/", pkg), '", ref="', ref, '")', sep="")
  opal.execute(opal, cmd, FALSE)
  oadmin.installed_package(opal, pkg)
}

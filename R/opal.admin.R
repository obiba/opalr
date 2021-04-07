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
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
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
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
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
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
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
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
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
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
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
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
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
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.installed_devtools(o)
#' opal.logout(o)
#' }
#' @export
oadmin.installed_devtools <- function(opal) {
  oadmin.installed_package(opal,'devtools')
}

#' Install a package from GitHub
#' 
#' Install a package from a source repository on GitHub.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param username GitHub user or organization name.
#' @param ref Desired git reference. Could be a commit, tag, or branch name. Defaults to "master".
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.install_github_package(o, 'opalr', 'obiba')
#' opal.logout(o)
#' }
#' @export
oadmin.install_github_package <- function(opal, pkg , username=getOption("github.user"), ref="master") {
  opal.post(opal, "service", "r", "packages", query = list(name = paste0(username, "%2F", pkg), ref = ref, manager = "gh"))
  oadmin.installed_package(opal, pkg)
}

#' Install a package from Bioconductor
#' 
#' Install a package from a source repository on GitHub.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.install_bioconductor_package(o, 'GWASTools')
#' opal.logout(o)
#' }
#' @export
oadmin.install_bioconductor_package <- function(opal, pkg) {
  opal.post(opal, "service", "r", "packages", query = list(name = pkg, manager = "bioc"))
  oadmin.installed_package(opal, pkg)
}

#' Install a package from a local archive file
#' 
#' Install a package from a package archive file. This will upload the archive file and run its installation in the R server.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param path Path to the package archive file.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # install a pre-built local archive file
#' oadmin.install_local_package(o, '~/Rserve_1.8-7.tar.gz')
#' # or build archive file from local package source (in current working folder)
#' oadmin.install_local_package(o, devtools::build())
#' opal.logout(o)
#' }
#' @export
oadmin.install_local_package <- function(opal, path) {
  if (!file.exists(path)) {
    stop("Package archive file cannot be found at: ", path)
  }
  filename <- basename(path)
  if (!endsWith(filename, ".tar.gz")) {
    stop("Not a package archive file: ", filename)
  }
  # strip suffix
  pkg <- strsplit(filename, "\\.")[[1]][1]
  # strip version
  pkg <- strsplit(pkg, "_")[[1]][1]
  
  tmp <- opal.file_mkdir_tmp(opal)
  opal.file_upload(opal, path, tmp)
  opal.file_write(opal, paste0(tmp, filename))
  opal.file_rm(opal, tmp)
  opal.execute(opal, paste0("install.packages('", filename, "', repos = NULL, type ='source')"))
  
  oadmin.installed_package(opal, pkg)
}

#' Add or update a R permission
#' 
#' Add or update a permission on the R service.
#' 
#' @param opal Opal connection object.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: use.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.perm(o)
#' oadmin.perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.perm_add <- function(opal, subject, type = "user", permission) {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('use' = 'R_USE')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid R permission name: ", permission)
  }
  oadmin.perm_delete(opal, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "system", "permissions", "r", query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the R permissions
#' 
#' Get the permissions that were applied to the R service.
#' 
#' @param opal Opal connection object.
#' 
#' @return A data.frame with columns: subject, type, permission
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.perm(o)
#' oadmin.perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.perm <- function(opal) {
  perms <- list('R_USE' = 'use', 'R_ALL' = 'administrate')
  acls <- opal.get(opal, "system", "permissions", "r")
  .aclsToDataFrame(perms, acls)
}

#' Delete a R permission
#' 
#' Delete a permission that was applied to the R service. Silently returns when there is no such permission.
#' 
#' @param opal Opal connection object.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.perm(o)
#' oadmin.perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.perm_delete <- function(opal, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "system", "permissions", "r", query = list(principal = subject[i], type = toupper(type)))  
  }
}
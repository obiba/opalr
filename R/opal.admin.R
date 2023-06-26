#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Install CRAN package
#' 
#' Install package from CRAN repos. To install the latest version of a package, it has to be removed first.
#' 
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param repos Character vector, the base URLs of the repositories to use.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @return TRUE if successfully installed
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.install_package(o, 'xxx')
#' opal.logout(o)
#' }
#' @export
oadmin.install_package <- function(opal, pkg, repos=NULL, profile = NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){oadmin.install_package(o, pkg, repos)})
  } else if (opal.version_compare(opal, "4.0")<0) {
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
    oadmin.install_cran_package(opal, pkg, profile = profile)
    oadmin.installed_package(opal, pkg, profile = profile)
  }
}

#' Remove package
#' 
#' Remove package permanently.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.remove_package(o, 'xxx')
#' opal.logout(o)
#' }
#' @export
oadmin.remove_package <- function(opal, pkg, profile = NULL) {
  if (opal.version_compare(opal, "4.0")<0)
    ignore <- tryCatch(opal.execute(opal, paste('remove.packages("', pkg, '")', sep=''), FALSE), error=function(e){})
  else {
    cluster <- .toSafeProfile(opal, profile)
    ignore <- opal.delete(opal, "service", "r", "cluster", cluster, "package", pkg)
  }
}

#' Check package is installed
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @return TRUE if installed
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.installed_package(o, 'xxx')
#' oadmin.installed_package(o, 'stats')
#' opal.logout(o)
#' }
#' @export
oadmin.installed_package <- function(opal, pkg, profile = NULL) {
  if (opal.version_compare(opal, "4.0")<0)
    opal.execute(opal, paste('require("', pkg, '", character.only=TRUE)', sep=''), FALSE)
  else {
    cluster <- .toSafeProfile(opal, profile)
    dto <- tryCatch(opal.get(opal, "service", "r", "cluster", cluster, "package", pkg), error = function(e) { NULL })
    !is.null(dto)
  }
}

#' List installed packages
#'
#' Get the installed packages from all the R servers in the cluster described by the profile.
#'
#' @family administration functions
#' @param opal Opal object.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @return The result of the installed.packages() call
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.installed_packages(o)
#' opal.logout(o)
#' }
#' @export
oadmin.installed_packages <- function(opal, profile = NULL) {
  if (opal.version_compare(opal, "4.0")<0)
    opal.execute(opal, "installed.packages()")
  else {
    cluster <- .toSafeProfile(opal, profile)
    dtos <- opal.get(opal, "service", "r", "cluster", cluster, "packages")
    n <- length(dtos)
    name <- replicate(n, NA)
    cluster <- replicate(n, NA)
    rserver <- replicate(n, NA)
    fields <- list()
    if (n>0) {
      # scan for description fields
      for (i in 1:n) {
        for (fieldName in sapply(dtos[[i]]$description, function(f) f$key)) 
          if (!(fieldName %in% names(fields))) 
            fields[[fieldName]] <- replicate(n, NA)
      }
      # populate
      for (i in 1:n) {
        name[i] <- dtos[[i]]$name
        cluster[i] <- dtos[[i]]$cluster
        rserver[i] <- dtos[[i]]$rserver
        for (field in dtos[[i]]$description)
          fields[[field$key]][i] <- .nullToNA(field$value)
      }
    }
    df <- data.frame(name = name, cluster = cluster, rserver = rserver, stringsAsFactors = FALSE)
    for (fieldName in names(fields))
      df[[fieldName]] <- fields[[fieldName]]
    df
  }
}

#' Get package description
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param fields A character vector giving the fields to extract from each package's DESCRIPTION file in addition to the default ones, or NULL (default). Unavailable fields result in NA values.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.package_description(o, 'stats')
#' opal.logout(o)
#' }
#' @export
oadmin.package_description <- function(opal, pkg, fields=NULL, profile = NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){oadmin.package_description(o, pkg, fields=fields)})
  } else if (opal.version_compare(opal, "4.0")<0) {
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
  } else {
    cluster <- .toSafeProfile(opal, profile)
    dto <- opal.get(opal, "service", "r", "cluster", cluster, "package", pkg)
    description <- list()
    for (entry in dto$description) {
      description[[entry$key]] <- .nullToNA(entry$value)
    }
    description
  }
}

#' Install devtools package
#' 
#' Install devtools package if not already available.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.install_devtools(o)
#' opal.logout(o)
#' }
#' @export
oadmin.install_devtools <- function(opal, profile = NULL) {
  oadmin.install_cran_package(opal, 'devtools', profile = profile)
}

#' Check devtools package
#' 
#' Check if devtools package is installed.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.installed_devtools(o)
#' opal.logout(o)
#' }
#' @export
oadmin.installed_devtools <- function(opal, profile = NULL) {
  oadmin.installed_package(opal, 'devtools', profile = profile)
}

#' Install a package from CRAN
#' 
#' Install a package from configured CRAN repositories.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.install_cran_package(o, 'opalr', 'obiba')
#' opal.logout(o)
#' }
#' @export
oadmin.install_cran_package <- function(opal, pkg, profile = NULL) {
  if (opal.version_compare(opal, "4.0")<0)
    opal.post(opal, "service", "r", "packages", query = list(name = pkg, manager = "cran"))
  else {
    cluster <- .toSafeProfile(opal, profile)
    opal.post(opal, "service", "r", "cluster", cluster, "packages", query = list(name = pkg, manager = "cran"))
  }
  oadmin.installed_package(opal, pkg, profile = profile)
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
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.install_github_package(o, 'opalr', 'obiba')
#' opal.logout(o)
#' }
#' @export
oadmin.install_github_package <- function(opal, pkg , username=getOption("github.user"), ref="master", profile = NULL) {
  if (opal.version_compare(opal, "4.0")<0)
    opal.post(opal, "service", "r", "packages", query = list(name = paste0(username, "/", pkg), ref = ref, manager = "gh"))
  else {
    cluster <- .toSafeProfile(opal, profile)
    opal.post(opal, "service", "r", "cluster", cluster, "packages", query = list(name = paste0(username, "/", pkg), ref = ref, manager = "gh"))
  }
  oadmin.installed_package(opal, pkg, profile = profile)
}

#' Install a package from Bioconductor
#' 
#' Install a package from a source repository on GitHub.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.install_bioconductor_package(o, 'GWASTools')
#' opal.logout(o)
#' }
#' @export
oadmin.install_bioconductor_package <- function(opal, pkg, profile = NULL) {
  if (opal.version_compare(opal, "4.0")<0)
    opal.post(opal, "service", "r", "packages", query = list(name = pkg, manager = "bioc"))
  else {
    cluster <- .toSafeProfile(opal, profile)
    opal.post(opal, "service", "r", "cluster", cluster, "packages", query = list(name = pkg, manager = "bioc"))
  }
  oadmin.installed_package(opal, pkg, profile = profile)
}

#' Install a package from a local archive file
#' 
#' Install a package from a package archive file. This will upload the archive file and run its installation in the R server.
#' The R server profile to which the operation applies is the one specified at login time.
#'
#' @family administration functions
#' @param opal Opal object or list of opal objects.
#' @param path Path to the package archive file.
#' @param profile The R servers profile name to which operation applies. See also \link{opal.profiles}.
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
oadmin.install_local_package <- function(opal, path, profile = NULL) {
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
  
  if (opal.version_compare(opal, "4.2")<0) {
    opal.file_write(opal, paste0(tmp, filename))
    opal.execute(opal, paste0("install.packages('", filename, "', repos = NULL, type ='source')"))
  } else {
    cluster <- .toSafeProfile(opal, profile)
    opal.post(opal, "service", "r", "cluster", cluster, "packages", query = list(name = paste0(tmp, filename), manager = "local"))
  }
  
  opal.file_rm(opal, tmp)
  oadmin.installed_package(opal, pkg, profile = .toSafeProfile(opal, profile))
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
#' oadmin.r_perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.r_perm(o)
#' oadmin.r_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.r_perm_add <- function(opal, subject, type = "user", permission = 'use') {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('use' = 'R_USE')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid R permission name: ", permission)
  }
  oadmin.r_perm_delete(opal, subject, type)
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
#' oadmin.r_perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.r_perm(o)
#' oadmin.r_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.r_perm <- function(opal) {
  perms <- list('R_USE' = 'use')
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
#' oadmin.r_perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.r_perm(o)
#' oadmin.r_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.r_perm_delete <- function(opal, subject, type = "user") {
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

#' Get system metrics
#' 
#' Get some metrics about the Opal system status. The following information are returned:
#' `timestamp` (the EPOC time at which the metrics were collected),
#' `uptime` (the running time in millis),
#' `heapMemory` (the memory currently used),
#' `nonHeapMemory` (the memory that can be used),
#' `threads` (the current (count) and maximum (peak) numbers of threads),
#' `gcs` (the garbage collectors activity).
#' 
#' @param opal Opal connection object.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.system_metrics(o)
#' opal.logout(o)
#' }
#' @export
oadmin.system_metrics <- function(opal) {
  opal.get(opal, "system", "status")
}

#' Add or update a System permission
#' 
#' Add or update a permission on the whole system.
#' 
#' @param opal Opal connection object.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: project_add or administrate.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.system_perm_add(o, c('andrei', 'valentina'), 'user', 'project_add')
#' oadmin.system_perm(o)
#' oadmin.system_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.system_perm_add <- function(opal, subject, type = "user", permission) {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('project_add' = 'PROJECT_ADD', 'administrate' = 'SYSTEM_ALL')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid system permission name: ", permission)
  }
  oadmin.system_perm_delete(opal, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "system", "permissions", "administration", query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the System permissions
#' 
#' Get the permissions that were applied to the whole system.
#' 
#' @param opal Opal connection object.
#' 
#' @return A data.frame with columns: subject, type, permission
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.system_perm_add(o, c('andrei', 'valentina'), 'user', 'project_add')
#' oadmin.system_perm(o)
#' oadmin.system_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.system_perm <- function(opal) {
  perms <- list('PROJECT_ADD' = 'project_add', 'SYSTEM_ALL' = 'administrate')
  acls <- opal.get(opal, "system", "permissions", "administration")
  .aclsToDataFrame(perms, acls)
}

#' Delete a System permission
#' 
#' Delete a permission that was applied to the whole system. Silently returns when there is no such permission.
#' 
#' @param opal Opal connection object.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.system_perm_add(o, c('andrei', 'valentina'), 'user', 'project_add')
#' oadmin.system_perm(o)
#' oadmin.system_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.system_perm_delete <- function(opal, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "system", "permissions", "administration", query = list(principal = subject[i], type = toupper(type)))  
  }
}

#
# Deprecated functions
#

#' Add or update a R permission (deprecated)
#' 
#' Deprecated, use \link{oadmin.r_perm_add}.
#' 
#' @param opal Opal connection object.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: use.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.r_perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.r_perm(o)
#' oadmin.r_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.perm_add <- function(opal, subject, type = "user", permission) {
  warning("Deprecated: use oadmin.r_perm_add()")
  oadmin.r_perm_add(opal, subject, type, permission)
}

#' Get the R permissions (deprecated)
#' 
#' Deprecated, use \link{oadmin.r_perm}.
#' 
#' @param opal Opal connection object.
#' 
#' @return A data.frame with columns: subject, type, permission
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.r_perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.r_perm(o)
#' oadmin.r_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.perm <- function(opal) {
  warning("Deprecated: use oadmin.r_perm()")
  oadmin.r_perm(opal)
}

#' Delete a R permission (deprecated)
#' 
#' Deprecated, use \link{oadmin.r_perm_delete}.
#' 
#' @param opal Opal connection object.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.r_perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' oadmin.r_perm(o)
#' oadmin.r_perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
oadmin.perm_delete <- function(opal, subject, type = "user") {
  oadmin.perm_delete(opal, subject, type)
}


#' Get R activity
#'
#' Get the recorded R session metrics. 
#'
#' @param opal Opal connection object.
#' @param user Optional user name.
#' @param profile Optional profile name.
#' @param from Optional start date.
#' @param to Optional end date.
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # all sessions metrics
#' oadmin.activity(o)
#' # user and profile specific session metrics
#' oadmin.activity(o, user = 'dsuser', profile = 'default')
#' # user sessions in a time range
#' oadmin.activity(o, user = "dsuser", from = "2022-07-01", to = "2023-01-01")
#' opal.logout(o)
#' }
#' @export
oadmin.activity <- function(opal, user = NULL, profile = NULL, from = NULL, to = NULL, df = TRUE) {
  if (opal.version_compare(opal,"4.6")<0) {
    stop("R activity requires Opal 4.6 or higher.")
  }
  dtos <- opal.get(opal, "service", "r", "activity", query = list(context = "R", user = user, profile = profile, from = from, to = to))
  if (df) {
    n <- length(dtos)
    profile <- rep(NA, n)
    user <- rep(NA, n)
    created <- rep(NA, n)
    updated <- rep(NA, n)
    executionTimeMillis <- rep(NA, n)
    if (n>0) {
      for (i in 1:n) {
        profile[i] <- dtos[[i]]$profile
        user[i] <- dtos[[i]]$user
        created[i] <- dtos[[i]]$createdDate
        updated[i] <- dtos[[i]]$updatedDate
        executionTimeMillis[i] <- dtos[[i]]$executionTimeMillis
      }
    }
    data.frame(profile = profile, user = user, created = created, updated = updated, executionTimeMillis = executionTimeMillis, stringsAsFactors = FALSE)
  } else {
    dtos
  }
}

#' Get R activity summary
#'
#' Get the recorded R session metrics, grouped by profile and user. 
#'
#' @param opal Opal connection object.
#' @param user Optional user name.
#' @param profile Optional profile name.
#' @param from Optional start date.
#' @param to Optional end date.
#' @param df Return a data.frame (default is TRUE)
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # all sessions metrics
#' oadmin.activity_summary(o)
#' # user and profile specific session metrics
#' oadmin.activity_summary(o, user = 'dsuser', profile = 'default')
#' # user sessions in a time range
#' oadmin.activity_summary(o, user = "dsuser", from = "2022-07-01", to = "2023-01-01")
#' opal.logout(o)
#' }
#' @export
oadmin.activity_summary <- function(opal, user = NULL, profile = NULL, from = NULL, to = NULL, df = TRUE) {
  if (opal.version_compare(opal,"4.6")<0) {
    stop("DataSHIELD activity requires Opal 4.6 or higher.")
  }
  dtos <- opal.get(opal, "service", "r", "activity", "_summary",  query = list(context = "R", user = user, profile = profile, from = from, to = to))
  if (df) {
    n <- length(dtos)
    profile <- rep(NA, n)
    user <- rep(NA, n)
    start <- rep(NA, n)
    end <- rep(NA, n)
    executionTimeMillis <- rep(NA, n)
    sessionsCount <- rep(NA, n)
    if (n>0) {
      for (i in 1:n) {
        profile[i] <- dtos[[i]]$profile
        user[i] <- dtos[[i]]$user
        start[i] <- dtos[[i]]$start
        end[i] <- dtos[[i]]$end
        executionTimeMillis[i] <- dtos[[i]]$executionTimeMillis
        sessionsCount[i] <- dtos[[i]]$sessionsCount
      }
    }
    data.frame(profile = profile, user = user, start = start, end = end, executionTimeMillis = executionTimeMillis, sessionsCount = sessionsCount, stringsAsFactors = FALSE)
  } else {
    dtos
  }
}

#' Get Opal main logs
#' 
#' @param opal Opal connection object.
#' @param all Get all or only latest log messages.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.log(o)
#' opal.logout(o)
#' }
#' @export
#' @import readr
oadmin.log <- function(opal, all = TRUE) {
  tmp <- tempfile()
  opal.get(opal, "system", "log", "opal.log", acceptType = "text/plain", query = list(all = all), outFile = tmp)
  rval <- readr::read_lines(tmp)
  unlink(tmp)
  rval
}

#' Get Opal REST API logs
#' 
#' @param opal Opal connection object.
#' @param all Get all or only latest log messages.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.log_rest(o)
#' opal.logout(o)
#' }
#' @export
#' @import jsonlite
oadmin.log_rest <- function(opal, all = TRUE) {
  tmp <- tempfile()
  opal.get(opal, "system", "log", "rest.log", acceptType = "text/plain", query = list(all = all), outFile = tmp)
  rval <- jsonlite::stream_in(file(tmp))
  unlink(tmp)
  rval
}

#' Get Opal SQL API logs
#' 
#' @param opal Opal connection object.
#' @param all Get all or only latest log messages.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.log_sql(o)
#' opal.logout(o)
#' }
#' @export
#' @import jsonlite
oadmin.log_sql <- function(opal, all = TRUE) {
  tmp <- tempfile()
  opal.get(opal, "system", "log", "sql.log", acceptType = "text/plain", query = list(all = all), outFile = tmp)
  rval <- jsonlite::stream_in(file(tmp))
  unlink(tmp)
  rval
}
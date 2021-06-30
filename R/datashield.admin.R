#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get DataSHIELD profiles
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @return The DataSHIELD profiles as a data.frame or a list
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.package_descriptions(o)
#' opal.logout(o)
#' }
#' @export
dsadmin.profiles <- function(opal, df=TRUE) {
  if (opal.version_compare(opal,"4.2")<0) {
    warning("DataSHIELD profiles require Opal 4.2 or higher.")
    # emulated response
    if (df) {
      data.frame(name="default", cluster="default", enabled=TRUE, restrictedAccess=FALSE, stringsAsFactors = FALSE)
    } else {
      list(name="default", cluster="default", enabled=TRUE, restrictedAccess=FALSE)
    }
  } else {
    dtos <- opal.get(opal, "datashield", "profiles")
    if (df) {
      n <- length(dtos)
      name <- rep(NA, n)
      cluster <- rep(NA, n)
      enabled <- rep(NA, n)
      restrictedAccess <- rep(NA, n)
      if (n>0) {
        for (i in 1:n) {
          name[i] <- dtos[[i]]$name
          cluster[i] <- dtos[[i]]$cluster
          enabled[i] <- dtos[[i]]$enabled
          restrictedAccess[i] <- dtos[[i]]$restrictedAccess
        }
      }
      data.frame(name=name, cluster = cluster, enabled = enabled, restrictedAccess = restrictedAccess, stringsAsFactors = FALSE)
    } else {
      dtos
    }
  }
}

#' Create a DataSHIELD profile
#'
#' The created DataSHIELD profile will not be enabled and no access restrictions is applied.
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param name Name of the profile.
#' @param cluster Name of the R servers cluster to which the profile will be attached to. Default value is 'default'.
#' @param rParser Version of the DataSHIELD R parser that applies to this profile. If not
#' specified, the system's default one will be used. A valid version would be one of 'v1' or 'v2'.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_create(o, name = 'survival', cluster = 'demo')
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_create <- function(opal, name, cluster = "default", rParser = NULL) {
  if (opal.version_compare(opal,"4.2")<0) {
    stop("DataSHIELD profiles require Opal 4.2 or higher.")
  }
  profile <- list(name = name, cluster = cluster, enabled = FALSE, restrictedAccess = FALSE)
  if (!.is.empty(rParser)) {
    profile$rParser <- rParser
  }
  ignore <- opal.post(opal, "datashield", "profiles", contentType = "application/json", body = jsonlite::toJSON(profile, auto_unbox = TRUE))
}

#' Initialize a DataSHIELD profile
#'
#' Clean the DataSHIELD's profile settings from all methods and options (including custom ones). These settings
#' are then repopulated with installed DataSHIELD R packages settings, optionaly filtered by the name.
#' See also \link{dsadmin.publish_package}, \link{dsadmin.set_package_methods} or \link{dsadmin.set_option}.
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param name Name of the profile.
#' @param packages A list DataSHIELD R package names
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_create(o, name = 'survival', cluster = 'demo')
#' dsadmin.profile_init(o, name = 'survival', packages = c('dsSurvival'))
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_init <- function(opal, name, packages = NULL) {
  if (opal.version_compare(opal,"4.2")<0) {
    stop("DataSHIELD profiles require Opal 4.2 or higher.")
  }
  dsadmin.rm_methods(opal, profile = name)
  dsadmin.rm_options(opal, profile = name)
  ignore <- lapply(dsadmin.package_descriptions(opal, profile = name)$Package, function(p) {
    if (is.null(packages) || length(packages)==0 || p %in% packages)
      dsadmin.publish_package(opal, p, profile = name)
  })
}

#' Get a DataSHIELD profile
#'
#' Note that getting a specific DataSHIELD profile details is not allowed for regular DataSHIELD
#' users when the profile has no restricted access. This function is for profiles
#' editors only (system administrators or DataSHIELD administrators).
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param name Name of the profile.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile(o, name = 'default')
#' opal.logout(o)
#' }
#' @export
dsadmin.profile <- function(opal, name) {
  opal.get(opal, "datashield", "profile", name)
}

#' Check a DataSHIELD profile exists
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param name Name of the profile.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' if (!dsadmin.profile_exists(o, name = 'survival'))
#'   dsadmin.profile_create(o, name = 'survival', cluster = 'demo')
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_exists <- function(opal, name) {
  name %in% dsadmin.profiles(opal)$name
}

#' Delete a DataSHIELD profile
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param name Name of the profile.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_create(o, name = 'survival', cluster = 'demo')
#' dsadmin.profile_delete(o, name = 'survival')
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_delete <- function(opal, name) {
  if (opal.version_compare(opal,"4.2")<0) {
    stop("DataSHIELD profiles require Opal 4.2 or higher.")
  }
  ignore <- opal.delete(opal, "datashield", "profile", name)
}

#' Enable or disable a DataSHIELD profile
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param name Name of the profile.
#' @param enabled Default value is TRUE.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_create(o, name = 'survival', cluster = 'demo')
#' dsadmin.profile_enable(o, name = 'survival', enabled = TRUE)
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_enable <- function(opal, name, enabled = TRUE) {
  if (opal.version_compare(opal,"4.2")<0) {
    stop("DataSHIELD profiles require Opal 4.2 or higher.")
  }
  if (enabled)
    ignore <- opal.put(opal, "datashield", "profile", name, "_enable")
  else
    ignore <- opal.delete(opal, "datashield", "profile", name, "_enable")
}

#' Restrict or open access to a DataSHIELD profile
#'
#' When access is restricted, only users (or group of users) with this profile use permissions
#' will be allowed to use this profile. When access is not restricted, all
#' DataSHIELD users are allowed to use this profile. See also \link{dsadmin.profile_perm}.
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param name Name of the profile.
#' @param restricted Default value is TRUE.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_create(o, name = 'survival', cluster = 'demo')
#' dsadmin.profile_access(o, name = 'survival', restricted = TRUE)
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_access <- function(opal, name, restricted = TRUE) {
  if (opal.version_compare(opal,"4.2")<0) {
    stop("DataSHIELD profiles require Opal 4.2 or higher.")
  }
  if (restricted)
    ignore <- opal.put(opal, "datashield", "profile", name, "_access")
  else
    ignore <- opal.delete(opal, "datashield", "profile", name, "_access")
}

#' Set or remove the R parser version of a DataSHIELD profile
#'
#' @family DataSHIELD profiles
#' @param opal Opal object.
#' @param name Name of the profile.
#' @param rParser Version of the DataSHIELD R parser that applies to this profile. If not
#' specified, the system's default one will be used. A valid version would be one of 'v1' or 'v2'.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_create(o, name = 'survival', cluster = 'demo')
#' # apply R parser version v2
#' dsadmin.profile_rparser(o, name = 'survival', rParser = 'v2')
#' # apply system's default R parser version
#' dsadmin.profile_rparser(o, name = 'survival')
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_rparser <- function(opal, name, rParser = NULL) {
  if (opal.version_compare(opal,"4.2")<0) {
    stop("DataSHIELD profiles require Opal 4.2 or higher.")
  }
  if (.is.empty(rParser))
    ignore <- opal.delete(opal, "datashield", "profile", name, "_rparser")
  else
    ignore <- opal.put(opal, "datashield", "profile", name, "_rparser", query = list(version = rParser))
}

#' Add or update a permission on a DataSHIELD profile
#'
#' When adding/updating some permissions on a DataSHIELD profile, this profile
#' is automatically set in restricted access mode.
#'
#' @family DataSHIELD profiles
#' @param opal Opal connection object.
#' @param name Profile.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: use.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_perm_add(o, 'default', c('andrei', 'valentina'), 'user', 'use')
#' dsadmin.profile_perm(o, 'default')
#' dsadmin.profile_perm_delete(o, 'default', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_perm_add <- function(opal, name, subject, type = "user", permission = "use") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('use' = 'DATASHIELD_PROFILE_USE')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid DataSHIELD profile permission name: ", permission)
  }
  dsadmin.profile_perm_delete(opal, name, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "datashield", "profile", name, "permissions", query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the permissions of a DataSHIELD profile
#'
#' @family DataSHIELD profiles
#' @param opal Opal connection object.
#' @param name Profile name.
#'
#' @return A data.frame with columns: subject, type, permission
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_perm_add(o, 'default', c('andrei', 'valentina'), 'user', 'use')
#' dsadmin.profile_perm(o, 'default')
#' dsadmin.profile_perm_delete(o, 'default', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_perm <- function(opal, name) {
  perms <- list('DATASHIELD_PROFILE_USE' = 'use')
  acls <- opal.get(opal, "datashield", "profile", name, "permissions")
  .aclsToDataFrame(perms, acls)
}

#' Delete a permission from a DataSHIELD profile
#'
#' Delete a permission that was applied on a DataSHIELD profile. Silently returns when there is no such permission.
#'
#' @family DataSHIELD profiles
#' @param opal Opal connection object.
#' @param name Profile name.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.profile_perm_add(o, 'default', c('andrei', 'valentina'), 'user', 'use')
#' dsadmin.profile_perm(o, 'default')
#' dsadmin.profile_perm_delete(o, 'default', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
dsadmin.profile_perm_delete <- function(opal, name, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "datashield", "profile", name, "permissions", query = list(principal = subject[i], type = toupper(type)))
  }
}

#' Get DataSHIELD package descriptions
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param fields A character vector giving the fields to extract from each package's
#' DESCRIPTION file in addition to the default ones, or NULL (default). Unavailable fields result in NA values.
#' @param df Return a data.frame (default is TRUE)
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @return The DataSHIELD package descriptions as a data.frame or a list
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.package_descriptions(o)
#' opal.logout(o)
#' }
#' @export
dsadmin.package_descriptions <- function(opal, fields=NULL, df=TRUE, profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.package_descriptions(o, fields=fields, df=df, profile=profile)})
  } else {
    query <- list()
    if (!is.null(fields) && length(fields)) {
      query <- append(query,list(fields=paste(fields, collapse=',')))
    }
    query$profile <- .toSafeProfile(opal, profile)
    dtos <- opal.get(opal, "datashield", "packages", query=query)
    packageList <- c()
    for (dto in dtos) {
      packageDescription <- list()
      for (desc in dto$description) {
        packageDescription[[desc$key]] <- desc$value
      }
      packageList[[dto$name]] <- packageDescription
    }
    if (df) {
      n <- length(packageList)
      package <- rep(NA, n)
      libPath <- rep(NA, n)
      version <- rep(NA, n)
      depends <- rep(NA, n)
      license <- rep(NA, n)
      built <- rep(NA, n)
      title <- rep(NA, n)
      description <- rep(NA, n)
      author <- rep(NA, n)
      maintainer <- rep(NA, n)
      aggregateMethods <- rep(NA, n)
      assignMethods <- rep(NA, n)
      options <- rep(NA, n)
      i <- 1
      for (name in names(packageList)) {
        package[i] <- packageList[[name]]$Package
        libPath[i] <- packageList[[name]]$LibPath
        version[i] <- packageList[[name]]$Version
        depends[i] <- .nullToNA(packageList[[name]]$Depends)
        license[i] <- packageList[[name]]$License
        built[i] <- packageList[[name]]$Built
        title[i] <- .nullToNA(packageList[[name]]$Title)
        description[i] <- .nullToNA(packageList[[name]]$Description)
        author[i] <- .nullToNA(packageList[[name]]$Author)
        maintainer[i] <-  .nullToNA(packageList[[name]]$Maintainer)
        aggregateMethods[i] <- .nullToNA(packageList[[name]]$AggregateMethods)
        assignMethods[i] <- .nullToNA(packageList[[name]]$AssignMethods)
        options[i] <- .nullToNA(packageList[[name]]$Options)
        i <- i + 1
      }
      data.frame(Package=package, LibPath=libPath, Version=version, Depends=depends, License=license, Built=built,
                 Title=title, Description=description, Author=author, Maintainer=maintainer,
                 AggregateMethods=aggregateMethods, AssignMethods=assignMethods, Options=options, stringsAsFactors = FALSE)
    } else {
      packageList
    }
  }
}

#' Get DataSHIELD package description
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param fields A character vector giving the fields to extract from each package's
#' DESCRIPTION file in addition to the default ones, or NULL (default). Unavailable fields result in NA values.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.package_description(o, 'dsBase')
#' opal.logout(o)
#' }
#' @export
dsadmin.package_description <- function(opal, pkg, fields=NULL, profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.package_description(o, pkg, fields=fields, profile=profile)})
  } else {
    query <- NULL
    if (!is.null(fields) && length(fields) > 0) {
      query <- list(fields=paste(fields, collapse=','))
    }
    query$profile <- .toSafeProfile(opal, profile)
    dtos <- opal.get(opal, "datashield", "package", pkg, query=query)
    dto <- dtos
    if (is.list(dtos))
      dto <- dtos[[1]]
    packageDescription <- list()
    for (desc in dto$description) {
      packageDescription[[desc$key]] <- desc$value
    }
    packageDescription
  }
}

#' Install a DataSHIELD package
#'
#' Install a package from DataSHIELD public package repository or (if Git reference and GitHub username is provided) from DataSHIELD source repository on GitHub.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param githubusername GitHub username of git repository. If NULL (default), try to install from DataSHIELD package repository.
#' @param ref Desired git reference (could be a commit, tag, or branch name). If NULL (default), try to install from DataSHIELD package repository.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @return TRUE if installed
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.install_package(o, 'dsBase')
#' opal.logout(o)
#' }
#' @export
dsadmin.install_package <- function(opal, pkg, githubusername=NULL, ref=NULL, profile=NULL) {
  if (!is.null(githubusername) && !is.null(ref)) {
    dsadmin.install_github_package(opal, pkg, username=githubusername, ref=ref, profile=profile)
  } else {
    if(is.list(opal)){
      lapply(opal, function(o){dsadmin.install_package(o, pkg, githubusername=githubusername, ref=ref, profile=profile)})
    } else {
      query <- list(name=pkg, profile=.toSafeProfile(opal, profile))
      opal.post(opal, "datashield", "packages", query=query)
      dsadmin.installed_package(opal, pkg)
    }
  }
}

#' Install a DataSHIELD package from GitHub
#'
#' Install a package from a DataSHIELD source repository on GitHub.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param username GitHub username/organization of the git repository. Default is 'datashield'.
#' @param ref Desired git reference (could be a commit, tag, or branch name). Default is 'master'.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @return TRUE if installed
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.install_github_package(o, 'dsOmics', username='isglobal-brge')
#' opal.logout(o)
#' }
#' @export
dsadmin.install_github_package <- function(opal, pkg, username='datashield', ref='master', profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.install_github_package(o, pkg, username=username, ref=ref, profile=profile)})
  } else {
    query <- list(name=paste(username,pkg,sep="/"), ref=ref, manager = "github", profile=.toSafeProfile(opal, profile))
    opal.post(opal, "datashield", "packages", query=query)
    dsadmin.installed_package(opal, pkg)
  }
}

#' Install a DataSHIELD package from a local archive file
#'
#' Install a package from a package archive file, resulting from the build of a server-side DataSHIELD package.
#' This will upload the archive file and run its installation in the R server.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param path Path to the package archive, ending with .
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # install a pre-built local archive file
#' dsadmin.install_local_package(o, '~/dsExposome_1.0.0.tar.gz')
#' # or build archive file from local package source (in current working folder)
#' dsadmin.install_local_package(o, devtools::build())
#' opal.logout(o)
#' }
#' @export
dsadmin.install_local_package <- function(opal, path, profile=profile) {
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

  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.install_local_package(o, path, profile=profile)})
  } else {

    tmp <- opal.file_mkdir_tmp(opal)
    opal.file_upload(opal, path, tmp)

    if (opal.version_compare(opal, "4.2")<0) {
      opal.file_write(opal, paste0(tmp, filename))
      opal.execute(opal, paste0("install.packages('", filename, "', repos = NULL, type ='source')"))
    } else {
      opal.post(opal, "datashield", "packages", query = list(name = paste0(tmp, filename), manager = "local", profile = .toSafeProfile(opal, profile)))
    }

    opal.file_rm(opal, tmp)
  }
}

#' Remove DataSHIELD package
#'
#' Remove a DataSHIELD package permanently.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.remove_package(o, 'dsBase')
#' opal.logout(o)
#' }
#' @export
dsadmin.remove_package <- function(opal, pkg, profile=profile) {
  if(is.list(opal)){
    resp <- lapply(opal, function(o){dsadmin.remove_package(o, pkg, profile=profile)})
  } else {
    resp <- opal.delete(opal, "datashield", "package", pkg, query = list(profile=.toSafeProfile(opal, profile)), callback=function(o,r){})
  }
}

#' Check DataSHIELD package
#'
#' Check if a DataSHIELD package is installed.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @return TRUE if installed
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.installed_package(o, 'dsBase')
#' opal.logout(o)
#' }
#' @export
dsadmin.installed_package <- function(opal, pkg, profile=NULL) {
  if(is.list(opal)){
    resp <- lapply(opal, function(o){dsadmin.installed_package(o, pkg, profile = profile)})
  } else {
    opal.get(opal, "datashield", "package", pkg, query=list(profile=.toSafeProfile(opal, profile)), callback=function(o,r){
      code <- status_code(r)
      if(code == 404) {
        FALSE
      } else if (code >= 400) {
        NULL
      } else {
        TRUE
      }
    })
  }
}

#' Set DataSHIELD method
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param name Name of the method, as it will be accessed by DataSHIELD users.
#' @param func Function name or function code.
#' @param path Path to the R file containing the script (mutually exclusive with func).
#' @param type Type of the method: "aggregate" (default) or "assign"
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # as a package's function
#' dsadmin.set_method(o, 'foo', func = 'base::mean')
#' # as a custom function
#' dsadmin.set_method(o, 'foo', func = function(x) { base::mean(x) })
#' opal.logout(o)
#' }
#' @export
dsadmin.set_method <- function(opal, name, func=NULL, path=NULL, type="aggregate", profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.set_method(o, name, func=func, path=path, type=type, profile=profile)})
  } else {
    # build method dto
    if(is.null(func)) {
      # read script from file
      rscript <- paste(readLines(path),collapse="\\n")
      rscript <- gsub('\"','\\\\"', rscript)
      methodDto <- jsonlite::toJSON(list(
        name = name,
        "DataShield.RScriptDataShieldMethodDto.method" = list(
          script = rscript
        )
      ), auto_unbox = TRUE)
    } else if (is.function(func)) {
      rscript <- paste0(deparse(func), collapse = "\n")
      methodDto <- jsonlite::toJSON(list(
        name = name,
        "DataShield.RScriptDataShieldMethodDto.method" = list(
          script = rscript
        )
      ), auto_unbox = TRUE)
    } else {
      methodDto <- paste('{"name":"', name, '","DataShield.RFunctionDataShieldMethodDto.method":{"func":"', func, '"}}', sep='')
    }
    dsadmin.rm_method(opal, name, type=type, profile=profile)
    ignore <- opal.post(opal, "datashield", "env", type, "methods", query=list(profile=.toSafeProfile(opal, profile)), body=methodDto, contentType="application/json");
  }
}

#' Remove DataSHIELD method
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param name Name of the method, as it is accessed by DataSHIELD users.
#' @param type Type of the method: "aggregate" (default) or "assign"
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.rm_method(o, 'foo')
#' opal.logout(o)
#' }
#' @export
dsadmin.rm_method <- function(opal, name, type="aggregate", profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.rm_method(o, name, type=type, profile=profile)})
  } else {
    # ignore errors and returned value
    resp <- opal.delete(opal, "datashield", "env", type, "method", name, query=list(profile=.toSafeProfile(opal, profile)), callback=function(o,r){})
  }
}

#' Remove DataSHIELD methods.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param type Type of the method: "aggregate" or "assign". Default is NULL (=all type of methods).
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.rm_methods(o)
#' opal.logout(o)
#' }
#' @export
dsadmin.rm_methods <- function(opal, type=NULL, profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.rm_methods(o, type=type, profile=profile)})
  } else {
    # ignore errors and returned value
    if (is.null(type)) {
      dsadmin.rm_methods(opal, type="aggregate", profile=profile)
      dsadmin.rm_methods(opal, type="assign", profile=profile)
    } else {
      resp <- opal.delete(opal, "datashield", "env", type, "methods", query=list(profile=profile), callback=function(o,r){})
    }
  }
}

#' Get a DataSHIELD method
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param name Name of the method, as it is accessed by DataSHIELD users.
#' @param type Type of the method: "aggregate" (default) or "assign"
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.get_method(o, 'class')
#' opal.logout(o)
#' }
#' @export
dsadmin.get_method <- function(opal, name, type="aggregate", profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.get_method(o, name, type=type, profile=profile)})
  } else {
    m <- opal.get(opal, "datashield", "env", type, "method", name, query=list(profile=.toSafeProfile(opal, profile)))
    class <- "function"
    value <- m$DataShield.RFunctionDataShieldMethodDto.method$func
    pkg <- NA
    version <- NA
    if (is.null(value)) {
      class <- "script"
      value <- m$DataShield.RScriptDataShieldMethodDto.method$script
    } else {
      pkg <- m$DataShield.RFunctionDataShieldMethodDto.method$rPackage
      if (is.null(pkg)) {
        pkg <- NA
      }
      version <- m$DataShield.RFunctionDataShieldMethodDto.method$version
      if (is.null(version)) {
        version <- NA
      }
    }
    list(name=m$name, type=type, class=class, value=value, package=pkg, version=version)
  }
}

#' Get DataSHIELD methods
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param type Type of the method: "aggregate" (default) or "assign"
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.get_methods(o)
#' opal.logout(o)
#' }
#' @export
dsadmin.get_methods <- function(opal, type="aggregate", profile=NULL) {
  rlist <- opal.get(opal, "datashield", "env", type, "methods", query=list(profile=.toSafeProfile(opal, profile)))
  name <- lapply(rlist,function(m){
    m$name
  })
  t <- lapply(rlist,function(m){
    type
  })
  class <- lapply(rlist,function(m){
    if (is.null(m$DataShield.RFunctionDataShieldMethodDto.method$func)) {
      "script"
    } else {
      "function"
    }
  })
  value <- lapply(rlist,function(m){
    val <- m$DataShield.RFunctionDataShieldMethodDto.method$func
    if (is.null(val)) {
      val <- m$DataShield.RScriptDataShieldMethodDto.method$script
    }
    val
  })
  pkg <- lapply(rlist,function(m){
    val <- m$DataShield.RFunctionDataShieldMethodDto.method$rPackage
    if (is.null(val)) {
      val <- NA
    }
    val
  })
  version <- lapply(rlist,function(m){
    val <- m$DataShield.RFunctionDataShieldMethodDto.method$version
    if (is.null(val)) {
      val <- NA
    }
    val
  })
  rval <- data.frame(unlist(name), unlist(t), unlist(class), unlist(value), unlist(pkg), unlist(version), stringsAsFactors = FALSE)
  if (nrow(rval)>0) {
    colnames(rval) <- c("name","type", "class", "value","package","version")
  }
  rval
}

#' Publish DataSHIELD package settings
#'
#' Declare DataSHIELD aggregate/assign methods and options as defined by the package.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @return TRUE if successfull
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.publish_package(o, 'dsBase')
#' opal.logout(o)
#' }
#' @export
dsadmin.publish_package <- function(opal, pkg, profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.publish_package(o, pkg, profile)})
  } else {
    if (dsadmin.installed_package(opal, pkg, profile = profile)) {
      # put methods
      opal.put(opal, "datashield", "package", pkg, "_publish", query=list(profile=.toSafeProfile(opal, profile)))
      TRUE
    } else {
      FALSE
    }
  }
}

#' Unpublish DataSHIELD package settings
#'
#' Remove DataSHIELD aggregate/assign methods and options as defined by the package from the DataSHIELD configuration.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @return TRUE if successfull
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.unpublish_package(o, 'dsBase')
#' opal.logout(o)
#' }
#' @export
dsadmin.unpublish_package <- function(opal, pkg, profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.unpublish_package(o, pkg, profile)})
  } else {
    if (dsadmin.installed_package(opal, pkg, profile = profile)) {
      # put methods
      opal.delete(opal, "datashield", "package", pkg, "_publish", query=list(profile=.toSafeProfile(opal, profile)))
      TRUE
    } else {
      FALSE
    }
  }
}

#' Set DataSHIELD package methods
#'
#' Declare DataSHIELD aggregate and assign methods as defined by the package.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param type Type of the method: "aggregate" or "assign". Default is NULL (=all type of methods).
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @return TRUE if successfull
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.set_package_methods(o, 'dsBase')
#' opal.logout(o)
#' }
#' @export
dsadmin.set_package_methods <- function(opal, pkg, type=NULL, profile=NULL) {
  if(is.list(opal)){
    lapply(opal, function(o){dsadmin.set_package_methods(o, pkg, type, profile)})
  } else {
    if (dsadmin.installed_package(opal, pkg, profile = profile)) {
      # put methods
      methods <- opal.put(opal, "datashield", "package", pkg, "methods", query=list(profile=.toSafeProfile(opal, profile)))
      TRUE
    } else {
      FALSE
    }
  }
}

#' Remove DataSHIELD package methods
#'
#' Remove DataSHIELD aggregate and assign methods defined by the package.
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param pkg Package name.
#' @param type Type of the method: "aggregate" or "assign". Default is NULL (=all type of methods).
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.rm_package_methods(o, 'dsBase')
#' opal.logout(o)
#' }
#' @export
dsadmin.rm_package_methods <- function(opal, pkg, type=NULL, profile=NULL) {
  if(is.list(opal)) {
    lapply(opal, function(o){dsadmin.rm_package_methods(o, pkg, type, profile)})
  } else {
    # get methods
    do_rm_methods <- function(mtype) {
      ms <- dsadmin.get_methods(opal, type = mtype, profile = profile)
      names <- subset(ms, ms$package == pkg)$name
      rval <- lapply(names, function(n) {
        dsadmin.rm_method(opal, n, type = mtype, profile = profile)
      })
    }

    if (is.null(type) || type == "aggregate") {
      do_rm_methods("aggregate")
    }
    if (is.null(type) || type == "assign") {
      do_rm_methods("assign")
    }
  }
}

#' Get the DataSHIELD options
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.get_options(o)
#' opal.logout(o)
#' }
#' @export
dsadmin.get_options <- function (opal, profile=NULL) {
  if(is.list(opal)) {
    lapply(opal, function(o){dsadmin.get_options(o, profile = profile)})
  } else {
    # get options
    options <- opal.get(opal, "datashield", "options", query=list(profile=.toSafeProfile(opal, profile)))
    names <- lapply(options, function(opt) {
      opt$name
    })
    values <- lapply(options, function(opt) {
      opt$value
    })
    data.frame(name=unlist(names), value=unlist(values), stringsAsFactors = FALSE)
  }
}

#' Set DataSHIELD option
#'
#' Set a DataSHIELD option (add or update).
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param name Name of the option
#' @param value Value of the option
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.set_option(o, 'foo', 'bar')
#' opal.logout(o)
#' }
#' @export
dsadmin.set_option <- function (opal, name, value, profile=NULL) {
  if(is.list(opal)) {
    lapply(opal, function(o){dsadmin.set_option(o, name, value, profile)})
  } else {
    # set option
    payload <- jsonlite::toJSON(list(
      name = name,
      value = value
    ), auto_unbox = TRUE)
    ignore <- opal.post(opal, "datashield", "option", query=list(profile=.toSafeProfile(opal, profile)), body=payload, contentType="application/json")
  }
}

#' Remove a DataSHIELD option
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param name Name of the option
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.rm_option(o, 'foo')
#' opal.logout(o)
#' }
#' @export
dsadmin.rm_option <- function (opal, name, profile=NULL) {
  if(is.list(opal)) {
    lapply(opal, function(o){dsadmin.rm_option(o, name, profile)})
  } else {
    # rm option
    ignore <- opal.delete(opal, "datashield", "option", query=list(name=name, profile=.toSafeProfile(opal, profile)))
  }
}

#' Remove all DataSHIELD options
#'
#' @family DataSHIELD functions
#' @param opal Opal object or list of opal objects.
#' @param profile The DataSHIELD profile name to which operation applies. See also \link{dsadmin.profiles}.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.rm_options(o, 'foo')
#' opal.logout(o)
#' }
#' @export
dsadmin.rm_options <- function (opal, profile=NULL) {
  if(is.list(opal)) {
    lapply(opal, function(o){dsadmin.rm_options(o, profile)})
  } else {
    opts <- dsadmin.get_options(opal, profile=profile)
    if (nrow(opts)>0) {
      ignore <- lapply(opts$name, function(n) {
        opal.delete(opal, "datashield", "options", query=list(name=n, profile=.toSafeProfile(opal, profile)))
      })
    }
  }
}

#' Add or update a DataSHIELD permission
#'
#' Add or update a permission on the DataSHIELD service.
#'
#' @param opal Opal connection object.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: use or administrate.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' dsadmin.perm(o)
#' dsadmin.perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
dsadmin.perm_add <- function(opal, subject, type = "user", permission) {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('use' = 'DATASHIELD_USE', 'administrate' = 'DATASHIELD_ALL')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid DataSHIELD permission name: ", permission)
  }
  dsadmin.perm_delete(opal, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "system", "permissions", "datashield", query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the DataSHIELD permissions
#'
#' Get the permissions that were applied to the DataSHIELD service.
#'
#' @param opal Opal connection object.
#'
#' @return A data.frame with columns: subject, type, permission
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' dsadmin.perm(o)
#' dsadmin.perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
dsadmin.perm <- function(opal) {
  perms <- list('DATASHIELD_USE' = 'use', 'DATASHIELD_ALL' = 'administrate')
  acls <- opal.get(opal, "system", "permissions", "datashield")
  .aclsToDataFrame(perms, acls)
}

#' Delete a DataSHIELD permission
#'
#' Delete a permission that was applied to the DataSHIELD service. Silently returns when there is no such permission.
#'
#' @param opal Opal connection object.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.perm_add(o, c('andrei', 'valentina'), 'user', 'use')
#' dsadmin.perm(o)
#' dsadmin.perm_delete(o, c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
dsadmin.perm_delete <- function(opal, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "system", "permissions", "datashield", query = list(principal = subject[i], type = toupper(type)))
  }
}

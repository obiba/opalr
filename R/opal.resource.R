#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the resource references of a project
#' 
#' @family resource functions
#' @param opal Opal object.
#' @param project Name of the project.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resources(o, 'RSRC')
#' opal.logout(o)
#' }
#' @export
opal.resources <- function(opal, project, df=TRUE) {
  if (!is.na(opal$version) && opal.version_compare(opal,"3.0")<0) {
    stop("Resources are not available in opal ", opal$version, " (3.0.0 or higher is required)")
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

#' Get a resource reference of a project
#' 
#' @family resource functions
#' @param opal Opal object.
#' @param project Name of the project.
#' @param resource Name of the resource in the project.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resource(o, 'RSRC', 'CNSIM1')
#' opal.logout(o)
#' }
#' @export
opal.resource <- function(opal, project, resource) {
  if (!is.na(opal$version) && opal.version_compare(opal,"3.0")<0) {
    stop("Resources are not available in opal ", opal$version, " (3.0.0 or higher is required)")
  }
  opal.get(opal, "project", project, "resource", resource);
}

#' Get the resource object of a project
#' 
#' @family resource functions
#' @param opal Opal object.
#' @param project Name of the project.
#' @param resource Name of the resource in the project.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' res <- opal.resource_get(o, 'RSRC', 'CNSIM1')
#' # then interpret locally the resource object (load the appropriate R packages)
#' library(resourcer)
#' # coerce to a data.frame
#' as.data.frame(res)
#' # or get the resource client object for low-level interactions
#' rescli <- resourcer::newResourceClient(res)
#' opal.logout(o)
#' }
#' @export
opal.resource_get <- function(opal, project, resource) {
  if (!is.na(opal$version) && opal.version_compare(opal,"3.0")<0) {
    stop("Resources are not available in opal ", opal$version, " (3.0.0 or higher is required)")
  }
  resref <- opal.resource(opal, project, resource)
  opal.assign.resource(opal, resource, paste0(project, ".", resource))
  res <- opal.execute(opal, paste0(resource, "$getResource()"))
  opal.symbol_rm(opal, resource)
  res
}

#' Check a resource reference exists
#' 
#' Check whether a resource already exists in the project (and is visible by the requesting user).
#' 
#' @family resource functions
#' @param opal Opal object.
#' @param project Name of the project.
#' @param resource Name of the resource in the project.
#' @return A logical
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resource_exists(o, 'RSRC', 'CNSIM1')
#' opal.logout(o)
#' }
#' @export
opal.resource_exists <- function(opal, project, resource) {
  res <- tryCatch(opal.resource(opal, project, resource), 
                  error = function(cond) { NULL })
  !is.null(res)
}

#' Create a resource reference in a project
#' 
#' @family resource functions
#' @param opal Opal object.
#' @param project Name of the project.
#' @param name Name of the resource in the project.
#' @param url The URL of the resource.
#' @param description The description of the resource (optional).
#' @param format The format of the data described by the resource (optional).
#' @param package The R package to be loaded prior to the assignment of the resource (optional).
#' @param identity The identity key or username to be used when accessing the resource (optional).
#' @param secret The secret key or password to be used when accessing the resource (optional).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resource_create(o, 'RSRC', 'CNSIM4', 
#'   url = 'opal+https://opal-demo.obiba.org/ws/files/projects/RSRC/CNSIM3.zip', 
#'   format = 'csv', secret = 'EeTtQGIob6haio5bx6FUfVvIGkeZJfGq')
#' opal.logout(o)
#' }
#' @export
opal.resource_create <- function(opal, project, name, url, description=NULL, format=NULL, package=NULL, identity=NULL, secret=NULL) {
  parameters <- list(url = url)
  parameters$format <- format
  parameters$`_package` <- package
  credentials <- list(identity = identity, secret = secret)
  credentials$identifier <- identity
  credentials$secret <- secret
  if (length(credentials)==0) {
    credentials <- NULL
  }
  opal.resource_extension_create(opal, project, name, "resourcer", "default", description = description, parameters = parameters, credentials = credentials)
}

#' Create an extended resource reference in a project
#' 
#' @family resource functions
#' @param opal Opal object.
#' @param project Name of the project.
#' @param name Name of the resource in the project.
#' @param provider Name of the R package in which the resource is defined.
#' @param factory Name of the JS function that turns parameters and credentials into a resource object.
#' @param parameters A named list of the resource parameters.
#' @param description The description of the resource (optional).
#' @param credentials A named list of the resource credentials (optional).
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resource_extension_create(o, 'RSRC', 'ga4gh_1000g',
#'   provider = 'dsOmics', factory = 'ga4gh-htsget',
#'   parameters = list(
#'     host = 'https://htsget.ga4gh.org',
#'     sample = '1000genomes.phase1.chr1',
#'     reference = '1',
#'     start = '1',
#'     end = '100000',
#'     format = 'GA4GHVCF'
#'   )
#' )
#' opal.logout(o)
#' }
#' @export
opal.resource_extension_create <- function(opal, project, name, provider, factory, parameters, description=NULL, credentials=NULL) {
  if (!is.na(opal$version) && opal.version_compare(opal,"3.0")<0) {
    stop("Resources are not available in opal ", opal$version, " (3.0.0 or higher is required)")
  }
  if (!opal.resource_exists(opal, project, name)) {
    resjson <- list(
      provider = provider,
      factory = factory,
      project = project,
      name = name
    )
    resjson$description <- description
    resjson$parameters <- jsonlite::toJSON(parameters, auto_unbox = TRUE)
    if (!is.null(credentials)) {
      resjson$credentials <- jsonlite::toJSON(credentials, auto_unbox = TRUE)
    }
    body <- jsonlite::toJSON(resjson, auto_unbox = TRUE)
    ignore <- opal.post(opal, "project", project, "resources", contentType = "application/json", body = body)
  } else {
    warning("Resource ", name, " in project " , project, " already exists.")
  }
}

#' Delete a resource reference
#'
#' Removes the reference to a resource. The targeted resource remains untouched.
#'
#' @family resource functions
#' @param opal Opal connection object.
#' @param project Project name where the resource is located.
#' @param resource Resource name to be deleted.
#' @param silent Warn if resource does not exist, default is TRUE.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resource_delete(o, "RSRC", "CNSIM4")
#' opal.logout(o)
#' }
#' @export
opal.resource_delete <- function(opal, project, resource, silent = TRUE) {
  if (opal.resource_exists(opal, project, resource)) {
    ignore <- opal.delete(opal, "project", project, "resource", resource)  
  } else if (!silent) {
    warning("Resource '", resource,"' does not exist in project '", project, "'")
  }
}

#' Add or update a permission on a resource
#' 
#' Add or update a permission on a resource
#' 
#' @family resource functions
#' @param opal Opal connection object.
#' @param project The project name.
#' @param resource The resource name.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: view, administrate. The 'view' permission
#' is suitable for DataSHIELD operations. 
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resource_perm_add(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user', 'view')
#' opal.resource_perm(o, 'CNSIM', 'CNSIM1')
#' opal.resource_perm_delete(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.resource_perm_add <- function(opal, project, resource, subject, type = "user", permission) {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('view' = 'RESOURCE_VIEW',
                'administrate' = 'RESOURCE_ALL')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid resource permission name: ", permission)
  }
  opal.resource_perm_delete(opal, project, resource, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "project", project, "permissions", "resource", resource, query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the permissions on a resource
#' 
#' Get the permissions that were applied on a resource.
#' 
#' @family resource functions
#' @param opal Opal connection object.
#' @param project The project name.
#' @param resource The resource name.
#' 
#' @return A data.frame with columns: subject, type, permission
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resource_perm_add(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user', 'view')
#' opal.resource_perm(o, 'CNSIM', 'CNSIM1')
#' opal.resource_perm_delete(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.resource_perm <- function(opal, project, resource) {
  perms <- list('RESOURCE_VIEW' = 'view',
                'RESOURCE_ALL' = 'administrate')
  acls <- opal.get(opal, "project", project, "permissions", "resource", resource)
  .aclsToDataFrame(perms, acls)
}

#' Delete a permission from a resource
#' 
#' Delete a permission that was applied on a resource. Silently returns when there is no such permission.
#' 
#' @family resource functions
#' @param opal Opal connection object.
#' @param project The project name.
#' @param resource The resource name.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resource_perm_add(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user', 'view')
#' opal.resource_perm(o, 'CNSIM', 'CNSIM1')
#' opal.resource_perm_delete(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.resource_perm_delete <- function(opal, project, resource, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "project", project, "permissions", "resource", resource, query = list(principal = subject[i], type = toupper(type)))
  }
}

#' Add or update a permission on any resource
#' 
#' Add or update a global permission on the project's resources
#' 
#' @family resource functions
#' @param opal Opal connection object.
#' @param project The project name.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: view, administrate. The 'view' permission
#' is suitable for DataSHIELD operations. 
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resources_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'view')
#' opal.resources_perm(o, 'CNSIM')
#' opal.resources_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.resources_perm_add <- function(opal, project, subject, type = "user", permission) {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('view' = 'RESOURCES_VIEW',
                'administrate' = 'RESOURCES_ALL')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid resource permission name: ", permission)
  }
  opal.resources_perm_delete(opal, project, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "project", project, "permissions", "resources", query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the permissions on any resource
#' 
#' Get the permissions that were applied globally on the project's resources.
#' 
#' @family resource functions
#' @param opal Opal connection object.
#' @param project The project name.
#' 
#' @return A data.frame with columns: subject, type, permission
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resources_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'view')
#' opal.resources_perm(o, 'CNSIM')
#' opal.resources_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.resources_perm <- function(opal, project) {
  perms <- list('RESOURCES_VIEW' = 'view',
                'RESOURCES_ALL' = 'administrate')
  acls <- opal.get(opal, "project", project, "permissions", "resources")
  .aclsToDataFrame(perms, acls)
}

#' Delete a permission from any resource
#' 
#' Delete a permission that was applied globally on the project's resources. Silently returns when there is no such permission.
#' 
#' @family resource functions
#' @param opal Opal connection object.
#' @param project The project name.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.resources_perm_add(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user', 'view')
#' opal.resources_perm(o, 'CNSIM', 'CNSIM1')
#' opal.resources_perm_delete(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.resources_perm_delete <- function(opal, project, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "project", project, "permissions", "resources", query = list(principal = subject[i], type = toupper(type)))
  }
}

#' Create an Opal view over a resource reference
#'
#' Create an Opal view if a table with same name does not already exist. The resource 
#' reference is required.The dictionary of the created view will be discovered at 
#' initialization time. Use \link{opal.table_dictionary_update} to apply a dictionary.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the view will be located.
#' @param table View name to be created.
#' @param resource Fully qualified resource name.
#' @param type Entity type, default is "Participant".
#' @param idColumn Name of the column which contains the entity identifiers. 
#' If not specified, the first column will be used.
#' @param profile R server profile to use for establishing the connection with the 
#' resource. If not specifed, the profile will guessed based on the resource definition.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # make a view over a resource
#' opal.resource_view_create(o, "CNSIM", "CNSIM4", resource = "RSRC.CNSIM1")
#' opal.resource_view_create(o, "CNSIM", "FEMALE_2439", 
#'                           resource = "RSRC.FEMALE_2439", idColumn = "Name")
#' opal.logout(o)
#' }
#' @export
opal.resource_view_create <- function(opal, project, table, resource, type = "Participant", idColumn = NULL, profile = NULL) {
  if (!opal.table_exists(opal, project, table)) {
    params <- list(entityType = type)
    if (!is.null(idColumn)) {
      params$idColumn <- idColumn
    }
    if (!is.null(profile)) {
      params$profile <- profile
    }
    body <- jsonlite::toJSON(list(name = table, from = list(resource), "Magma.ResourceViewDto.view" = params), auto_unbox = TRUE)
    ignore <- opal.post(opal, "datasource", project, "views", contentType = "application/json", body = body)
  } else {
    stop("Table '", table,"' already exists in project '", project, "'.")
  }
}

#' Reconnect an Opal view to its underlying resource
#'
#' A view over a resource handles a connection to this resource. When the resource 
#' changes (data update, broken connection etc.), the connection to this resource
#' can be re-initialized.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the view is located.
#' @param table View name to be reconnected.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # make a view over a resource
#' opal.resource_view_create(o, "CNSIM", "CNSIM4", resource = "RSRC.CNSIM1")
#' # re-initialize the view's connection to the resource
#' opal.resource_view_reconnect(o, "CNSIM", "CNSIM4")
#' opal.logout(o)
#' }
#' @export
opal.resource_view_reconnect <- function(opal, project, table) {
  invisible(opal.put(opal, "datasource", project, "view", table, "_init"))
}


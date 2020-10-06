#-------------------------------------------------------------------------------
# Copyright (c) 2020 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get projects
#' 
#' @family project functions
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.projects(o)
#' opal.logout(o)
#' }
#' @export
opal.projects <- function(opal, df=TRUE) {
  res <- opal.get(opal, "projects", query=list(digest="true"))
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    title <- rep(NA, n)
    tags <- rep(NA, n)
    created <- rep(NA, n)
    lastUpdate <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      title[i] <- item$title
      if (!is.null(item$tags)) {
        tags[i] <- paste0(item$tags, collapse = "|")
      }
      created[i] <- item$timestamps$created
      lastUpdate[i] <- item$timestamps$lastUpdate
    }
    data.frame(name, title, tags, created, lastUpdate)
  } else {
    data.frame()
  }
}

#' Get a project
#' 
#' @family project functions
#' @param opal Opal object.
#' @param project Name of the project
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.project(o, 'datashield')
#' opal.logout(o)
#' }
#' @export
opal.project <- function(opal, project) {
  opal.get(opal, "project", project)
}

#' Create a project
#' 
#' @family project functions
#' @param opal Opal object.
#' @param project Name of the project
#' @param database The database name (as declared in Opal) to be used to store project's data. If not provided, the project
#' can have views and resources but no raw tables. 
#' @param title The title of the project (optional).
#' @param description The description of the project (optional).
#' @param tags A list of tag names (optional).
#' @param exportFolder The default location of the exported data files in the Opal file system (optional).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.project_create(o, 'test', database='opal_data', title='This is a test', tags=list('Test'))
#' opal.logout(o)
#' }
#' @export
opal.project_create <- function(opal, project, database = NULL, title = NULL, description = NULL, tags = NULL, exportFolder = NULL) {
  if (!opal.project_exists(opal, project)) {
    # {"name":"test","title":"This is the title","description":"This is the description","database":"opal_data","vcfStoreService":null,"exportFolder":"/home/administrator/export","tags":["DataSHIELD,","Resources"]}
    projson <- list(name = project)
    if (!is.null(database)) {
      projson$database <- database
    }
    if (!is.null(title)) {
      projson$title <- title
    } else {
      projson$title <- project
    }
    if (!is.null(description)) {
      projson$description <- description
    }
    if (!is.null(tags) && is.list(tags)) {
      projson$tags <- tags
    }
    if (!is.null(exportFolder)) {
      projson$exportFolder <- exportFolder
    }
    body <- jsonlite::toJSON(projson, auto_unbox = TRUE)
    ignore <- opal.post(opal, "projects", contentType = "application/json", body = body)
  } else {
    warning("Project ", project, " already exists.")
  }
}

#' Delete a project
#' 
#' Delete a project and every data what could have been associated to it.
#' 
#' @family project functions
#' @param opal Opal object.
#' @param project Name of the project
#' @param archive Logical that is TRUE if the complete removal of the project is requested.
#' @param silent Warn if project does not exist, default is TRUE.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.project_delete(o, 'test')
#' opal.logout(o)
#' }
#' @export
opal.project_delete <- function(opal, project, archive = FALSE, silent = TRUE) {
  if (opal.project_exists(opal, project)) {
    ignore <- opal.delete(opal, "project", project, query = list(archive = ifelse(archive, "true", "false")))  
  } else if (!silent) {
    warning("Project ", project, " does not exist or is not accessible.")
  }
}

#' Check a project exists
#' 
#' Check whether a project already exists (and is visible by the requesting user).
#' 
#' @family project functions
#' @param opal Opal object.
#' @param project Name of the project
#' @return A logical
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.project_exists(o, 'test')
#' opal.logout(o)
#' }
#' @export
opal.project_exists <- function(opal, project) {
  res <- tryCatch(opal.project(opal, project), 
                  error = function(cond) { NULL })
  !is.null(res)
}

#' Add or update a permission on a project
#' 
#' Add or update a permission on a project.
#' 
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: administrate.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.project_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'administrate')
#' opal.project_perm(o, 'CNSIM')
#' opal.project_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.project_perm_add <- function(opal, project, subject, type = "user", permission) {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('administrate' = 'PROJECT_ALL')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid project permission name: ", permission)
  }
  opal.project_perm_delete(opal, project, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "project", project, "permissions", "project", query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the permissions on a project
#' 
#' Get the permissions that were applied on a project.
#' 
#' @param opal Opal connection object.
#' @param project Project name.
#' 
#' @return A data.frame with columns: subject, type, permission
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.project_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'administrate')
#' opal.project_perm(o, 'CNSIM')
#' opal.project_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.project_perm <- function(opal, project) {
  perms <- list('PROJECT_ALL' = 'administrate')
  acls <- opal.get(opal, "project", project, "permissions", "project")
  .aclsToDataFrame(perms, acls)
}

#' Delete a permission from a project
#' 
#' Delete a permission that was applied on a project. Silently returns when there is no such permission.
#' 
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.project_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'administrate')
#' opal.project_perm(o, 'CNSIM')
#' opal.project_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.project_perm_delete <- function(opal, project, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "project", project, "permissions", "project", query = list(principal = subject[i], type = toupper(type)))  
  }
}
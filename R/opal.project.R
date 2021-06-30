#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
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

#' Get projects databases
#' 
#' When creating a project for storing data, it is required to name the database to be associated.
#' 
#' @family project functions
#' @param opal Opal object.
#' @return A character vector of databases names.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.projects_databases(o)
#' opal.logout(o)
#' }
#' @export
opal.projects_databases <- function(opal) {
  sapply(opal.get(opal, "system", "databases", query = list(usage = "storage")), function(db) db$name)
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
#' can have views and resources but no raw tables. If the the value is a logical and is TRUE, the default database will be 
#' selected or the first one if there is no default.
#' @param title The title of the project (optional).
#' @param description The description of the project (optional).
#' @param tags A list of tag names (optional).
#' @param exportFolder The default location of the exported data files in the Opal file system (optional).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # with named database
#' opal.project_create(o, 'test', database='opal_data', title='This is a test', tags=list('Test'))
#' # with default database
#' opal.project_create(o, 'test_default_db', database = TRUE)
#' # no database, for views and resources only
#' opal.project_create(o, 'test_no_db')
#' opal.logout(o)
#' }
#' @export
opal.project_create <- function(opal, project, database = NULL, title = NULL, description = NULL, tags = NULL, exportFolder = NULL) {
  if (!opal.project_exists(opal, project)) {
    # {"name":"test","title":"This is the title","description":"This is the description","database":"opal_data","vcfStoreService":null,"exportFolder":"/home/administrator/export","tags":["DataSHIELD,","Resources"]}
    projson <- list(name = project)
    if (!.is.empty(database)) {
      dbs <- opal.get(opal, "system", "databases", query = list(usage = "storage"))
      dbNames <- sapply(dbs, function(db) db$name)
      if (is.logical(database)) {
        if (database && length(dbNames)>0) {
          projson$database <- dbNames[1]
          # apply default db, if there is any
          lapply(dbs, function(db) {
            if (db$defaultStorage)
              projson$database <- db$name
          })
        }
      } else {
        if (!(database %in% dbNames)) {
          stop("Not a valid project database name: '", database, "'. Expecting one of: '", paste0(dbNames, collapse = "', '"), "'")
        }
        projson$database <- database
      }
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
    if (!.is.empty(database)) {
      pObj <- opal.project(opal, project)
      waited <- 0
      while(pObj$datasourceStatus != "READY") {
        # delay is proportional to the time waited, but no more than 10s
        delay <- min(10, max(1, round(waited/10)))
        Sys.sleep(delay)
        waited <- waited + delay
        pObj <- opal.project(opal, project)
      }
    }
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

#' Backup a project
#' 
#' The project backup task has a limited scope: tables (dictionary and data export), 
#' views (either as a logical table or as an exported table), resources, files and report 
#' templates. Other project elements that are not part of the backup: user and group 
#' permissions, view change history, table analysis, report executions etc.
#'
#' @param opal Opal object.
#' @param project Name of the project.
#' @param archive Archive directory path in the Opal file system. If folder (and parents) does not exist, it will be created.
#' @param viewsAsTables Treat views as tables, i.e. export data instead of keeping derivation scripts. Default is FALSE.
#' @param override Overwrite an existing backup folder. Default is TRUE.
#' @param wait Wait for backup task completion. Default is TRUE.
#' @return The project command ID if wait parameter is FALSE. See \link{opal.project_command} to retrieve asynchronous command state.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.project_backup(o, 'GREENSPACE', '/home/administrator/backup/GREENSPACE')
#' opal.file_download(o, '/home/administrator/backup/GREENSPACE', 'GREENSPACE.zip')
#' opal.logout(o)
#' }
#' @export
opal.project_backup <- function(opal, project, archive, viewsAsTables = FALSE, override = TRUE, wait=TRUE) {
  params <- list(archive = archive, viewsAsTables = viewsAsTables, override = override)
  location <- opal.post(opal, "project", project, "commands", "_backup", body = jsonlite::toJSON(params, auto_unbox = TRUE), contentType = "application/json", callback=.handleResponseLocation)
  if (!is.null(location)) {
    # /shell/command/<id>
    task <- substring(location, 16)
    if (wait) {
      status <- 'NA'
      waited <- 0
      while(!is.element(status, c('SUCCEEDED','FAILED','CANCELED'))) {
        # delay is proportional to the time waited, but no more than 10s
        delay <- min(10, max(1, round(waited/10)))
        Sys.sleep(delay)
        waited <- waited + delay
        command <- opal.project_command(opal, project, task)
        status <- command$status
      }
      if (is.element(status, c('FAILED','CANCELED'))) {
        stop(paste0('Backup of "', project, '" ended with status: ', status, '. Messages: \n', 
                    paste0(sapply(command$messages, function(m) paste("> ", trimws(m$msg))), collapse = "\n")), call.=FALSE)
      }
    } else {
      # returns the task ID so that task completion can be followed
      task
    }
  } else {
    # not supposed to be here
    location
  }
}

#' Restore a project
#' 
#' Restore the data of a project from a backup archive file to be found on the Opal file system. 
#' The destination project must exist and can have a name different from the original one 
#' (beware that this could break views). Default behavior is to stop when an item to restore 
#' already exist (override can be forced).
#'
#' @param opal Opal object.
#' @param project Name of the project.
#' @param archive Archive directory or zip file path in the Opal file system.
#' @param key Archive zip file password (if applies).
#' @param override Overwrite existing items (table, view, resource, report). Project files override is not checked. Default is TRUE.
#' @param wait Wait for restore task completion. Default is TRUE.
#' @return The project command ID if wait parameter is FALSE. See \link{opal.project_command} to retrieve asynchronous command state.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # create the project to restore, with the default database (to store tables)
#' opal.project_create(o, 'GREENSPACE2', database = TRUE)
#' # upload backup zip and launch restore task
#' opal.file_upload(o, 'GREENSPACE.zip', '/home/administrator')
#' opal.project_restore(o, 'GREENSPACE2', '/home/administrator/GREENSPACE.zip')
#' opal.logout(o)
#' }
#' @export
opal.project_restore <- function(opal, project, archive, key = NULL, override = TRUE, wait=TRUE) {
  params <- list(archive = archive, override = override)
  if (!.is.empty(key)) {
    params["password"] <- key
  }
  location <- opal.post(opal, "project", project, "commands", "_restore", body = jsonlite::toJSON(params, auto_unbox = TRUE), contentType = "application/json", callback=.handleResponseLocation)
  if (!is.null(location)) {
    # /shell/command/<id>
    task <- substring(location, 16)
    if (wait) {
      status <- 'NA'
      waited <- 0
      while(!is.element(status, c('SUCCEEDED','FAILED','CANCELED'))) {
        # delay is proportional to the time waited, but no more than 10s
        delay <- min(10, max(1, round(waited/10)))
        Sys.sleep(delay)
        waited <- waited + delay
        command <- opal.project_command(opal, project, task)
        status <- command$status
      }
      if (is.element(status, c('FAILED','CANCELED'))) {
        stop(paste0('Restore of "', project, '" ended with status: ', status, '. Messages: \n', 
                    paste0(sapply(command$messages, function(m) paste("> ", trimws(m$msg))), collapse = "\n")), call.=FALSE)
      }
    } else {
      # returns the task ID so that task completion can be followed
      task
    }
  } else {
    # not supposed to be here
    location
  }
}

#' Get project task
#' 
#' Get the project's task command object.
#' 
#' @param opal Opal object.
#' @param project Name of the project.
#' @param id The project command ID.
#' @return The command state object.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' id <- opal.project_backup(o, 'GREENSPACE', '/home/administrator/backup/GREENSPACE', wait = FALSE)
#' opal.project_command(opal, 'GREENSPACE', id)
#' opal.logout(o)
#' }
#' @export
opal.project_command <- function(opal, project, id) {
  opal.get(opal, "project", project, "command", id)
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
opal.project_perm_add <- function(opal, project, subject, type = "user", permission = "administrate") {
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
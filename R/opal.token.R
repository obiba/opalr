#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the personal access tokens
#' 
#' @family token functions
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.tokens(o)
#' opal.logout(o)
#' }
#' @export
opal.tokens <- function(opal, df=TRUE) {
  if (!is.na(opal$version) && opal.version_compare(opal,"2.15")<0) {
    stop("Personal access tokens are not available in opal ", opal$version, " (2.15.0 or higher is required)")
  }
  res <- opal.get(opal, "system", "subject-token", "_current", "tokens")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    projects <- rep(NA, n)
    access <- rep(NA, n)
    commands <- rep(NA, n)
    createProject <- rep(NA, n)
    updateProject <- rep(NA, n)
    deleteProject <- rep(NA, n)
    useR <- rep(NA, n)
    useDatashield <- rep(NA, n)
    useSQL <- rep(NA, n)
    sysAdmin <- rep(NA, n)
    created <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      if (!is.null(item$projects)) {
        projects[i] <- paste0(item$projects, collapse = ", ")
      }
      if (!is.null(item$commands)) {
        commands[i] <- paste0(item$commands, collapse = ", ")
      }
      access[i] <- .nullToNA(item$access)
      createProject[i] <- .nullToNA(item$createProject)
      updateProject[i] <- .nullToNA(item$updateProject)
      deleteProject[i] <- .nullToNA(item$deleteProject)
      useR[i] <- item$useR
      useDatashield[i] <- item$useDatashield
      useSQL[i] <- item$useSQL
      sysAdmin[i] <- item$sysAdmin
      created[i] <- item$created
    }
    data.frame(name, projects, access, commands, createProject, updateProject, deleteProject, useR, useDatashield, useSQL, sysAdmin, created) 
  } else {
    data.frame()
  }
}

#' Create a personal access token for R usage
#' 
#' @family token functions
#' @param opal Opal object.
#' @param name Name of the token
#' @param projects Vector of project names, to which the token applies. Default is NULL (all projects).
#' @param access Data access level: 'READ' (read-only) or 'READ_NO_VALUES' (read-only, without access to individual-level data) or NULL (default).
#' @param commands Task commands that can launched on a project: 'import' and/or 'export'. Default is 'export' (use NULL for no task commands).
#' @return The token value.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' token <- opal.token_r_create(o, 'r-1', access = 'READ', commands = 'export')
#' opal.logout(o)
#' }
#' @export
opal.token_r_create <- function(opal, name, projects = NULL, access = NULL, commands = c('export')) {
  token <- .generateToken()
  body <- jsonlite::toJSON(list(name = name, token = token, projects = projects, access = access, commands = commands, useR = TRUE), auto_unbox = TRUE, null = 'null')
  ignore <- opal.post(opal, "system", "subject-token", "_current", "tokens", contentType = "application/json", body = body)
  token
}

#' Create a personal access token for Datashield usage
#' 
#' @family token functions
#' @param opal Opal object.
#' @param name Name of the token
#' @param projects Vector of project names, to which the token applies. Default is NULL (all projects).
#' @return The token value.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' token <- opal.token_datashield_create(o, 'ds-1')
#' opal.logout(o)
#' }
#' @export
opal.token_datashield_create <- function(opal, name, projects = NULL) {
  token <- .generateToken()
  body <- jsonlite::toJSON(list(name = name, token = token, projects = projects, access = 'READ_NO_VALUES', useDatashield = TRUE), auto_unbox = TRUE, null = 'null')
  ignore <- opal.post(opal, "system", "subject-token", "_current", "tokens", contentType = "application/json", body = body)
  token
}

#' Create a personal access token for SQL usage
#' 
#' @family token functions
#' @param opal Opal object.
#' @param name Name of the token
#' @param projects Vector of project names, to which the token applies. Default is NULL (all projects).
#' @return The token value.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' token <- opal.token_sql_create(o, 'sql-1')
#' opal.logout(o)
#' }
#' @export
opal.token_sql_create <- function(opal, name, projects = NULL) {
  token <- .generateToken()
  body <- jsonlite::toJSON(list(name = name, token = token, projects = projects, access = 'READ', useSQL = TRUE), auto_unbox = TRUE, null = 'null')
  ignore <- opal.post(opal, "system", "subject-token", "_current", "tokens", contentType = "application/json", body = body)
  token
}

#' Get a personal access token
#' 
#' @family token functions
#' @param opal Opal object.
#' @param name Name of the token
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.token(o, 'sql-1')
#' opal.logout(o)
#' }
#' @export
opal.token <- function(opal, name) {
  tks <- opal.tokens(opal, df = FALSE)
  res <- tks[sapply(tks, function(tk) { tk$name == name})]
  if (length(res)>0)
    res[[1]]
  else
    NULL
}

#' Delete a personal access token
#' 
#' @family token functions
#' @param opal Opal object.
#' @param name Name of the token
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.token_delete(o, 'sql-1')
#' opal.logout(o)
#' }
#' @export
opal.token_delete <- function(opal, name) {
  ignore <- tryCatch(opal.delete(opal, "system", "subject-token", "_current", "token", name))
}

#' @keywords internal
.generateToken <- function() {
  x <- 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
  token <- ''
  for (i in sample(1:52, 32)) {
    token <- paste0(token, substr(x, i, i))
  }
  token
}

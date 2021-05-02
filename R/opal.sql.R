#-------------------------------------------------------------------------------
# Copyright (c) 2020 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Execute a SQL query on tables
#' 
#' The SQL query can apply to raw tables and/or views and require the permission to 
#' view the values of these tables. When all tables belong to a project, it is possible to simplify 
#' the SQL query by providing the project name parameter. Otherwise the fully qualified table names 
#' (`<project>.<table>`) must be specified in the FROM statements.
#' 
#' @param opal Opal connection object.
#' @param query The SQL query statement.
#' @param project Project name where the table(s) are located. If not provided, the SQL query must refer to the 
#' full table name(s) (use backquotes to escape, see examples).
#' @param id.name The name of the column representing the entity identifiers. Default is '_id'.
#' @return The lists of columns and rows, as a data.frame.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # with project context
#' opal.sql(o,  
#'   'select avg(LAB_HDL) as HDL_AVG, GENDER 
#'      from CNSIM1 
#'      where LAB_HDL is not null 
#'      group by GENDER',
#'   'CNSIM')
#' # without project context
#' opal.sql(o,
#'   'select avg(LAB_HDL) as HDL_AVG, GENDER 
#'      from `CNSIM.CNSIM1` 
#'      where LAB_HDL is not null 
#'      group by GENDER')
#' opal.logout(o)
#' }
#' @export
opal.sql <- function(opal, query, project = NULL, id.name = '_id') {
  if (is.na(opal$version) || opal.version_compare(opal,"4.1")<0) {
    stop("SQL queries are not available for opal ", opal$version, " (4.1.0 or higher is required)")
  }
  if (is.null(project)) {
    location <- paste0(c('datasources', '_rsql'), collapse = '/')
  } else {
    location <- paste0(c('datasource', project, '_rsql'), collapse = '/')
  }
  out <- tempfile()
  r <- httr::POST(
    .url(opal, location),
    body = list(
      query = query,
      id = id.name
    ),
    encode = "form",
    write_disk(out, overwrite = TRUE), accept("application/x-rdata"), 
    config=opal$config, handle=opal$handle, .verbose()
  )
  if (r$status>=300) {
    .handleError(opal, r)
  }
  res <- readRDS(out)
  unlink(out)
  res
}

#' SQL query execution history
#' 
#' Getting the SQL execution is for being able to re-execute a previously submitted own SQL query (regular users)
#' and for auditing users SQL activity (administrators only).
#' 
#' @param opal Opal connection object.
#' @param project Project name used as the SQL execution context, to filter. If not specified, history from any context 
#' is returned. If NA is specified, the history of SQL executions without context is returned. Default is NULL.
#' @param offset Number of history items to skip. Default is 0 (note that the items are ordered by most recent first).
#' @param limit Maximum number of history items to return. Default is 100.
#' @param user Filter by user name, only administrators can retrieve SQL history of other users. If NA is specified, the 
#' SQL execution history of all users will be retrieved. Default is the current user name.
#' @param df Result is a data.frame or a list of raw data.
#' @return A data frame.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # with project context
#' opal.sql_history(o, 'CNSIM')
#' # without project context
#' opal.sql_history(o, NA)
#' # with or without project context
#' opal.sql_history(o)
#' opal.logout(o)
#' }
#' @export
opal.sql_history <- function(opal, project = NULL, offset = 0, limit = 100, user = NULL, df = TRUE) {
  if (is.na(opal$version) || opal.version_compare(opal,"4.1")<0) {
    stop("SQL queries are not available for opal ", opal$version, " (4.1.0 or higher is required)")
  }
  q <- list(offset = offset, limit = limit)
  if (!is.null(project))
    if (is.na(project))
      q$datasource <- '*'
    else
      q$datasource <- project
  subject <- "_current"
  if (!is.null(user))
    if (is.na(user))
      subject <- '*'
    else if (user != opal$username)
      subject <- user
  res <- opal.get(opal, "system", "subject-profile", subject, "sql-history", query = q)
  if (df) {
    if (length(res)>0) {
      user <- replicate(length(res), opal$username)
      query <- replicate(length(res), NA)
      project <- replicate(length(res), NA)
      error <- replicate(length(res), NA)
      start <- replicate(length(res), NA)
      end <- replicate(length(res), NA)
      elapsed <- replicate(length(res), NA)
      for (i in 1:length(res)) {
        item <- res[[i]]
        user[[i]] <- item$user
        query[[i]] <- item$query
        if (!is.null(item$datasource))
          project[[i]] <- item$datasource
        if (!is.null(item$error))
          error[[i]] <- item$error
        start[[i]] <- item$start/1000
        end[[i]] <- item$end/1000
        elapsed[[i]] <- item$end - item$start
      }
      data.frame(user, query, project, error, start = as.POSIXct(start, origin='1970-01-01'), end = as.POSIXct(end, origin='1970-01-01'), elapsed)  
    } else {
      data.frame()
    }
  } else {
    res
  }
  
}
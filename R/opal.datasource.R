#-------------------------------------------------------------------------------
# Copyright (c) 2020 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get datasources
#' 
#' @family datasource functions
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.datasources(o)
#' opal.logout(o)
#' }
#' @export
opal.datasources <- function(opal, df=TRUE) {
  res <- opal.get(opal, "datasources")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    type <- rep(NA, n)
    tables <- rep(NA, n)
    created <- rep(NA, n)
    lastUpdate <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      type[i] <- item$type
      if (!is.null(item$table)) {
        tables[i] <- paste0(item$table, collapse = "|")
      }
      created[i] <- item$timestamps$created
      lastUpdate[i] <- item$timestamps$lastUpdate
    }
    data.frame(name, tables, type, created, lastUpdate)
  } else {
    data.frame()
  }
}

#' Get a datasource
#' 
#' @family datasource functions
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.datasource(o, 'datashield')
#' opal.logout(o)
#' }
#' @export
opal.datasource <- function(opal, datasource) {
  opal.get(opal, "datasource", datasource)
}

#' Get tables of a datasource
#' 
#' @family datasource functions
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param counts Flag to get the number of variables and entities (default is FALSE).
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.tables(o, 'datashield')
#' opal.logout(o)
#' }
#' @export
opal.tables <- function(opal, datasource, counts=FALSE, df=TRUE) {
  if (counts) {
    res <- opal.get(opal, "datasource", datasource, "tables", query=list(counts="true"))  
  } else {
    res <- opal.get(opal, "datasource", datasource, "tables")
  }
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    ds <- rep(datasource, n)
    entityType <- rep(NA, n)
    variables <- rep(NA, n)
    entities <- rep(NA, n)
    created <- rep(NA, n)
    lastUpdate <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      entityType[i] <- item$entityType
      if (counts) {
        variables[i] <- item$variableCount
        entities[i] <- item$valueSetCount
      }
      created[i] <- item$timestamps$created
      lastUpdate[i] <- item$timestamps$lastUpdate
    }
    if (counts) {
      data.frame(name, datasource=ds, entityType, variables, entities, created, lastUpdate)
    } else {
      data.frame(name, datasource=ds, entityType, created, lastUpdate) 
    }
  } else {
    data.frame()
  }
}

#' Get a table of a datasource
#' 
#' @family datasource functions
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param counts Flag to get the number of variables and entities (default is FALSE).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.table(o, 'datashield', 'CNSIM1')
#' opal.logout(o)
#' }
#' @export
opal.table <- function(opal, datasource, table, counts=FALSE) {
  if (counts) {
    opal.get(opal, "datasource", datasource, "table", table, query=list(counts="true")); 
  } else {
    opal.get(opal, "datasource", datasource, "table", table);
  }  
}

#' Get variables of a table
#' 
#' @family datasource functions
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param locale The language for labels (default is "en").
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.variables(o, 'datashield', 'CNSIM1')
#' opal.logout(o)
#' }
#' @export
opal.variables <- function(opal, datasource, table, locale="en", df=TRUE) {
  res <- opal.get(opal, "datasource", datasource, "table", table, "variables")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    ds <- rep(datasource, n)
    tbl <- rep(table, n)
    label <- rep(NA, n)
    description <- rep(NA, n)
    entityType <- rep(NA, n)
    valueType <- rep(NA, n)
    unit <- rep(NA, n)
    referencedEntityType <- rep(NA, n)
    mimeType <- rep(NA, n)
    repeatable <- rep(FALSE, n)
    occurrenceGroup <- rep(NA, n)
    index <- rep(NA, n)
    categories <- rep(NA, n)
    categories.missing <- rep(NA, n)
    categories.label <- rep(NA, n)
    annotations <- list()
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      if (!is.null(item$attributes)) {
        labels <- item$attributes[lapply(item$attributes, function(attr) attr$name) == "label"]
        label[i] <- .extractLabel(locale, labels)
        descriptions <- item$attributes[lapply(item$attributes, function(attr) attr$name) == "description"]
        description[i] <- .extractLabel(locale, descriptions)
        annots <- item$attributes[lapply(item$attributes, function(attr) "namespace" %in% names(attr)) == TRUE]
        for (annot in annots) {
          key <- paste0(annot$namespace, ".", annot$name)
          if (!(key %in% names(annotations))) {
            a <- list()
            a[[key]] <- rep(NA, n)
            annotations <- append(annotations, a)
          }
          annotations[[key]][i] <- annot$value
        }
      }
      entityType[i] <- item$entityType
      valueType[i] <- item$valueType
      unit[i] <- .nullToNA(item$unit)
      referencedEntityType[i] <- .nullToNA(item$referencedEntityType)
      mimeType[i] <- .nullToNA(item$mimeType)
      repeatable[i] <- ifelse(is.null(item$repeatable), FALSE, TRUE)
      occurrenceGroup[i] <- .nullToNA(item$occurrenceGroup)
      index[i] <- item$index
      if (!is.null(item$categories)) {
        categories[i] <- paste0(lapply(item$categories, function(cat) cat$name), collapse = "|")
        categories.missing[i] <- paste0(lapply(item$categories, function(cat) ifelse(cat$isMissing, "T", "F")), collapse = "|")
        categories.label[i] <- paste(lapply(item$categories, function(cat) { 
          if (is.null(cat$attributes)) {
            ""
          } else {
            labels <- cat$attributes[lapply(cat$attributes, function(attr) { attr$name }) == "label"]
            if (length(labels)>0) {
              .extractLabel(locale, labels)
            } else {
              ""
            }
          }
        }), collapse = "|")
      }
      
    }
    df <- data.frame(name, datasource=ds, table=tbl, label, description, entityType, valueType, unit, referencedEntityType, mimeType, repeatable, occurrenceGroup, index, categories, categories.missing, categories.label) 
    for (col in names(annotations)) {
      df[[col]] <- annotations[[col]]
    }
    df
  } else {
    data.frame()
  }
}

#' Get a variable of a table
#' 
#' @family datasource functions
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param variable Name of the variable in the table.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.variable(o, 'datashield', 'CNSIM1', 'GENDER')
#' opal.logout(o)
#' }
#' @export
opal.variable <- function(opal, datasource, table, variable) {
  opal.get(opal, "datasource", datasource, "table", table, "variable", variable)
}

#' Get a vector of values
#' 
#' Get a vector of values (for each locale) matching the given attribute namespace and name. Vector is null if no such attribute is found.
#' 
#' @family datasource functions
#' @param attributes A list of attributes, usually variable or category attributes.
#' @param namespace Optional attribute namespace.
#' @param name Required attribute name.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' var <- opal.variable(o, 'datashield', 'CNSIM1', 'GENDER')
#' opal.attribute_values(var$attributes)
#' opal.logout(o)
#' }
#' @export
opal.attribute_values <- function(attributes, namespace=NULL, name="label") {
  rval <- c()
  if (length(attributes) == 0) return(rval)
  
  for (attr in attributes) {
    if (identical(attr$name, name) && identical(attr$namespace, namespace) && nchar(attr$value)>0) {
      if (is.null(attr$locale)) {
        rval <- append(rval, attr$value)
      } else {
        rval <- append(rval, paste0("[", attr$locale, "] ", attr$value))  
      }
    }
  }
  rval
}

#' Get the values of an entity
#' 
#' Get the values of an entity in a table.
#'
#' @family datasource functions
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param identifier Entity identifier.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.valueset(o, 'datashield', 'CNSIM1', '1008573362')
#' opal.logout(o)
#' }
#' @export
opal.valueset <- function(opal, datasource, table, identifier) {
  response <- opal.get(opal, "datasource", datasource, "table", table, "valueSet", identifier)
  valueset <- list()
  i <- 1
  for (variable in response$variables) {
    value <- response$valueSets[[1]]$values[[i]]$value
    if(is.list(value)) {
      valueset[[variable]] <- lapply(value, function(val) {
        val$value
      })
    } else {
      valueset[[variable]] <- value  
    }
    i <- i + 1
  }
  valueset
}

#' Add or update a permission on any table of a project
#' 
#' Add or update a permission on any table of a project.
#' 
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: view-values, add, or administrate.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.tables_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'administrate')
#' opal.tables_perm(o, 'CNSIM')
#' opal.tables_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.tables_perm_add <- function(opal, project, subject, type = "user", permission) {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('view-values' = 'DATASOURCE_VIEW',
                'add' = 'TABLE_ADD',
                'administrate' = 'DATASOURCE_ALL')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid tables permission name: ", permission)
  }
  opal.tables_perm_delete(opal, project, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "project", project, "permissions", "datasource", query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the permissions on any table of a project
#' 
#' Get the permissions that were applied on any table of a project.
#' 
#' @param opal Opal connection object.
#' @param project Project name.
#' 
#' @return A data.frame with columns: subject, type, permission
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.tables_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'administrate')
#' opal.tables_perm(o, 'CNSIM')
#' opal.tables_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.tables_perm <- function(opal, project) {
  perms <- list('DATASOURCE_VIEW' = 'view-values',
                'TABLE_ADD' = 'add',
                'DATASOURCE_ALL' = 'administrate')
  acls <- opal.get(opal, "project", project, "permissions", "datasource")
  .aclsToDataFrame(perms, acls)
}

#' Delete a permission from any table of a project
#' 
#' Delete a permission that was applied on any table of a project. Silently returns when there is no such permission.
#' 
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.tables_perm_add(o, 'CNSIM', c('andrei', 'valentina'), 'user', 'administrate')
#' opal.tables_perm(o, 'CNSIM')
#' opal.tables_perm_delete(o, 'CNSIM', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.tables_perm_delete <- function(opal, project, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "project", project, "permissions", "datasource", query = list(principal = subject[i], type = toupper(type)))  
  }
}


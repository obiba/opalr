#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get a Opal table as a tibble
#'
#' Shortcut function to assign a Opal table to a tibble in the R server-side session
#' and then retrieve it into the R client-side session. Requires to have the permission to
#' see the individual values of the table and to perform R assignments.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name from which the tibble should be extracted.
#' @param id.name The name of the column representing the entity identifiers. Default is 'id'. Requires Opal 4.0+.
#' @param variables (Deprecated) List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refer to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings (Deprecated) Include the missing values (default is TRUE).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' cqx <- opal.table_get(o, "CPTP", "Cag_coreqx")
#' opal.logout(o)
#' }
#' @export
opal.table_get <- function(opal, project, table, id.name='id', variables = NULL, missings = TRUE) {
  tblObj <- opal.table(opal, project, table, counts = TRUE)
  if (tblObj$valueSetCount == 0) {
    tibble::tibble()
  } else {
    pb <- NULL
    localfile <- tempfile(fileext = ".rds")
    if (opal.version_compare(opal,"4.0")<0) {
      pb <- .newProgress(total = 5)
      .tickProgress(pb, tokens = list(what = paste0("Assigning ", project, ".", table)))
      opal.assign.table.tibble(opal, symbol = ".D", value = paste0(project, ".", table), variables = variables, missings = missings)
      
      .tickProgress(pb, tokens = list(what = paste0("Saving in R data file")))
      opal.assign.script(opal, ".file", quote(tempfile(tmpdir = getwd(), fileext = '.rds')))
      file <- opal.execute(opal, ".file")
      filename <- basename(file)
      opal.execute(opal, paste0("saveRDS(.D, file=.file)"))
    
      # clean up 
      tryCatch(opal.symbol_rm(opal, ".D"))
      opal.execute(opal, "gc()")
      
      .tickProgress(pb, tokens = list(what = paste0("Downloading R data file")))
      opaltmp <- opal.file_mkdir_tmp(opal)
      opalfile <- paste0(opaltmp, "/", filename)
      opal.file_read(opal, filename, opalfile)
      opal.execute(opal, paste0("unlink(.file)"))
      opal.file_download(opal, opalfile, localfile)
      opal.file_rm(opal, opaltmp)
      
    } else {
      pb <- .newProgress(total = 4)
      .tickProgress(pb, tokens = list(what = paste0("Exporting ", project, ".", table)))
      localfile <- tempfile(fileext = ".rds")
      filename <- basename(localfile)
      opaltmp <- opal.file_mkdir_tmp(opal)
      opalfile <- paste0(opaltmp, "/", filename)
      opal.table_export(opal, project, table, file = opalfile, id.name = id.name)
      
      .tickProgress(pb, tokens = list(what = paste0("Downloading R data file")))
      opal.file_download(opal, opalfile, localfile)
      opal.file_rm(opal, opaltmp)
    }
    
    .tickProgress(pb, tokens = list(what = paste0("Loading R data file")))
    rval <- readRDS(localfile)
    unlink(localfile)
    .tickProgress(pb, tokens = list(what = "Data loaded"))
    rval
  }
}

#' Delete a Opal table
#'
#' Removes both values and data dictionary of a table, or remove the table's logic if the table is a view.
#' Fails if the table does not exist. See also \link{opal.table_truncate}.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name to be deleted.
#' @param silent Warn if table does not exist, default is TRUE.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.table_delete(o, "CNSIM", "CNSIM1")
#' opal.logout(o)
#' }
#' @export
opal.table_delete <- function(opal, project, table, silent = TRUE) {
  if (opal.table_exists(opal, project, table)) {
    ignore <- opal.delete(opal, "datasource", project, "table", table)  
  } else if (!silent) {
    warning("Table '", table,"' does not exist in project '", project, "'")
  }
}

#' Check a Opal table exists
#'
#' Check whether a Opal table exists (and is visible). Optionally check whether the table is a raw table 
#' or a view.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name.
#' @param view Logical to perform an additional check whether the table is a view (TRUE) or a raw table (FALSE).
#' If NULL or NA, the table can be indifferently a view or a raw table. Default is NA.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # check table exists
#' opal.table_exists(o, "CNSIM", "CNSIM1")
#' # check table exists AND is a NOT a view
#' opal.table_exists(o, "CNSIM", "CNSIM1", view = FALSE)
#' # check table exists AND is a view
#' opal.table_exists(o, "CNSIM", "CNSIM1", view = TRUE)
#' opal.logout(o)
#' }
#' @export
opal.table_exists <- function(opal, project, table, view = NA) {
  res <- tryCatch(opal.table(opal, datasource = project, table = table), 
                  error = function(cond) {
                    NULL
                  })
  if (!is.null(res) && !.is.empty(view) && is.logical(view)) {
    if (view) {
      !is.null(res$viewLink)
    } else {
      is.null(res$viewLink)
    }
  } else {
    !is.null(res)  
  }
}

#' Create a Opal table or view
#'
#' Create a Opal table if it does not already exist. If a list of table references are provided,
#' the table will be a view. The table/view created will have no dictionary, use 
#' \link{opal.table_dictionary_update} to apply a dictionary.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name to be deleted.
#' @param type Entity type, default is "Participant". Ignored if some table references are 
#' provided.
#' @param tables List of the fully qualified table names that are referred by the view.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # make a raw table
#' opal.table_create(o, "CNSIM", "CNSIM4")
#' # make a view
#' opal.table_create(o, "CNSIM", "CNSIM123", 
#'                   tables = c("CNSIM.CNSIM1", "CNSIM.CNSIM2", "CNSIM.CNSIM3"))
#' opal.logout(o)
#' }
#' @export
opal.table_create <- function(opal, project, table, type = "Participant", tables = NULL) {
  if (!opal.table_exists(opal, project, table)) {
    if (.is.empty(tables)) {
      body <- jsonlite::toJSON(list(name = table, entityType = type), auto_unbox = TRUE)
      ignore <- opal.post(opal, "datasource", project, "tables", contentType = "application/json", body = body)
    } else {
      body <- jsonlite::toJSON(list(name = table, from = tables, "Magma.VariableListViewDto.view" = list(variables = list())), auto_unbox = TRUE)
      ignore <- opal.post(opal, "datasource", project, "views", contentType = "application/json", body = body)    
    }
  } else {
    stop("Table '", table,"' already exists in project '", project, "'.")
  }
}

#' Truncate a Opal table
#'
#' Removes the values of a table and keep the dictionary untouched. Fails if the table does
#' not exist or is a view. See also \link{opal.table_delete}.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name to be truncated.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.table_truncate(o, "CNSIM", "CNSIM1")
#' opal.logout(o)
#' }
#' @export
opal.table_truncate <- function(opal, project, table) {
  if (opal.table_exists(opal, project = project, table = table, view = FALSE)) {
    ignore <- opal.delete(opal, "datasource", project, "table", table, "valueSets")  
  } else {
    warning("Table '", table,"' does not exist in project '", project, "' or is a view.")
  }
}

#' Save a local tibble as a Opal table
#'
#' Upload a local tibble to the R server side through Opal, assign this tibble to the provided
#' symbol name and import it as a table into a Opal project.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param tibble The tibble object to be imported.
#' @param project Project name where the table will be located.
#' @param table Destination table name.
#' @param overwrite If the destination table already exists, it will be replaced (deleted, 
#' re-created with associated permissions reinstated and then imported). Otherwise the table
#' will be updated (data dictionaries merge may conflict). Default  is TRUE. See 
#' also \link{opal.table_truncate} function.
#' @param force If the destination already exists, stop with an informative message if this flag 
#' is FALSE (default).
#' @param identifiers Name of the identifiers mapping to use when assigning entities to Opal.
#' @param policy Identifiers policy: 'required' (each identifiers must be mapped prior importation 
#' (default)), ignore' (ignore unknown identifiers) and 'generate' (generate a system identifier for 
#' each unknown identifier).
#' @param id.name The name of the column representing the entity identifiers. Default is 'id'.
#' @param type Entity type (what the data are about). Default is 'Participant'
#' @return An invisible logical indicating whether the destination table exists.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' cqx <- opal.table_get(o, "CPTP", "Cag_coreqx")
#' # do some (meta)data transformations, then save in opal's database
#' opal.table_save(o, cqx, "CPTP", "Cag_coreqx", overwrite = TRUE, force = TRUE)
#' # or overwrite data only (keep original data dictionary)
#' opal.table_save(o, cqx, "CPTP", "Cag_coreqx", overwrite = 'values', force = TRUE)
#' opal.logout(o)
#' }
#' @export
#' @import jsonlite
opal.table_save <- function(opal, tibble, project, table, overwrite = TRUE, force = FALSE, identifiers=NULL, policy='required', id.name='id', type='Participant') {
  tbl <- tibble
  if (!tibble::is_tibble(tibble)) {
    if (!is.data.frame(tibble))
      stop("The tibble parameter must be a tibble.")
    else {
      warning("Coercing data.frame to a tibble...")
      tbl <- tibble::as_tibble(tibble)
    }
  }
  if (!(id.name %in% names(tbl))) {
    stop("The identifiers column '", id.name,"' is missing.")
  }
  if (!is.character(tbl[[id.name]])) {
    tbl[[id.name]] <- as.character(tbl[[id.name]])  
  }
  dictionary.inspect(tbl, id.name = id.name)
  
  if (opal.version_compare(opal,"4.0")<0) {
    pb <- .newProgress(total = 7)
  } else {
    pb <- .newProgress(total = 6)
  }
  
  .tickProgress(pb, tokens = list(what = paste0("Checking ", project, " project")))
  if (opal.table_exists(opal, project, table, view = FALSE)) {
    if (overwrite) {
      if (!force) {
        stop("Destination table needs to be deleted or truncated. Use 'force' parameter to proceed.")
      }
      .tickProgress(pb, tokens = list(what = paste0("Deleting ", table, " from ", project)))
      acls <- opal.table_perm(opal, project, table)
      opal.table_delete(opal, project, table)
      opal.table_create(opal, project, table, type = type)
      if (nrow(acls)>0) {
        lapply(1:nrow(acls), function(i) {
          acl <- acls[i,]
          opal.table_perm_add(opal, project, table, acl$subject, acl$type, acl$permission)
        })
      }
    } else {
      if (!force) {
        stop("Destination table will be updated. There could be data dictionary conflicts. Use 'force' parameter to proceed.")
      }
      .tickProgress(pb, tokens = list(what = paste0("Merging with ", table, " from ", project)))
    }
  } else if (opal.table_exists(opal, project, table, view = TRUE)) {
    stop("Destination table is a view.")
  } else {
    .tickProgress(pb, tokens = list(what = paste0("Creating table ", table, " in ", project)))
    opal.table_create(opal, project, table, type = type)
  }
  
  .tickProgress(pb, tokens = list(what = paste0("Saving in R data file")))
  file <- tempfile(fileext = ".rds")
  saveRDS(tbl, file = file)
  
  .tickProgress(pb, tokens = list(what = paste0("Uploading R data file")))
  tmp <- opal.file_mkdir_tmp(opal)
  opal.file_upload(opal, file, tmp)
  filename <- basename(file)
  unlink(file)
  opalFile <- paste0(tmp, filename)
  
  if (opal.version_compare(opal,"4.0")<0) {
    # write file in R session
    opal.file_write(opal, opalFile)
    opal.file_rm(opal, tmp)
    
    .tickProgress(pb, tokens = list(what = paste0("Loading R data file")))
    opal.execute(opal, paste0("assign('", table, "', readRDS('", filename, "'))"))
    opal.execute(opal, paste0("unlink('", filename, "')"))
    
    .tickProgress(pb, tokens = list(what = paste0("Importing ", table, " into ", project)))
    opal.symbol_import(opal, table, project = project, identifiers = identifiers, policy = policy, id.name = id.name, type = type)
    
    # clean up R session
    tryCatch(opal.symbol_rm(opal, table))
    opal.execute(opal, "gc()")
  } else {
    .tickProgress(pb, tokens = list(what = paste0("Importing ", table, " into ", project)))
    opal.table_import(opal, file = opalFile, project = project, table = table, identifiers = identifiers, policy = policy, id.name = id.name, type = type)
    opal.file_rm(opal, tmp)
  }
  
  rval <- table %in% opal.datasource(opal, project)$table
  .tickProgress(pb, tokens = list(what = "Save completed"))
  invisible(rval)
}

#' Import a file as table
#' 
#' Import a file as a table in Opal. The file formats supported are: RDS (.rds), SPSS (.sav), 
#' SPSS compressed (.zsav), SAS (.sas7bdat), SAS Transport (.xpt), Stata (.dta). 
#' The RDS format is a serialized single R object (expected to be of tibble class), that can be obtained using base::saveRDS().
#' The other file formats are the ones supported by the haven R package.
#' This operation creates an importation task in Opal that can be followed (see tasks related functions).
#' 
#' @family table functions
#' @param opal Opal object.
#' @param file Path in Opal to the file that will be read as a tibble.
#' @param project Name of the project into which the data are to be imported.
#' @param table Destination table name.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to Opal.
#' @param policy Identifiers policy: 'required' (each identifiers must be mapped prior importation (default)), 'ignore' (ignore unknown identifiers) and 'generate' (generate a system identifier for each unknown identifier). 
#' @param id.name The name of the column representing the entity identifiers. Default is 'id'.
#' @param type Entity type (what the data are about). Default is 'Participant'.
#' @param wait Wait for import task completion. Default is TRUE.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.table_import(o, '/home/administrator/mydataset.rds', 'test', 'mytable')
#' opal.logout(o)
#' }
#' @export
#' @import tools
opal.table_import <- function(opal, file, project, table, identifiers=NULL, policy='required', id.name='id', type='Participant', wait=TRUE) {
  if (!(tools::file_ext(file) %in% c("rds", "sav", "zsav", "sas7bdat", "xpt", "dta"))) {
    stop("File extension not supported for import: ", tools::file_ext(file))
  }
  if (endsWith(file, "rds") && !is.na(opal$version) && opal.version_compare(opal,"4.0")<0) {
    stop("Importing a RDS data file into a table is not available for opal ", opal$version, " (4.0.0 or higher is required)")
  } else {
    # create a transient datasource
    dsFactory <- list(file=file, symbol=table, entityType=type, idColumn=id.name)
    if (is.null(identifiers)) {
      dsFactory <- paste0('{"Magma.RHavenDatasourceFactoryDto.params": ', .listToJson(dsFactory), '}') 
    } else {
      idConfig <- list(name=identifiers)
      if (policy == 'required') {
        idConfig["allowIdentifierGeneration"] <- TRUE
        idConfig["ignoreUnknownIdentifier"] <- TRUE
      } else if (policy == 'ignore') {
        idConfig["allowIdentifierGeneration"] <- FALSE
        idConfig["ignoreUnknownIdentifier"] <- TRUE
      } else {
        idConfig["allowIdentifierGeneration"] <- FALSE
        idConfig["ignoreUnknownIdentifier"] <- FALSE
      }
      dsFactory <- paste0('{"Magma.RHavenDatasourceFactoryDto.params": ', .listToJson(dsFactory), ', "idConfig":', .listToJson(idConfig),'}')
    }
    created <- opal.post(opal, "project", project, "transient-datasources", body=dsFactory, contentType="application/json")
    # launch a import task
    importCmd <- list(destination=project, tables=list(paste0(created$name, '.', table)))
    location <- opal.post(opal, "project", project, "commands", "_import", body=.listToJson(importCmd), contentType="application/json", callback=.handleResponseLocation)
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
          stop(paste0('Import of "', file, '" ended with status: ', status), call.=FALSE)
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
}

#' Export a table as a file
#' 
#' Export a table as file in the specified format. The file destination is in the Opal server 
#' file system. See \link{opal.file_download} to download the file locally. See also 
#' \link{opal.table_get} to get directly the table as an R object.
#'
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name to export.
#' @param file Destination file in the Opal file system. The expected file extensions are: 
#' rds (RDS), sav (SPSS), zsav (SPSS compressed), sas7bdat (SAS), xpt (SAS Transport), 
#' dta (Stata).RDS (serialized single R object) is to be read by base::readRDS(), 
#' while other formats are supported by the haven R package.
#' @param identifiers Name of the identifiers mapping to use when exporting entities from Opal.
#' @param id.name The name of the column representing the entity identifiers. Default is 'id'.
#' @param wait Wait for import task completion. Default is TRUE.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' cqx <- opal.table_export(o, "CNSIM", "CNSIM1", 
#'                          file = "/home/administrator/cnsim1.sav")
#' opal.logout(o)
#' }
#' @export
opal.table_export <- function(opal, project, table, file, identifiers=NULL, id.name='id', wait = TRUE) {
  format <- tools::file_ext(file)
  if (!(format %in% c("rds", "sav", "zsav", "sas7bdat", "xpt", "dta"))) {
    stop("Format not supported for export: ", format)
  }
  config <- list(
    tables = list(paste0(project, ".", table)),
    format = format,
    out = file,
    nonIncremental = TRUE,
    noVariables = FALSE,
    copyNullValues = TRUE,
    entityIdNames = id.name
  )
  if (!is.null(identifiers)) {
    config$idConfig <- list(
      name = identifiers,
      allowIdentifierGeneration = FALSE,
      ignoreUnknownIdentifier = FALSE
    )
  }
  location <- opal.post(opal, "project", project, "commands", "_export", body=jsonlite::toJSON(config, auto_unbox = TRUE), contentType="application/json", callback=.handleResponseLocation)
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
        command <- opal.get(opal, "shell", "command", task)
        status <- command$status
      }
      if (is.element(status, c('FAILED','CANCELED'))) {
        stop(paste0('Export of "', project, ".", table, '" ended with status: ', status), call.=FALSE)
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

#' Update the dictionary of a Opal table
#' 
#' Directly update the dictionary of a Opal table with the provided dictionary.
#' 
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param table Destination table name.
#' @param variables A data frame with one row per variable (column name) and then one column per property/attribute (Opal Excel format).
#' @param categories A data frame with one row per category (columns variable and name) and then column per property/attribute (Opal Excel format). If there are
#' no categories, this parameter is optional.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' variables <- tibble::tribble(
#'   ~name, ~valueType, ~`label:en`,  ~`Namespace::Name`, ~unit, ~repeatable, ~index,
#'   "mpg", "decimal", "Mpg label",  "Value1", "years", 0, 1,
#'   "cyl", "decimal", "Cyl label",  "Value2", "kg/m2", 0, 2,
#'   "disp", "decimal", "Disp label", NA, NA, 1, 3
#' )
#' categories <- tibble::tribble(
#'   ~variable, ~name, ~missing, ~`label:en`, ~`label:fr`,
#'   "cyl", "4", 0, "Four", "Quatre",
#'   "cyl", "6", 0, "Six", "Six",
#'   "cyl", "8", 1, "Height", "Huit"
#' )
#' opal.table_dictionary_update(o, "test", "mtcars", variables, categories)
#' opal.logout(o)
#' }
#' @export
opal.table_dictionary_update <- function(opal, project, table, variables, categories = NULL) {
  body <- .toJSONVariables(table=table, variables = variables, categories = categories)
  ignore <- opal.post(opal, "datasource", project, "table", table, "variables", contentType = "application/json", body = body)
}

#' Get the dictionary of a Opal table
#' 
#' Get the dictionary of a Opal table in a format that can be re-applied with opal.table_dictionary_update.
#' 
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param table Table name.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dico <- opal.table_dictionary_get(o, "CNSIM", "CNSIM1")
#' opal.logout(o)
#' }
#' @export
opal.table_dictionary_get <- function(opal, project, table) {
  
  toAttributeKey <- function(attr) {
    key <- attr$name
    if ("namespace" %in% names(attr)) {
      key <- paste0(attr$namespace, "::", key)  
    }
    if ("locale" %in% names(attr)) {
      key <- paste0(key, ":", attr$locale)  
    }
    key
  }
  
  res <- opal.get(opal, "datasource", project, "table", table, "variables")
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    entityType <- rep(NA, n)
    valueType <- rep(NA, n)
    unit <- rep(NA, n)
    referencedEntityType <- rep(NA, n)
    mimeType <- rep(NA, n)
    repeatable <- rep(FALSE, n)
    occurrenceGroup <- rep(NA, n)
    index <- rep(NA, n)
    variables.attributes <- list()
    
    categories.variable <- c()
    categories.name <- c()
    categories.missing <- c()
    categories.attributes <- list()
    
    # read variables
    for (i in 1:n) {
      var <- res[[i]]
      name[i] <- var$name
      entityType[i] <- var$entityType
      valueType[i] <- var$valueType
      unit[i] <- .nullToNA(var$unit)
      referencedEntityType[i] <- .nullToNA(var$referencedEntityType)
      mimeType[i] <- .nullToNA(var$mimeType)
      repeatable[i] <- ifelse(is.null(var$isRepeatable), FALSE, var$isRepeatable)
      occurrenceGroup[i] <- .nullToNA(var$occurrenceGroup)
      index[i] <- var$index
      
      # read variable's attributes
      if (!is.null(var$attributes)) {
        for (attribute in var$attributes) {
          key <- toAttributeKey(attribute)
          if (!(key %in% names(variables.attributes))) {
            a <- list()
            a[[key]] <- rep(NA, n)
            variables.attributes <- append(variables.attributes, a)
          }
          variables.attributes[[key]][i] <- attribute$value
        }
      }
      
      # read variable's categories
      catn <- length(var$categories)
      cat.name <- rep(NA, catn)
      cat.missing <- rep(NA, catn)
      cat.attributes <- list()
      if (catn > 0) {
        for (j in 1:catn) {
          cat <- var$categories[[j]]
          cat.name[j] <- cat$name
          cat.missing[j] <- ifelse(cat$isMissing, TRUE, FALSE)
          if (!is.null(cat$attributes)) {
            for (attribute in cat$attributes) {
              key <- toAttributeKey(attribute)
              if (!(key %in% names(cat.attributes))) {
                a <- list()
                a[[key]] <- rep(NA, catn)
                cat.attributes <- append(cat.attributes, a)
              }
              cat.attributes[[key]][j] <- attribute$value
            }
          }
        }
        
        lg <- length(categories.name) # original length before appending
        categories.variable <- append(categories.variable, rep(var$name, catn))
        categories.name <- append(categories.name, cat.name)
        categories.missing <- append(categories.missing, cat.missing)
        for (col in names(cat.attributes)) {
          if (!(col %in% names(categories.attributes))) {
            # init with NAs
            categories.attributes[[col]] <- rep(NA, lg)
          } else {
            # complete with NAs
            times <- lg - length(categories.attributes[[col]])
            categories.attributes[[col]] <- append(categories.attributes[[col]], rep(NA, times))
          }
          categories.attributes[[col]] <- append(categories.attributes[[col]], cat.attributes[[col]])
        }
      }
    }
    
    # build output data frames
    variables <- data.frame(name, entityType, valueType, unit, referencedEntityType, mimeType, repeatable, occurrenceGroup, index, stringsAsFactors = FALSE) 
    for (col in names(variables.attributes)) {
      variables[[col]] <- variables.attributes[[col]]
    }
    categories <- data.frame(variable = categories.variable, name = categories.name, missing = categories.missing, stringsAsFactors = FALSE)
    for (col in names(categories.attributes)) {
      times <- length(categories.name) - length(categories.attributes[[col]])
      if (times>0) {
        categories.attributes[[col]] <- append(categories.attributes[[col]], rep(NA, times))
      }
      categories[[col]] <- categories.attributes[[col]]
    }
    list(project = project, table = table, variables = variables, categories = categories)
  } else {
    list()
  }
}

#' Add or update a permission on a table
#' 
#' Add or update a permission on a table.
#' 
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param table Destination table name.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @param permission The permission to apply: view, view-values, edit, edit-values, administrate. The 'view' permission
#' is suitable for DataSHIELD operations. 
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.table_perm_add(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user', 'view')
#' opal.table_perm(o, 'CNSIM', 'CNSIM1')
#' opal.table_perm_delete(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.table_perm_add <- function(opal, project, table, subject, type = "user", permission) {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  perms <- list('view' = 'TABLE_READ',
               'view-values' = 'TABLE_VALUES',
               'edit' = 'TABLE_EDIT',
               'edit-values' = 'TABLE_VALUES_EDIT',
               'administrate' = 'TABLE_ALL')
  perm <- perms[[permission]]
  if (is.null(perm)) {
    stop("Not a valid table permission name: ", permission)
  }
  opal.table_perm_delete(opal, project, table, subject, type)
  for (i in 1:length(subject)) {
    ignore <- opal.post(opal, "project", project, "permissions", "table", table, query = list(principal = subject[i], type = toupper(type), permission = perm))
  }
}

#' Get the permissions on a table
#' 
#' Get the permissions that were applied on a table.
#' 
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param table Destination table name.
#' 
#' @return A data.frame with columns: subject, type, permission
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.table_perm_add(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user', 'view')
#' opal.table_perm(o, 'CNSIM', 'CNSIM1')
#' opal.table_perm_delete(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.table_perm <- function(opal, project, table) {
  perms <- list('TABLE_READ' = 'view',
                'TABLE_VALUES' = 'view-values',
                'TABLE_EDIT' = 'edit',
                'TABLE_VALUES_EDIT' = 'edit-values',
                'TABLE_ALL' = 'administrate')
  acls <- opal.get(opal, "project", project, "permissions", "table", table)
  .aclsToDataFrame(perms, acls)
}

#' Delete a permission from a table
#' 
#' Delete a permission that was applied on a table. Silently returns when there is no such permission.
#' 
#' @family table functions
#' @param opal Opal connection object.
#' @param project Project name where the table will be located.
#' @param table Destination table name.
#' @param subject A vector of subject identifiers: user names or group names (depending on the type).
#' @param type The type of subject: user (default) or group.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.table_perm_add(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user', 'view')
#' opal.table_perm(o, 'CNSIM', 'CNSIM1')
#' opal.table_perm_delete(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
opal.table_perm_delete <- function(opal, project, table, subject, type = "user") {
  if (!(tolower(type) %in% c("user", "group"))) {
    stop("Not a valid subject type: ", type)
  }
  if (length(subject)<1) {
    stop("At least one subject is required")
  }
  for (i in 1:length(subject)) {
    ignore <- opal.delete(opal, "project", project, "permissions", "table", table, query = list(principal = subject[i], type = toupper(type)))
  }
}

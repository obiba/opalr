#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
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
#' @param opal Opal connection object.
#' @param project Project name where the table is located.
#' @param table Table name from which the tibble should be extracted.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings Include the missing values (default is TRUE).
#' @examples 
#' \donttest{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' cqx <- harmo.table_get(o, "CPTP", "Cag_coreqx")
#' opal.logout(o)
#' }
#' @export
harmo.table_get <- function(opal, project, table, variables = NULL, missings = TRUE) {
  pb <- .newProgress(total = 5)
  .tickProgress(pb, tokens = list(what = paste0("Assigning ", project, ".", table)))
  opal.assign.table.tibble(opal, symbol = ".D", value = paste0(project, ".", table), variables = variables, missings = missings)

  .tickProgress(pb, tokens = list(what = paste0("Saving in R data file")))
  opal.assign.script(opal, ".file", quote(tempfile(tmpdir = getwd(), fileext = '.rda')))
  file <- opal.execute(opal, ".file")
  filename <- basename(file)
  opal.execute(opal, paste0("save(.D, file=.file)"))
  opal.symbol_rm(opal, ".D")
  opal.execute(opal, "gc()")

  .tickProgress(pb, tokens = list(what = paste0("Downloading R data file")))
  opalfile <- paste0("/home/", opal$username, "/", filename)
  opal.file_read(opal, filename, opalfile)
  opal.execute(opal, paste0("unlink(.file)"))
  opal.file_download(opal, opalfile)
  opal.file_rm(opal, opalfile)

  .tickProgress(pb, tokens = list(what = paste0("Loading R data file")))
  env <- new.env()
  load(filename, envir = env)
  unlink(filename)
  rval <- get(".D", envir = env)
  .tickProgress(pb, tokens = list(what = "Data loaded"))
  rval
}

#' Save a local tibble as a Opal table
#'
#' Upload a local tibble to the R server side through Opal, assign this tibble to the provided
#' symbol name and import it as a table into a Opal project.
#'
#' @param opal Opal connection object.
#' @param tibble The tibble object to be imported.
#' @param project Project name where the table will be located.
#' @param table Destination table name.
#' @param overwrite If the destination table already exists, it will be replaced (deleted and then
#' imported). Otherwise the table will be updated (data dictionaries merge may conflict). Default is TRUE.
#' @param force If the destination already exists, stop with an informative message if this flag is FALSE (default).
#' @param identifiers Name of the identifiers mapping to use when assigning entities to Opal.
#' @param policy Identifiers policy: 'required' (each identifiers must be mapped prior importation (default)), 'ignore' (ignore unknown identifiers) and 'generate' (generate a system identifier for each unknown identifier).
#' @param id.name The name of the column representing the entity identifiers. Default is 'id'.
#' @param type Entity type (what the data are about). Default is 'Participant'
#' @return An invisible logical indicating whether the destination table exists.
#' @examples 
#' \donttest{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' cqx <- harmo.table_get(o, "CPTP", "Cag_coreqx")
#' # do some (meta)data transformations
#' harmo.table_get(o, cqx, "CPTP", "Cag_coreqx", overwrite = TRUE, force = TRUE)
#' opal.logout(o)
#' }
#' @export
#' @import jsonlite
harmo.table_save <- function(opal, tibble, project, table, overwrite = TRUE, force = FALSE, identifiers=NULL, policy='required', id.name='id', type='Participant') {
  if (!("tbl" %in% class(tibble))) {
    stop("The tibble parameter must be a tibble.")
  }
  pb <- .newProgress(total = 7)
  .tickProgress(pb, tokens = list(what = paste0("Checking ", project, " project")))
  if (table %in% opal.datasource(opal, project)$table) {
    if (overwrite) {
      if (!force) {
        stop("Destination table needs to be deleted. Use 'force' parameter to proceed.")
      }
      .tickProgress(pb, tokens = list(what = paste0("Deleting ", table, " from ", project)))
      opal.delete(opal, "datasource", project, "table", table)
    } else {
      if (!force) {
        stop("Destination table will be updated. There could be data dictionary conflicts. Use 'force' parameter to proceed.")
      }
      .tickProgress(pb, tokens = list(what = paste0("Merging with ", table, " from ", project)))
    }
  } else {
    .tickProgress(pb, tokens = list(what = paste0("Creating table ", table, " in ", project)))
  }
  
  .tickProgress(pb, tokens = list(what = paste0("Saving in R data file")))
  file <- tempfile(fileext = ".rda")
  save(tibble, file = file)
  
  .tickProgress(pb, tokens = list(what = paste0("Uploading R data file")))
  opal.file_upload(opal, file, "/tmp")
  filename <- basename(file)
  unlink(file)
  opal.file_write(opal, paste0("/tmp/", filename))
  opal.file_rm(opal, paste0("/tmp/", filename))
  
  .tickProgress(pb, tokens = list(what = paste0("Loading R data file")))
  opal.execute(opal, paste0("load(file='", filename, "')"))
  opal.execute(opal, paste0("unlink('", filename, "')"))
  opal.execute(opal, paste0("assign('", table, "', tibble)"))
  opal.execute(opal, paste0("rm(tibble)"))
  
  .tickProgress(pb, tokens = list(what = paste0("Importing ", table, " into ", project)))
  opal.symbol_import(opal, table, project = project, identifiers = identifiers, policy = policy, id.name = id.name, type = type)
  
  tryCatch(opal.symbol_rm(opal, table))
  opal.execute(opal, "gc()")
  rval <- table %in% opal.datasource(opal, project)$table
  .tickProgress(pb, tokens = list(what = "Save completed"))
  invisible(rval)
}


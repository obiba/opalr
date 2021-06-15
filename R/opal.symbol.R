#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' List R symbols
#' 
#' Get the R symbols available in the remote R session.
#' 
#' @family symbol functions
#' @param opal Opal object.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.symbols(o)
#' opal.logout(o)
#' }
#' @export
opal.symbols <- function(opal) {
  ignore <- .getRSessionId(opal)
  opal.get(opal, "r", "session", opal$rid, "symbols", acceptType = "application/octet-stream")
}

#' Remove a R symbol
#' 
#' Remove a symbol from the remote R session.
#' 
#' @family symbol functions
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.symbol_rm(o, 'D')
#' opal.logout(o)
#' }
#' @export
opal.symbol_rm <- function(opal, symbol) {
  ignore <- .getRSessionId(opal)
  tryCatch(opal.delete(opal, "r", "session", opal$rid, "symbol", symbol), error=function(e){})
}

#' Remove a R symbol (deprecated)
#' 
#' Remove a symbol from the current R session. Deprecated: see opal.symbol_rm function instead.
#' 
#' @family symbol functions
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.rm(o, 'D')
#' opal.logout(o)
#' }
#' @export
opal.rm <- function(opal, symbol) {
  opal.symbol_rm(opal, symbol)
}

#' Save a tibble
#' 
#' Save a tibble identified by symbol as a file of format SAS, SPSS, Stata, CSV or TSV in the remote R session working directory.
#' 
#' @family symbol functions
#' @param opal Opal object.
#' @param symbol Name of the R symbol representing a tibble.
#' @param destination The path of the file in the R session workspace. Supported file extensions are: 
#' .sav (SPSS), .zsav (compressed SPSS), .sas7bdat (SAS), .xpt (SAS Transport), .dta (Stata), 
#' .csv (comma separated values), .tsv (tab separated values). 
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.symbol_save(o, 'D', 'test.sav')
#' opal.logout(o)
#' }
#' @export
opal.symbol_save <- function(opal, symbol, destination) {
  ignore <- .getRSessionId(opal)
  if (!is.na(opal$version) && opal.version_compare(opal,"2.8")<0) {
    stop("Saving tibble in a file is not available for opal ", opal$version, " (2.8.0 or higher is required)")
  } else {
    if (is.null(destination)) {
      stop("Destination file path is missing or empty.")
    }
    if (endsWith(destination, ".zsav") || endsWith(destination, ".xpt")) {
      if (!is.na(opal$version) && opal.version_compare(opal,"2.14")<0) {
        stop("Saving tibble in a compressed SPSS or SAS Transport file is not available for opal ", opal$version, " (2.14.0 or higher is required)")
      }
    }
    query <- list(destination=destination)
    res <- opal.put(opal, "r", "session", opal$rid, "symbol", symbol, "_save", query=query)
  }
}

#' Import a tibble
#' 
#' Import a tibble identified by the symbol as a table in Opal. This operation creates an importation task 
#' in Opal that can be followed (see tasks related functions).
#' 
#' @family symbol functions
#' @param opal Opal object.
#' @param symbol Name of the R symbol representing a tibble.
#' @param project Name of the project into which the data are to be imported.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to Opal.
#' @param policy Identifiers policy: 'required' (each identifiers must be mapped prior importation (default)), 'ignore' (ignore unknown identifiers) and 'generate' (generate a system identifier for each unknown identifier). 
#' @param id.name The name of the column representing the entity identifiers. Default is 'id'.
#' @param type Entity type (what the data are about). Default is 'Participant'.
#' @param wait Wait for import task completion. Default is TRUE.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.symbol_import(o, 'D', 'test')
#' opal.logout(o)
#' }
#' @export
opal.symbol_import <- function(opal, symbol, project, identifiers=NULL, policy='required', id.name='id', type='Participant', wait=TRUE) {
  rid <- .getRSessionId(opal)
  if (!is.na(opal$version) && opal.version_compare(opal,"2.8")<0) {
    warning("Importing tibble in a table not available for opal ", opal$version, " (2.8.0 or higher is required)")
  } else {
    # create a transient datasource
    dsFactory <- list(session=rid, symbol=symbol, entityType=type, idColumn=id.name)
    if (is.null(identifiers)) {
      dsFactory <- paste0('{"Magma.RSessionDatasourceFactoryDto.params": ', .listToJson(dsFactory), '}') 
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
      dsFactory <- paste0('{"Magma.RSessionDatasourceFactoryDto.params": ', .listToJson(dsFactory), ', "idConfig":', .listToJson(idConfig),'}')
    }
    created <- opal.post(opal, "project", project, "transient-datasources", body=dsFactory, contentType="application/json")
    # launch a import task
    importCmd <- list(destination=project, tables=list(paste0(created$name, '.', symbol)))
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
          command <- opal.get(opal, "shell", "command", task)
          status <- command$status
        }
        if (is.element(status, c('FAILED','CANCELED'))) {
          stop(paste0('Import of "', symbol, '" ended with status: ', status), call.=FALSE)
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
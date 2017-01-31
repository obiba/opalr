#-------------------------------------------------------------------------------
# Copyright (c) 2016 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the R symbols available in the remote R session.
#' 
#' @title List R symbols
#' 
#' @param opal Opal object.
#' @export
opal.symbols <- function(opal) {
  ignore <- .getRSessionId(opal)
  .get(opal, "r", "session", opal$rid, "symbols")
}

#' Remove a symbol from the current R session.
#' 
#' @title Remove a R symbol
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @export
opal.symbol_rm <- function(opal, symbol) {
  ignore <- .getRSessionId(opal)
  tryCatch(.delete(opal, "r", "session", opal$rid, "symbol", symbol), error=function(e){})
}

#' Remove a symbol from the current R session. Deprecated: see opal.symbol_rm function instead.
#' 
#' @title Remove a R symbol (deprecated)
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @export
opal.rm <- function(opal, symbol) {
  opal.symbol_rm(opal, symbol)
}

#' Import a data.frame symbol as a table in opal project.
#' 
#' @title Import a data.frame
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @param project Opal project into the table is to be imported.
#' @export
opal.import <- function(opal, symbol, project) {
  ignore <- .getRSessionId(opal)
  if (!is.na(opal$version) && opal.version_compare(opal,"2.8")<0) {
    warning("Import from data.frame or tibble not available for opal ", opal$version, " (2.8.0 or higher is required)")
  } else {
    query <- list(project=project)
    res <- .put(opal, "r", "session", opal$rid, "symbol", symbol, "_import", query=query)
  }
}

#' Export a Opal table as a tibble identified by a R symbol in the current R session.
#' 
#' @title Data assignment
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @param table The fully qualified name of a table in Opal.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param multilines When there are value sequences, if multilines is TRUE (default) there will be one row per occurrence. Otherwise a list column will be produced, with one row per entity.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param id.name Add a vector with the given name representing the entity identifiers (from Opal 2.6). Default is NULL.
#' @param updated.name Add a vector with the given name representing the creation and last update timestamps (from Opal 2.6). Default is NULL.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @export
opal.export <- function(opal, symbol, table, variables=NULL, multilines=TRUE, identifiers=NULL, id.name='id', updated.name=NULL, async=FALSE) {
  ignore <- .getRSessionId(opal)
  if (!is.na(opal$version) && opal.version_compare(opal,"2.8")<0) {
    warning("Export to tibble not available for opal ", opal$version, " (2.8.0 or higher is required)")
  } else {
    body <- table
    variableFilter <- NULL
    if (is.character(variables)) {
      if (length(variables) > 1) {
        # case variables is a char vector of variable names
        variableFilter <- as.list(variables)
      } else {  
        # case variables is a magma script
        variableFilter <- variables
      }
    } else if (is.list(variables)) {
      # case variables is a list of variable names
      variableFilter <- variables
    }
    
    # make a script from a list of variable names
    if (is.list(variableFilter)) {
      variableFilter <- paste("name().any('", paste(variableFilter, sep="", collapse="','"), "')", sep="")
    }
    query <- list(variables=variableFilter)
    if (multilines) {
      query["multilines"] <- "true"
    } else {
      query["multilines"] <- "false"
    }
    if (!is.null(identifiers)) {
      query["identifiers"] <- identifiers
    }
    if (!is.null(id.name)) {
      query["id"] <- id.name
    }
    if (!is.null(updated.name)) {
      query["updated"] <- updated.name
    }
    if (async) {
      query["async"] <- "true"
    }
    res <- .put(opal, "r", "session", opal$rid, "symbol", symbol, "_export", body=body, query=query)
  }
}

#' Save the tibble identified by the symbol in a file of the R session workspace 
#' 
#' @title Data assignment
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @param destination The path of the file in the Opal file system. Supported file extensions are: .sav (SPSS), .sas7bdat (SAS), .dta (Stata). 
#' @export
opal.symbol_save <- function(opal, symbol, destination) {
  ignore <- .getRSessionId(opal)
  if (!is.na(opal$version) && opal.version_compare(opal,"2.8")<0) {
    warning("Saving tibble in a file not available for opal ", opal$version, " (2.8.0 or higher is required)")
  } else {
    if (is.null(destination)) {
      stop("Destination file path is missing or empty.")
    }
    query <- list(destination=destination)
    res <- .put(opal, "r", "session", opal$rid, "symbol", symbol, "_save", query=query)
  }
}
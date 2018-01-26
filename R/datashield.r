#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Aggregates the expression result using the specified aggregation method in the current Datashield session.
#' 
#' @title Data aggregation
#' 
#' @param opal Opal object or list of opal objects.
#' @param expr Expression to evaluate.
#' @param async R script is executed asynchronously within the session (default is TRUE).
#' @param wait Wait for the R script asynchronously executed to complete (makes sense only with async=TRUE).
#' @return The result of the aggregation or the R command ID if the async flag is TRUE and if the wait flag is FALSE and if Opal version is at least 2.1.
#' @rdname datashield.aggregate
#' @export
datashield.aggregate=function(opal, expr, async=TRUE, wait=TRUE) {
  UseMethod('datashield.aggregate');
}

#' @rdname datashield.aggregate
#' @export
datashield.aggregate.opal=function(opal, expr, async=TRUE, wait=TRUE) {
  expression = expr
  # convert a call to a string
  if(is.language(expr)) {
    expression <- .deparse(expr)
  } else if(! is.character(expr) ) {
    stop("Invalid expression type: '", class(expr), "'. Expected a call or character vector.")
  }
  
  query <- list()
  if(async) {
    query["async"] <- "true"
  }
  ignore <- .getDatashieldSessionId(opal)
  res <- .post(opal, "datashield", "session", opal$rid, "aggregate", query=query, body=expression, contentType="application/x-rscript")
  
  if (async && wait) {
    res <- datashield.command_result(opal, res, wait=TRUE)
  }
  return(res)
}

#' @rdname datashield.aggregate
#' @export
datashield.aggregate.list=function(opal, expr, async=TRUE, wait=TRUE) {
  res <- lapply(opal, FUN=datashield.aggregate.opal, expr, async=async, wait=FALSE)
  if (async && wait) {
    res <- datashield.command_result(opal, res, wait=TRUE)
  }
  return(res)
}

#' Assign a Opal value to a R symbol in the current Datashield session.
#' 
#' @title Data assignment
#' 
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value Fully qualified name of a variable or a table in Opal (must be the same in each Opal) or a R expression with allowed assign functions calls.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param async R script is executed asynchronously within the session (default is TRUE).
#' @param wait Wait for the R script asynchronously executed to complete (makes sense only with async=TRUE).
#' @return The R command ID if the async flag is TRUE and if the wait flag is FALSE and if Opal version is at least 2.1, NULL otherwise.
#' @rdname datashield.assign
#' @examples 
#' \dontrun{
#' # assign a list of variables from table HOP of opal object o
#' datashield.assign(o, symbol="D", value="demo.HOP", variables=list("GENDER","LAB_GLUC"))
#' 
#' # assign all the variables matching 'LAB' from table HOP of opal object o
#' datashield.assign(o, symbol="D", value="demo.HOP", variables="name().matches('LAB_')")
#' }
#' @export
datashield.assign=function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE) {
  UseMethod('datashield.assign');
}

#' @rdname datashield.assign
#' @export
datashield.assign.opal=function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE) {
  if(is.language(value) || is.function(value)) {
    contentType <- "application/x-rscript"
    body <- .deparse(value)
    query <- list()
  } else if(is.character(value)) {
    contentType <- "application/x-opal"
    body <- value
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
    query <- list(missings=missings, variables=variableFilter)
    if (!is.null(identifiers)) {
      query["identifiers"] <- identifiers
    }
  } else {
    stop("Invalid value type: '", class(value), "'. Use quote() to protect from early evaluation.")
  }
  
  if(async) {
    query["async"] <- "true"
  }
  ignore <- .getDatashieldSessionId(opal)
  res <- .put(opal, "datashield", "session", opal$rid, "symbol", symbol, query=query, body=body, contentType=contentType)
  
  if (async && wait) {
    datashield.command(opal, res, wait=TRUE)
    datashield.command_rm(opal, res)
    res <- raw(0)
  }
  invisible(res)
}

#' @rdname datashield.assign
#' @export
datashield.assign.list=function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=TRUE, wait=TRUE) {
  res <- lapply(opal, FUN=datashield.assign.opal, symbol, value, variables=variables, missings=missings, identifiers=identifiers, async=async, wait=FALSE)
  if (async && wait) {
    datashield.command(opal, res, wait=TRUE)
    datashield.command_rm(opal, res)
  }
  invisible(raw(0))
}

#' Extract R session Id from opal object, create a new Datashield R session if not found.
#' @keywords internal
.getDatashieldSessionId <- function(opal) {
  if(is.null(opal$rid)) {
    opal$rid <- .newDatashieldSession(opal, restore=opal$restore)
  }
  if(is.null(opal$rid)) {
    stop("Remote Datashield R session not available")
  }
  return(opal$rid)
}

#' Create a new Datashield R session in Opal.
#' @keywords internal
.newDatashieldSession <- function(opal, restore=NULL) {
  query <- list()
  if (!is.null(restore)) {
    query <- list(restore=restore)  
  }
  res <- .extractJsonField(.post(opal, "datashield", "sessions", query=query), c("id"), isArray=FALSE)
  return(res$id)
}

#' Remove a Datashield R session in Opal.
#' @keywords internal
.rmDatashieldSession <- function(opal, save=NULL) {
  query <- list()
  if (is.character(save)) {
    query <- list(save=save)
  }
  try(.delete(opal, "datashield", "session", opal$rid, query=query), silent=TRUE)
}
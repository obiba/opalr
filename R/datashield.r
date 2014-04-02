#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Create a new Datashield session in Opal(s).
#' 
#' @title New Datashield session
#' 
#' @param opals Opal object or list of opal objects.
#' @return The identifier of the session created.
#' @rdname datashield.newSession
#' @export
datashield.newSession <- function(opals) {
  UseMethod('datashield.newSession');
}

#' @rdname datashield.newSession
#' @method datashield.newSession opal
#' @S3method datashield.newSession opal
datashield.newSession.opal <- function(opal) {
  opal:::.extractJsonField(.post(opal, "datashield", "sessions"), c("id"), isArray=FALSE)
}

#' @rdname datashield.newSession
#' @method datashield.newSession list
#' @S3method datashield.newSession list
datashield.newSession.list <- function(opals) {
  lapply(opals, FUN=datashield.newSession.opal)
}

#' Set current Datashield session in Opal(s).
#' 
#' @title Set Datashield session
#' 
#' @param opals Opal object or list of opal objects.
#' @param sessionId The identifier of the session.
#' @rdname datashield.setSession
#' @export
datashield.setSession <- function(opal, sessionId) {
  UseMethod('datashield.setSession');
}

#' @rdname datashield.setSession
#' @method datashield.setSession opal
#' @S3method datashield.setSession opal
datashield.setSession.opal <- function(opal, sessionId) {
  opal:::.put(opal, "datashield", "session", sessionId, "current");
}

#' @rdname datashield.setSession
#' @method datashield.setSession list
#' @S3method datashield.setSession list
datashield.setSession.list <- function(opals, sessionId) {
  lapply(opals, FUN=datashield.setSession.opal, sessionId)
}

#' Remove Datashield sessions in Opal(s).
#' 
#' @title Remove Datashield sessions
#' 
#' @param opals Opal object or list of opal objects.
#' @rdname datashield.rmSessions
#' @export
datashield.rmSessions <- function(opal) {
  UseMethod('datashield.rmSessions');
}

#' @rdname datashield.rmSessions
#' @method datashield.rmSessions opal
#' @S3method datashield.rmSessions opal
datashield.rmSessions.opal <- function(opal) {
  opal:::.delete(opal, "datashield", "sessions");
}

#' @rdname datashield.rmSessions
#' @method datashield.rmSessions list
#' @S3method datashield.rmSessions list
datashield.rmSessions.list <- function(opals) {
  lapply(opals, FUN=datashield.rmSessions.opal)
}

#' Aggregates the expression result using the specified aggregation method in the current Datashield session.
#' 
#' @title Data aggregation
#' 
#' @param opals Opal object or list of opal objects.
#' @param expr Expression to evaluate.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1, older versions will return the result).
#' @return The result of the aggregation or the R command ID if the async flag is TRUE and if Opal version is at least 2.1.
#' @rdname datashield.aggregate
#' @export
datashield.aggregate=function(opals, expr, async=FALSE) {
  UseMethod('datashield.aggregate');
}

#' @rdname datashield.aggregate
#' @method datashield.aggregate opal
#' @S3method datashield.aggregate opal
datashield.aggregate.opal=function(opal, expr, async=FALSE) {
  expression = expr
  # convert a call to a string
  if(is.language(expr)) {
    expression <- .deparse(expr)
  } else if(! is.character(expr) ) {
    stop("Invalid expression type: '", class(value), "'. Expected a call or character vector.")
  }
  
  query <- list()
  if(async) {
    query["async"] <- "true"
  }
  
  opal:::.post(opal, "datashield", "session", "current", "aggregate", query=query, body=expression, contentType="application/x-rscript")
}

#' @rdname datashield.aggregate
#' @method datashield.aggregate list
#' @S3method datashield.aggregate list
datashield.aggregate.list=function(opals, expr, async=FALSE) {
  lapply(opals, FUN=datashield.aggregate.opal, expr, async=async)
}

#' Assign a Opal value to a R symbol in the current Datashield session.
#' 
#' @title Data assignment
#' 
#' @param opals Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value Fully qualified name of a variable or a table in Opal (must be the same in each Opal) or a R expression with allowed assign functions calls.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @return The R command ID if the async flag is TRUE and if Opal version is at least 2.1, NULL otherwise.
#' @rdname datashield.assign
#' @examples {
#' # assign a list of variables from table HOP of opal object o
#' datashield.assign(o, symbol="D", value="demo.HOP", variables=list("GENDER","LAB_GLUC"))
#' 
#' # assign all the variables matching 'LAB' from table HOP of opal object o
#' datashield.assign(o, symbol="D", value="demo.HOP", variables="name().matches('LAB_')")
#' }
#' @export
datashield.assign=function(opals, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=FALSE) {
  UseMethod('datashield.assign');
}

#' @rdname datashield.assign
#' @method datashield.assign opal
#' @S3method datashield.assign opal
datashield.assign.opal=function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=FALSE) {
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
  
  res <- opal:::.put(opal, "datashield", "session", "current", "symbol", symbol, query=query, body=body, contentType=contentType)
}

#' @rdname datashield.assign
#' @method datashield.assign list
#' @S3method datashield.assign list
datashield.assign.list=function(opals, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=FALSE) {
  res <- lapply(opals, FUN=datashield.assign.opal, symbol, value, variables=variables, missings=missings, identifiers=identifiers, async=async)
}

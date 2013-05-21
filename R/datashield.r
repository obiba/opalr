#-------------------------------------------------------------------------------
# Copyright (c) 2013 OBiBa. All rights reserved.
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
#' @method datashield.setession list
#' @S3method datashield.setSession list
datashield.setSession.list <- function(opals, sessionId) {
  lapply(opals, FUN=datashield.setSession.opal, sessionId)
}

#' Aggregates the expression result using the specified aggregation method in the current Datashield session.
#' 
#' @title Data aggregation
#' 
#' @param opals Opal object or list of opal objects.
#' @param expr Expression to evaluate.
#' @rdname datashield.aggregate
#' @export
datashield.aggregate=function(opals, expr) {
  UseMethod('datashield.aggregate');
}

#' @rdname datashield.aggregate
#' @method datashield.aggregate opal
#' @S3method datashield.aggregate opal
datashield.aggregate.opal=function(opal, expr) {
  expression = expr
  # convert a call to a string
  if(is.language(expr)) {
    expression <- .deparse(expr)
  } else if(! is.character(expr) ) {
    return(print(paste("Invalid expression type: '", class(value), "'. Expected a call or character vector.", sep="")))
  }
  
  opal:::.post(opal, "datashield", "session", "current", "aggregate", body=expression, contentType="application/x-rscript")
}

#' @rdname datashield.aggregate
#' @method datashield.aggregate list
#' @S3method datashield.aggregate list
datashield.aggregate.list=function(opals, expr) {
  lapply(opals, FUN=datashield.aggregate.opal, expr)
}

#' Assign a Opal value to a R symbol in the current Datashield session.
#' 
#' @title Data assignment
#' 
#' @param opals Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value Fully qualified name of a variable or a table in Opal (must be the same in each Opal) or a R expression with allowed assign functions calls.
#' @param variableFilter Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @rdname datashield.assign
#' @export
datashield.assign=function(opals, symbol, value, variableFilter=NULL, missings=FALSE) {
  UseMethod('datashield.assign');
}

#' @rdname datashield.assign
#' @method datashield.assign opal
#' @S3method datashield.assign opal
datashield.assign.opal=function(opal, symbol, value, variableFilter=NULL, missings=FALSE) {
  if(is.language(value) || is.function(value)) {
    contentType <- "application/x-rscript"
    body <- .deparse(value)
    query <- list()
  } else if(is.character(value)) {
    contentType <- "application/x-opal"
    body <- value
    query <- list(missings=missings, variables=variableFilter)
  } else {
    return(print(paste("Invalid value type: '", class(value), "'. Use quote() to protect from early evaluation.", sep="")))
  }
  
  resp <- opal:::.put(opal, "datashield", "session", "current", "symbol", symbol, body=body, contentType=contentType, query=query)
}

#' @rdname datashield.assign
#' @method datashield.assign list
#' @S3method datashield.assign list
datashield.assign.list=function(opals, ...) {
  resp <- lapply(opals, FUN=datashield.assign.opal, ...)
}

#' Get the R symbols available after the datashield.assign calls in the current Datashield session.
#' 
#' @title List R symbols
#' 
#' @param opal Opal object or list of opal objects.
#' @rdname datashield.symbols
#' @export
datashield.symbols=function(object, ...) {
  UseMethod('datashield.symbols');
}

#' @rdname datashield.symbols
#' @method datashield.symbols opal
#' @S3method datashield.symbols opal
datashield.symbols.opal=function(opal) {
  opal:::.get(opal, "datashield", "session", "current", "symbols")
}

#' @rdname datashield.symbols
#' @method datashield.symbols list
#' @S3method datashield.symbols list
datashield.symbols.list=function(opals) {
  lapply(opals, FUN=datashield.symbols.opal)
}

#' Remove a symbol from the current Datashield session.
#' 
#' @title Remove a R symbol
#' 
#' @param opals Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @export
datashield.rm=function(opals, symbol) {
  UseMethod('datashield.rm');
}

#' @rdname datashield.rm
#' @method datashield.rm opal
#' @S3method datashield.rm opal
datashield.rm.opal=function(opal, symbol) {
  opal:::.delete(opal, "datashield", "session", "current", "symbol", symbol)
}

#' @rdname datashield.rm
#' @method datashield.rm list
#' @S3method datashield.rm list
datashield.rm.list=function(opals, symbol) {
  lapply(opals, FUN=datashield.rm.opal, symbol)
}

#' Get available Datashield methods of a given type.
#' 
#' @title List Datashield methods
#' 
#' @param opals Opal object or list of opal objects.
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @export
datashield.methods=function(opals, type="aggregate") {
  UseMethod('datashield.methods');
}

#' @rdname datashield.methods
#' @method datashield.methods opal
#' @S3method datashield.methods opal
datashield.methods.opal=function(opal, type="aggregate") {
  opal:::.get(opal, "datashield", "env", type, "methods")
}

#' @rdname datashield.methods
#' @method datashield.methods list
#' @S3method datashield.methods list
datashield.methods.list=function(opals, type="aggregate") {
  lapply(opals, FUN=datashield.methods.opal, type)
}
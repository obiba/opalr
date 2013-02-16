#-------------------------------------------------------------------------------
# Copyright (c) 2011 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Create a new Datashield session in Opal(s).
#' 
#' @param opal opal object or list of opal objects
#' @rdname datashield.newSession
#' @export
#' 
datashield.newSession <- function(opal) {
  UseMethod('datashield.newSession');
}

#' Create a new Datashield session in Opal.
#'
#' @param opal opal object
#' @rdname datashield.newSession
#' @method datashield.newSession opal
#' @S3method datashield.newSession opal
#' 
datashield.newSession.opal <- function(opal) {
  .extractJsonField(.post(opal, "datashield", "sessions"), c("id"), isArray=FALSE)
}

#' Create a new Datashield session in each Opal.
#'
#' @param opals opal objects list
#' @rdname datashield.newSession
#' @method datashield.newSession list
#' @S3method datashield.newSession list
#' 
datashield.newSession.list <- function(opals) {
  lapply(opals, FUN=datashield.newSession.opal)
}

#' Set current Datashield session in Opal.
#' 
#' @param opal
#' @export
#' 
datashield.setSession <- function(opal, ...) {
  UseMethod('datashield.setSession');
}

datashield.setSession.opal <- function(opal, sessionId) {
  .put(opal, "datashield", "session", sessionId, "current");
}

datashield.setSession.list <- function(opals, sessionId) {
  lapply(opals, FUN=datashield.setSession.opal, sessionId)
}

#' Aggregate.
#' 
#' @param opal
#' @param expr
#' @export
#' 
datashield.aggregate=function(object, ...) {
  UseMethod('datashield.aggregate');
}

# Inner methods that sends a script, and aggregates the result using the specified aggregation method
datashield.aggregate.opal=function(opal, expr) {
  expression = expr
  # convert a call to a string
  if(is.language(expr)) {
    expression <- .deparse(expr)
  } else if(! is.character(expr) ) {
    return(print(paste("Invalid expression type: '", class(value), "'. Expected a call or character vector.", sep="")))
  }
  
  .post(opal, "datashield", "session", "current", "aggregate", body=expression, contentType="application/x-rscript")
}

datashield.aggregate.list=function(opals, expr) {
  lapply(opals, FUN=datashield.aggregate.opal, expr)
}

#' Assign a value to a symbol in Opal(s).
#' 
#' @param opal
#' @param symbol
#' @param value
#' @rdname datashield.assign
#' @export
#' 
datashield.assign=function(object, ...) {
  UseMethod('datashield.assign');
}

#' Assign a value to a symbol in Opal.
#'
#' @param opal opal object
#' @param symbol
#' @param value
#' @rdname datashield.assign
#' @method datashield.assign opal
#' @S3method datashield.assign opal
#'
datashield.assign.opal=function(opal, symbol, value) {
  if(is.language(value) || is.function(value)) {
    contentType <- "application/x-rscript"
    body <- .deparse(value)
  } else if(is.character(value)) {
    contentType <- "application/x-opal"
    body <- value
  } else {
    return(print(paste("Invalid value type: '", class(value), "'. Use quote() to protect from early evaluation.", sep="")))
  }
  
  .put(opal, "datashield", "session", "current", "symbol", symbol, body=body, contentType=contentType)
  # Return the new symbols length
  #datashield.length(opal, as.symbol(symbol))
}


#' Assign a value to a symbol in each Opal.
#'
#' @param opals opal objects list
#' @param symbol
#' @param value
#' @rdname datashield.assign
#' @method datashield.assign list
#' @S3method datashield.assign list
#' 
datashield.assign.list=function(opals, ...) {
  lapply(opals, FUN=datashield.assign.opal, ...)
}

#' Get symbols from Opal(s).
#' 
#' @param opal
#' @rdname datashield.symbols
#' @export
#' 
datashield.symbols=function(object, ...) {
  UseMethod('datashield.symbols');
}

#' Get symbols from Opal.
#' 
#' @param opal
#' @rdname datashield.symbols
#' @method datashield.symbols opal
#' @S3method datashield.symbols opal
#' 
datashield.symbols.opal=function(opal) {
  .get(opal, "datashield", "session", "current", "symbols")
}

#' Get symbols from each Opal.
#' 
#' @param opal
#' @rdname datashield.symbols
#' @method datashield.symbols list
#' @S3method datashield.symbols list
#' 
datashield.symbols.list=function(opals) {
  lapply(opals, FUN=datashield.symbols.opal)
}

#' Remove a symbol.
#' 
#' @param opal
#' @param symbol
#' @export
#' 
datashield.rm=function(object, ...) {
  UseMethod('datashield.rm');
}

datashield.rm.list=function(opals, ...) {
  lapply(opals, FUN=datashield.rm.opal, ...)
}

datashield.rm.opal=function(opal, symbol) {
  .delete(opal, "datashield", "session", "current", "symbol", symbol)
}

.deparse <- function(expr) {
  expression <- deparse(expr)
  if(length(expression) > 1) {
    expression = paste(expression, collapse='\n')
  }
  expression
}
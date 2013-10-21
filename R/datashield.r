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
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @rdname datashield.assign
#' @examples {
#' # assign a list of variables from table HOP of opal object o
#' datashield.assign(o, symbol="D", value"demo.HOP", variables=list("GENDER","LAB_GLUC"))
#' 
#' # assign all the variables matching 'LAB' from table HOP of opal object o
#' datashield.assign(o, symbol="D", value"demo.HOP", variables="name().matches('LAB_')")
#' }
#' @export
datashield.assign=function(opals, symbol, value, variables=NULL, missings=FALSE) {
  UseMethod('datashield.assign');
}

#' @rdname datashield.assign
#' @method datashield.assign opal
#' @S3method datashield.assign opal
datashield.assign.opal=function(opal, symbol, value, variables=NULL, missings=FALSE) {
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
  res <- opal:::.delete(opal, "datashield", "session", "current", "symbol", symbol)
}

#' @rdname datashield.rm
#' @method datashield.rm list
#' @S3method datashield.rm list
datashield.rm.list=function(opals, symbol) {
  res <- lapply(opals, FUN=datashield.rm.opal, symbol)
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
  rlist <- opal:::.get(opal, "datashield", "env", type, "methods")
  name <- lapply(rlist,function(m){
    m$name
  })
  t <- lapply(rlist,function(m){
    type
  })
  class <- lapply(rlist,function(m){
    if (is.null(m$DataShield.RFunctionDataShieldMethodDto.method$func)) {
      "script"
    } else {
      "function"
    }
  })
  value <- lapply(rlist,function(m){
    val <- m$DataShield.RFunctionDataShieldMethodDto.method$func
    if (is.null(val)) {
      val <- m$DataShield.RScriptDataShieldMethodDto.method$script
    }
    val
  })
  rval <- data.frame(unlist(name), unlist(t), unlist(class), unlist(value))
  colnames(rval) <- c("name","type", "class", "value")
  rval
}

#' @rdname datashield.methods
#' @method datashield.methods list
#' @S3method datashield.methods list
datashield.methods.list=function(opals, type="aggregate") {
  lapply(opals, FUN=datashield.methods.opal, type)
}


#' Get a Datashield method of a given type by its name.
#' 
#' @title Get Datashield method by its name
#' 
#' @param opals Opal object or list of opal objects.
#' @param name Name of the method
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @export
datashield.method=function(opals, name, type="aggregate") {
  UseMethod('datashield.method');
}

#' @rdname datashield.method
#' @method datashield.method opal
#' @S3method datashield.method opal
datashield.method.opal=function(opal, name, type="aggregate") {
  # TODO this request is currently not accessible to ds user
  #opal:::.get(opal, "datashield", "env", type, "method", name)
  ms <- datashield.methods(opal, type);
  rval <- ms[ms$name == name,]
  if (nrow(rval) > 0) {
    # TODO there is certainly a simpler way to this... 
    rval <- list(name=as.character(rval$name), type=as.character(rval$type), class=as.character(rval$class), value=as.character(rval$value))
  } else {
    rval <- NULL
  }
  rval
}

#' @rdname datashield.method
#' @method datashield.method list
#' @S3method datashield.method list
datashield.method.list=function(opals, name, type="aggregate") {
  lapply(opals, FUN=datashield.method.opal, name, type)
}

#' Check existence of a Datashield method of any type by its name.
#' 
#' @title Check existence of a Datashield method by its name
#' 
#' @param opals Opal object or list of opal objects.
#' @param name Name of the method
#' @export
datashield.has_method=function(opals, name) {
  UseMethod('datashield.has_method');
}

#' @rdname datashield.has_method
#' @method datashield.has_method opal
#' @S3method datashield.has_method opal
datashield.has_method.opal=function(opal, name) {
  rval <- !is.null(datashield.method(opal,name, type="aggregate"))
  if (!rval) {
    rval <- !is.null(datashield.method(opal,name, type="assign"))
  }
  rval
}

#' @rdname datashield.has_method
#' @method datashield.has_method list
#' @S3method datashield.has_method list
datashield.has_method.list=function(opals, name) {
  lapply(opals, FUN=datashield.has_method.opal, name)
}
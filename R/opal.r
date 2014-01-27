#-------------------------------------------------------------------------------
# Copyright (c) 2013 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------


#' Log in Opal(s).
#' 
#' @title Opal login
#' 
#' @return A opal object or a list of opal objects.
#' @param username User name in opal(s).
#' @param password User password in opal(s).
#' @param url Opal url or list of opal urls.
#' @param opts Curl options
#' @export
opal.login <- function(username = NULL,password = NULL,url,opts=list()) {
  if(is.list(url)){
    lapply(url, function(u){opal.login(username, password, u, opts=opts)})
  } else {
    .opal.login(username, password, url, opts)
  }
}

#' Clear the R sessions and logout from Opal(s).
#' 
#' @title Logout from Opal(s)
#' 
#' @param opals Opal object or a list of opals.
#' @export
opal.logout <- function(opals) {
  if (is.list(opals)) {
    res <- lapply(opals, function(o){opal.logout(o)})  
  } else {
    res <- opal.rmSessions(opals)
  }
}

#' Create a new R session in Opal.
#' 
#' @title New R session
#' 
#' @return The identifier of the session created.
#' @param opal Opal object.
#' @export
opal.newSession <- function(opal) {
  .extractJsonField(.post(opal, "r", "sessions"), c("id"), isArray=FALSE)
}

#' Get all session identifiers in Opal.
#' 
#' @title Get R sessions
#' 
#' @return The list of session identifiers.
#' @param opal Opal object.
#' @export
opal.getSessions <- function(opal) {
  .extractJsonField(.get(opal, "r", "sessions"), c("id"))
}

#' Set current R session in Opal.
#' 
#' @title Set R session
#' 
#' @return The identifier of the session created.
#' @param opal Opal object.
#' @param sessionId The identifier of the session.
#' @export
opal.setSession <- function(opal, sessionId) {
  .put(opal, "r", "session", sessionId, "current");
}

#' Remove R session from Opal.
#' 
#' @title Remove R session
#' 
#' @param opal Opal object.
#' @param sessionId The identifier of the session to be removed. If omitted, current session is removed.
#' @export
opal.rmSession <- function(opal, sessionId=NULL) {
  if (is.null(sessionId)) {
    .delete(opal, "r", "session", "current");
  } else {
    .delete(opal, "r", "session", sessionId);
  }
}

#' Remove all R sessions from Opal.
#' 
#' @title Remove all R sessions
#' 
#' @param opal Opal object.
#' @export
opal.rmSessions <- function(opal) {
  .delete(opal, "r", "sessions");
}

#' Get datasources from a opal.
#' 
#' @param opal Opal object.
#' @export
opal.datasources=function(opal) {
  .extractJsonField(.get(opal, "datasources"))
}

#' Get a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @export
opal.datasource=function(opal, datasource) {
  .extractJsonField(.get(opal, "datasource", datasource))
}

#' Get tables of a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @export
opal.tables <- function(opal, datasource) {
  .extractJsonField(.get(opal, "datasource", datasource, "tables"));
}

#' Get a table of a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param counts Flag to get the number of variables and entities.
#' @export
opal.table <- function(opal, datasource, table, counts=FALSE) {
  if (counts) {
    .extractJsonField(.get(opal, "datasource", datasource, "table", table, query=list(counts="true"))); 
  } else {
    .extractJsonField(.get(opal, "datasource", datasource, "table", table));  
  }  
}

#' Get variables of a table from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @export
opal.variables <- function(opal, datasource, table) {
  .extractJsonField(.get(opal, "datasource", datasource, "table", table, "variables"))
}

#' Get a variable of a table from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param variable Name of the variable in the table.
#' @export
opal.variable <- function(opal, datasource, table, variable) {
  .extractJsonField(.get(opal, "datasource", datasource, "table", table, "variable", variable))
}

#' Get a vector of values (for each locale) matching the given attribute namespace and name. Vector is null if no such attribute is found.
#' 
#' @param attributes A list of attributes, usually variable or category attributes.
#' @param namespace Optional attribute namespace.
#' @param name Required attribute name.
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

#' Execute a R script on Opal(s).
#' 
#' @title Execute a R script
#'
#' @param opals Opal object or list of opal objects.
#' @param script R script to execute.
#' @param session Execute in current R session (default is TRUE).
#' @export
opal.execute <- function(opal, script, session=TRUE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.execute(o, script, session=session)})
  } else {
    if (session) {
      .post(opal, "r", "session", "current", "execute", body=script, contentType="application/x-rscript")
    } else {
      .post(opal, "r", "execute", body=script, contentType="application/x-rscript")
    }
  }
}

#' Assign a Opal value to a R symbol in the current R session.
#' 
#' @title Data assignment
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @param value Fully qualified name of a variable or a table in Opal or a R expression.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0) 
#' #' @examples {
#' # assign a list of variables from table HOP of opal object o
#' opal.assign(o, symbol="D", value"demo.HOP", variables=list("GENDER","LAB_GLUC"))
#' 
#' # assign all the variables matching 'LAB' from table HOP of opal object o
#' opal.assign(o, symbol="D", value"demo.HOP", variables="name().matches('LAB_')")
#' }
#' @export
opal.assign <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL) {
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
    return(message(paste("Invalid value type: '", class(value), "'. Use quote() to protect from early evaluation.", sep="")))
  }
  
  .put(opal, "r", "session", "current", "symbol", symbol, body=body, contentType=contentType, query=query)
}

#' Get the R symbols available after the datashield.assign calls in the current Datashield session.
#' 
#' @title List R symbols
#' 
#' @param opal Opal object.
#' @export
opal.symbols <- function(opal) {
  .get(opal, "r", "session", "current", "symbols")
}

#' Remove a symbol from the current Datashield session.
#' 
#' @title Remove a R symbol
#' 
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @export
opal.rm <- function(opal, symbol) {
  .delete(opal, "r", "session", "current", "symbol", symbol)
}

#' Load dependencies.
.onLoad <- function(libname, pkgname) {
  require(RCurl)
  require(rjson)
}

#' Utility method to build urls. Concatenates all arguments and adds a '/' separator between each element
#' @keywords internal
.url <- function(opal, ..., query=list()) {
  .tmp <- paste(opal$url, "ws", paste(sapply(c(...), curlEscape), collapse="/"), sep="/")
  if(length(query)) {
    .params <- paste(sapply(names(query), function(id) paste(id, curlEscape(query[[id]]), sep = "="), simplify=FALSE), collapse = "&")
    .tmp <- paste(.tmp, .params, sep="?")
  }
  .tmp
}

#' Constructs the value for the Authorization header
#' @keywords internal
.authToken <- function(username, password) {
  paste("X-Opal-Auth", base64(paste(username, password, sep=":")))
}

#' Issues a request to opal for the specified resource
#' @keywords internal
.get <- function(opal, ..., query=list(), callback=NULL) {
  opts = curlOptions(httpget=TRUE, customrequest=NULL, .opts=opal$opts)
  .perform(opal, .url(opal, ..., query=query), opts, callback=callback)
}

#' Post a request w/o body content
#' @keywords internal
.post <- function(opal, ..., query=list(), body='', contentType='application/x-rscript', callback=NULL) {
  .nobody <- missing(body) || length(body) == 0
  if(.nobody) {
    # Act like a GET, but send a POST. This is required when posting without any body 
    opts = curlOptions(httpget=TRUE, customrequest="POST", .opts=opal$opts)
  } else {
    opts = curlOptions(post=TRUE, customrequest=NULL, httpheader=c(opal$opts$httpheader, 'Content-Type'=contentType), postfields=body, .opts=opal$opts)
  }
  .perform(opal, .url(opal, ..., query=query), opts, callback=callback)
}

#' Put a request w/o body content
#' @keywords internal
.put <- function(opal, ..., query=list(), body='', contentType='application/x-rscript', callback=NULL) {
  .nobody <- missing(body) || length(body) == 0
  if(.nobody) {
    # Act like a GET, but send a PUT. This is required when posting without any body 
    opts = curlOptions(httpget=TRUE, customrequest="PUT", .opts=opal$opts)
  } else {
    opts = curlOptions(post=TRUE, httpheader=c(opal$opts$httpheader, 'Content-Type'=contentType), postfields=body, customrequest="PUT", .opts=opal$opts)
  }
  .perform(opal, .url(opal, ..., query=query), opts, callback=callback)
}

#' Delete a resource
#' @keywords internal
.delete <- function(opal, ..., query=list(), callback=NULL) {
  # Act like a GET, but send a DELETE.
  opts = curlOptions(httpget=TRUE, customrequest="DELETE", .opts=opal$opts)
  .perform(opal, .url(opal, ..., query=query), opts, callback=callback)
}

#' Perform the request
#' @keywords internal
.perform <- function(opal, url, opts, callback=NULL) {
  opal$reader <- dynCurlReader(opal$curl)
  
  handle <- opal$curl
  curlPerform(url=url, .opts=opts, writefunction=opal$reader$update,  curl=handle, verbose=getOption("verbose", FALSE))
  content <- opal$reader$value()
  header <- parseHTTPHeader(opal$reader$header())
  info <- getCurlInfo(handle)
  response <- list(code=info$response.code, content.type=info$content.type, cookielist=info$cookielist, content=content, headers=header)
  if (is.null(callback)) {
    .handleResponse(opal, response)  
  } else {
    handler <- match.fun(callback)
    handler(opal, response)
  }
}

#' Default request response handler.
#' @keywords internal
.handleResponse <- function(opal, response) {
  if(response$code >= 400) { 
    msg <- gsub("[\n\r]","",response$headers['statusMessage'])
    msg <- paste0(opal$name, ": ", msg, " (", response$code, ")")
    if (!.isContentEmpty(response$content)) {
      msg <- paste0(msg, ": ", response$content)
    }
    stop(msg)
    NULL
  }	else {
    if(length(grep("octet-stream", response$content.type))) {
      unserialize(response$content)
    } else if(length(grep("json", response$content.type))) {
      if(is.raw(response$content)) {
        fromJSON(readChar(response$content, length(response$content)));
      } else {
        fromJSON(response$content);
      }
    }
  }
}

#' Check if response content is empty.
#' @keywords internal
.isContentEmpty <- function(content) {
  return(is.null(content) 
  || (is.raw(content) && nchar(rawToChar(content))==0)
  || (is.character(content) && nchar(content)==0))
}

#' Extract JSON
#' @keywords internal
.extractJsonField <- function(json, fields=NULL, isArray=TRUE) {
  if(is.null(fields)) {
    json 
  } else {
    if(isArray) {
      lapply(json, function(obj) {obj[fields]})
    } else {
      json[fields]
    }
  }
}

#' Returns a list r such that r[[i]] == l[[i]][field] for all i:length(l)
#' @keywords internal
.select <- function(l, field) {
  lapply(l, function(obj) {obj[[field]]})
}

#' Create the opal object
#' @keywords internal
.opal.login <- function(username,password,url,opts=list()) {
  opal <- new.env(parent=globalenv())
  
  # Strip trailing slash
  opal$url <- sub("/$", "", url)
  
  # Domain name
  opal$name <- gsub("[:/].*", "", gsub("http[s]*://", "", opal$url))
  
  # cookielist="" activates the cookie engine
  headers <- c(Accept="application/json, application/octet-stream");
  if(is.null(username) == FALSE) {
    headers <- c(headers, Authorization=.authToken(username, password));
  }
  # set default ssl options if https
  protocol <- strsplit(url, split="://")[[1]][1]
  options <- opts
  if (protocol=="https" & length(opts) == 0) {
    options <- list(ssl.verifyhost=0,ssl.verifypeer=0,sslversion=3)
  }
  opal$opts <- curlOptions(header=TRUE, httpheader=headers, cookielist="", .opts=options)
  opal$curl <- curlSetOpt(.opts=opal$opts)
  opal$reader <- dynCurlReader(curl=opal$curl)
  class(opal) <- "opal"
  
  opal
}

#' Turn expression into character strings.
#' @keywords internal
.deparse <- function(expr) {
  expression <- deparse(expr)
  if(length(expression) > 1) {
    expression = paste(expression, collapse='\n')
  }
  expression
}
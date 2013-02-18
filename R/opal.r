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

#' Get datasources from a opal.
#' 
#' @param opal Opal object.
#' @param fields
#' @export
opal.datasources=function(opal, fields=NULL) {
  .extractJsonField(.get(opal, "datasources"), fields)
}

#' Get a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param fields
#' @export
opal.datasource=function(opal, datasource, fields=NULL) {
  .extractJsonField(.get(opal, "datasource", datasource), fields)
}

#' Get tables of a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param fields
#' @export
opal.tables <- function(opal, datasource, fields=NULL) {
  .extractJsonField(.get(opal, "datasource", datasource, "tables"), fields);
}

#' Get a table of a datasource from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param fields
#' @export
opal.table <- function(opal, datasource, table, fields=NULL) {
  .extractJsonField(.get(opal, "datasource", datasource, "table", table), fields);
}

#' Get variables of a table from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param fields
#' @export
opal.variables <- function(opal, datasource, table, fields=NULL) {
  .extractJsonField(.get(opal, "datasource", datasource, "table", table, "variables"), fields)
}

#' Get a variable of a table from a opal.
#' 
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param variable Name of the variable in the table.
#' @param fields
#' @export
opal.variable <- function(opal, datasource, table, variable, fields=NULL) {
  .extractJsonField(.get(opal, "datasource", datasource, "table", table, "variable", variable), fields)
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
#' @param value Fully qualified name of a variable or a table in Opal.
#' @export
opal.assign <- function(opal, symbol, value) {
  if(is.language(value) || is.function(value)) {
    contentType <- "application/x-rscript"
    body <- .deparse(value)
  } else if(is.character(value)) {
    contentType <- "application/x-opal"
    body <- value
  } else {
    return(print(paste("Invalid value type: '", class(value), "'. Use quote() to protect from early evaluation.", sep="")))
  }
  
  .put(opal, "r", "session", "current", "symbol", symbol, body=body, contentType=contentType)
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
.url <- function(opal, ..., query=list()) {
	.tmp <- paste(opal$url, "ws", paste(sapply(c(...), curlEscape), collapse="/"), sep="/")
	if(length(query)) {
		.params <- paste(sapply(names(query), function(id) paste(id, curlEscape(query[[id]]), sep = "="), simplify=FALSE), collapse = "&")
		.tmp <- paste(.tmp, .params, sep="?")
	}
	.tmp
}

#' Constructs the value for the Authorization header
.authToken <- function(username, password) {
	paste("X-Opal-Auth", base64(paste(username, password, sep=":")))
}

#' Issues a request to opal for the specified resource
.get <- function(opal, ..., query=list()) {
	opts = curlOptions(httpget=TRUE, customrequest=NULL, .opts=opal$opts)
	.perform(opal, .url(opal, ..., query=query), opts)
}

.post <- function(opal, ..., query=list(), body='', contentType='application/x-rscript') {
	.nobody <- missing(body) || length(body) == 0
	if(.nobody) {
		# Act like a GET, but send a POST. This is required when posting without any body 
		opts = curlOptions(httpget=TRUE, customrequest="POST", .opts=opal$opts)
	} else {
		opts = curlOptions(post=TRUE, customrequest=NULL, httpheader=c(opal$opts$httpheader, 'Content-Type'=contentType), postfields=body, .opts=opal$opts)
	}
	.perform(opal, .url(opal, ..., query=query), opts)
}

.put <- function(opal, ..., query=list(), body='', contentType='application/x-rscript') {
	.nobody <- missing(body) || length(body) == 0
	if(.nobody) {
		# Act like a GET, but send a PUT. This is required when posting without any body 
		opts = curlOptions(httpget=TRUE, customrequest="PUT", .opts=opal$opts)
	} else {
		opts = curlOptions(post=TRUE, httpheader=c(opal$opts$httpheader, 'Content-Type'=contentType), postfields=body, customrequest="PUT", .opts=opal$opts)
	}
	.perform(opal, .url(opal, ..., query=query), opts)
}

.delete <- function(opal, ..., query=list()) {
	# Act like a GET, but send a DELETE.
	opts = curlOptions(httpget=TRUE, customrequest="DELETE", .opts=opal$opts)
	.perform(opal, .url(opal, ..., query=query), opts)
}

.perform <- function(opal, url, opts) {
	opal$reader <- dynCurlReader(opal$curl)

	handle <- opal$curl
	curlPerform(url=url, .opts=opts, writefunction=opal$reader$update,  curl=handle)
	content <- opal$reader$value()
	header <- parseHTTPHeader(opal$reader$header())
	info <- getCurlInfo(handle)
	.handleResponse(list(code=info$response.code, content.type=info$content.type, cookielist=info$cookielist, content=content, headers=header))
}

.handleResponse <- function(response) {
	if(response$code >= 400 && response$code < 500) {
		print(paste("Invalid request(", response$code, "):", response$content))
		NULL
	}	else if(response$code >= 500) {
		print(paste("Server error: ", response$code, " ", response$content))
		NULL
	} else {
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

.extractJsonField <- function(json, fields, isArray=TRUE) {
	if(is.null(fields)) {
	  json 
	} else {
		if(isArray) {
          lapply(l, function(obj) {obj[fields]})
		} else {
			json[fields]
  		}
	}
}

# Returns a list r such that r[[i]] == l[[i]][field] for all i:length(l)
.select <- function(l, field) {
  lapply(l, function(obj) {obj[[field]]})
}

# Do the opal login
.opal.login <- function(username,password,url,opts=list()) {
  opal <- new.env(parent=globalenv())
  
  # Strip trailing slash
  opal$url <- sub("/$", "", url)
  
  # cookielist="" activates the cookie engine
  headers <- c(Accept="application/octet-stream, application/json");
  if(is.null(username) == FALSE) {
    headers <- c(headers, Authorization=.authToken(username, password));
  }
  opal$opts <- curlOptions(header=TRUE, httpheader=headers, cookielist="", .opts=opts)
  opal$curl <- curlSetOpt(.opts=opal$opts)
  opal$reader <- dynCurlReader(curl=opal$curl)
  class(opal) <- "opal"

  opal
}

#
.deparse <- function(expr) {
  expression <- deparse(expr)
  if(length(expression) > 1) {
    expression = paste(expression, collapse='\n')
  }
  expression
}
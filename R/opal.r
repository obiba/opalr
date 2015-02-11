#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
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
#' @param username User name in opal(s). Can be provided by "opal.username" option.
#' @param password User password in opal(s). Can be provided by "opal.password" option.
#' @param url Opal url or list of opal urls. Can be provided by "opal.url" option.
#' @param opts Curl options. Can be provided by "opal.opts" option.
#' @export
#' @examples {
#'
#'#### The below examples illustrate the different ways to login in opal ####
#'
#'# explicite username/password login
#'o <- opal.login(username='administrator',password='password',url='https://demo.obiba.org:8443')
#'
#'# login using options
#'options(opal.username='administrator',
#'  opal.password='password',
#'  opal.url='https://demo.obiba.org:8443')
#'o <- opal.login()
#'
#'# login using ssl key pair
#'options(opal.opts=list(
#'    sslcert='my-publickey.pem',
#'    sslkey='my-privatekey.pem'))
#'o <- opal.login(url='https://demo.obiba.org:8443')
#'}
opal.login <- function(username=getOption("opal.username"), password=getOption("opal.password"), url=getOption("opal.url"), opts=getOption("opal.opts", list())) {
  if (is.null(url)) stop("opal url is required", call.=FALSE)
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
    try(.rmSession(opals), silent=TRUE)
    opals$rid <- NULL
  }
}

#' @export
print.opal <- function(opal) {
  cat("url:", opal$url, "\n")
  cat("name:", opal$name, "\n")
  cat("version:", opal$version, "\n")
  if (!is.null(opal$rid)) {
    cat("rid:", opal$rid, "\n")  
  }
}

#' Compare Opal version with the provided one. Note that a request must have been done 
#' in order to have a non-null Opal version.
#' 
#' @title Compare 
#' 
#' @return >0 if Opal version is more recent, 0 if equals, <0 otherwise.
#' @param opal Opal object.
#' @param version The semantic version string to be compared.
#' @export
opal.version_compare <- function(opal, version) {
  if (is.null(opal$version)) {
    stop("opal version is not set", call.=FALSE)
  }
  ov <- strsplit(opal$version, "-")[[1]][1]
  if (ov == version) return(0)
  if (ov > version) return(1)
  return(-1)
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

#' Get a file from its path in the Opal file system
#' 
#' @title Get a file
#' 
#' @param path Path to the file in the Opal file system.
#' @export
opal.file <- function(opal, path) {
  p <- strsplit(substring(path, 2), "/")[[1]]
  .get(opal, append("files", p))
}

#' Execute a R script on Opal(s).
#' 
#' @title Execute a R script
#'
#' @param opals Opal object or list of opal objects.
#' @param script R script to execute.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @param session Execute in current R session (default is TRUE).
#' @export
opal.execute <- function(opal, script, async=FALSE, session=TRUE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.execute(o, script, async=async, session=session)})
  } else {
    if (session) {
      query <- list()
      if (async) query <- list(async="true")
      ignore <- .getRSessionId(opal)
      .post(opal, "r", "session", opal$rid, "execute", query=query, body=script, contentType="application/x-rscript")
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
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' #' @examples {
#' # assign a list of variables from table HOP of opal object o
#' opal.assign(o, symbol="D", value"demo.HOP", variables=list("GENDER","LAB_GLUC"))
#' 
#' # assign all the variables matching 'LAB' from table HOP of opal object o
#' opal.assign(o, symbol="D", value"demo.HOP", variables="name().matches('LAB_')")
#' }
#' @export
opal.assign <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, async=FALSE) {
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
  
  if (async) {
    query["async"] <- "true"
  }
  res <- .put(opal, "r", "session", .getRSessionId(opal), "symbol", symbol, body=body, contentType=contentType, query=query)
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
  if (is.null(opal$version)) {
    opal$version <- as.character(response$headers['X-Opal-Version'])
  }
  if (is.null(opal$sid)) {
    opal$sid <- .extractOpalSessionId(response$cookielist)
  }
  #print(response)
  #print(response$headers)
  #print(paste0("content.type: ", response$content.type))
  if(response$code >= 400) { 
    msg <- gsub("[\n\r]","",response$headers['statusMessage'])
    msg <- paste0(opal$name, ": ", msg, " (", response$code, ")")  
    if (!.isContentEmpty(response$content)) {
      error <- response$content
      if(is.raw(error)) {
        error <- readChar(response$content, length(response$content))
      }
      msg <- paste0(msg, ": ", error)
    }
    stop(msg, call.=FALSE)
  }	else {
    if(length(grep("octet-stream", response$content.type))) {
      unserialize(response$content)
    } else if(length(grep("json", response$content.type))) {
      if(is.raw(response$content)) {
        fromJSON(readChar(response$content, length(response$content)));
      } else {
        fromJSON(response$content);
      }
    } else if (length(grep("text", response$content.type))) {
      as.character(response$content)
    } else {
      response$content
    }
  }
}

#' Extract opalsid from cookie list.
#' @keywords internal
.extractOpalSessionId <- function(cookielist) {
  for (cookieStr in cookielist) {
    cookie <- unlist(strsplit(cookieStr, '\t'))
    if (cookie[6] == "opalsid") {
      return(cookie[7])
    }
  }
  return(NULL)
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
.opal.login <- function(username, password, url, opts=list()) {
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
  if (protocol=="https") {
    if (!is.null(options$sslcert)) {
      options$sslcert <- .getPEMFilePath(options$sslcert)
    }
    if (!is.null(options$sslkey)) {
      options$sslkey <- .getPEMFilePath(options$sslkey)
    }
    if (is.null(options$ssl.verifyhost)) {
      options$ssl.verifyhost = 0
    }
    if (is.null(options$ssl.verifypeer)) {
      options$ssl.verifypeer = 0
    }
  }
  opal$opts <- curlOptions(header=TRUE, httpheader=headers, cookielist="", .opts=options)
  opal$curl <- curlSetOpt(.opts=opal$opts)
  opal$reader <- dynCurlReader(curl=opal$curl)
  opal$rid <- NULL
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

#' Extract absolute path to the pem file
#' @keywords internal
.getPEMFilePath <- function(pem, directory="~/.ssh") {
  path <- pem
  if (file.access(pem) == 0) {
    # file exists (absolute path)
    path <- path.expand(pem)
  } else if (file.access(paste0(directory, "/", pem)) == 0) {
    # file relative to given dir
    path <- path.expand(paste0(directory, "/", pem))
  } else if (file.access(paste0(getwd(), "/", pem)) == 0) {
    # file relative to working directory
    path <- paste0(getwd(), "/", pem)
  }
  
  path
}

#' Extract R session Id from opal object
#' @keywords internal
.getRSessionId <- function(opal) {
  if(is.null(opal$rid)) {
    opal$rid <- .newSession(opal)
  }
  if(is.null(opal$rid)) {
    stop("Remote R session not available")
  }
  return(opal$rid)
}

#' Create a new R session in Opal.
#' @keywords internal
.newSession <- function(opal) {
  .extractJsonField(.post(opal, "r", "sessions"), c("id"), isArray=FALSE)$id
}

#' Remove a R session from Opal.
#' @keywords internal
.rmSession <- function(opal) {
  if (!is.null(opal$rid)) {
    .delete(opal, "r", "session", opal$rid);
  }
}

#' Get all R session in Opal.
#' @keywords internal
.getSessions <- function(opal) {
  .extractJsonField(.get(opal, "r", "sessions"))
}
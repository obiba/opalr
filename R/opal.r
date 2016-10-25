#-------------------------------------------------------------------------------
# Copyright (c) 2016 OBiBa. All rights reserved.
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
#' @param restore Workspace ID to be restored (see also opal.logout)
#' @export
#' @examples {
#'
#'#### The below examples illustrate the different ways to login in opal ####
#'
#'# explicite username/password login
#'o <- opal.login(username='administrator',password='password',url='https://opal-demo.obiba.org')
#'
#'# login using options
#'options(opal.username='administrator',
#'  opal.password='password',
#'  opal.url='https://opal-demo.obiba.org')
#'o <- opal.login()
#'
#'# login using ssl key pair
#'options(opal.opts=list(
#'    sslcert='my-publickey.pem',
#'    sslkey='my-privatekey.pem'))
#'o <- opal.login(url='https://opal-demo.obiba.org')
#'}
opal.login <- function(username=getOption("opal.username"), password=getOption("opal.password"), url=getOption("opal.url"), opts=getOption("opal.opts", list()), restore=NULL) {
  if (is.null(url)) stop("opal url is required", call.=FALSE)
  if(is.list(url)){
    lapply(url, function(u){opal.login(username, password, u, opts=opts, restore=restore)})
  } else {
    .opal.login(username, password, url, opts=opts, restore=restore)
  }
}

#' Clear the R sessions and logout from Opal(s).
#' 
#' @title Logout from Opal(s)
#' 
#' @param opals Opal object or a list of opals.
#' @param save Save the workspace with given identifier (default value is FALSE, current session ID if TRUE).
#' @export
opal.logout <- function(opals, save=FALSE) {
  res <- NULL
  if (is.list(opals)) {
    res <- lapply(opals, function(o){opal.logout(o, save)})  
  } else {
    if ((is.logical(save) && save) || is.character(save)) {
      if (!is.na(opal$version) && opal.version_compare(opals,"2.6")<0) {
        warning("Workspaces are not available for opal ", opals$version, " (2.6.0 or higher is required)")
      }
    }
    res <- try(.rmSession(opals, save), silent=TRUE)
    opals$rid <- NULL
  }
  if (!is.null(res) && length(res) > 0) {
    return(res)
  }
}

#' @export
print.opal <- function(opal) {
  cat("url:", opal$url, "\n")
  cat("name:", opal$name, "\n")
  cat("version:", opal$version, "\n")
  cat("username:", opal$username, "\n")
  if (!is.null(opal$rid)) {
    cat("rid:", opal$rid, "\n")  
  }
  if (!is.null(opal$restore)) {
    cat("restore:", opal$restore, "\n")  
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
  if (is.null(opal$version) || is.na(opal$version)) {
    stop("opal version is not set", call.=FALSE)
  }
  ov <- strsplit(opal$version, "-")[[1]][1]
  if (ov == version) return(0)
  if (ov > version) return(1)
  return(-1)
}


#' Execute a R script on Opal(s).
#' 
#' @title Execute a R script
#'
#' @param opal Opal object or list of opal objects.
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

#' Assign a Opal table, or a R expression or a R object to a R symbol in the current R session.
#' 
#' @title Data or expression assignment
#' 
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value The value to assign evaluated in the following order: a R expression, a function, a fully qualified name of a variable or a table in Opal or any other R object (data.frame, vector).
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param id.name Add a vector with the given name representing the entity identifiers (from Opal 2.6). Default is NULL.
#' @param updated.name Add a vector with the given name representing the creation and last update timestamps (from Opal 2.6). Default is NULL.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples {
#' # assign a list of variables from table HOP of opal object o
#' opal.assign(o, symbol="D", value="demo.HOP", variables=list("GENDER","LAB_GLUC"))
#' 
#' # assign all the variables matching 'LAB' from table HOP of opal object o
#' opal.assign(o, symbol="D", value="demo.HOP", variables="name().matches('LAB_')")
#' 
#' # assign a function and call it
#' opal.assign.script(o, 'hello', quote(function(x) { print(paste0('Hello ', x , '!'))}))
#' opal.execute(o, "hello('Mr Bean')")
#' 
#' # push an arbitrary data frame to the R server
#' opal.assign(o, "D", mtcars)
#' 
#' # push an arbitrary vector to the R server
#' opal.assign(o, "C", mtcars$cyl)
#' }
#' @export
opal.assign <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, updated.name=NULL, async=FALSE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.assign(o, symbol, value, variables=variables, missings=missings, identifiers=identifiers, id.name=id.name, updated.name=updated.name, async=async)})
  } else {
    if(is.language(value) || is.function(value)) {
      opal.assign.script(opal, symbol, value, async=async)
    } else if(is.character(value)) {
      opal.assign.table(opal, symbol, value, variables=variables, missings=missings, identifiers=identifiers, id.name=id.name, updated.name=updated.name, async=async)
    } else {
      opal.assign.data(opal, symbol, value, async=async)
    } 
  }
}

#' Assign a Opal table to a R symbol in the current R session.
#' 
#' @title Data assignment
#' 
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value The value to assign evaluated in the following order: a R expression, a function, a fully qualified name of a variable or a table in Opal or any other R object (data.frame, vector).
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param id.name Add a vector with the given name representing the entity identifiers (from Opal 2.6). Default is NULL.
#' @param updated.name Add a vector with the given name representing the creation and last update timestamps (from Opal 2.6). Default is NULL.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples {
#' # assign a list of variables from table HOP of opal object o
#' opal.assign.table(o, symbol="D", value="demo.HOP", variables=list("GENDER","LAB_GLUC"))
#' 
#' # assign a table HOP with a identifiers column
#' opal.assign.table(o, symbol="H", value="demo.HOP", id.name="id")
#' 
#' # assign all the variables matching 'LAB' from table HOP of opal object o
#' opal.assign.table(o, symbol="D", value="demo.HOP", variables="name().matches('LAB_')")
#' }
#' @export
opal.assign.table <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, updated.name=NULL, async=FALSE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.assign.table(o, symbol, value, variables=variables, missings=missings, identifiers=identifiers, id.name=id.name, update.name=updated.name, async=async)})
  } else {
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
    if (!is.null(id.name)) {
      query["id"] <- id.name
    }
    if (!is.null(updated.name)) {
      query["updated"] <- updated.name
    }
    if (async) {
      query["async"] <- "true"
    }
    ignore <- .getRSessionId(opal)
    res <- .put(opal, "r", "session", opal$rid, "symbol", symbol, body=body, contentType=contentType, query=query)
  }
}

#' Assign a R script or expression to a R symbol in the current R session.
#' 
#' @title R script assignment
#' 
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value The R expression to assign.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples {
#' # assign a function and call it
#' opal.assign.script(o, 'hello', quote(function(x) { print(paste0('Hello ', x , '!'))}))
#' opal.execute(o, "hello('Mr Bean')")
#' }
#' @export
opal.assign.script <- function(opal, symbol, value, async=FALSE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.assign.script(o, symbol, value, async=async)})
  } else {
    contentType <- "application/x-rscript"
    body <- .deparse(value)
    query <- list()
    if (async) {
      query["async"] <- "true"
    }
    ignore <- .getRSessionId(opal)
    res <- .put(opal, "r", "session", opal$rid, "symbol", symbol, body=body, contentType=contentType, query=query)
  }
}

#' Assign a R object to a R symbol in the current R session.
#' 
#' @title Data assignment
#' 
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value The R object to assign (data.frame, vector).
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples {
#' # push an arbitrary data frame to the R server
#' opal.assign.data(o, "D", mtcars)
#' 
#' # push an arbitrary vector to the R server
#' opal.assign.data(o, "C", mtcars$cyl)
#' 
#' # push a string
#' opal.assign.data(o, "S", "Hello!")
#' }
#' @export
opal.assign.data <- function(opal, symbol, value, async=FALSE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.assign.data(o, symbol, value, async=async)})
  } else {
    contentType <- "application/x-rdata"
    body <- base64enc::base64encode(serialize(value, NULL))
    query <- list()
    if (async) {
      query["async"] <- "true"
    }
    ignore <- .getRSessionId(opal)
    res <- .post(opal, "r", "session", opal$rid, "symbol", symbol, body=body, contentType=contentType, query=query)
    }
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
  if (is.null(opal$version) || is.na(opal$version)) {
    opal$version <- as.character(response$headers['X-Opal-Version'])
  }
  if (is.null(opal$sid)) {
    opal$sid <- .extractOpalSessionId(response$cookielist)
  }
  #print(response)
  headers <- strsplit(response$headers, "\n")
  disposition <- headers['Content-Disposition']
  attachment <- FALSE
  if(!is.na(disposition) && length(grep("attachment", disposition))) {
    attachment <- TRUE
  }
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
  }	else if(attachment) {
    .handleAttachment(opal, response, as.character(disposition))
  } else {
    .handleContent(opal, response)
  }
}

.handleAttachment <- function(opal, response, disposition) {
  filename <- strsplit(disposition,"\"")[[1]][2]
  filetype <- guess_type(filename)
  if(is.raw(response$content)) {
    if (grepl("text/", response$content.type) || (grepl("application/", response$content.type) && grepl("text/", filetype))){
      as.character(readChar(response$content, length(response$content)))
    } else {
      readBin(response$content,what = raw(),length(response$content))
    }
  } else if (length(grep("text/", response$content.type))) {
    as.character(response$content)
  } else {
    response$content
  }
}

.handleContent <- function(opal, response) {
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
.opal.login <- function(username, password, url, opts=list(), restore=NULL) {
  opal <- new.env(parent=globalenv())
  
  # Username
  opal$username <- username
  
  # Strip trailing slash
  opal$url <- sub("/$", "", url)
  
  # Domain name
  opal$name <- gsub("[:/].*", "", gsub("http[s]*://", "", opal$url))
  
  # Version default value
  opal$version <- NA
  
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
  opal$restore <- restore
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

#' Extract R session Id from opal object, create a new R session if not found.
#' @keywords internal
.getRSessionId <- function(opal) {
  if(is.null(opal$rid)) {
    opal$rid <- .newSession(opal, restore=opal$restore)
  }
  if(is.null(opal$rid)) {
    stop("Remote R session not available")
  }
  return(opal$rid)
}

#' Create a new R session in Opal.
#' @keywords internal
.newSession <- function(opal, restore=NULL) {
  query <- list()
  if (!is.null(restore)) {
    query <- list(restore=restore)  
  }
  res <- .extractJsonField(.post(opal, "r", "sessions", query=query), c("id"), isArray=FALSE)
  return(res$id)
}

#' Remove a R session from Opal.
#' @keywords internal
.rmSession <- function(opal, save=FALSE) {
  if (!is.null(opal$rid)) {
    if ((is.logical(save) && save) || is.character(save)) {
      saveId <- save
      if(is.logical(save) && save) {
        saveId <- opal$rid
      }
      .delete(opal, "r", "session", opal$rid, query=list(save=saveId))
      if(saveId != save) {
        return(saveId)
      }
    } else {
      .delete(opal, "r", "session", opal$rid)
    }
  }
}

#' Get all R session in Opal.
#' @keywords internal
.getSessions <- function(opal) {
  .extractJsonField(.get(opal, "r", "sessions"))
}
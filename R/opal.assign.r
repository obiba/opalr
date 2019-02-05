#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Assign a Opal table, or a R expression or a R object to a R symbol in the current R session.
#' 
#' @title Data or expression assignment
#' 
#' @family assignment functions
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value The value to assign evaluated in the following order: a R expression, a function, a fully qualified name of a variable or a table in Opal or any other R object (data.frame, vector).
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE. Ignored if value is an R expression.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param id.name Add a vector with the given name representing the entity identifiers (from Opal 2.6). Default is NULL.
#' @param updated.name Add a vector with the given name representing the creation and last update timestamps (from Opal 2.6). Default is NULL.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples
#' \dontrun{
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

#' Assign a Opal table to a data.frame identified by a R symbol in the current R session.
#' 
#' @title Data assignment to a data.frame
#' 
#' @family assignment functions
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value The value to assign evaluated in the following order: a fully qualified name of a variable or a table in Opal.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param id.name Add a vector with the given name representing the entity identifiers (from Opal 2.6). Default is NULL.
#' @param updated.name Add a vector with the given name representing the creation and last update timestamps (from Opal 2.6). Default is NULL.
#' @param class The data frame class into which the table is written: can 'data.frame' (default and fallback) or 'tibble' (from Opal 2.6).
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples 
#' \dontrun{
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
opal.assign.table <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, id.name=NULL, updated.name=NULL, class='data.frame', async=FALSE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.assign.table(o, symbol, value, variables=variables, missings=missings, identifiers=identifiers, id.name=id.name, updated.name=updated.name, class=class, async=async)})
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
    if (!is.null(class)) {
      query["class"] <- class
    }
    if (async) {
      query["async"] <- "true"
    }
    ignore <- .getRSessionId(opal)
    res <- opal.put(opal, "r", "session", opal$rid, "symbol", symbol, body=body, contentType=contentType, query=query)
  }
}

#' Assign a Opal table to a tibble identified by a R symbol in the current R session.
#' 
#' @title Data assignment to a tibble
#' 
#' @family assignment functions
#' @param opal Opal object.
#' @param symbol Name of the R symbol.
#' @param value The fully qualified name of a table in Opal.
#' @param variables List of variable names or Javascript expression that selects the variables of a table (ignored if value does not refere to a table). See javascript documentation: http://wiki.obiba.org/display/OPALDOC/Variable+Methods
#' @param missings If TRUE, missing values will be pushed from Opal to R, default is FALSE.
#' @param identifiers Name of the identifiers mapping to use when assigning entities to R (from Opal 2.0).
#' @param id.name Add a vector with the given name representing the entity identifiers (from Opal 2.6). Default is 'id'.
#' @param updated.name Add a vector with the given name representing the creation and last update timestamps (from Opal 2.6). Default is NULL.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @export
opal.assign.table.tibble <- function(opal, symbol, value, variables=NULL, missings=FALSE, identifiers=NULL, id.name='id', updated.name=NULL, async=FALSE) {
  ignore <- .getRSessionId(opal)
  if (!is.na(opal$version) && opal.version_compare(opal,"2.8")<0) {
    warning("Export to tibble not available for opal ", opal$version, " (2.8.0 or higher is required)")
  } else {
    opal.assign.table(opal, symbol, value, variables=variables, missings=missings, identifiers=identifiers, id.name=id.name, updated.name=updated.name, class="tibble", async=async)
  }
}

#' Assign a R script or expression to a R symbol in the current R session.
#' 
#' @title R script assignment
#' 
#' @family assignment functions
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value The R expression to assign.
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples 
#' \dontrun{
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
    res <- opal.put(opal, "r", "session", opal$rid, "symbol", symbol, body=body, contentType=contentType, query=query)
  }
}

#' Assign a R object to a R symbol in the current R session.
#' 
#' @title Data assignment
#' 
#' @family assignment functions
#' @param opal Opal object or list of opal objects.
#' @param symbol Name of the R symbol.
#' @param value The R object to assign (data.frame, vector).
#' @param async R script is executed asynchronously within the session (default is FALSE). If TRUE, the value returned is the ID of the command to look for (from Opal 2.1).
#' @examples 
#' \dontrun{
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
#' @import jsonlite
opal.assign.data <- function(opal, symbol, value, async=FALSE) {
  if(is.list(opal)){
    lapply(opal, function(o){opal.assign.data(o, symbol, value, async=async)})
  } else {
    contentType <- "application/x-rdata"
    body <- jsonlite::base64_enc(serialize(value, NULL))
    query <- list()
    if (async) {
      query["async"] <- "true"
    }
    ignore <- .getRSessionId(opal)
    res <- opal.post(opal, "r", "session", opal$rid, "symbol", symbol, body=body, contentType=contentType, query=query)
  }
}

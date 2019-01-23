#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get available Datashield methods of a given type.
#' 
#' @title List Datashield methods
#' 
#' @param opal Opal object or list of opal objects.
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @export
datashield.methods=function(opal, type="aggregate") {
  UseMethod('datashield.methods');
}

#' @rdname datashield.methods
#' @export
datashield.methods.opal=function(opal, type="aggregate") {
  rlist <- .get(opal, "datashield", "env", type, "methods")
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
  pkg <- lapply(rlist,function(m){
    val <- m$DataShield.RFunctionDataShieldMethodDto.method$rPackage
    if (is.null(val)) {
      val <- NA
    }
    val
  })
  version <- lapply(rlist,function(m){
    val <- m$DataShield.RFunctionDataShieldMethodDto.method$version
    if (is.null(val)) {
      val <- NA
    }
    val
  })
  rval <- data.frame(unlist(name), unlist(t), unlist(class), unlist(value), unlist(pkg), unlist(version))
  colnames(rval) <- c("name","type", "class", "value","package","version")
  rval
}

#' @rdname datashield.methods
#' @export
datashield.methods.list=function(opal, type="aggregate") {
  lapply(opal, FUN=datashield.methods.opal, type)
}


#' Get a Datashield method of a given type by its name.
#' 
#' @title Get Datashield method by its name
#' 
#' @param opal Opal object or list of opal objects.
#' @param name Name of the method
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @export
datashield.method=function(opal, name, type="aggregate") {
  UseMethod('datashield.method');
}

#' @rdname datashield.method
#' @export
datashield.method.opal=function(opal, name, type="aggregate") {
  # this request is currently not accessible to ds user
  #.get(opal, "datashield", "env", type, "method", name)
  ms <- datashield.methods(opal, type);
  rval <- ms[ms$name == name,]
  if (nrow(rval) > 0) {
    # there is certainly a simpler way to this... 
    rval <- list(name=as.character(rval$name), type=as.character(rval$type), class=as.character(rval$class), value=as.character(rval$value))
  } else {
    rval <- NULL
  }
  rval
}

#' @rdname datashield.method
#' @export
datashield.method.list=function(opal, name, type="aggregate") {
  lapply(opal, FUN=datashield.method.opal, name, type)
}

#' Check existence of a Datashield method of any type by its name.
#' 
#' @title Check existence of a Datashield method by its name
#' 
#' @param opal Opal object or list of opal objects.
#' @param name Name of the method
#' @export
datashield.has_method=function(opal, name) {
  UseMethod('datashield.has_method');
}

#' @rdname datashield.has_method
#' @export
datashield.has_method.opal=function(opal, name) {
  rval <- !is.null(datashield.method(opal,name, type="aggregate"))
  if (!rval) {
    rval <- !is.null(datashield.method(opal,name, type="assign"))
  }
  rval
}

#' @rdname datashield.has_method
#' @export
datashield.has_method.list=function(opal, name) {
  lapply(opal, FUN=datashield.has_method.opal, name)
}
#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

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

#' Turn expression into character strings.
#' @keywords internal
.deparse <- function(expr) {
  expression <- deparse(expr)
  if(length(expression) > 1) {
    expression = paste(expression, collapse='\n')
  }
  expression
}

#' Simple transformation function of a list into a JSON object/array.
#' @keywords internal
.listToJson <- function(l) {
  str <- ''
  valueToString <- function(v) {
    if (is.list(v)) {
      .listToJson(v)
    } else if (is.logical(v)) {
      if (v) {
        'true'
      } else {
        'false'
      }
    } else {
      paste0('"', v, '"')
    }
  }
  if (is.null(names(l))) {
    # array
    for (value in l) {
      if (nchar(str)>0) {
        str <- paste0(str,',')
      }
      str <- paste0(str, valueToString(value))
    }
    paste0('[', str, ']') 
  } else {
    # object
    for (name in names(l)) {
      if (nchar(str)>0) {
        str <- paste0(str,',')
      }
      str <- paste0(str, '"', name, '": ', valueToString(l[[name]]))
    }
    paste0('{', str, '}') 
  }
}


#' @keywords internal
.nullToNA <- function(x) {
  ifelse(is.null(x), NA, x)
}

#' Extract label for locale. If not found, fallback to undefined language label (if any).
#' @keywords internal
.extractLabel <- function(locale="en", labels=list(), localeKey="locale", valueKey="value") {
  if (is.null(labels) || length(labels) == 0) {
    return(NA)
  }
  label <- NA
  label.und <- NA
  for (i in 1:length(labels)) {
    l <- labels[[i]]
    if (!(localeKey %in% names(l))) {
      label.und <- l[[valueKey]]
    } else if (l[[localeKey]] == locale) {
      label <- l[[valueKey]]
    }
  }
  if (is.na(label)) {
    label.und
  } else {
    label
  }
}

#' Returns a list r such that r[[i]] == l[[i]][field] for all i:length(l)
#' @keywords internal
.select <- function(l, field) {
  lapply(l, function(obj) {obj[[field]]})
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

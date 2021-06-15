#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
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

#' @keywords internal
.norm2nastr <- function(value) {
  if (.is.empty(value)) {
    "N/A"
  } else {
    as.character(value)
  }
}

#' @keywords internal
.is.empty <- function(value) {
  if (length(value) == 0 || is.null(value) || is.na(value)) {
    TRUE
  } else {
    str <- as.character(value)
    if (length(str) == 1 && nchar(str) == 0) {
      TRUE
    } else {
      FALSE
    }
  }
}

#' @keywords internal
.localized2str <- function(item, locale) {
  for (msg in item) {
    if (msg$locale == locale) {
      return(msg$text)
    }
  }
  return("")
}

#' @keywords internal
.splitAttributeKey <- function(key) {
  str <- strsplit(key, ":")[[1]]
  namespace <- NULL
  name <- NULL
  loc <- NULL
  if (length(str)>2 && str[2] == "") {
    namespace <- str[1]
    name <- str[3]
    if(length(str) == 4) {
      loc <- str[4]
    }
  } else {
    name <- str[1]
    if(length(str) == 2) {
      loc <- str[2]
    }
  }
  rval <- list()
  if (!is.null(namespace)) {
    rval$namespace <- namespace
  }
  rval$name <- name
  if (!is.null(loc)) {
    rval$locale <- loc
  }
  rval
}

#' Create a new progress instance with default settings.
#' @import progress
#' @keywords internal
.newProgress <- function(format = "  :what [:bar] :percent /:elapsed", clear = getOption("opal.progress.clear", FALSE), total, width = 100) {
  progress::progress_bar$new(format = format, clear = clear, total = total, width = width)
}

#' Output the progress status if option "datashield.progress" is allows to.
#' @keywords internal
.tickProgress <- function(progress, tokens = list()) {
  if (getOption("opal.progress", TRUE)) progress$tick(tokens = tokens)
}

#' Turns a value into 0/1 numeric.
#' @keywords internal
.as.zeroOne <- function(value) {
  if (is.null(value) || is.na(value)) {
    0
  } else if (is.logical(value) && value) {
    1
  } else if (is.character(value) && value == "1") {
    1
  } else if (is.numeric(value) && value == 1) {
    1
  } else {
    0
  }
}

#' @keywords internal
.toJSONVariables <- function(table = NULL, variables, categories = NULL, pretty = FALSE) {
  varCols <- names(variables)
  vars <- variables
  # filter variables by table of interest
  if ("table" %in% varCols && !is.null(table)) {
    vars <- variables[variables$table == table,]
  }
  is.empty <- function(x) {
    is.null(x) || is.na(x)
  }
  varArray <- list()
  for (i in 1:nrow(vars)) {
    var <- vars[i,]
    varObj <- list(name=jsonlite::unbox(var$name))
    if ("valueType" %in% varCols && !is.empty(var$valueType)) {
      varObj$valueType <- jsonlite::unbox(var$valueType)
    } else {
      varObj$valueType <- jsonlite::unbox("text")
    }
    if ("entityType" %in% varCols && !is.empty(var$entityType)) {
      varObj$entityType <- jsonlite::unbox(var$entityType)
    } else {
      varObj$entityType <- jsonlite::unbox("Participant")
    }
    if ("unit" %in% varCols && !is.empty(var$unit)) {
      varObj$unit <- jsonlite::unbox(var$unit)
    }
    if ("mimeType" %in% varCols && !is.empty(var$mimeType)) {
      varObj$mimeType <- jsonlite::unbox(var$mimeType)
    }
    if ("referencedEntityType" %in% varCols && !is.empty(var$referencedEntityType)) {
      varObj$referencedEntityType <- jsonlite::unbox(var$referencedEntityType)
    }
    if ("repeatable" %in% varCols && !is.empty(var$repeatable)) {
      varObj$isRepeatable <- jsonlite::unbox(as.logical(.as.zeroOne(var$repeatable)))
    } else {
      varObj$isRepeatable <- jsonlite::unbox(FALSE)
    }
    if ("occurrenceGroup" %in% varCols && !is.empty(var$occurrenceGroup)) {
      varObj$occurrenceGroup <- jsonlite::unbox(var$occurrenceGroup)
    }
    if ("index" %in% varCols && !is.empty(var$index)) {
      varObj$index <- jsonlite::unbox(as.numeric(var$index))
    }
    attrs <- list()
    j <- 1
    for (col in varCols) {
      if (!(col %in% c("table", "name", "valueType", "entityType", "unit", "mimeType", "referencedEntityType", "repeatable", "occurrenceGroup", "index"))
          && !is.empty(var[[col]])) {
        attr <- .splitAttributeKey(col)
        attr$value <- jsonlite::unbox(var[[col]])
        attrs[[j]] <- attr
        j <- j + 1
      }
    }
    varObj$attributes <- attrs
    
    if (!is.null(categories)) {
      catCols <- names(categories)
      cats <- categories[categories$variable == var$name,]
      # filter categories by table of interest
      if ("table" %in% catCols && !is.null(table)) {
        cats <- cats[cats$table == table,]
      }
      if (nrow(cats)>0) {
        catArray <- list()
        for (k in 1:nrow(cats)) {
          cat <- cats[k,]
          catObj <- list(name = jsonlite::unbox(as.character(cat$name)))
          if ("missing" %in% names(cats) && !is.empty(cat$missing)) {
            catObj$isMissing <- jsonlite::unbox(as.logical(.as.zeroOne(cat$missing)))
          } else {
            catObj$isMissing <- jsonlite::unbox(FALSE)
          }
          attrs <- list()
          j <- 1
          for (col in catCols) {
            if (!(col %in% c("table", "variable", "name", "missing")) && !is.empty(cat[[col]])) {
              attr <- .splitAttributeKey(col)
              attr$value <- jsonlite::unbox(cat[[col]])
              attrs[[j]] <- attr
              j <- j + 1
            }
          }
          catObj$attributes <- attrs
          
          catArray[[k]] <- catObj
        }
        varObj$categories <- catArray
      }
    }
    
    varArray[[i]] <- varObj
  }
  jsonlite::toJSON(varArray, pretty = pretty, auto_unbox = TRUE)
}

#' @keywords internal
.toSafeProfile <- function(opal, profile) {
  safeProfile <- profile
  if (is.null(safeProfile)) {
    if (is.null(opal$profile))
      safeProfile <- "default"
    else
      safeProfile <- opal$profile
  }
  safeProfile
}
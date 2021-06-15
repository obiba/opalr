#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the annotations of a Opal table
#' 
#' Directly retrieves from the table's data dictionary the variable annotations (attributes with a namespace).
#' 
#' @family datasource functions
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @return A data frame in long format (one row per annotation). 
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.annotations(o, 'CPTP', 'Coreqx_final')
#' opal.logout(o)
#' }
#' @export
opal.annotations <- function(opal, datasource, table) {
  vars <- opal.get(opal, "datasource", datasource, "table", table, "variables")
  
  variable <- c()
  taxonomy <- c()
  vocabulary <- c()
  term <- c()
  
  for (var in vars) {
    for (a in var$attributes) {
      if (!is.null(a$namespace)) {
        variable <- append(variable, var$name)
        taxonomy <- append(taxonomy, a$namespace)
        vocabulary <- append(vocabulary, a$name)
        term <- append(term, a$value)
      }
    }  
  }
  
  data.frame(variable, taxonomy, vocabulary, term)
}

#' Apply the annotations to a Opal table
#' 
#' Set the provided annotations (as the one that can be retrieved from \link{opal.annotations})
#' to the table's data dictionary. Variables that do not exists in the table are ignored.
#' 
#' @family datasource functions
#' @param opal Opal object.
#' @param datasource Name of the datasource.
#' @param table Name of the table in the datasource.
#' @param annotations A data frame of annotations, with the expected columns: 'variable' (variable name),
#' 'taxonomy' (the taxonomy name), 'vocabulary' (the vocabulary name) and 'term' (the term value, if NULL
#' of NA the annotation is removed).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' annots <- opal.annotations(o, 'CPTP', 'Coreqx_final')
#' opal.annotate(o, 'CPTP', 'Cag_coreqx', annots)
#' opal.logout(o)
#' }
#' @export
opal.annotate <- function(opal, datasource, table, annotations) {
  if (is.null(annotations)) {
    return()
  }
  .checkcolumn <- function(col) {
    if (!(col %in% colnames(annotations))) {
      stop("Missing ", col, " column in annotations data.frame.")
    }
  }
  .checkcolumn('variable')
  .checkcolumn('taxonomy')
  .checkcolumn('vocabulary')
  .checkcolumn('term')
  
  # ensure only existing variables are in
  vars <- opal.get(opal, "datasource", datasource, "table", table, "variables")
  varNames <- unlist(lapply(vars, function(v) v$name))
  annots <- annotations[annotations$variable %in% varNames,]
  if (nrow(annots) == 0) {
    return()
  }
  
  # subset by taxonomy, vocabulary, term
  # to apply by variable batch
  for (taxonomy in unique(annots$taxonomy)) {
    if (!is.null(taxonomy) && !is.na(taxonomy)) {
      taxoAnnots <- annots[annots$taxonomy == taxonomy, ]
      for (vocabulary in unique(taxoAnnots$vocabulary)) {
        if (!is.null(vocabulary) && !is.na(vocabulary)) {
          vocAnnots <- taxoAnnots[taxoAnnots$vocabulary == vocabulary, ]
          for (term in unique(vocAnnots$term)) {
            query <- list(namespace=taxonomy, name=vocabulary)
            termAnnots <- data.frame()
            if (is.null(term) || is.na(term)) {
              termAnnots <- vocAnnots[is.na(vocAnnots$term), ]
              # remove attribute
              message("Removing ", taxonomy, "::", vocabulary, " from: ", paste(unique(termAnnots$variable), collapse = ", "))
              
            } else {
              termAnnots <- vocAnnots[!is.na(vocAnnots$term) & vocAnnots$term == term, ]
              # add or update attribute
              message("Applying ", taxonomy, "::", vocabulary, "=", term, " to: ", paste(unique(termAnnots$variable), collapse = ", "))
              query[["value"]] <- term
            }
            body <- paste0("variable=", paste(unique(termAnnots$variable), collapse = "&variable="))
            opal.put(opal, "datasource", datasource, "table", table, "variables", "_attribute", 
                     query = query, body = body, contentType="application/x-www-form-urlencoded")
          }
        }
      }  
    }
  }
}


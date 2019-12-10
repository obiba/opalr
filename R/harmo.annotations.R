#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' List the annotations
#'
#' List the annotations of each of the variables.
#'
#' @param tibble Tibble to be annotated
#' @param variables A character vector of variable names to be inspected. If NULL or empty, all
#' the columns of the tibble will be inspected.
#' @param taxonomy Filter by taxonomy name(s) (if provided).
#' @param vocabulary Filter by vocabulary name(s) (if provided).
#' @return A data frame in long format (one row per annotation).
#' @examples 
#' \donttest{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' cqx <- harmo.table_get(o, "CPTP", "Cag_coreqx")
#' annot <- harmo.annotations(cqx, taxonomy = "Mlstr_harmo", vocabulary = "status")
#' opal.logout(o)
#' }
#' @export
harmo.annotations <- function(tibble, variables = NULL, taxonomy = NULL, vocabulary = NULL) {
  if (!("tbl" %in% class(tibble))) {
    stop("The tibble parameter must be a tibble.")
  }
  vars <- variables
  if (is.null(vars)) {
    vars <- colnames(tibble)
  }

  variable <- c()
  taxo <- c()
  voc <- c()
  term <- c()

  for (var in vars) {
    if (var %in% colnames(tibble)) {
      attrs <- attributes(tibble[[var]])
      if (!is.null(attrs)) {
        for (n in names(attrs)) {
          if (grepl("::", n)) {
            tokens <- unlist(strsplit(n, "::"))
            if ((is.null(taxonomy) || tokens[1] %in% taxonomy) &&
                (is.null(vocabulary) || tokens[2] %in% vocabulary)) {
              variable <- append(variable, var)
              taxo <- append(taxo, tokens[1])
              voc <- append(voc, tokens[2])
              term <- append(term, attrs[[n]])  
            }
          }
        }
      }
    }
  }

  data.frame(variable, taxonomy = taxo, vocabulary = voc, term)
}

#' Set variable annotation with a taxonomy term
#'
#' Apply or remove an annotation from a set of variables.
#'
#' @param tibble Tibble to be annotated.
#' @param variables A character vector of variable names to be annotated. If NULL or empty, all
#' the columns of the tibble will be annotated.
#' @param taxonomy The taxonomy to which the vocabulary belongs. If NULL, the annotation is a simple
#' attribute (i.e. without a taxonomy reference).
#' @param vocabulary The vocabulary to which the term belongs.
#' @param term The term to apply. If NULL, the annotation will be deleted.
#' @return The annotated tibble
#' @examples 
#' \donttest{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' cqx <- harmo.table_get(o, "CPTP", "Cag_coreqx")
#' cqx <- harmo.annotate(cqx, 
#'   variables = c("A_SDC_EDU_LEVEL", "A_SDC_EDU_LEVEL_AGE"), 
#'   taxonomy = "Mlstr_area", 
#'   vocabulary = "Sociodemographic_economic_characteristics", 
#'   term = "Education")
#' opal.logout(o)
#' }
#' @export
harmo.annotate <- function(tibble, variables = NULL, taxonomy = "Mlstr_area", vocabulary, term) {
  if (!("tbl" %in% class(tibble))) {
    stop("The tibble parameter must be a tibble.")
  }
  attrstr <- paste0(taxonomy, "::", vocabulary)
  if (is.null(taxonomy)) {
    attrstr <- vocabulary
  }
  vars <- variables
  if (is.null(vars) || length(vars) == 0) {
    vars <- colnames(tibble)
  }
  for (var in vars) {
    if (var %in% colnames(tibble)) {
      if (is.null(term)) {
        message("Removing ", attrstr, " from ", var, " ...")
      } else {
        message("Applying ", attrstr, "=", term, " to ", var, " ...")
      }
      
      if (is.null(attributes(tibble[[var]]))) {
        attr <- list()
        attr[[attrstr]] <- term
        attributes(tibble[[var]]) <- attr
      } else {
        attributes(tibble[[var]])[[attrstr]] <- term
      }
    } else {
      warning("Not a valid variable name: ", var)
    }
  }
  tibble
}

#' Set variable annotation with Harmonization Status term
#'
#' Apply or remove an harmonization status annotation from a set of variables. The harmonization status
#' is described by the "status" vocabulary in the "Mlstr_harmo" taxonomy.
#'
#' @param tibble Tibble to be annotated.
#' @param variables A character vector of variable names to be annotated. If NULL or empty, all
#' the columns of the tibble will be annotated.
#' @param status The harmonization status to apply: 'complete', 'undetermined' or 'impossible'. If NULL, the annotation will be deleted.
#' @return The annotated tibble
#' @examples 
#' \donttest{
#' o <- opal.login('administrator','password','https://opal-demo.obiba.org')
#' cqx <- harmo.table_get(o, "CPTP", "Cag_coreqx")
#' cqx <- harmo.annotate.status(cqx, 
#'   variables = c("A_SDC_EDU_LEVEL", "A_SDC_EDU_LEVEL_AGE"), 
#'   status = "complete")
#' opal.logout(o)
#' }
#' @export
harmo.annotate.status <- function(tibble, variables = NULL, status) {
  if (is.null(status) || status %in% c("complete", "undetermined", "impossible")) {
    harmo.annotate(tibble, variables, "Mlstr_harmo", "status", status)
  } else {
    stop("Not a valid harmonization status: ", status, call. = FALSE)
  }
}

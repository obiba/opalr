#-------------------------------------------------------------------------------
# Copyright (c) 2019 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------


#' Get taxonomies from a opal.
#' 
#' @param opal Opal object.
#' @param locale The language for labels (default is "en").
#' @param df Return a data.frame (default is TRUE)
#' @export
opal.taxonomies <- function(opal, locale="en", df=TRUE) {
  res <- opal.get(opal, "system", "conf", "taxonomies")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    author <- rep(NA, n)
    license <- rep(NA, n)
    title <- rep(NA, n)
    description <- rep(NA, n)
    vocabularies <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      author[i] <- .nullToNA(item$author)
      license[i] <- .nullToNA(item$license)
      if (!is.null(item$title)) {
        title[i] <- .nullToNA(.extractLabel(locale, item$title, valueKey="text"))
      }
      if (!is.null(item$description)) {
        description[i] <- .nullToNA(.extractLabel(locale, item$description, valueKey="text"))
      }
      if (!is.null(item$vocabularies)) {
        vocabularies[i] <- paste0(lapply(item$vocabularies, function(v) v$name), collapse="|")
      }
    }
    data.frame(name, author, license, title, description, vocabularies)
  } else {
    data.frame()
  }
}

#' Get a taxonomy from a opal.
#' 
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy
#' @export
opal.taxonomy <- function(opal, taxonomy) {
  opal.get(opal, "system", "conf", "taxonomy", taxonomy)
}

#' Get the vocabularies of a taxonomy from a opal.
#' 
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy
#' @param locale The language for labels (default is "en").
#' @param df Return a data.frame (default is TRUE)
#' @export
opal.vocabularies <- function(opal, taxonomy, locale="en", df=TRUE) {
  res <- opal.get(opal, "system", "conf", "taxonomy", taxonomy, "vocabularies")
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    title <- rep(NA, n)
    description <- rep(NA, n)
    terms <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      if (!is.null(item$title)) {
        title[i] <- .nullToNA(.extractLabel(locale, item$title, valueKey="text"))
      }
      if (!is.null(item$description)) {
        description[i] <- .nullToNA(.extractLabel(locale, item$description, valueKey="text"))
      }
      if (!is.null(item$terms)) {
        terms[i] <- paste0(lapply(item$terms, function(v) v$name), collapse="|")
      }
    }
    data.frame(name, title, description, terms)
  } else {
    data.frame()
  }
}

#' Get a taxonomy vocabulary from a opal.
#' 
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy
#' @param vocabulary Name of the vocabulary in the taxonomy
#' @export
opal.vocabulary <- function(opal, taxonomy, vocabulary) {
  opal.get(opal, "system", "conf", "taxonomy", taxonomy, "vocabulary", vocabulary)
}

#' Get the terms of a vocabulary from a opal.
#' 
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy
#' @param vocabulary Name of the vocabulary in the taxonomy
#' @param locale The language for labels (default is "en").
#' @param df Return a data.frame (default is TRUE)
#' @export
opal.terms <- function(opal, taxonomy, vocabulary, locale="en", df=TRUE) {
  res <- opal.get(opal, "system", "conf", "taxonomy", taxonomy, "vocabulary", vocabulary)$terms
  if (!df) {
    return(res)
  }
  n <- length(res)
  if (n > 0) {
    name <- rep(NA, n)
    title <- rep(NA, n)
    description <- rep(NA, n)
    keywords <- rep(NA, n)
    for (i in 1:n) {
      item <- res[[i]]
      name[i] <- item$name
      if (!is.null(item$title)) {
        title[i] <- .nullToNA(.extractLabel(locale, item$title, valueKey="text"))
      }
      if (!is.null(item$description)) {
        description[i] <- .nullToNA(.extractLabel(locale, item$description, valueKey="text"))
      }
      if (!is.null(item$keywords)) {
        keywords[i] <- .nullToNA(.extractLabel(locale, item$keywords, valueKey="text"))
      }
    }
    data.frame(name, title, description, keywords)
  } else {
    data.frame()
  }
}

#' Get a vocabulary term from a opal.
#' 
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy
#' @param vocabulary Name of the vocabulary in the taxonomy
#' @param term Name of the term in the vocabulary
#' @export
opal.term <- function(opal, taxonomy, vocabulary, term) {
  opal.get(opal, "system", "conf", "taxonomy", taxonomy, "vocabulary", vocabulary, "term", term)
}
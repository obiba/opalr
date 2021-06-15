#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------


#' Get taxonomies
#' 
#' Get all taxonomies. A taxonomy describes the annotations that can be applied to the variables. 
#' Taxonomies also drive the variables search interface.
#' 
#' @family taxonomy functions
#' @param opal Opal object.
#' @param locale The language for labels (default is "en").
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.taxonomies(o)
#' opal.logout(o)
#' }
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

#' Get a taxonomy
#' 
#' Get a specific taxonomy details.
#' 
#' @family taxonomy functions
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.taxonomy(o, 'Mlstr_area')
#' opal.logout(o)
#' }
#' @export
opal.taxonomy <- function(opal, taxonomy) {
  opal.get(opal, "system", "conf", "taxonomy", taxonomy)
}

#' Delete a taxonomy
#' 
#' Delete a taxonomy, without failing if the taxonomy does not exist.
#' 
#' @family taxonomy functions
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.taxonomy_delete(o, 'Mlstr_area')
#' opal.logout(o)
#' }
#' @export
opal.taxonomy_delete <- function(opal, taxonomy) {
  ignore <- opal.delete(opal, "system", "conf", "taxonomy", taxonomy)
}

#' Download a taxonomy file
#' 
#' Download a taxonomy stored in a file in YAML format.
#' 
#' @family taxonomy functions
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy.
#' @param destination Path to the taxonomy YAML file. If not provided, the downloaded file will
#' have the taxonomy name with the '.yml' extension and will be located in the working directory. 
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.taxonomy_download(o, 'Mlstr_area', '~/some/dir/Mlstr_area.yml')
#' opal.logout(o)
#' }
#' @export
opal.taxonomy_download <- function(opal, taxonomy, destination=NULL) {
  content <- opal.get(opal, "system", "conf", "taxonomy", taxonomy, "_download")
  dest <- destination
  if (is.null(destination)) {
    dest <- paste0(taxonomy, ".yml")
  } else if (dirname(destination) != ".") {
    dir.create(dirname(destination), showWarnings=FALSE, recursive=TRUE)
  }
  fh <- file(dest,'wb')
  writeLines(content, fh)
  close(fh)
}

#' Upload a taxonomy file
#' 
#' Upload a taxonomy stored in a local file in YAML format. This operation 
#' will fail if the taxonomy already exists, see \link{opal.taxonomy_delete}.
#' 
#' @family taxonomy functions
#' @param opal Opal object.
#' @param path Path to the taxonomy YAML file.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.taxonomy_upload(o, '~/some/dir/taxo.yml')
#' opal.logout(o)
#' }
#' @export
opal.taxonomy_upload <- function(opal, path) {
  pb <- .newProgress(total = 3)
  
  .tickProgress(pb, tokens = list(what = paste0("Uploading taxonomy file")))
  tmp <- opal.file_mkdir_tmp(opal)
  opal.file_upload(opal, path, tmp)
  filename <- basename(path)
  file <- paste0(tmp, filename)
  
  .tickProgress(pb, tokens = list(what = paste0("Uploading taxonomy file")))
  opal.post(opal, "system", "conf", "taxonomies", "import", "_file", query = list(file = file))
  opal.file_rm(opal, tmp)
  
  rval <- .tickProgress(pb, tokens = list(what = "Save completed"))
}

#' Get the vocabularies of a taxonomy
#' 
#' Get all the vocabularies of a taxonomy. A vocabulary describes the possible values of variable annotations.
#' 
#' @family taxonomy functions
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy
#' @param locale The language for labels (default is "en").
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.vocabularies(o, 'Mlstr_area')
#' opal.logout(o)
#' }
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

#' Get a taxonomy vocabulary
#' 
#' Get a specific vocabulary details.
#' 
#' @family taxonomy functions
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy
#' @param vocabulary Name of the vocabulary in the taxonomy
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.vocabulary(o, 'Mlstr_area', 'Lifestyle_behaviours')
#' opal.logout(o)
#' }
#' @export
opal.vocabulary <- function(opal, taxonomy, vocabulary) {
  opal.get(opal, "system", "conf", "taxonomy", taxonomy, "vocabulary", vocabulary)
}

#' Get the terms of a vocabulary
#' 
#' Get all the terms of a vocabulary. The term describes the value of a variable annotation. 
#' 
#' @family taxonomy functions
#' @param opal Opal object.
#' @param taxonomy Name of the taxonomy
#' @param vocabulary Name of the vocabulary in the taxonomy
#' @param locale The language for labels (default is "en").
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.terms(o, 'Mlstr_area', 'Lifestyle_behaviours')
#' opal.logout(o)
#' }
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

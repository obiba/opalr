#-------------------------------------------------------------------------------
# Copyright (c) 2016 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get file content from the Opal file system
#' 
#' @title Get file content
#' 
#' @param path Path to the file in the Opal file system.
#' @export
opal.file <- function(opal, path) {
  p <- strsplit(substring(path, 2), "/")[[1]]
  .get(opal, append("files", p))
}

#' Download a file or a folder from the Opal file system
#' 
#' @title Download a file
#' 
#' @param source Path to the file in the Opal file system.
#' @param destination Path to the file to be written. If ommitted, file with same name in the working directory will be written.
#' @examples {
#' # download a file
#' opal.file.download(o, '/home/administrator/joins/join-src-3.csv')
#' opal.file.download(o, '/home/administrator/datashield/hop.R')
#' opal.file.download(o, '/home/administrator/datashield/CNSIM1.zip')
#' 
#' # download, create destination folder and rename file
#' opal.file.download(o, '/home/administrator/spss/DatabaseTest.sav', 'spss/test.sav')
#' 
#' # download a folder
#' opal.file.download(o, '/home/administrator/joins', 'joins.zip')
#' }
#' @export
opal.file.download <- function(opal, source, destination=NULL) {
  content <- opal.file(opal, source)
  name <- basename(source)
  dest <- destination
  if (is.null(destination)) {
    dest <- name
  } else if (dirname(destination) != ".") {
    dir.create(dirname(destination), showWarnings=FALSE, recursive=TRUE)
  }
  if (is.raw(content)) {
    fh <- file(dest,'wb')
    writeBin(content, fh)
    close(fh)
  } else {
    fh <- file(dest,'wb')
    writeChar(content, fh)
    close(fh)
  }
}

#' @export
opal.file.upload <- function(opal, source, destination) {
  fh <- file(source,'rb')
  readChar(fh)
}

#' Write a file from the Opal file system into the R session workspace
#' 
#' @title Write a file
#' 
#' @param source Path to the file in the Opal file system (must exists and be accessible for the user).
#' @param destination Path to the destination file, relative to the R session workspace. Any required sub-folders will be created. If ommitted, file with same name will be written.
#' @examples {
#' # user home expansion
#' opal.file.write(o, "~/spss/DatabaseTest.sav")
#' # rename file
#' opal.file.write(o, "/home/administrator/spss/DatabaseTest.sav", "x.sav")
#' # create sub-folder
#' opal.file.write(o, "/home/administrator/spss/DatabaseTest.sav", "test/x.sav")
#' }
#' @export
opal.file.write <- function(opal, source, destination=NULL) {
  query <- list()
  if (!is.null(source)) {
    query["source"] <- source
  }
  if (!is.null(destination)) {
    query["destination"] <- destination
  }
  ignore <- .getRSessionId(opal)
  res <- .put(opal, "r", "session", opal$rid, "file", "_push", query=query)
}

#' Read a file from the R session workspace into the Opal file system 
#' 
#' @title Read a file
#' 
#' @param source Path to the file in the R session workspace (must exists).
#' @param destination Path to the destination file or folder. Any required sub-folders will be created.
#' @examples {
#' # read into folder
#' opal.file.read(o,"DatabaseTest.sav", "/tmp")
#' # read and rename
#' opal.file.read(o,"test/DatabaseTest.sav", "/tmp/Test.sav")
#' # user home expansion
#' opal.file.read(o,"DatabaseTest.sav", "~/coucou/pwel.sav")
#' }
#' @export
opal.file.read <- function(opal, source, destination) {
  query <- list()
  if (!is.null(source)) {
    query["source"] <- source
  }
  if (!is.null(destination)) {
    query["destination"] <- destination
  }
  ignore <- .getRSessionId(opal)
  res <- .put(opal, "r", "session", opal$rid, "file", "_pull", query=query)
}
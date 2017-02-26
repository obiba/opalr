#-------------------------------------------------------------------------------
# Copyright (c) 2017 OBiBa. All rights reserved.
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
#' @param opal Opal object.
#' @param path Path to the file in the Opal file system.
#' @param key File encryption key: downloaded file will be a zip file with content encrypted (use 7zip to decrypt).
#' @export
opal.file <- function(opal, path, key=NULL) {
  p <- append("files", strsplit(substring(path, 2), "/")[[1]])
  if (is.null(key)) {
    .get(opal, p) 
  } else {
    body <- paste0("key=", key)
    .post(opal, p, body=body, contentType="application/x-www-form-urlencoded")
  }
}

#' Download a file or a folder from the Opal file system
#' 
#' @title Download a file
#' 
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system.
#' @param destination Path to the file to be written. If ommitted, file with same name in the working directory will be written.
#' @param key File encryption key: downloaded file will be a zip file with content encrypted (use 7zip to decrypt).
#' @examples 
#' \dontrun{
#' # download a file
#' opal.file_download(o, '/home/administrator/joins/join-src-3.csv')
#' opal.file_download(o, '/home/administrator/datashield/hop.R')
#' opal.file_download(o, '/home/administrator/datashield/CNSIM1.zip')
#' 
#' # download a file encrypted by a key: resulting file is a zip with an encrypted content
#' opal.file_download(o, '/home/administrator/export/some-data.csv', 
#'                       destination='some-data.zip', key='AZF57893FBDE')
#' 
#' # download, create destination folder and rename file
#' opal.file_download(o, '/home/administrator/spss/DatabaseTest.sav', 'spss/test.sav')
#' 
#' # download a folder
#' opal.file_download(o, '/home/administrator/joins', 'joins.zip')
#' }
#' @export
opal.file_download <- function(opal, source, destination=NULL, key=NULL) {
  content <- opal.file(opal, source, key)
  name <- basename(source)
  dest <- destination
  if (is.null(destination)) {
    dest <- name
    if(!is.null(key)) {
      dest <- paste0(dest, ".zip")
    }
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

#' Upload a file into the Opal file system
#' 
#' @title Upload a file
#' 
#' @param opal Opal object.
#' @param source Path to the file in the local file system.
#' @param destination Path of the destination folder in the Opal file system.
#' @export
opal.file_upload <- function(opal, source, destination) {
  location <- append("files", strsplit(substring(destination, 2), "/")[[1]])
  url <- .url(opal, location)
  sourceFile <- RCurl::fileUpload(source)
  opts = RCurl::curlOptions(httpheader=c(opal$opts$httpheader, 'Accept'='text/html'), verbose=getOption("verbose", FALSE), .opts=opal$opts)
  res <- RCurl::postForm(uri=url, file=sourceFile, .opts=opts, curl=opal$curl)
  res <- opal.file_ls(opal, destination)
}

#' Move and/or rename a file or a folder in the Opal file system
#' 
#' @title Move and/or rename a file
#' 
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system.
#' @param destination New path to the file in the Opal file system.
#' @examples 
#' \dontrun{
#' # move a file to another folder
#' opal.file_mv(o, '/home/administrator/export/some-data.csv', '/home/userx/deliverables')
#' 
#' # rename a file
#' opal.file_mv(o, '/home/administrator/export/some-data-20170123.csv', 
#'                 '/home/administrator/export/some-data.csv')
#' 
#' # move and rename a file
#' opal.file_mv(o, '/home/administrator/export/some-data-20170123.csv', 
#'                 '/home/userx/deliverables/some-data.csv')
#' }
#' @export
opal.file_mv <- function(opal, source, destination) {
  query <- list(action='move', file=source)
  location <- append("files", strsplit(substring(destination, 2), "/")[[1]])
  res <- .put(opal, location, query=query)
}

#' Copy a file or a folder to another location in the Opal file system
#' 
#' @title Copy a file
#' 
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system.
#' @param destination New path to the file in the Opal file system.
#' @examples 
#' \dontrun{
#' # copy a file to another folder
#' opal.file_cp(o, '/home/administrator/export/some-data.csv', '/home/userx/deliverables')
#' 
#' # copy recursively a folder to another folder
#' opal.file_cp(o, '/home/administrator/export', '/home/userx/deliverables')
#' }
#' @export
opal.file_cp <- function(opal, source, destination) {
  query <- list(action='copy', file=source)
  location <- append("files", strsplit(substring(destination, 2), "/")[[1]])
  res <- .put(opal, location, query=query)
}

#' Make a folder in the Opal file system
#' 
#' @title Make a folder
#' 
#' @param opal Opal object.
#' @param path Path to the new folder in the Opal file system.
#' @examples 
#' \dontrun{
#' # make a folder
#' opal.file_mkdir(o, '/home/userx/deliverables')
#' }
#' @export
opal.file_mkdir <- function(opal, path) {
  res <- .post(opal, 'files', body=path, contentType='text/plain')
}

#' List content of a folder in the Opal file system
#' 
#' @title List content of a folder
#' 
#' @param opal Opal object.
#' @param path Path to the folder in the Opal file system.
#' @examples 
#' \dontrun{
#' # list content of a folder
#' opal.file_ls(o, '/home/userx/deliverables')
#' }
#' @export
opal.file_ls <- function(opal, path) {
  location <- append("files", append("_meta",strsplit(substring(path, 2), "/")[[1]]))
  ls <- .get(opal, location)
  res <- NULL
  if (ls$type == 'FOLDER') {
    res <- data.frame(t(sapply(ls$children,c)), stringsAsFactors = FALSE)
    if(!is.null(res$children)) {
      res$children <- NULL
    }
    res$name <- unlist(res$name)
    res$path <- unlist(res$path)
    res$type <- unlist(res$type)
    if (is.null(res$size)) {
      res$size <- rep(NA_integer_, length(res$name))
    } else {
      res$size <- unlist(lapply(res$size, function(s) {
        if(is.numeric(s)) {s} 
        else {NA_integer_}
      }))  
    }
    res$lastModifiedTime <- as.POSIXct(unlist(res$lastModifiedTime)/1000, origin='1970-01-01')
    res$readable <- unlist(res$readable)
    res$writable <- unlist(res$writable)
  } else {
    res <- data.frame(name=ls$name, path=ls$path, type=ls$type, size=ls$size,
                      lastModifiedTime=as.POSIXct(ls$lastModifiedTime/1000, origin='1970-01-01'),
                      readable=ls$readable, writable=ls$writable)
  }
  res
}

#' Remove a file or a folder from the Opal file system
#' 
#' @title Remove a file
#' 
#' @param opal Opal object.
#' @param path Path to the file in the Opal file system.
#' @examples 
#' \dontrun{
#' # remove a file
#' opal.file_rm(o, '/home/administrator/export/some-data.csv')
#' 
#' # remove recursively a folder
#' opal.file_rm(o, '/home/administrator/export')
#' }
#' @export
opal.file_rm <- function(opal, path) {
  location <- append("files", strsplit(substring(path, 2), "/")[[1]])
  res <- tryCatch(.delete(opal, location), error=function(e){})
}

#' Write a file from the Opal file system into the R session workspace
#' 
#' @title Write a file
#' 
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system (must exists and be accessible for the user).
#' @param destination Path to the destination file, relative to the R session workspace. Any required sub-folders will be created. If ommitted, file with same name will be written.
#' @examples 
#' \dontrun{
#' # user home expansion
#' opal.file_write(o, "~/spss/DatabaseTest.sav")
#' # rename file
#' opal.file_write(o, "/home/administrator/spss/DatabaseTest.sav", "x.sav")
#' # create sub-folder
#' opal.file_write(o, "/home/administrator/spss/DatabaseTest.sav", "test/x.sav")
#' }
#' @export
opal.file_write <- function(opal, source, destination=NULL) {
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
#' @param opal Opal object.
#' @param source Path to the file in the R session workspace (must exists).
#' @param destination Path to the destination file or folder. Any required sub-folders will be created.
#' @examples 
#' \dontrun{
#' # read into folder
#' opal.file_read(o,"DatabaseTest.sav", "/tmp")
#' # read and rename
#' opal.file_read(o,"test/DatabaseTest.sav", "/tmp/Test.sav")
#' # user home expansion
#' opal.file_read(o,"DatabaseTest.sav", "~/coucou/pwel.sav")
#' }
#' @export
opal.file_read <- function(opal, source, destination) {
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
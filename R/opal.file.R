#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get file content
#' 
#' Get file content from the Opal file system.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param path Path to the file in the Opal file system.
#' @param key File encryption key: downloaded file will be a zip file with content encrypted (use 7zip to decrypt).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.file(o, '/home/administrator/joins/join-src-3.csv')
#' opal.logout(o)
#' }
#' @export
opal.file <- function(opal, path, key=NULL) {
  p <- append("files", strsplit(substring(path, 2), "/")[[1]])
  if (is.null(key)) {
    opal.get(opal, p) 
  } else {
    body <- paste0("key=", key)
    opal.post(opal, p, body=body, contentType="application/x-www-form-urlencoded")
  }
}

#' Download a file
#' 
#' Download a file or a folder from the Opal file system.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system.
#' @param destination Path to the file to be written. If ommitted, file with same name in the working directory will be written.
#' @param key File encryption key: downloaded file will be a zip file with content encrypted (use 7zip to decrypt).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # download a file
#' opal.file_download(o, '/home/administrator/joins/join-src-3.csv')
#' # download a file encrypted by a key: resulting file is a zip with an encrypted content
#' opal.file_download(o, '/home/administrator/export/some-data.csv', 
#'                       destination='some-data.zip', key='AZF57893FBDE')
#' # download, create destination folder and rename file
#' opal.file_download(o, '/home/administrator/spss/DatabaseTest.sav', 'spss/test.sav')
#' # download a folder
#' opal.file_download(o, '/home/administrator/export', 'export.zip')
#' opal.logout(o)
#' }
#' @export
opal.file_download <- function(opal, source, destination=NULL, key=NULL) {
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
  p <- append("files", strsplit(substring(source, 2), "/")[[1]])
  if (is.null(key)) {
    ignore <- opal.get(opal, p, outFile = dest)
  } else {
    body <- paste0("key=", key)
    ignore <- opal.post(opal, p, body=body, contentType="application/x-www-form-urlencoded", outFile = dest)
  }
}

#' Upload a file or a folder
#' 
#' Upload a file or a folder into the Opal file system. Creates the destination folder (and parents)
#' when necessary. Hidden files and folders (with name starting with dot) can be excluded.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param source Path to the file in the local file system.
#' @param destination Path of the destination folder in the Opal file system. Folder (and parents) will be created if missing.
#' @param all.files	When FALSE, upload only visible files (following Unix-style visibility, that is files whose name 
#' does not start with a dot). Default is TRUE.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # upload a file
#' opal.file_upload(o, 'some_data.csv', '/home/administrator')
#' # upload a folder
#' opal.file_upload(o, 'some_data', '/home/administrator')
#' opal.logout(o)
#' }
#' @export
opal.file_upload <- function(opal, source, destination, all.files = TRUE) {
  if (!file.exists(source)) {
    stop("Source file does not exist")
  }
  
  doUploadFile <- function(src, dest) {
    location <- append("files", strsplit(substring(dest, 2), "/")[[1]])
    r <- POST(.url(opal, location), body=list(file=upload_file(src)), encode = "multipart", 
              content_type("multipart/form-data"), accept("text/html"), 
              config=opal$config, handle=opal$handle, .verbose())
    res <- .handleResponse(opal, r)
  }
  
  if (file.info(source)$isdir) {
    sourceDir <- normalizePath(source)
    parentDir <- dirname(sourceDir)
    pb <- .newProgress(total = 2 + length(list.files(sourceDir, recursive = TRUE, include.dirs = TRUE, all.files = all.files)))
    # split path into segments, excluding '.'
    split_path <- function(x) {
      if (dirname(x) == x) x
      else {
        d <- dirname(x)
        if (d == ".")
          basename(x)
        else
          c(basename(x), split_path(d))
      }
    }
    
    lapply(list.dirs(sourceDir, recursive = TRUE), function(d) {
      # check for dots in relative directory
      rd <- substring(d, first = nchar(parentDir) + 2)
      if (all.files || !any(startsWith(split_path(rd), "."))) {
        # make opal directory
        od <- paste0(destination, "/", rd)
        .tickProgress(pb, tokens = list(what = paste0("Make directory ", od)))
        opal.file_mkdir(opal, od, parents = TRUE)
        # upload regular files from local directory
        lapply(list.files(d, full.names = TRUE, all.files = all.files), function(f) {
          if (!file.info(f)$isdir) {
            .tickProgress(pb, tokens = list(what = paste0("Upload file ", f)))
            doUploadFile(f, od)
          }
        })
      }
    })
    ignore <- .tickProgress(pb, tokens = list(what = paste0("Uploaded: ", source)))
  } else if (all.files || !startsWith(basename(normalizePath(source)), ".")) {
    opal.file_mkdir(opal, destination, parents = TRUE)
    doUploadFile(source, destination)
  }
}

#' Move and/or rename a file
#' 
#' Move and/or rename a file or a folder in the Opal file system.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system.
#' @param destination New path to the file in the Opal file system.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # move a file to another folder
#' opal.file_mv(o, '/home/administrator/export/some-data.csv', '/home/userx/deliverables')
#' # rename a file
#' opal.file_mv(o, '/home/administrator/export/some-data-20170123.csv', 
#'                 '/home/administrator/export/some-data.csv')
#' # move and rename a file
#' opal.file_mv(o, '/home/administrator/export/some-data-20170123.csv', 
#'                 '/home/userx/deliverables/some-data.csv')
#' opal.logout(o)
#' }
#' @export
opal.file_mv <- function(opal, source, destination) {
  query <- list(action='move', file=source)
  location <- append("files", strsplit(substring(destination, 2), "/")[[1]])
  res <- opal.put(opal, location, query=query)
}

#' Copy a file
#' 
#' Copy a file or a folder to another location in the Opal file system.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system.
#' @param destination New path to the file in the Opal file system.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # copy a file to another folder
#' opal.file_cp(o, '/home/administrator/export/some-data.csv', '/home/userx/deliverables')
#' # copy recursively a folder to another folder
#' opal.file_cp(o, '/home/administrator/export', '/home/userx/deliverables')
#' opal.logout(o)
#' }
#' @export
opal.file_cp <- function(opal, source, destination) {
  query <- list(action='copy', file=source)
  location <- append("files", strsplit(substring(destination, 2), "/")[[1]])
  res <- opal.put(opal, location, query=query)
}

#' Make a folder
#' 
#' Make a folder in the Opal file system. Use the parents parameter to ignore if it already 
#' exist and to create parent folders.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param path Path to the new folder in the Opal file system.
#' @param parents No error if existing, make parent directories as needed. Default is FALSE. 
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # make a folder
#' opal.file_mkdir(o, '/home/administrator/test', parents = TRUE)
#' opal.logout(o)
#' }
#' @export
opal.file_mkdir <- function(opal, path, parents = FALSE) {
  segments <- strsplit(substring(path, 2), "/")[[1]]
  if (parents) {
    subpath <- ""
    for (i in 1:length(segments)) {
      parentpath <- subpath
      segment <- segments[[i]]
      subpath <- paste0(parentpath, "/", segment)
      if (!(segment %in% opal.file_ls(opal, ifelse(parentpath == "", "/", parentpath))$name)) {
        opal.file_mkdir(opal, subpath, parents = FALSE)
      }
    }
  } else {
    location <- append("files", segments)
    folder <- location[[length(location)]]
    location <- location[1:length(location)-1]
    res <- opal.post(opal, location, body=folder, contentType='text/plain') 
  }
}

#' Make a temporary folder
#' 
#' Make a user personal temporary folder in the Opal file system (make sure it does not exists).
#' 
#' @family file functions
#' @param opal Opal object.
#' @return The path of the created folder.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # make a folder
#' path <- opal.file_mkdir_tmp(o)
#' opal.logout(o)
#' }
#' @export
opal.file_mkdir_tmp <- function(opal) {
  tmpName <- sample(1000000:9999999, 1)
  basePath <- paste0("/home/", opal$username, "/R")
  opal.file_mkdir(opal, basePath, parents = TRUE)
  tmpExists <- tmpName %in% opal.file_ls(opal, basePath)$name
  while(tmpExists) {
    tmpName <- sample(1000000:9999999, 1)
    tmpExists <- tmpName %in% opal.file_ls(opal, basePath)$name
  }
  tmp <- paste0(basePath, "/", tmpName, "/")
  opal.file_mkdir(opal, tmp)
  tmp
}

#' List content of a folder
#' 
#' List content of a folder in the Opal file system.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param path Path to the folder in the Opal file system.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # list content of a folder
#' opal.file_ls(o, '/home/administrator')
#' opal.logout(o)
#' }
#' @export
opal.file_ls <- function(opal, path) {
  location <- append("files", append("_meta",strsplit(substring(path, 2), "/")[[1]]))
  ls <- opal.get(opal, location)
  res <- NULL
  if (ls$type == 'FILE') {
    # a regular file
    res <- data.frame(name=ls$name, path=ls$path, type=ls$type, size=ls$size,
                      lastModifiedTime=as.POSIXct(ls$lastModifiedTime/1000, origin='1970-01-01'),
                      readable=ls$readable, writable=ls$writable)
  } else if (!is.null(ls$children)) {
    # a folder with children
    children <- lapply(ls$children, function(child) {
      if (child$type == 'FOLDER') {
        # size is missing, force size value (but don't know how to insert an element or how to order the elements of a list)
        child <- list(name=child$name, path=child$path, type=child$type, size=NA_integer_, lastModifiedTime=child$lastModifiedTime, readable=child$readable, writable=child$writable)
      }
      child
    })
    res <- data.frame(t(sapply(children,c)), stringsAsFactors = FALSE)
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
    # an empty folder
    res <- data.frame()
  }
  res
}

#' Remove a file
#' 
#' Remove a file or a folder from the Opal file system.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param path Path to the file in the Opal file system.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # remove a file
#' opal.file_rm(o, '/home/administrator/export/some-data.csv')
#' # remove recursively a folder
#' opal.file_rm(o, '/home/administrator/export')
#' opal.logout(o)
#' }
#' @export
opal.file_rm <- function(opal, path) {
  location <- append("files", strsplit(substring(path, 2), "/")[[1]])
  res <- tryCatch(opal.delete(opal, location), error=function(e){})
}

#' Write a file
#' 
#' Write a file from the Opal file system into the R session workspace.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system (must exists and be accessible for the user).
#' @param destination Path to the destination file, relative to the R session workspace. Any required sub-folders will be created. If ommitted, file with same name will be written.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # user home expansion
#' opal.file_write(o, "~/spss/DatabaseTest.sav")
#' # rename file
#' opal.file_write(o, "/home/administrator/spss/DatabaseTest.sav", "x.sav")
#' # create sub-folder
#' opal.file_write(o, "/home/administrator/spss/DatabaseTest.sav", "test/x.sav")
#' opal.logout(o)
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
  res <- opal.put(opal, "r", "session", opal$rid, "file", "_push", query=query)
}

#' Read a file
#' 
#' Read a file from the R session workspace into the Opal file system.
#' 
#' @family file functions
#' @param opal Opal object.
#' @param source Path to the file in the R session workspace (must exists).
#' @param destination Path to the destination file or folder. Any required sub-folders will be created.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # read into folder
#' opal.file_read(o,"DatabaseTest.sav", "/tmp")
#' # read and rename
#' opal.file_read(o,"test/DatabaseTest.sav", "/tmp/Test.sav")
#' # user home expansion
#' opal.file_read(o,"DatabaseTest.sav", "~/coucou/pwel.sav")
#' opal.logout(o)
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
  res <- opal.put(opal, "r", "session", opal$rid, "file", "_pull", query=query)
}

#' Unzip a zip archive file
#'
#' Unzip a zip archive file from the Opal file system.
#'
#' @family file functions
#' @param opal Opal object.
#' @param source Path to the file in the Opal file system (must exist and have the ".zip" file extension).
#' @param destination Path to the destination file or folder in the Opal file system.
#' @param key Key to decrypt archive.
#' @return The path of the extracted archive folder in the Opal file system.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # unzip
#' path <- opal.file_unzip(o, "/tmp/TESTING.zip", "/home/administrator")
#' opal.logout(o)
#' }
#' @export
opal.file_unzip <- function(opal, source, destination, key=NULL) {
  if (opal.version_compare(opal,"4.2")<0) {
    stop("Zip archive extraction requires Opal 4.2 or higher.")
  }
  query <- list(action = "unzip", destination = destination)

  if (!is.null(key)) {
    query["key"] <- key
  }

  location <- append("files", strsplit(substring(source, 2), "/")[[1]])
  res <- opal.post(opal, location, query=query, contentType = "application/json")
  res$path
}
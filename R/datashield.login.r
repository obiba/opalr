#-------------------------------------------------------------------------------
# Copyright (c) 2013 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' 
#' @title Logs in and assigns variables to R
#' @description This function allows for clients to login to opal servers 
#' and (optionaly) assign all the data or specific variables from Opal 
#' datasources to R. The assigned dataframes (one for each opal server) 
#' are named 'D'.
#' @param logins a dataframe table that holds login details. This table holds five elements 
#' required to login to the servers where the data to analyse is stored. 
#' See the documentation of the examplar input table \code{logindata} for details of the login 
#' elements.
#' @param assign name of the dataframe to which the opal's dataset will be assigned after login into the server(s).
#' @param variables specific variables to assign. If \code{assign} is set to FALSE
#' this argument is ignored otherwise the specified variables are assigned to R.
#' If no variables are specified (default) the whole dataset is assigned.
#' @param dir directory where to look for key pairs files. If key file path is relative, default is to look in 
#' user .ssh directory, then in current working directory.
#' @return object(s) of class opal
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' #### The below examples illustrate an analysises that use test/simulated data ####
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # Example 1: just login (default)
#' opals <- datashield.login(logins=logindata)
#'
#' # Example 2: login and assign the whole dataset
#' opals <- datashield.login(logins=logindata,assign=TRUE)
#' 
#' # Example 3: login and assign specific variable(s)
#' myvar <- list("LAB_TSC")
#' opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#' }
#' 
datashield.login <- function(logins=NULL, assign=NULL, variables=NULL, dir=NULL){
  
  # issue an alert and stop the process if no login table is provided
  if(is.null(logins)){
    stop(" Provide valid login details!\n\n")
  }
  
  # studies names
  stdnames <- as.character(logins$server)
  
  # URLs 
  urls <- as.character(logins$url)
  
  # usernames
  userids <- as.character(logins$userID)
  
  # passwords
  pwds <- as.character(logins$pwd)
  
  # opal directories where the microdata is stored
  paths <- as.character(logins$opalPath)
  
  # put the server names in a list
  opals <- as.list(stdnames)  
  
  # login to the opals keeping the server names as specified in the login file
  cat("\nLogging into the collaborating servers\n")
  opals <- vector("list", length(urls))
  names(opals) <- as.character(logins[,1])
  for(i in 1:length(opals)) {
    # if the connection is HTTPS use ssl options else they are not required
    protocol <- strsplit(urls[i], split="://")[[1]][1]
    if(protocol=="https"){
      # pem files or username/password ?
      if (grepl("\\.pem$",userids[i])) {
        cert <- .getPEMFilePath(userids[i], dir)
        private <- .getPEMFilePath(pwds[i], dir)
        credentials <- list(sslcert=cert, sslkey=private, ssl.verifyhost=0, ssl.verifypeer=0, sslversion=3)
        opals[[i]] <- opal.login(url=urls[i], opts=credentials)
      } else {
        options <- list(ssl.verifyhost=0, ssl.verifypeer=0, sslversion=3)
        opals[[i]] <- opal.login(username=userids[i], password=pwds[i], url=urls[i], opts=options)
      }
    } else {
      opals[[i]] <- opal.login(username=userids[i], password=pwds[i], url=urls[i])  
    }
  }
  
  # if argument 'assign' is true assign data to the opal server(s) you logged 
  # in to. If no variables are specified the whole dataset is assigned
  # i.e. all the variables in the opal database are assigned
  if(!is.null(assign)) {
    symbol <- assign
    # case of misusage
    if (is.logical(assign)) {
      symbol <- "D"
    }
    if(is.null(variables)){
      # if the user does not specify variables (default behaviour)
      # display a message telling the user that the whole dataset
      # will be assigned since he did not specify variables
      cat("\n  No variables have been specified. \n  All the variables in the opal datasource \n  (the whole dataset) will be assigned to R!\n\n")
      cat("\nAssigining data:\n")
      for(i in 1:length(opals)) {
        cat(stdnames[i],"\n")
        datashield.assign(opals[[i]], symbol, paths[i])
      }
      cat("\nVariables assigned:\n")
      varnames <- datashield.aggregate(opals[1], quote(colnames(D)))
      cat(paste(unlist(varnames), collapse=", "), "\n\n")
    } else {
      cat("\nAssigining data:\n")
      for(i in 1:length(opals)) {
        cat(stdnames[i],"\n")
        datashield.assign(opals[[i]], symbol, paths[i], variables)
      }
      cat("\nVariables assigned:\n")
      cat(paste(unlist(variables), collapse=", "), "\n\n")
    }
  }
  
  # return the 'opal' object
  return(opals)
}

#' Extract absolute path to the pem file
#' @keywords internal
.getPEMFilePath <- function(pem, dir="~/.ssh") {
  path <- pem
  if (file.access(pem)) {
    # file exists (absolute path)
    path <- path.expand(pem)
  } else if (file.access(paste0(dir, pem))) {
    # file relative to given dir
    path <- path.expand(paste0(dir, pem))
  } else if (file.access(paste0(getwd(), pem))) {
    # file relative to working directory
    path <- paste0(getwd(), pem)
  }
  
  path
}
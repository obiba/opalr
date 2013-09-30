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
#' @param logins A dataframe table that holds login details. This table holds five elements 
#' required to login to the servers where the data to analyse is stored. The expected column names are 'server' (the server name),
#' 'url' (the opal url), 'user' (the user name or the certificate file path), 'password' (the user password or the private key file path),
#' 'table' (the fully qualified name of the dataset in opal). See the documentation of the examplar input table \code{logindata} 
#' for details of the login elements.
#' @param symbol Name of the dataframe to which the opal's dataset will be assigned after login into the server(s). By default no 
#' assignment is performed.
#' @param variables Specific variables to assign. If \code{symbol} is not set this argument is ignored, otherwise the specified 
#' variables are assigned to R. If no variables are specified (default) the whole dataset is assigned.
#' @param directory Directory where to look for key pairs files (certificate and private key). If key file path is relative, default is to look in 
#' user '.ssh' directory, then in current working directory.
#' @return Object(s) of class opal
#' @author Gaye, A.
#' @export
#' @examples {
#' 
#' #### The below examples illustrate an analysises that use test/simulated data ####
#' 
#' # load that contains the login details
#' data(logindata)
#' 
#' # or build your data.frame
#' server <- c("study1", "study2")
#' url <- c("https://some.opal.host:8443","https://another.opal.host")
#' user <- c("user1", "datashield-certificate.pem")
#' password <- c("user1pwd", "datashield-private.pem")
#' table <- c("store.Dataset","foo.DS")
#' logindata <- data.frame(server,url,user,password,table)
#' 
#' # Example 1: just login (default)
#' opals <- datashield.login(logins=logindata)
#'
#' # Example 2: login and assign the whole dataset
#' opals <- datashield.login(logins=logindata,symbol="D")
#' 
#' # Example 3: login and assign specific variable(s)
#' opals <- datashield.login(logins=logindata,symbol="D",variables=list("LAB_TSC"))
#' }
#' 
datashield.login <- function(logins=NULL, symbol=NULL, variables=NULL, directory="~/.ssh"){
  
  # issue an alert and stop the process if no login table is provided
  if(is.null(logins)){
    stop("Login details are missing!\n")
  }
  if (!is.data.frame(logins)) {
    stop("Provide valid login details!\n")
  }
  
  # studies names
  stdnames <- as.character(logins$server)
  
  # url
  urls <- as.character(logins$url)
  
  # user or username or userID
  userids <- as.character(logins$user)
  
  # password or pwd
  pwds <- as.character(logins$password)
  if (length(pwds) == 0) {
    pwds <- as.character(logins$pwd)
  }
  
  # opal table fully qualified name: table or opalPath
  paths <- as.character(logins$table)
  if (length(paths) == 0) {
    paths <- as.character(logins$opalPath)
  }
  
  # put the server names in a list
  opals <- as.list(stdnames)  
  
  # login to the opals keeping the server names as specified in the login file
  cat("Logging into the collaborating servers...\n")
  opals <- vector("list", length(urls))
  names(opals) <- as.character(logins[,1])
  for(i in 1:length(opals)) {
    # if the connection is HTTPS use ssl options else they are not required
    protocol <- strsplit(urls[i], split="://")[[1]][1]
    if(protocol=="https"){
      # pem files or username/password ?
      if (grepl("\\.pem$",userids[i])) {
        cert <- .getPEMFilePath(userids[i], directory)
        private <- .getPEMFilePath(pwds[i], directory)
        credentials <- list(sslcert=cert, sslkey=private, ssl.verifyhost=0, ssl.verifypeer=0, sslversion=3)
        opals[[i]] <- opal.login(url=urls[i], opts=credentials)
      } else {
        options <- list(ssl.verifyhost=0, ssl.verifypeer=0, sslversion=3)
        opals[[i]] <- opal.login(username=userids[i], password=pwds[i], url=urls[i], opts=options)
      }
    } else {
      opals[[i]] <- opal.login(username=userids[i], password=pwds[i], url=urls[i])  
    }
    # set the study name to corresponding opal object
    opals[[i]]$name <- stdnames[i]
  }
  
  # if argument 'assign' is true assign data to the opal server(s) you logged 
  # in to. If no variables are specified the whole dataset is assigned
  # i.e. all the variables in the opal database are assigned
  if(!is.null(symbol)) {
    cat("Assigning data:\n")
    if(is.null(variables)){
      # if the user does not specify variables (default behaviour)
      # display a message telling the user that the whole dataset
      # will be assigned since he did not specify variables
      cat(" No variables have been specified.\n All the variables in the opal table (the whole dataset) will be assigned to R!\n")
      for(i in 1:length(opals)) {
        cat(paste(stdnames[i],":",paths[i]),"\n")
        datashield.assign(opals[[i]], symbol, paths[i])
      }
    } else {
      for(i in 1:length(opals)) {
        cat(paste("",stdnames[i],":",paths[i], paste0('[',paste(unlist(variables), collapse=", "),']'),"\n"))
        datashield.assign(opals[[i]], symbol, paths[i], variables=variables)
      }
    }
  }
  
  # return the 'opal' object
  return(opals)
}

#' Clear the Datashield R sessions and logout from Opal(s).
#' 
#' @title Logout from Opal(s)
#' 
#' @param opals Opal object or a list of opals.
#' @export
datashield.logout <- function(opals) {
  if (is.list(opals)) {
    lapply(opals, function(o){datashield.logout(o)})  
  } else {
    datashield.rmSessions(opals)
  }
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
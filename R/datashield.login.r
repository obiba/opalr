#-------------------------------------------------------------------------------
# Copyright (c) 2014 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#'
#'@title Logs in and assigns variables to R
#'@description This function allows for clients to login to opal servers 
#'and (optionaly) assign all the data or specific variables from Opal 
#'tables to R dataframes. The assigned dataframes (one for each opal server) 
#'are named 'D' (by default).
#'@param logins A dataframe table that holds login details. This table holds five elements 
#'required to login to the servers where the data to analyse is stored. The expected column names are 'server' (the server name),
#''url' (the opal url), 'user' (the user name or the certificate file path), 'password' (the user password or the private key file path),
#''table' (the fully qualified name of the table in opal). An additional column 'identifiers' can be specified for identifiers
#'mapping (from Opal 2.0).
#'See also the documentation of the examplar input table \code{logindata} for details of the login 
#'elements.
#'@param assign A boolean which tells whether or not data should be assigned from the opal 
#'table to R after login into the server(s).
#'@param variables Specific variables to assign. If \code{assign} is set to FALSE
#'this argument is ignored otherwise the specified variables are assigned to R.
#'If no variables are specified (default) the whole opal's table is assigned.
#'@param symbol A character, the name of the dataframe to which the opal's table will be assigned after login 
#'into the server(s).
#'@param directory A character that indicates the location of the key pairs files (certificate and private key). If 
#'the default location is the users '.ssh' directory.
#'@return object(s) of class opal
#'@author Gaye, A.
#'@export
#'@examples {
#'
#'#### The below examples illustrate an analysises that use test/simulated data ####
#'
#'# build your data.frame
#'server <- c("study1", "study2")
#'url <- c("https://some.opal.host:8443","https://another.opal.host")
#'user <- c("user1", "datashield-certificate.pem")
#'password <- c("user1pwd", "datashield-private.pem")
#'table <- c("store.Dataset","foo.DS")
#'logindata <- data.frame(server,url,user,password,table)
#'
#'# or load the data.frame that contains the login details
#'data(logindata)
#'
#'# Example 1: just login (default)
#'opals <- datashield.login(logins=logindata)
#'
#'# Example 2: login and assign the whole dataset
#'opals <- datashield.login(logins=logindata,assign=TRUE)
#'
#'# Example 3: login and assign specific variable(s)
#'myvar <- list("LAB_TSC")
#'opals <- datashield.login(logins=logindata,assign=TRUE,variables=myvar)
#'}
#'
datashield.login <- function(logins=NULL, assign=FALSE, variables=NULL, symbol="D", directory="~/.ssh"){
  
  # issue an alert and stop the process if no login table is provided
  if(is.null(logins)){
    stop("Provide valid login details!", call.=FALSE)
  }
  
  # studies names
  stdnames <- as.character(logins$server)
  
  # URLs 
  urls <- as.character(logins$url)
  
  # usernames
  userids <- as.character(logins$user)
  
  # passwords
  pwds <- as.character(logins$password)
  
  # opal table fully qualified name
  paths <- as.character(logins$table)
  
  # identifiers mapping
  idmappings <- logins$identifiers
  
  # opal specific options
  options <- logins$options
  print(options)
  
  # name of the assigned dataframe - check the user gave a character string as name
  if(!(is.character(symbol))){
    message("\nWARNING: symbol has been set to 'D' because the provided value is not a valid character!")
    symbol <- "D"
  }
  
  # put the server names in a list
  opals <- as.list(stdnames)  
  
  # login to the opals keeping the server names as specified in the login file
  message("\nLogging into the collaborating servers")
  opals <- vector("list", length(urls))
  names(opals) <- as.character(logins[,1])
  for(i in 1:length(opals)) {
    # if the connection is HTTPS use ssl options else they are not required
    protocol <- strsplit(urls[i], split="://")[[1]][1]
    if(protocol=="https"){
      opal.opts <- eval(parse(text=as.character(options[[i]])))
      # pem files or username/password ?
      if (grepl("\\.pem$",userids[i])) {
        cert <- opal:::.getPEMFilePath(userids[i], directory)
        private <- opal:::.getPEMFilePath(pwds[i], directory)
        opal.opts <- append(opal.opts, list(sslcert=cert, sslkey=private, ssl.verifyhost=0, ssl.verifypeer=0))
        opals[[i]] <- opal.login(url=urls[i], opts=opal.opts)
      } else {
        opal.opts <- append(opal.opts, list(ssl.verifyhost=0, ssl.verifypeer=0))
        opals[[i]] <- opal.login(username=userids[i], password=pwds[i], url=urls[i], opts=opal.opts)
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
  if(assign){
    if(is.null(variables)){
      # if the user does not specify variables (default behaviour)
      # display a message telling the user that the whole dataset
      # will be assigned since he did not specify variables
      message("\n  No variables have been specified. \n  All the variables in the opal table \n  (the whole dataset) will be assigned to R!")
    }
  
    # Assign data in parallel
    message("\nAssigining data:")
    rids <- lapply(1:length(opals), function(i) {
      message(stdnames[i],"...")
      datashield.assign(opals[[i]], symbol, paths[i], variables, identifiers=idmappings[i], async=TRUE, wait=FALSE)
    })
    rcmds <- datashield.command(opals, rids, wait=TRUE)
    lapply(1:length(stdnames), function(i) {
      if (!is.null(rcmds[[i]]) && rcmds[[i]]$status == "FAILED") {
        if (!is.null(rcmds[[i]]$error) && rcmds[[i]]$error == "Unable to locate current JTA transaction") {
          # second chance because of a bug in opal 2.1.0 (OPAL-2583)
          datashield.assign(opals[[i]], symbol, paths[i], variables, identifiers=idmappings[i], async=FALSE, wait=TRUE)
        } else {
          warning("Data assignment of '", paths[i],"' failed for '", stdnames[i],"': ", rcmds[[i]]$error, call.=FALSE, immediate.=TRUE)
        }
      }
    })
    
    # Get column names in parallel
    message("\nVariables assigned:")
    res <- datashield.aggregate(opals, paste0('colnames(',symbol,')'))
    lapply(1:length(stdnames), function(i) {
      varnames <- res[[i]]
      if(length(varnames[[1]]) > 0) {
        message(stdnames[i],"--",paste(unlist(varnames), collapse=", "))
      } else {
        message(stdnames[i],"-- No variables assigned. Please check login details for this study and verify that the variables are available!")
      }
    })
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
    res <- lapply(opals, function(o){datashield.logout(o)})
  } else {
    res <- datashield.rmSessions(opals)
  }
}


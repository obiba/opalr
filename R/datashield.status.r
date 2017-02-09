#-------------------------------------------------------------------------------
# Copyright (c) 2017 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the status of the table(s), method(s), and packgage(s) in differents Opal servers.
#'
#' @title Check Datashield configuration status
#' @description This function allows for clients to check each server for data access, configuration and versions.
#' @param logins A dataframe table that holds login details. This table holds five elements 
# 'required to login to the servers where the data to analyse is stored. The expected column names are 'server' (the server name),
#' 'url' (the opal url), 'user' (the user name or the certificate file path), 'password' (the user password or the private key file path),
#' 'table' (the fully qualified name of the table in opal). An additional column 'identifiers' can be specified for identifiers
#' mapping (from Opal 2.0).
#' See also the documentation of the examplar input table \code{logindata} for details of the login 
#' elements.
#' @param study Limit the status inspection to one or more studies which name is specified by this parameter.
#' @param directory Where to look for certificate and private key. Default is user's .ssh folder.
#' @param timeout Time in seconds after which a request must have been completed. 0 (zero) means it never times out during transfer. Default is 20 seconds.
#' @return A list of various system status
#' @author Mbatchou, S.
#' @export
#' @examples 
#' \dontrun{
#'
#' #### The below examples illustrate an analysises that use test/simulated data ####
#'
#' # build your data.frame
#' server <- c("study1", "study2")
#' url <- c("https://some.opal.host:8443","https://another.opal.host")
#' user <- c("user1", "datashield-certificate.pem")
#' password <- c("user1pwd", "datashield-private.pem")
#' table <- c("store.Dataset","foo.DS")
#' logindata <- data.frame(server,url,user,password,table)
#'
#' # or load the data.frame that contains the login details
#' data(logindata)
#'
#' # Example 1: check all servers (default)
#' datashield.status(logins=logindata)
#'
#' # Example 2: check a list of named servers
#' datashield.status(logins=logindata,study=c("study1"))
#'
#'}
#'
datashield.status <- function(logins=NULL, study=NULL, directory="~/.ssh", timeout=20){
  
  # issue an alert and stop the process if no login table is provided
  if(missing(logins)){
    stop("Provide valid login details!", call.=FALSE)
  }
  # study list provided?
  if(missing(study)){
    study_flag<-F
  }else{
    study<-as.list(study)
    study_flag<-T
  }
  
  #sanity check of study list provided
  if(study_flag & any(! study %in% logins$server)){
    stop('Provide a list of valid study(ies) name(s)',call.=FALSE)
  }
  
  #subset logins if study list is provided
  if(study_flag){
    logins <- logins[logins$server %in% study,]
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
  
  # put the server names in a list
  opals <- as.list(stdnames)  
  
  # login to the opals keeping the server names as specified in the login file
  message("\nChecking status of the collaborating servers")
  opals <- vector("list", length(urls))
  names(opals) <- as.character(logins[,1])
  default.opts <- c(timeout=timeout)
  for(i in 1:length(opals)) {
    # connection options
    opal.opts <- eval(parse(text=as.character(options[[i]])))
    if (!is.null(opal.opts)) {
      opal.opts <- append(default.opts, opal.opts)
    } else {
      opal.opts <- default.opts
    }
    # if the connection is HTTPS use ssl options else they are not required
    protocol <- strsplit(urls[i], split="://")[[1]][1]
    if(protocol=="https"){
      # pem files or username/password ?
      if (grepl("\\.pem$",userids[i])) {
        cert <- .getPEMFilePath(userids[i], directory)
        private <- .getPEMFilePath(pwds[i], directory)
        opal.opts <- append(opal.opts, list(sslcert=cert, sslkey=private, ssl.verifyhost=0, ssl.verifypeer=0))
        opals[[i]] <- opal.login(url=urls[i], opts=opal.opts)
      } else {
        opal.opts <- append(opal.opts, list(ssl.verifyhost=0, ssl.verifypeer=0))
        opals[[i]] <- opal.login(username=userids[i], password=pwds[i], url=urls[i], opts=opal.opts)
      }
    } else {
      opals[[i]] <- opal.login(username=userids[i], password=pwds[i], url=urls[i], opts=opal.opts)
    }
    # set the study name to corresponding opal object
    opals[[i]]$name <- stdnames[i]
  }
  
  # check status
  table_status <- datashield.table_status(opals,logins)
  
  # filter out the opal servers that did not respond
  valid_opals <- Filter(function(o) {!is.na(o$version)},opals)
  
  pkg_status<-datashield.pkg_status(valid_opals)
  
  type_fctn =  list(aggregate_status = 'aggregate',assign_status = 'assign')
  fctn_status<-lapply(type_fctn,function(x) datashield.method_status(valid_opals,x))
  
  whole_status<-c(list(table_status = table_status),pkg_status,fctn_status)
  
  
  #login out of datashield
  datashield.logout(opals)
  
  #return status
  return(whole_status)
}

#' Get the status of the tables in differents Opals servers.
#'
#' @title Status of table(s) in Opal(s)
#'
#' @param opal A list of opal objects.
#' @param logins A dataframe table that holds login details.
#' @return Servers and tables accessibility status
#' @export
datashield.table_status<-function(opal,logins){
  if(is.null(opal)){
    stop("Provide opal object!", call.=FALSE)
  }
  if(is.null(logins)){
    stop("Provide valid login details!", call.=FALSE)
  }
  
  # studies names
  stdnames <- as.character(logins$server)
  
  # opal table fully qualified name
  tbl_full_name <- as.character(logins$table)
  
  # URLs 
  urls <- as.character(logins$url)
  
  # opal specific options
  options <- logins$options
  
  # verify table accessibility and validity
  info_df<-data.frame(NULL)
  for(i in seq(length(opal))){
    stdname<-stdnames[i]
    tbl_full_name_split<-unlist(strsplit(tbl_full_name[i],'\\.'))
    datasource<-tbl_full_name_split[1]
    tbl_name<-tbl_full_name_split[2]
    login_info<-try(opal.table(opal[[stdname]],datasource=datasource,table=tbl_name),silent=T)
    is_error<-inherits(login_info,what='try-error')
    o<-opal[[stdname]]
    accessibility<-T
    if (is_error){
      warning(stdname,': table ' ,tbl_full_name[i], ' is not accessible or does not exist...',call.=F,immediate.=T)
      accessibility<-F
    }
    df<-data.frame(study = stdname,url = urls[i],version=o$version,table_name=tbl_full_name[i],accessibility = accessibility)
    info_df<-rbind(info_df,df)
    
  }
  return (info_df)
}


#' Get the status of the datashield method(s) in different(s) Opal(s) server(s).
#'
#' @title Status of datashield method(s) in Opal(s)
#'
#' @param opal A list of opal objects.
#' @param type Type of the method: "aggregate" (default) or "assign".
#' @return Methods availability on each server.
#' @export
datashield.method_status<-function(opal,type='aggregate'){
  
  if(is.null(opal)){
    stop("Provide valid opal object!", call.=FALSE)
  }
  
  res<-datashield.methods(opal,type)
  #unique method names 
  unique_name<-unique(unlist(lapply(res,function(x) x$name)))
  
  #loop over each study and look if each function (y) in unique name is present in study (x)
  status<-sapply(res,function(x)  { sapply(unique_name,function(y){ y %in% x$name})})
  
  df_fnc<-data.frame(name = unique_name,status)
  
  return (df_fnc)
}


#' Get the status of the datashield package(s) in different(s) Opal(s) server(s).
#'
#' @title Status of datashield package(s) in Opal(s)
#'
#' @param opal A list of opal objects.
#' @return Packages status for each server.
#' @export
datashield.pkg_status <- function(opal) {
  if(is.null(opal)){
    stop("Provide valid opal object!", call.=FALSE)
  }
  types = c('aggregate','assign')
  df_pkges<-data.frame(NULL)
  df_verses<-data.frame(NULL)
  pkg_checked<-NULL
  
  for(type in types){
    res<-datashield.methods(opal,type)
    #package names by type
    unique_pkg<-unique(unlist(lapply(res,function(x) x$package)))
    
    #new package to check
    pkg_tocheck<-subset(unique_pkg,!unique_pkg %in% pkg_checked)
    
    #pkg_tbl
    status<-sapply(res,function(x) { sapply(pkg_tocheck,function(y){ y %in% x$package})})
    df_pkg<-data.frame(package = pkg_tocheck, status)
    df_pkges<-rbind(df_pkges,df_pkg)
    
    #pkg_vers
    vers <-sapply(res,function(x){   
      sapply(pkg_tocheck,function(y){
        pkg.in.study<-as.character(x$package) #avoid problem with different factor levels (e.g: one study has less levels than pkg_tocheck)
        idx<-which(y == pkg.in.study)
        idx<-idx[1]
        return (x$version[idx])
      })
    })
    
    df_vers<-data.frame(package = pkg_tocheck, vers)    
    df_verses<-rbind(df_verses,df_vers)
    
    #update already checked package
    pkg_checked<-c(pkg_checked, pkg_tocheck)
  }
  
  df_pkges<-subset(df_pkges,!is.na(df_pkges$package)) #take only valid packages (eg: <NA> is not package)
  df_verses<-subset(df_verses,!is.na(df_verses$package)) #take only valid packages (eg: <NA> is not package)
  
  pkg_status_list<-list(package_status = unique(df_pkges), version_status = unique(df_verses))
  return(pkg_status_list)
}
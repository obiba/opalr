#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the users
#' 
#' Get the users from the Opal internal users registry.
#' 
#' @family user functions
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.users(o)
#' opal.logout(o)
#' }
#' @export
oadmin.users <- function(opal, df=TRUE) {
  users <- opal.get(opal, "system", "subject-credentials")
  if (df) {
    n <- length(users)
    name <- replicate(n, NA)
    authenticationType <- replicate(n, NA)
    groups <- replicate(n, NA)
    enabled <- replicate(n, NA)
    if (n>0) {
      for (i in 1:n) {
        name[i] <- users[[i]]$name
        authenticationType[i] <- users[[i]]$authenticationType
        groups[i] <- paste0(users[[i]]$groups, collapse = ", ")
        enabled[i] <- users[[i]]$enabled
      }
    }
    data.frame(name = name, groups = groups, authenticationType = authenticationType, enabled = enabled, stringsAsFactors = FALSE)
  } else {
    users
  }
}

#' Get user profiles
#' 
#' When a user has logged in Opal, he/she has a profile representing its activity. The user
#' can be defined in the Opal internal user registry, or in an external realm.
#' 
#' @family user functions
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' oadmin.user_profiles(o)
#' opal.logout(o)
#' }
#' @export
oadmin.user_profiles <- function(opal, df = TRUE) {
  uprofiles <- opal.get(opal, "system", "subject-profiles")
  if (df) {
    n <- length(uprofiles)
    principal <- replicate(n, NA)
    realm <- replicate(n, NA)
    groups <- replicate(n, NA)
    created <- replicate(n, NA)
    lastUpdate <- replicate(n, NA)
    if (n>0) {
      for (i in 1:n) {
        principal[i] <- uprofiles[[i]]$principal
        realm[i] <- uprofiles[[i]]$realm
        groups[i] <- paste0(uprofiles[[i]]$groups, collapse = ", ")
        created[i] <- uprofiles[[i]]$created
        lastUpdate[i] <- uprofiles[[i]]$lastUpdate
      }
    }
    data.frame(principal, groups, realm, created, lastUpdate, stringsAsFactors = FALSE)
  } else {
    uprofiles
  }
}

#' Check user exists
#' 
#' Check whether a user exists, either in the internal user registry (see \link{oadmin.users}) 
#' or as an external user that already logged in (see \link{oadmin.user_profiles}).
#' 
#' @family user functions
#' @param opal Opal object.
#' @param name User name
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' if (!oadmin.user_exists(o, "foo"))
#'   oadmin.user_add(o, "foo", password = "bar123")
#' opal.logout(o)
#' }
#' @export
oadmin.user_exists <- function(opal, name) {
  name %in% oadmin.users(opal)$name ||
    name %in% oadmin.user_profiles(opal)$principal
}

#' Add a user
#' 
#' Add a user in Opal internal users registry.
#' 
#' @family user functions
#' @param opal Opal object.
#' @param name User name
#' @param groups User groups
#' @param password User password. If not provided, a password will be generated and returned.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' pwd <- oadmin.user_add(o, "foo", groups = c("datashield", "CNSIM"))
#' opal.logout(o)
#' }
#' @export
oadmin.user_add <- function(opal, name, groups = NULL, password = NULL) {
  if (.is.empty(name))
    stop("User name is required")
  if (oadmin.user_exists(opal, name))
    stop("A user already exists with same name")
  pwd <- password
  if (.is.empty(pwd))
    pwd <- .generatePwd()
  else if (nchar(pwd)<8)
    stop("User password is too short, minimum length is 8")
  gps <- unlist(groups)
  if (is.null(gps))
    gps <- NA
  user <- list(
    name = name,
    authenticationType = "PASSWORD",
    password = pwd,
    certificate = "",
    enabled = TRUE,
    groups = gps
  )
  ignore <- opal.post(opal, "system", "subject-credentials", body = jsonlite::toJSON(user, auto_unbox = TRUE), contentType = "application/json")
  if (.is.empty(password))
    pwd
}

#' Reset user password
#' 
#' Reset the password of a user from Opal internal users registry.
#' 
#' @family user functions
#' @param opal Opal object.
#' @param name User name
#' @param password User password. If not provided, a password will be generated and returned.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' pwd <- oadmin.user_add(o, "foo", groups = c("datashield", "CNSIM"))
#' oadmin.user_reset_password(o, "foo", password = "password1234")
#' oadmin.user_rm(o, "foo")
#' opal.logout(o)
#' }
#' @export
oadmin.user_reset_password <- function(opal, name, password = NULL) {
  if (.is.empty(name))
    stop("User name is required")
  pwd <- password
  if (.is.empty(pwd))
    pwd <- .generatePwd()
  else if (nchar(pwd)<8)
    stop("User password is too short, minimum length is 8")
  user <- opal.get(opal, "system", "subject-credential", name)
  user$password <- pwd
  ignore <- opal.put(opal, "system", "subject-credential", name, body = jsonlite::toJSON(user, auto_unbox = TRUE), contentType = "application/json")
  if (.is.empty(password))
    pwd
}

#' Enable a user
#' 
#' Enable or disable a user from Opal internal users registry.
#' 
#' @family user functions
#' @param opal Opal object.
#' @param name User name
#' @param enabled Logical to enable a user.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' pwd <- oadmin.user_add(o, "foo", groups = c("datashield", "CNSIM"))
#' oadmin.user_enable(o, "foo", enabled = FALSE)
#' opal.logout(o)
#' }
#' @export
oadmin.user_enable <- function(opal, name, enabled = TRUE) {
  if (.is.empty(name))
    stop("User name is required")
  user <- opal.get(opal, "system", "subject-credential", name)
  user$enabled <- enabled
  user$groups <- NULL
  user$password <- NULL
  ignore <- opal.put(opal, "system", "subject-credential", name, body = jsonlite::toJSON(user, auto_unbox = TRUE), contentType = "application/json")
}

#' Delete a user
#' 
#' Delete a user from Opal internal users registry. Fails silently if user does not exist.
#' 
#' @family user functions
#' @param opal Opal object.
#' @param name User name
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' pwd <- oadmin.user_add(o, "foo", groups = c("datashield", "CNSIM"))
#' oadmin.user_delete(o, "foo")
#' opal.logout(o)
#' }
#' @export
oadmin.user_delete <- function(opal, name) {
  if (.is.empty(name))
    stop("User name is required")
  ignore <- tryCatch(opal.delete(opal, "system", "subject-credential", name), error = function(e) {})
}

#' Delete a user profile
#' 
#' Delete a user profile without deleting user if this one is defined in the Opal internal users registry.
#' Fails silently if user profile does not exist.
#' A user profile is the footprint of a user, created at first login. It keeps track of its activity, the realm
#' from which he/she was authenticated, its groups at time of the last login and more.
#' 
#' @family user functions
#' @param opal Opal object.
#' @param name User name
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' pwd <- oadmin.user_add(o, "foo", groups = c("datashield", "CNSIM"))
#' oadmin.user_profile_delete(o, "foo")
#' opal.logout(o)
#' }
#' @export
oadmin.user_profile_delete <- function(opal, name) {
  if (.is.empty(name))
    stop("User name is required")
  ignore <- tryCatch(opal.delete(opal, "system", "subject-profile", name), error = function(e) {})
}

#' @keywords internal
.generatePwd <- function() {
  lower <- 'abcdefghijklmnopqrstuvwxyz'
  upper <- 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
  digit <- '1234567890'
  special <- '@#$%^&+=!'
  x <- paste0(lower, upper, digit, special)
  pwd <- ''
  valid <- FALSE
  while (!valid) {
    pwd <- ''
    for (i in sample(1:71, 32)) {
      pwd <- paste0(pwd, substr(x, i, i))
    }
    valid <- grepl('[[:digit:]]', pwd) && grepl('[[:lower:]]', pwd) && grepl('[[:upper:]]', pwd) && grepl('[@#$%^&+=!]', pwd)
  }
  pwd
}

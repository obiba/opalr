#-------------------------------------------------------------------------------
# Copyright (c) 2023 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------


#' Get the permissions of a subject
#' 
#' Get the permissions of a subject. If the subject is a user, the permissions of
#' the groups to which the user belongs are also added to the result.
#' 
#' @family table functions
#' @param opal Opal connection object.
#' @param subject A subject identifier: user or group name.
#' @param type The type of subject: user (default) or group.
#' 
#' @return A data.frame with columns: subject, type, target (path to the opal object to which 
#' the permission applies), target_type and perm (the permission name)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.table_perm_add(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user', 'view')
#' opal.perms(o, 'andrei')
#' opal.table_perm_delete(o, 'CNSIM', 'CNSIM1', c('andrei', 'valentina'), 'user')
#' opal.logout(o)
#' }
#' @export
#' @import tibble
opal.perms <- function(opal, subject, type = "user") {
  
  perm_infos <- list(
    'PROJECT_ADD' = c('system', 'project_add'),
    'SYSTEM_ALL' = c('system', 'administrate'),
    'DATASHIELD_PROFILE_USE' = c('datashield profile', 'use'),
    'R_USE' = c('r service', 'use'),
    'DATASHIELD_USE' = c('datashield service', 'use'), 
    'DATASHIELD_ALL' = c('datashield service', 'administrate'),
    'PROJECT_ALL' = c('project', 'administrate'),
    'DATASOURCE_VIEW' = c('tables', 'view-values'),
    'TABLE_ADD' = c('tables', 'add'),
    'DATASOURCE_ALL' = c('tables', 'administrate'),
    'TABLE_READ' = c('table', 'view'),
    'TABLE_VALUES' = c('table', 'view-values'),
    'TABLE_EDIT' = c('table', 'edit'),
    'TABLE_VALUES_EDIT' = c('table', 'edit-values'),
    'TABLE_ALL' = c('table', 'administrate'),
    'RESOURCE_VIEW' = c('resource', 'view'),
    'RESOURCE_ALL' = c('resource', 'administrate'),
    'RESOURCES_VIEW' = c('resources', 'view'),
    'RESOURCES_ALL' = c('resource', 'administrate'))
  
  .action_to_perm <- function(action) {
    perm_infos[[action]]
  }
  
  .append_acl <- function(perms, acl) {
    perm_info <- .action_to_perm(acl$actions[[1]])
    if (is.null(perms)) {
      tibble(subject = acl$subject$principal, type = acl$subject$type, target = acl$resource, target_type = perm_info[1], perm = perm_info[2])
    } else {
      perms %>% add_row(subject = acl$subject$principal, type = acl$subject$type, target = acl$resource, target_type = perm_info[1], perm = perm_info[2])
    }
  }
  
  .append_acls <- function(perms, acls, principal = NULL) {
    rval <- perms
    if (!is.null(acls) && length(acls)>0) {
      for (i in 1:length(acls)) {
        acl <- acls[[i]]
        if (is.null(principal) || acl$subject$principal == principal) {
          rval <- .append_acl(rval, acl)  
        }
      }
    }
    rval
  }
  
  # permissions directly set
  all_perms <- NULL
  for (project in opal.projects(opal)$name) {
    acls <- opal.get(opal, "project", project, "permissions", "subject", subject, query = list(type = toupper(type)))
    all_perms <- .append_acls(all_perms, acls)
  }
  # datashield permissions
  all_perms <- .append_acls(all_perms, opal.get(opal, "system", "permissions", "datashield", query = list(type = toupper(type))), principal = subject)
  # datashield profile permissions
  dsProfiles <- dsadmin.profiles(opal)
  if (length(dsProfiles)>0) {
    for (i in 1:length(dsProfiles)) {
      profile <- as.list(dsProfiles[i,])
      if (isTRUE(profile$restrictedAccess)) {
        acls <- opal.get(opal, "datashield", "profile", profile$name, "permissions")
        all_perms <- .append_acls(all_perms, acls, principal = subject)
      }
    }
  }
  # R permissions
  all_perms <- .append_acls(all_perms, opal.get(opal, "system", "permissions", "r", query = list(type = toupper(type))), principal = subject)
  # system permissions
  all_perms <- .append_acls(all_perms, opal.get(opal, "system", "permissions", "administration", query = list(type = toupper(type))), principal = subject)
  
  if (toupper(type) == "USER") {
    # append inferred permissions
    userProfile <- opal.get(opal, "system", "subject-profile", subject)
    if (length(userProfile$groups)>0) {
      for (group in userProfile$groups) {
        perms <- opal.perms(opal, group, type = "group")
        if (is.null(all_perms)) {
          all_perms <- perms
        } else {
          all_perms <- all_perms %>% add_row(perms)  
        }
      }
    }
  }
  
  all_perms
}
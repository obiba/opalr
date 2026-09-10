#-------------------------------------------------------------------------------
# Copyright (c) 2026 OBiBa. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the DataSHIELD usage quotas
#'
#' Get the R usage quotas that apply to the DataSHIELD execution context. A quota is an allowance
#' of R usage, given to a subject (the system default, a group or a user) for a usage metric.
#' Having no quota means unlimited usage, which is not the same as a quota with a limit of zero
#' (that one forbids DataSHIELD altogether for the subject).
#'
#' Requires DataSHIELD or system administration permissions.
#'
#' @family DataSHIELD quotas
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @return The DataSHIELD quotas as a data.frame (columns are: id, context, type, subject, metric,
#' period, limit, enabled, where limit is expressed in minutes) or a list.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.quotas(o)
#' opal.logout(o)
#' }
#' @export
dsadmin.quotas <- function(opal, df = TRUE) {
  .quota.version_check(opal)
  dtos <- opal.get(opal, "service", "r", "quotas", query = list(context = .DATASHIELD_CONTEXT))
  if (df) {
    .quotasToDataFrame(dtos)
  } else {
    dtos
  }
}

#' Get a DataSHIELD usage quota
#'
#' Get the DataSHIELD usage quota that was defined for a subject and a usage metric. Note that this
#' is the quota as it was declared and not necessarily the one that applies to a user: see
#' \link{dsadmin.quota_usage} for the resolved one.
#'
#' Requires DataSHIELD or system administration permissions.
#'
#' @family DataSHIELD quotas
#' @param opal Opal object.
#' @param metric The usage metric: 'EXECUTION_TIME' (default) is the R server's cpu time consumed,
#' 'SESSION_TIME' is the time R sessions are held, whether they compute or not.
#' @param type The type of subject: system (default), user or group.
#' @param subject The user name or the group name, not applicable when the type is system.
#' @return The DataSHIELD quota as a list, or NULL when there is no such quota.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.quota(o, metric = 'EXECUTION_TIME', type = 'user', subject = 'andrei')
#' opal.logout(o)
#' }
#' @export
dsadmin.quota <- function(opal, metric = "EXECUTION_TIME", type = "system", subject = NULL) {
  .quota.version_check(opal)
  .quota.find(opal, metric = metric, type = type, subject = subject)
}

#' Check a DataSHIELD usage quota exists
#'
#' Check whether a DataSHIELD usage quota was defined for a subject and a usage metric.
#'
#' Requires DataSHIELD or system administration permissions.
#'
#' @family DataSHIELD quotas
#' @param opal Opal object.
#' @param metric The usage metric: 'EXECUTION_TIME' (default) or 'SESSION_TIME'.
#' @param type The type of subject: system (default), user or group.
#' @param subject The user name or the group name, not applicable when the type is system.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' if (!dsadmin.quota_exists(o, type = 'group', subject = 'datashield'))
#'   dsadmin.quota_save(o, limit = 60, type = 'group', subject = 'datashield')
#' opal.logout(o)
#' }
#' @export
dsadmin.quota_exists <- function(opal, metric = "EXECUTION_TIME", type = "system", subject = NULL) {
  !is.null(dsadmin.quota(opal, metric = metric, type = type, subject = subject))
}

#' Save a DataSHIELD usage quota
#'
#' Create or update the DataSHIELD usage quota of a subject: the system default, a group or a user.
#' A subject can have one quota per usage metric, therefore saving replaces the quota that the
#' subject already had for this metric, if any.
#'
#' When a user runs DataSHIELD, the quota that applies to them is their own if they have one, else
#' the most permissive of the ones given to the groups they belong to, else the system default. A
#' disabled quota is ignored by this resolution.
#'
#' Requires DataSHIELD or system administration permissions.
#'
#' @family DataSHIELD quotas
#' @param opal Opal object.
#' @param limit The allowance, in minutes of whatever the metric names. Zero is a meaningful value:
#' it forbids DataSHIELD altogether for the subject.
#' @param period The rolling window the usage is summed over: 'WEEKLY' (default) or 'DAILY'. There
#' is no reset instant, capacity returns as old activity ages out of the window.
#' @param metric The usage metric: 'EXECUTION_TIME' (default) is the R server's cpu time consumed
#' (an idle session costs nothing), 'SESSION_TIME' is the time R sessions are held, whether they
#' compute or not.
#' @param type The type of subject: system (default), user or group.
#' @param subject The user name or the group name, not applicable when the type is system.
#' @param enabled Whether the quota applies (default is TRUE).
#' @return The saved DataSHIELD quota, invisibly.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' # 10 hours of R execution time per week for everyone
#' dsadmin.quota_save(o, limit = 600)
#' # 2 hours of R execution time per day for a user
#' dsadmin.quota_save(o, limit = 120, period = 'DAILY', type = 'user', subject = 'andrei')
#' # 8 hours of R session time per week for a group
#' dsadmin.quota_save(o, limit = 480, metric = 'SESSION_TIME', type = 'group', subject = 'datashield')
#' dsadmin.quotas(o)
#' opal.logout(o)
#' }
#' @export
dsadmin.quota_save <- function(opal, limit, period = "WEEKLY", metric = "EXECUTION_TIME", type = "system", subject = NULL, enabled = TRUE) {
  .quota.version_check(opal)
  quota <- .quota.body(limit = limit, period = period, metric = metric, type = type, subject = subject, enabled = enabled)
  res <- opal.post(opal, "service", "r", "quotas", contentType = "application/json",
                   body = jsonlite::toJSON(quota, auto_unbox = TRUE))
  invisible(res)
}

#' Enable or disable a DataSHIELD usage quota
#'
#' A disabled quota is kept but does not apply: the subject falls back on the quota of its groups
#' or on the system default, and is unlimited when there is none.
#'
#' Requires DataSHIELD or system administration permissions.
#'
#' @family DataSHIELD quotas
#' @param opal Opal object.
#' @param enabled Default value is TRUE.
#' @param metric The usage metric: 'EXECUTION_TIME' (default) or 'SESSION_TIME'.
#' @param type The type of subject: system (default), user or group.
#' @param subject The user name or the group name, not applicable when the type is system.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.quota_enable(o, enabled = FALSE, type = 'user', subject = 'andrei')
#' opal.logout(o)
#' }
#' @export
dsadmin.quota_enable <- function(opal, enabled = TRUE, metric = "EXECUTION_TIME", type = "system", subject = NULL) {
  .quota.version_check(opal)
  quota <- .quota.find(opal, metric = metric, type = type, subject = subject)
  if (is.null(quota)) {
    stop("No such DataSHIELD quota", call. = FALSE)
  }
  quota$enabled <- enabled
  ignore <- opal.put(opal, "service", "r", "quota", as.character(quota$id), contentType = "application/json",
                     body = jsonlite::toJSON(quota, auto_unbox = TRUE))
}

#' Delete a DataSHIELD usage quota
#'
#' Delete the DataSHIELD usage quota of a subject for a usage metric. Silently returns when there
#' is no such quota. Deleting the quota that applied makes the subject unlimited again, unless a
#' broader quota still matches.
#'
#' Requires DataSHIELD or system administration permissions.
#'
#' @family DataSHIELD quotas
#' @param opal Opal object.
#' @param metric The usage metric: 'EXECUTION_TIME' (default) or 'SESSION_TIME'.
#' @param type The type of subject: system (default), user or group.
#' @param subject The user name or the group name, not applicable when the type is system.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.quota_delete(o, type = 'user', subject = 'andrei')
#' opal.logout(o)
#' }
#' @export
dsadmin.quota_delete <- function(opal, metric = "EXECUTION_TIME", type = "system", subject = NULL) {
  .quota.version_check(opal)
  quota <- .quota.find(opal, metric = metric, type = type, subject = subject)
  if (!is.null(quota)) {
    ignore <- opal.delete(opal, "service", "r", "quota", as.character(quota$id))
  }
}

#' Get the DataSHIELD usage quota of a user
#'
#' Get, for each usage metric, the DataSHIELD quota that applies to a user and what they have
#' consumed against it. Every metric is reported, whether or not a quota applies to it: an entry
#' with no limit is the unlimited case, and nothing is measured then.
#'
#' Requires DataSHIELD or system administration permissions. See \link{opal.quotas} for the
#' quotas of the current user, which requires no permission.
#'
#' @family DataSHIELD quotas
#' @param opal Opal object.
#' @param user The user name.
#' @param df Return a data.frame (default is TRUE)
#' @return The DataSHIELD quota usages as a data.frame (columns are: context, user, metric, period,
#' limit, used, exceeded, windowStartDate, nextCreditDate, openSessionsCount) or a list. Limits and
#' usages are expressed in minutes, and are NA when no quota applies to the metric.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' dsadmin.quota_usage(o, 'andrei')
#' opal.logout(o)
#' }
#' @export
dsadmin.quota_usage <- function(opal, user, df = TRUE) {
  .quota.version_check(opal)
  if (.is.empty(user)) {
    stop("User name is required", call. = FALSE)
  }
  dtos <- opal.get(opal, "service", "r", "quotas", "_usage", query = list(context = .DATASHIELD_CONTEXT, user = user))
  if (df) {
    .quotaUsagesToDataFrame(dtos)
  } else {
    dtos
  }
}

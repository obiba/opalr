#-------------------------------------------------------------------------------
# Copyright (c) 2026 OBiBa. All rights reserved.
#
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get own R usage quotas
#'
#' Get, for each usage metric, the R usage quota that applies to the current user in an execution
#' context and what they have consumed against it. Every metric is reported, whether or not a quota
#' applies to it: an entry with no limit is the unlimited case, and nothing is measured then.
#'
#' Reporting on oneself requires no special permission. See \link{dsadmin.quota_usage} for the
#' quotas of another user in the DataSHIELD context.
#'
#' @family DataSHIELD quotas
#' @param opal Opal object.
#' @param context The R execution context: 'DataSHIELD' (default), 'R', 'Import', 'Export', 'SQL'
#' or 'Analyse'.
#' @param df Return a data.frame (default is TRUE)
#' @return The R quota usages as a data.frame (columns are: context, user, metric, period, limit,
#' used, exceeded, windowStartDate, nextCreditDate, openSessionsCount) or a list. Limits and usages
#' are expressed in minutes, and are NA when no quota applies to the metric.
#' @examples
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.quotas(o)
#' opal.quotas(o, context = 'R')
#' opal.logout(o)
#' }
#' @export
opal.quotas <- function(opal, context = "DataSHIELD", df = TRUE) {
  .quota.version_check(opal)
  if (.is.empty(context)) {
    stop("R execution context is required", call. = FALSE)
  }
  dtos <- opal.get(opal, "service", "r", "quotas", "_current", query = list(context = context))
  if (df) {
    .quotaUsagesToDataFrame(dtos)
  } else {
    dtos
  }
}

# The R execution context of DataSHIELD
#' @keywords internal
.DATASHIELD_CONTEXT <- "DataSHIELD"

# The number of milliseconds in a minute: Opal exchanges R quota limits and usages in
# milliseconds, whereas they are expressed in minutes here.
#' @keywords internal
.QUOTA_MILLIS_PER_MINUTE <- 60000

# R usage quotas were introduced in Opal 6.0
#' @keywords internal
.quota.version_check <- function(opal) {
  if (opal.version_compare(opal, "6.0") < 0) {
    stop("R usage quotas require Opal 6.0 or higher.", call. = FALSE)
  }
}

# Normalize and verify a R quota usage metric
#' @keywords internal
.quota.metric <- function(metric) {
  value <- toupper(as.character(metric))
  if (length(value) != 1 || !(value %in% c("EXECUTION_TIME", "SESSION_TIME"))) {
    stop("Not a valid R quota metric: ", metric, " (expecting 'EXECUTION_TIME' or 'SESSION_TIME')", call. = FALSE)
  }
  value
}

# Normalize and verify a R quota period
#' @keywords internal
.quota.period <- function(period) {
  value <- toupper(as.character(period))
  if (length(value) != 1 || !(value %in% c("DAILY", "WEEKLY"))) {
    stop("Not a valid R quota period: ", period, " (expecting 'DAILY' or 'WEEKLY')", call. = FALSE)
  }
  value
}

# Normalize and verify a R quota subject type
#' @keywords internal
.quota.subject_type <- function(type) {
  value <- toupper(as.character(type))
  if (length(value) != 1 || !(value %in% c("SYSTEM", "GROUP", "USER"))) {
    stop("Not a valid R quota subject type: ", type, " (expecting 'system', 'group' or 'user')", call. = FALSE)
  }
  value
}

# Make the R quota body to be sent to Opal, in which the limit is expressed in milliseconds. The
# system default has no subject, which is the empty string and not NULL: Opal uses it in the
# quota's unique key.
#' @keywords internal
.quota.body <- function(limit, period, metric, type, subject, enabled) {
  subjectType <- .quota.subject_type(type)
  principal <- ""
  if (subjectType != "SYSTEM") {
    if (.is.empty(subject)) {
      stop("R quota subject name is required when the subject type is ", tolower(subjectType), call. = FALSE)
    }
    principal <- as.character(subject)
  }
  if (.is.empty(limit) || !is.numeric(limit) || limit < 0) {
    stop("R quota limit must be a positive number of minutes", call. = FALSE)
  }
  list(
    context = .DATASHIELD_CONTEXT,
    subjectType = subjectType,
    principal = principal,
    metric = .quota.metric(metric),
    period = .quota.period(period),
    limitMillis = round(limit) * .QUOTA_MILLIS_PER_MINUTE,
    enabled = enabled
  )
}

# Find the DataSHIELD quota of a subject for a usage metric, NULL when there is none. The quota is
# addressed by its identifier in Opal, whereas the subject and the metric are what identifies it
# here, so it has to be looked up.
#' @keywords internal
.quota.find <- function(opal, metric, type, subject) {
  subjectType <- .quota.subject_type(type)
  principal <- ""
  if (subjectType != "SYSTEM") {
    if (.is.empty(subject)) {
      stop("R quota subject name is required when the subject type is ", tolower(subjectType), call. = FALSE)
    }
    principal <- as.character(subject)
  }
  metricValue <- .quota.metric(metric)
  dtos <- opal.get(opal, "service", "r", "quotas", query = list(context = .DATASHIELD_CONTEXT))
  found <- Filter(function(dto) {
    dto$subjectType == subjectType && dto$principal == principal && dto$metric == metricValue
  }, dtos)
  if (length(found) == 0) NULL else found[[1]]
}

# Turn R quotas into a data.frame, limits being reported in minutes
#' @keywords internal
.quotasToDataFrame <- function(dtos) {
  n <- length(dtos)
  id <- rep(NA, n)
  context <- rep(NA, n)
  type <- rep(NA, n)
  subject <- rep(NA, n)
  metric <- rep(NA, n)
  period <- rep(NA, n)
  limit <- rep(NA, n)
  enabled <- rep(NA, n)
  if (n > 0) {
    for (i in 1:n) {
      id[i] <- dtos[[i]]$id
      context[i] <- dtos[[i]]$context
      type[i] <- tolower(dtos[[i]]$subjectType)
      subject[i] <- dtos[[i]]$principal
      metric[i] <- dtos[[i]]$metric
      period[i] <- dtos[[i]]$period
      limit[i] <- dtos[[i]]$limitMillis / .QUOTA_MILLIS_PER_MINUTE
      enabled[i] <- dtos[[i]]$enabled
    }
  }
  data.frame(id = id, context = context, type = type, subject = subject, metric = metric,
             period = period, limit = limit, enabled = enabled, stringsAsFactors = FALSE)
}

# Turn R quota usages into a data.frame, limits and usages being reported in minutes. An entry
# with no quota is the unlimited case: there is no limit, no period and no window to report, and
# nothing is measured against nothing.
#' @keywords internal
.quotaUsagesToDataFrame <- function(dtos) {
  n <- length(dtos)
  context <- rep(NA, n)
  user <- rep(NA, n)
  metric <- rep(NA, n)
  period <- rep(NA, n)
  limit <- rep(NA, n)
  used <- rep(NA, n)
  exceeded <- rep(NA, n)
  windowStartDate <- rep(NA, n)
  nextCreditDate <- rep(NA, n)
  openSessionsCount <- rep(NA, n)
  if (n > 0) {
    for (i in 1:n) {
      dto <- dtos[[i]]
      context[i] <- dto$context
      user[i] <- dto$user
      metric[i] <- dto$metric
      # nothing is measured when no quota applies, so what Opal reports then is not a usage of zero
      if (!is.null(dto$quota)) {
        period[i] <- dto$quota$period
        limit[i] <- dto$quota$limitMillis / .QUOTA_MILLIS_PER_MINUTE
        used[i] <- dto$usedMillis / .QUOTA_MILLIS_PER_MINUTE
        openSessionsCount[i] <- .nullToNA(dto$openSessionsCount)
      }
      exceeded[i] <- dto$exceeded
      windowStartDate[i] <- .nullToNA(dto$windowStartDate)
      nextCreditDate[i] <- .nullToNA(dto$nextCreditDate)
    }
  }
  data.frame(context = context, user = user, metric = metric, period = period, limit = limit,
             used = used, exceeded = exceeded, windowStartDate = windowStartDate,
             nextCreditDate = nextCreditDate, openSessionsCount = openSessionsCount,
             stringsAsFactors = FALSE)
}

#-------------------------------------------------------------------------------
# Copyright (c) 2018 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the tasks from a opal.
#' 
#' @param opal Opal object.
#' @export
opal.tasks=function(opal) {
  tasks <- .extractJsonField(.get(opal, "shell", "commands"))
  if (length(tasks)) {
    id <- c()
    type <- c()
    user <- c()
    project <- c()
    startDate <- c()
    endDate <- c()
    status <- c()
    for (i in 1:length(tasks)) {
      task <- tasks[i]
      id <- c(id, task[[1]]$id)
      type <- c(type, task[[1]]$name)
      user <- c(user, task[[1]]$owner)
      project <- c(project, task[[1]]$project)
      startDate <- c(startDate, task[[1]]$startTime)
      et <- ''
      if (!is.null(task[[1]]$endTime)) {
        et <- task[[1]]$endTime
      }
      endDate <- c(endDate, et)
      status <- c(status, task[[1]]$status)
    }
    data.frame(id=id, type=type, user=user, project=project, startDate=startDate, endDate=endDate, status=status)
  }
}

#' Get a task from a opal.
#' 
#' @param opal Opal object.
#' @param id Task identifier.
#' @export
opal.task=function(opal, id) {
  .get(opal, "shell", "command", id)
}

#' Tries to cancel a task from a opal.
#' 
#' @param opal Opal object.
#' @param id Task identifier.
#' @export
opal.task_cancel=function(opal, id) {
  ignore <- try(.put(opal, "shell", "command", id, "status", body='CANCELED', contentType='application/json'), silent=TRUE)
}

#' Wait for a task from a opal to complete.
#' 
#' @param opal Opal object.
#' @param id Task identifier.
#' @param max Maximum time (in seconds) to wait for the task completion. Default is NULL (no maximum).
#' @export
opal.task_wait=function(opal, id, max=NULL) {
  status <- 'NA'
  waited <- 0
  while(!is.element(status, c('SUCCEEDED','FAILED','CANCELED')) && (is.null(max) || waited<=max)) {
    # delay is proportional to the time waited, but no more than 10s
    delay <- min(10, max(1, round(waited/10)))
    Sys.sleep(delay)
    waited <- waited + delay
    task <- .get(opal, "shell", "command", id)
    status <- task$status
  }
  if (is.element(status, c('FAILED','CANCELED'))) {
    stop(paste0('Task "', id, '" ended with status: ', status), call.=FALSE)
  }
}
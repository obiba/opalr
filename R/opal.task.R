#-------------------------------------------------------------------------------
# Copyright (c) 2021 OBiBa. All rights reserved.
#  
# This program and the accompanying materials
# are made available under the terms of the GNU Public License v3.0.
#  
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------

#' Get the tasks
#' 
#' Get all the tasks with their status at the time of the request.
#' 
#' @family task functions
#' @param opal Opal object.
#' @param df Return a data.frame (default is TRUE)
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.tasks(o)
#' opal.logout(o)
#' }
#' @export
opal.tasks <- function(opal, df=TRUE) {
  tasks <- opal.get(opal, "shell", "commands")
  if (!df) {
    return(tasks)
  }
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

#' Get a task
#' 
#' Get the details of a specific task.
#' 
#' @family task functions
#' @param opal Opal object.
#' @param id Task identifier.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.task(o, '1')
#' opal.logout(o)
#' }
#' @export
opal.task=function(opal, id) {
  opal.get(opal, "shell", "command", id)
}

#' Cancel a task
#' 
#' Tries to cancel a task.
#' 
#' @family task functions
#' @param opal Opal object.
#' @param id Task identifier.
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.task_cancel(o, '1')
#' opal.logout(o)
#' }
#' @export
opal.task_cancel=function(opal, id) {
  ignore <- try(opal.put(opal, "shell", "command", id, "status", body='CANCELED', contentType='application/json'), silent=TRUE)
}

#' Wait for a task to complete.
#' 
#' The task completion is defined by its status: *SUCCEEDED*, *FAILED* or *CANCELED*.
#' 
#' @family task functions
#' @param opal Opal object.
#' @param id Task identifier.
#' @param max Maximum time (in seconds) to wait for the task completion. Default is NULL (no maximum).
#' @examples 
#' \dontrun{
#' o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')
#' opal.task_wait(o, '1')
#' opal.logout(o)
#' }
#' @export
opal.task_wait=function(opal, id, max=NULL) {
  status <- 'NA'
  waited <- 0
  while(!is.element(status, c('SUCCEEDED','FAILED','CANCELED')) && (is.null(max) || waited<=max)) {
    # delay is proportional to the time waited, but no more than 10s
    delay <- min(10, max(1, round(waited/10)))
    Sys.sleep(delay)
    waited <- waited + delay
    task <- opal.get(opal, "shell", "command", id)
    status <- task$status
  }
  if (is.element(status, c('FAILED','CANCELED'))) {
    stop(paste0('Task "', id, '" ended with status: ', status), call.=FALSE)
  }
}
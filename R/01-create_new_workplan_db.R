#' create a list of employees that are to be assigned to projects
#'
#' @param staff Names of staff members
#' @param projects Names of projects
#' @param calendar Dates of work plan
#' @param project_phases List of phases in any project in order of execution
#' @param out_of_office Names of staff that are going to be out of the office
#' @param public_holidays A data frame of dates of public holidays
#' @param time_estimates Time estimates of how long each phase will take in relation 
#' @param project_assignmentsAmount of time each staff is expected to dedicate to each [project, phase] 
#' @return TRUE if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
create_new_workplan_db = function(staff,
                               projects,
                               calendar,
                               project_phases,
                               out_of_office,
                               public_holidays,
                               time_estimates,
                               project_assignments,
                               db_name = "my_workplan.sqlite"){
  init_db(db_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  RSQLite::dbWriteTable(con, "staff", staff, append = T)
  RSQLite::dbWriteTable(con, "projects", projects, append = T)
  RSQLite::dbWriteTable(con, "project_phases", project_phases, append = T)
  RSQLite::dbWriteTable(con, "out_of_office", out_of_office, append = T)
  RSQLite::dbWriteTable(con, "public_holidays", public_holidays, append = T)
  RSQLite::dbWriteTable(con, "calendar", calendar, append = T)
  RSQLite::dbWriteTable(con, "time_estimates", time_estimates, append = T)
  RSQLite::dbWriteTable(con, "project_assignments", project_assignments, append = T)
  RSQLite::dbDisconnect(con)
  return(TRUE)
}



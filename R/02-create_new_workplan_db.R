#' Populate workplanr database with data
#'
#' @param staff Names of staff members
#' @param projects Names of projects
#' @param project_phases List of phases in any project in order of execution
#' @param project_roles List of project roles 
#' @param out_of_office Names of staff that are going to be out of the office
#' @param public_holidays A data frame of dates of public holidays
#' @param roles_responsibilities Data frame for responsibilites of roles across project phases
#' @param time_estimates Time estimates of how long each phase will take in relation 
#' @param staff_name_for_unassigned_work Dummy staff member to assign work to
#' @param db_name name of database to create
#' @return TRUE if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
create_new_workplan_db = function(staff,
                               projects,
                               project_phases,
                               project_roles,
                               out_of_office,
                               public_holidays,
                               time_estimates,
                               roles_responsibilities,
                               staff_name_for_unassigned_work = "unassigned",
                               db_name = "my_workplan.sqlite"){
  tmp <- normalise_inputs(staff, projects, calendar, project_phases, project_roles,
                   out_of_office, public_holidays, time_estimates, roles_responsibilities)
  init_db(db_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  RSQLite::dbWriteTable(con, "staff", tmp$staff, append = T)
  RSQLite::dbWriteTable(con, "projects", tmp$projects, append = T)
  RSQLite::dbWriteTable(con, "project_phases", tmp$project_phases, append = T)
  RSQLite::dbWriteTable(con, "project_roles", tmp$project_roles, append = T)
  RSQLite::dbWriteTable(con, "out_of_office", tmp$out_of_office, append = T)
  RSQLite::dbWriteTable(con, "public_holidays", tmp$public_holidays, append = T)
  buffer <- 180 #buffer of days either start of plan
  cals <- bizdays::create.calendar('normal', 
                                   weekdays = c('saturday', 'sunday'), 
                                   start.date = min(as.Date(tmp$projects$project_start))- buffer, 
                                   end.date = max(as.Date(tmp$projects$project_start)) + buffer)
  calendar <- data.frame(date = as.character(bizdays::bizseq(from = min(as.Date(tmp$projects$project_start))-180, 
                                                             to = max(as.Date(tmp$projects$project_start))+180, 'normal')))
  bizdays::remove.calendars('normal')
  RSQLite::dbWriteTable(con, "calendar", calendar, append = T)
  #TODO: just standardise these initially and then assign and remove
  RSQLite::dbWriteTable(con, "time_estimates", tmp$time_estimates, append = T) 
  RSQLite::dbWriteTable(con, "responsibilies", tmp$roles_responsibilities, append = T)
  project_assignments <- expand.grid(id_staff = 1:nrow(tmp$staff), 
                                     id_project_role = 1:nrow(project_roles),
                                     id_project_phase = 1:nrow(project_phases), 
                                     id_project = 1:nrow(projects),
                                     KEEP.OUT.ATTRS = FALSE)
  project_assignments$staff_contribution <- 0
  project_assignments <- dplyr::left_join(project_assignments, tmp$time_estimates) %>%
    dplyr::filter(abs(time_estimate) > 0) %>% dplyr::select(-time_estimate)
  project_assignments <- dplyr::left_join(project_assignments, tmp$roles_responsibilities) %>%
    dplyr::filter(responsibility_span == 1) %>% dplyr::select(-responsibility_span)
  RSQLite::dbWriteTable(con, "project_assignments", project_assignments, append = T)
  #assign all work to unassigned
  project_assignments$staff_contribution <- 100
  project_assignments$staff_name <- staff_name_for_unassigned_work
  project_assignments$staff_capacity <- 0
  project_assignments <- project_assignments %>% 
    dplyr::select(-id_staff) %>%
    dplyr::distinct()
  RSQLite::dbWriteTable(con, "project_unassignments", project_assignments, append = T)
  RSQLite::dbDisconnect(con)
  return(TRUE)
}



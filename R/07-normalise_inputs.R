#' Populate workplanr database with data
#'
#' @param staff Names of staff members
#' @param projects Names of projects
#' @param project_phases List of phases in any project in order of execution
#' @param out_of_office Names of staff that are going to be out of the office
#' @param public_holidays A data frame of dates of public holidays
#' @param roles_responsibilities Data frame for responsibilites of roles across project phases
#' @param time_estimates Time estimates of how long each phase will take in relation 
#' @keywords internal
normalise_inputs = function(staff, projects, calendar, project_phases, project_roles,
                            out_of_office, public_holidays, time_estimates, roles_responsibilities){
  tmp <- list()
  tmp$staff <- staff
  tmp$staff$id_staff <- 1:nrow(staff)
  tmp$projects <- projects
  tmp$projects$id_project <- 1:nrow(projects)
  tmp$project_phases <- project_phases
  tmp$project_phases$id_project_phase <- 1:nrow(project_phases)
  tmp$project_roles <- project_roles
  tmp$project_roles$id_project_role <- 1:nrow(project_roles)
  tmp$projects$project_start = as.character(tmp$projects$project_start)
  tmp$projects$project_end = as.character(tmp$projects$project_end)
  tmp$out_of_office <- out_of_office
  tmp$out_of_office$id_out_of_office <- 1:nrow(out_of_office)
  tmp$out_of_office <- tmp$out_of_office %>%
    dplyr::left_join(tmp$staff) %>%
    dplyr::select(-c(staff_name, staff_capacity))
  tmp$out_of_office$out_of_office_start = as.character(tmp$out_of_office$out_of_office_start)
  tmp$out_of_office$out_of_office_end = as.character(tmp$out_of_office$out_of_office_end)
  tmp$public_holidays <- public_holidays
  tmp$public_holidays$id_public_holidays <- 1:nrow(public_holidays)
  tmp$public_holidays$date = as.character(tmp$public_holidays$date)
  tmp$time_estimates <- time_estimates
  tmp$time_estimates <- tmp$time_estimates %>%
    dplyr::left_join(tmp$projects) %>% 
    dplyr::left_join(tmp$project_phases) %>%
    dplyr::select(id_project, id_project_phase, time_estimate)
  tmp$roles_responsibilities <- roles_responsibilities
  tmp$roles_responsibilities <- tmp$roles_responsibilities %>%
    dplyr::left_join(tmp$project_roles) %>% 
    dplyr::left_join(tmp$project_phases) %>%
    dplyr::select(id_project_role, id_project_phase, responsibility_span)
  return(tmp)
}
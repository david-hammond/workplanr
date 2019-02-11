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
#' @export
create_new_workplan = function(staff, projects, project_phases, project_roles,
                            out_of_office, public_holidays, time_estimates, roles_responsibilities,
                            staff_name_for_unassigned_work = "unassigned"){
  workplan <- new("workplan")
  workplan@staff <- workplanr_staff(staff_name = as.character(staff$staff_name), 
                               staff_capacity = as.numeric(staff$staff_capacity))
  projects <- projects %>%
    dplyr::arrange(projects$project_end)
  workplan@projects <- workplanr_projects(project_name = factor(unique(as.character(projects$project_name)),
                                                           levels = unique(as.character(projects$project_name)),
                                                           ordered = TRUE),
                                     project_confirmed = as.logical(projects$project_confirmed),
                                     project_start = as.Date(projects$project_start),
                                     project_end = as.Date(projects$project_end))
                                     
  workplan@project_phases <- workplanr_project_phases(project_phase_name = factor(unique(as.character(project_phases$project_phase_name)),
                                                                               levels = unique(as.character(project_phases$project_phase_name)),
                                                                               ordered = TRUE))
  workplan@project_roles <- workplanr_project_roles(project_role_name = factor(unique(as.character(project_roles$project_role_name)),
                                                                          levels = unique(as.character(project_roles$project_role_name)),
                                                                          ordered = TRUE))
  staff_name_check(out_of_office$staff_name, workplan)
  workplan@out_of_office <- workplanr_out_of_office(id_out_of_office = 1:nrow(out_of_office),
                                               staff_name = as.character(out_of_office$staff_name),
                                               out_of_office_start = as.Date(out_of_office$out_of_office_start),
                                               out_of_office_end = as.Date(out_of_office$out_of_office_end),
                                               work_related = as.logical(out_of_office$work_related))
  workplan@public_holidays <- workplanr_holidays(date = as.Date(public_holidays$date),
                                            holiday_name = as.character(public_holidays$holiday_name))
  project_role_name_check(roles_responsibilities$project_role_name, workplan)
  project_phase_name_check(roles_responsibilities$project_phase_name, workplan)
  workplan@roles_responsibilities <- workplanr_roles_responsibilities(project_role_name = factor(as.character(roles_responsibilities$project_role_name),
                                                                                           levels = levels(workplan@project_roles@project_role_name),
                                                                                           ordered = TRUE),
                                                                project_phase_name = factor(as.character(roles_responsibilities$project_phase_name),
                                                                                            levels = levels(workplan@project_phases@project_phase_name),
                                                                                            ordered = TRUE),
                                                                responsibility_span = as.logical(roles_responsibilities$responsibility_span))
  project_name_check(time_estimates$project_name, workplan)
  project_phase_name_check(time_estimates$project_phase_name, workplan)
  workplan@time_estimates <- workplanr_time_estimates(project_name = factor(as.character(time_estimates$project_name),
                                                                       levels = levels(workplan@projects@project_name),
                                                                       ordered = TRUE),
                                                 project_phase_name = factor(as.character(time_estimates$project_phase_name),
                                                                             levels = levels(workplan@project_phases@project_phase_name),
                                                                             ordered = TRUE),
                                                 time_estimate = as.numeric(time_estimates$time_estimate))
  
  project_assignments <- expand.grid(staff_name = workplan@staff@staff_name, 
                                     project_role_name = workplan@project_roles@project_role_name,
                                     project_phase_name = workplan@project_phases@project_phase_name, 
                                     project_name = workplan@projects@project_name,
                                     KEEP.OUT.ATTRS = FALSE)
  
  project_assignments$staff_contribution <- 0
  project_assignments <- project_assignments %>% 
    dplyr::left_join(as.data.frame(workplan@time_estimates)) %>%
    dplyr::filter(abs(time_estimate) > 0) %>% 
    dplyr::select(-time_estimate)
  
  project_assignments <- project_assignments %>%
    dplyr::left_join(as.data.frame(workplan@roles_responsibilities)) %>%
    dplyr::filter(responsibility_span == 1) %>% 
    dplyr::select(-responsibility_span)
  
  workplan@project_assignments <- workplanr_project_assignments(staff_name = as.character(project_assignments$staff_name),
                                                           project_role_name = project_assignments$project_role_name,
                                                           project_phase_name = project_assignments$project_phase_name,
                                                           project_name = project_assignments$project_name,
                                                           staff_contribution = project_assignments$staff_contribution)
  
  project_assignments <- project_assignments %>%
    dplyr::mutate(staff_name = staff_name_for_unassigned_work,
                  staff_contribution = 100) %>%
    dplyr::distinct()
  
  workplan@project_unassignments <- workplanr_project_assignments(staff_name = as.character(project_assignments$staff_name),
                                                           project_role_name = project_assignments$project_role_name,
                                                           project_phase_name = project_assignments$project_phase_name,
                                                           project_name = project_assignments$project_name,
                                                           staff_contribution = project_assignments$staff_contribution)
  
  workplan <- calculate_workplan(workplan)

  return(workplan)
}


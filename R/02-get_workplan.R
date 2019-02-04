#' create a list of employees that are to be assigned to projects
#'
#' @param staff Names of staff members
#' @param staff_capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @param projects Names of projects
#' @param project_probability Probability that the project will go ahead
#' @param project_start Expected start date of project
#' @param project_end Expected end date of the project
#' @param project_phases List of phases in any project in order of execution
#' @param project_time_estimates Time estimates of how long each phase will take in relation 
#' @param staff_on_leave Names of staff that are going to be out of the office
#' @param leave_start Starting date for leave
#' @param leave_end Ending date for leave
#' @param public_holidays A data frame of dates and names of public holidays
#' @param leave_description Type of leave, can be user defined but recommend "leave" or "work trip"
#' @param staff_project_assigned_capacity Amount of time each staff is expected to dedicate to each [project, phase] 
#' @param randomise Set to TRUE to create a randomised workplan
#' @return A reference table for staff
#' @examples 
#' library(workplanr)
#' @export
get_workplan <- function(staff, staff_capacity, projects, project_probability, project_start, project_end, project_phases, 
                         project_time_estimates = 10, staff_on_leave, 
                         leave_start, leave_end, leave_description, public_holidays,
                         staff_project_assigned_capacity = 100,
                         randomise = TRUE) {
  wp <- new("workplan")
  wp@resources <- set_resources(staff, staff_capacity)
  wp@projects <- set_projects(projects, project_probability, project_start, project_end)
  wp@phases <- set_phases(project_phases)
  wp@leave <- set_leave(staff_on_leave, leave_start, leave_end, leave_description)
  wp@holidays <- set_public_holidays(public_holidays)
  wp@time_estimates <- set_time_estimates(wp@projects@project, wp@phases@phase, project_time_estimates)
  wp@project_teams <- set_project_team(wp@projects@project, wp@phases@phase, 
                                       wp@resources@staff, 
                                       staff_project_assigned_capacity, randomise) 
  #initialise dummy schedules
  #TODO: try new()?
  wp@full_schedule <- full_sched(date = lubridate::today(), project = wp@projects@project[1], 
                                 phase = wp@phases@phase[1], 
                                 staff = wp@resources@staff[1], 
                                 assigned_capacity = wp@project_teams@assigned_capacity[1],
                                 capacity = wp@resources@capacity[1],
                                 public_holiday = "dummy",
                                 out_of_office = "dummy", project_duration = 1,
                                 num_holidays = 1, holiday_expansion_factor = 1,
                                 num_out_of_office = 1, leave_expansion_factor = 1,
                                 leave_adjusted_workload = wp@project_teams@assigned_capacity[1])
  wp@staff_schedule <- staff_sched(date = lubridate::today(), staff = wp@resources@staff[1], project = as.character(wp@projects@project[1]),
                                 workload = wp@project_teams@assigned_capacity[1], out_of_office = "dummy", public_holiday = "dummy")
  wp@team_schedule <- team_sched(date = lubridate::today(), workload = wp@project_teams@assigned_capacity[1])
  #calculate actual schedules
  wp@full_schedule <- set_full_schedule(wp)
  wp@staff_schedule <- set_staff_schedule(wp)
  wp@team_schedule <- set_team_schedule(wp)
  return(wp)
}



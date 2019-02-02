#' create a list of employees that are to be assigned to projects
#'
#' @param staff Names of staff members
#' @param capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @return A reference table for staff
#' @examples 
#' staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
#' capacity <- c(40,60,100,100,100)
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' roles <- c("lead", "researcher", "editor", "design")
#' wp <- get_workplan(staff, capacity, projects, probability, start, end, phases, roles)
#' @export
get_workplan <- function(staff, capacity, projects, probability, start, end, phases, 
                         roles, responsibilities = 1, time_estimates = 10, assigned_staff = "unassigned", assigned_capacity = 100) {
  wp <- new("workplan")
  wp@resources <- set_resources(staff, capacity)
  wp@projects <- set_projects(projects, probability, start, end)
  wp@phases <- set_phases(phases)
  wp@roles <- set_roles(roles)
  wp@responsibilities <- set_responsibilities(wp@roles@role, wp@phases@phase, responsibilities)
  wp@time_estimates <- set_time_estimates(wp@projects@project, wp@phases@phase, time_estimates)
  wp@project_teams <- set_project_team(wp@projects@project, wp@roles@role, assigned_staff, assigned_capacity) 
  return(wp)
}




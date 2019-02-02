#' Assigns staff roles on a project
#'
#' @param project_teams Data frame that lists project, role, staff, assigned_capacity
#' @return A reference table project teams and roles
#' @examples 
#' staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
#' capacity <- c(40,60,100,100,100)
#' resources <- set_resources(staff, capacity)
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' projects <- set_projects(projects, probability, start, end)
#' roles <- c("lead", "researcher", "editor", "design")
#' roles <- set_roles(roles)
#' project_teams <- expand.grid(project = projects$project, 
#'                              role = roles$role, 
#'                              KEEP.OUT.ATTRS = FALSE)
#' project_teams$staff <- sample(resources$staff, 
#'                               size = nrow(project_teams), 
#'                               replace = TRUE)
#' project_teams$assigned_capacity <- sample(c(25,25,75,100), 
#'                                    size = nrow(project_teams), 
#'                                    replace = TRUE)
#' project_teams <- set_project_team(project_teams)
#' @export
set_project_team = function(project_teams){

  return(project_teams)
}

#' Assigns staff roles on a project
#'
#' @param project_teams Data frame that lists project, role, staff, assigned_capacity
#' @return A reference table project teams and roles
#' @examples 
#' data("projects")
#' data("roles")
#' data("resources")
#' project_teams <- expand.grid(project = projects$project, role = roles, KEEP.OUT.ATTRS = FALSE)
#' project_teams$staff <- sample(resources$staff, size = nrow(project_teams), replace = TRUE)
#' project_teams$assigned_capacity <- sample(c(25,25,75,100), size = nrow(project_teams), replace = TRUE)
#' project_teams <- set_project_team(project_teams)
#' @export
set_project_team = function(project_teams){
  .db.save(project_teams)
  return(project_teams)
}


#' Retrieve project teams
#'
#' @examples 
#' get_project_teams()
#' @export
get_project_teams <- function() {
  
  return(.db.get("project_teams"))
  
}
#' Assigns staff roles on a project
#'
#' @param project_teams Data frame that lists project, role, staff, assigned_capacity
#' @return A reference table project teams and roles
#' @examples 
#' n <- 3
#' projects <- LETTERS[1:n]
#' roles <- c("lead", "researcher", "editor", "design")
#' project_teams <- expand.grid(project = projects, role = roles)
#' staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
#' project_teams$staff <- sample(staff, size = nrow(project_teams), replace = TRUE)
#' project_teams$assigned_capacity <- sample(c(25,25,75,100), size = nrow(project_teams), replace = TRUE)
#' project_teams <- create_project_team(project_teams)
#' @export
create_project_team = function(project_teams){
  #TODO: get_resources
  #TODO: get_roles etc
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
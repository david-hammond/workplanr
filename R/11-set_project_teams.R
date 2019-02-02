#' Assigns staff roles on a project
#'
#' @param projects List of projects
#' @param roles Roles of projects
#' @param staff Assigned staff to each [project, role] combination (needs to be at least length(project) x length(roles) in length)
#' @param assigned_capacity Amount of time each staff is expected to dedicate to each [project, role] 
#' combination (needs to be at least length(project) x length(roles) in length)
#' @return A reference table project teams and roles
#' @keywords internal

set_project_team = function(projects, roles, assigned_staff = NA, assigned_capacity = NA){
  
  tmp <- expand.grid(project = projects, 
                               role = roles,
                               KEEP.OUT.ATTRS = FALSE)
  tmp$assigned_staff = assigned_staff 
  tmp$assigned_capacity = assigned_capacity
  tmp <- team(project = tmp$project, 
              role = tmp$role, 
              staff = tmp$assigned_staff,
              assigned_capacity = tmp$assigned_capacity)
  return(tmp)
}


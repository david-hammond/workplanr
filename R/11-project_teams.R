#' Assigns staff roles on a project
#'
#' @param projects Data frame that lists project, role, staff, assigned_capacity
#' @param roles vvv
#' @param staff nnn
#' @param assigned_capacity vvv
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


#' Assign staff member to project phase
#'
#' @param workplan The name of the database to create 
#' @param staff_name staff name
#' @param project_name project name
#' @param project_role_name project_phase_name
#' @param staff_contribution staff con
#' @export
assign_staff = function(workplan, staff_name, project_name, project_role_name, staff_contribution){
  for (i in 1:length(staff_name)){
    pos = which(workplan@project_assignments@project_name == project_name[i] &
                  workplan@project_assignments@project_role_name == project_role_name[i] &
                  workplan@project_assignments@staff_name == staff_name[i])
    workplan@project_assignments@staff_contribution[pos] <- staff_contribution[i]
    pos <- which(workplan@project_unassignments@project_name == project_name[i] &
                  workplan@project_unassignments@project_role_name == project_role_name[i])
    workplan@project_unassignments@staff_contribution[pos] <- 0
  }
  workplan <- calculate_workplan(workplan)
  return(workplan)
}
#' Remove staff member from project phase
#' @param workplan The name of the database to create 
#' @param staff_name staff name
#' @param project_name project name
#' @param project_role_name project_phase_name
#' @export
remove_staff = function(db_name, staff_name, project_name, project_role_name){
  for (i in 1:length(staff_name)){
    pos = which(workplan@project_assignments@project_name == project_name[i] &
                  workplan@project_assignments@project_role_name == project_role_name[i] &
                  workplan@project_assignments@staff_name == staff_name[i])
    workplan@project_assignments@staff_contribution[pos] <- 0
    pos <- which(workplan@project_unassignments@project_name == project_name[i] &
                   workplan@project_unassignments@project_role_name == project_role_name[i])
    workplan@project_unassignments@staff_contribution[pos] <- max(workplan@staff@staff_capacity)
  }
  return(workplan)
}
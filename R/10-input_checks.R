#' Populate workplanr database with data
#'
#' @param string Names of staff members
#' @keywords internal
proper_capitalise = function(string){
  gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(string), perl=TRUE)
}
#' Populate workplanr database with data
#'
#' @param staff Names of staff members
#' @param workplan Workplan
#' @keywords internal
staff_name_check = function(staff_name, workplan){
  testthat::expect(length(setdiff(staff_name, workplan@staff@staff_name)) == 0 ,
                   message = paste(paste0(setdiff(staff_name, workplan@staff@staff_name), collapse = ", "),
                                   "Not in your staff list, please check spelling"))
}

#' Populate workplanr database with data
#'
#' @param project_name Names of staff members
#' @param workplan Workplan
#' @keywords internal
project_name_check = function(project_name, workplan){
  testthat::expect(length(setdiff(project_name, workplan@projects@project_name)) == 0 ,
                   message = paste(paste0(setdiff(project_name, workplan@projects@project_name), collapse = ", "), 
                                         "Not in your project list, please check spelling"))
}

#' Populate workplanr database with data
#'
#' @param project_phase_name Names of staff members
#' @param workplan Workplan
#' @keywords internal
project_phase_name_check = function(project_phase_name, workplan){
  testthat::expect(length(setdiff(project_phase_name, workplan@project_phases@project_phase_name)) == 0 ,
                   message = paste(paste0(setdiff(project_phase_name, workplan@project_phases@project_phase_name), collapse = ", "), 
                                   "Not in your phase list, please check spelling"))
}

#' Populate workplanr database with data
#'
#' @param project_role_name Names of staff members
#' @param workplan Workplan
#' @keywords internal
project_role_name_check = function(project_role_name, workplan){
  testthat::expect(length(setdiff(project_role_name, workplan@project_roles@project_role_name)) == 0 ,
                   message = paste(paste0(setdiff(project_role_name, workplan@project_roles@project_role_name), collapse = ", "), 
                   "Not in your role list, please check spelling"))
}


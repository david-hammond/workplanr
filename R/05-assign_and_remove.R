#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @export
assign_staff = function(db_name, staff_name, project_name, project_phase_name, staff_contribution){
  tmp <- get_staff(db_name)
  staff_name = match(staff_name, tmp$staff_name)
  tmp <- get_projects(db_name)
  project_name <- match(project_name, tmp$project_name)
  tmp <- get_project_phases(db_name)
  project_phase_name <- match(project_phase_name, tmp$project_phase_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  query <- paste0("UPDATE project_assignments SET staff_contribution = ", staff_contribution,  
  " WHERE id_staff = ", staff_name, 
  " AND id_project = ", project_name,
  " AND id_project_phase = ", project_phase_name, ";")
  rs <- RSQLite::dbSendQuery(con, query)
  RSQLite::dbClearResult(rs)
  RSQLite::dbDisconnect(con)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @export
remove_staff = function(db_name, staff_name, project_name, project_phase_name){
  tmp <- get_staff(db_name)
  staff_name = match(staff_name, tmp$staff_name)
  tmp <- get_projects(db_name)
  project_name <- match(project_name, tmp$project_name)
  tmp <- get_project_phases(db_name)
  project_phase_name <- match(project_phase_name, tmp$project_phase_name)
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  query <- paste0("UPDATE project_assignments SET staff_contribution = ", 0,  
                  " WHERE id_staff = ", staff_name, 
                  " AND id_project = ", project_name,
                  " AND id_project_phase = ", project_phase_name, ";")
  rs <- RSQLite::dbSendQuery(con, query)
  RSQLite::dbClearResult(rs)
  RSQLite::dbDisconnect(con)
}
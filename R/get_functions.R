#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @keywords internal
get_staff = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- RSQLite::dbReadTable(con, "staff")
  RSQLite::dbDisconnect(con)
  return(rs)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @keywords internal
get_projects = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- RSQLite::dbReadTable(con, "projects")
  RSQLite::dbDisconnect(con)
  return(rs)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @keywords internal
get_project_phases = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- RSQLite::dbReadTable(con, "project_phases")
  RSQLite::dbDisconnect(con)
  return(rs)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @keywords internal
get_out_of_office = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- RSQLite::dbSendQuery(con, "SELECT id_out_of_office, staff_name, 
                             out_of_office_start, 
                             out_of_office_end, 
                             work_related 
                             FROM out_of_office 
                             INNER JOIN staff ON staff.id_staff = out_of_office.id_staff;")
  rs <- RSQLite::dbFetch(rs)
  RSQLite::dbDisconnect(con)
  return(rs)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @keywords internal
get_public_holidays = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- RSQLite::dbReadTable(con, "public_holidays")
  RSQLite::dbDisconnect(con)
  return(rs)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @keywords internal
get_calendar = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- RSQLite::dbReadTable(con, "calendar")
  RSQLite::dbDisconnect(con)
  return(rs)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @keywords internal
get_time_estimates = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- RSQLite::dbSendQuery(con, "SELECT project_name, project_phase_name, project_start, project_end, 
                           project_confirmed, time_estimate FROM time_estimates INNER JOIN projects ON projects.id_project = time_estimates.id_project
                             INNER JOIN project_phases ON project_phases.id_project_phase =  time_estimates.id_project_phase;")
  rs <- RSQLite::dbFetch(rs)
  RSQLite::dbDisconnect(con)
  return(rs)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @keywords internal
get_project_assignments = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- RSQLite::dbSendQuery(con, "SELECT project_name, project_phase_name, staff_name, staff_capacity, staff_contribution
                                                              FROM project_assignments 
                             INNER JOIN projects ON projects.id_project = project_assignments.id_project
                             INNER JOIN project_phases ON project_phases.id_project_phase =  project_assignments.id_project_phase
                             INNER JOIN staff ON staff.id_staff =  project_assignments .id_staff;")
  rs <- RSQLite::dbFetch(rs)
  RSQLite::dbDisconnect(con)
  return(rs)
}


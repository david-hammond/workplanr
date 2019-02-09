#' Assign staff member to project phase
#'
#' @param db_name The name of the database to create 
#' @param staff_name staff name
#' @param project_name project name
#' @param project_role_name project_phase_name
#' @param staff_contribution staff con
#' @export
assign_staff = function(db_name, staff_name, project_name, project_role_name, staff_contribution){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  for (i in 1:length(staff_name)){
    tmp <- get_staff(db_name)
    id_staff = match(staff_name[i], tmp$staff_name)
    tmp <- get_projects(db_name)
    id_project <- match(project_name[i], tmp$project_name)
    tmp <- get_project_roles(db_name)
    id_project_role <- match(project_role_name[i], tmp$project_role_name)
    query <- paste0("UPDATE project_assignments SET staff_contribution = ", staff_contribution[i],  
                    " WHERE id_staff = ", id_staff, 
                    " AND id_project = ", id_project,
                    " AND id_project_role = ", id_project_role, ";")
    rs <- RSQLite::dbSendQuery(con, query)
    RSQLite::dbClearResult(rs)
    query <- paste0("UPDATE project_unassignments SET staff_contribution = ", 0,  
                    " WHERE id_project = ", id_project,
                    " AND id_project_role = ", id_project_role, ";")
    rs <- RSQLite::dbSendQuery(con, query)
    RSQLite::dbClearResult(rs)
  }
  RSQLite::dbDisconnect(con)
  return(TRUE)
}
#' Remove staff member from project phase
#' @param db_name The name of the database to create 
#' @param staff_name staff name
#' @param project_name project name
#' @param project_role_name project_phase_name
#' @export
remove_staff = function(db_name, staff_name, project_name, project_role_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  for (i in 1:length(staff_name)){
    tmp <- get_staff(db_name)
    id_staff = match(staff_name[i], tmp$staff_name)
    staff_name_for_unassigned_work <- tmp[nrow(tmp),]
    tmp <- get_projects(db_name)
    id_project <- match(project_name[i], tmp$project_name)
    tmp <- get_project_roles(db_name)
    id_project_role <- match(project_role_name[i], tmp$project_role_name)
    query <- paste0("UPDATE project_assignments SET staff_contribution = ", 0,  
                    " WHERE id_staff = ", id_staff, 
                    " AND id_project = ", id_project,
                    " AND id_project_role = ", id_project_role, ";")
    rs <- RSQLite::dbSendQuery(con, query)
    RSQLite::dbClearResult(rs)
    query <- paste0("UPDATE project_unassignments SET staff_contribution = ", 100,  
                    " WHERE id_project = ", id_project,
                    " AND id_project_role = ", id_project_role, ";")
    rs <- RSQLite::dbSendQuery(con, query)
    RSQLite::dbClearResult(rs)
  }
  RSQLite::dbDisconnect(con)
  return(TRUE)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @export
assign_staff = function(db_name, staff_name, project_name, project_phase_name, staff_contribution){
  for (i in length(staff_name)){
    tmp <- get_staff(db_name)
    id_staff = match(staff_name[i], tmp$staff_name)
    staff_name_for_unassigned_work <- tmp[nrow(tmp),]
    tmp <- get_projects(db_name)
    id_project <- match(project_name[i], tmp$project_name)
    tmp <- get_project_phases(db_name)
    id_project_phase <- match(project_phase_name[i], tmp$project_phase_name)
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
    query <- paste0("UPDATE project_assignments SET staff_contribution = ", staff_contribution,  
                    " WHERE id_staff = ", id_staff, 
                    " AND id_project = ", id_project,
                    " AND id_project_phase = ", id_project_phase, ";")
    rs <- RSQLite::dbSendQuery(con, query)
    RSQLite::dbClearResult(rs)
    RSQLite::dbDisconnect(con)
    remove_staff(db_name, staff_name_for_unassigned_work$staff_name, project_name, project_phase_name)
  }
  return(TRUE)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @export
remove_staff = function(db_name, staff_name, project_name, project_phase_name){
  for (i in length(staff_name)){
    tmp <- get_staff(db_name)
    id_staff = match(staff_name, tmp$staff_name)
    staff_name_for_unassigned_work <- tmp[nrow(tmp),]
    tmp <- get_projects(db_name)
    id_project <- match(project_name, tmp$project_name)
    tmp <- get_project_phases(db_name)
    id_project_phase <- match(project_phase_name, tmp$project_phase_name)
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
    query <- paste0("UPDATE project_assignments SET staff_contribution = ", 0,  
                    " WHERE id_staff = ", id_staff, 
                    " AND id_project = ", id_project,
                    " AND id_project_phase = ", id_project_phase, ";")
    rs <- RSQLite::dbSendQuery(con, query)
    RSQLite::dbClearResult(rs)
    query <- paste0("SELECT MAX(staff_contribution) FROM project_assignments 
                    WHERE id_project = ", id_project,
                    " AND id_project_phase = ", id_project_phase, ";")
    rs <- RSQLite::dbGetQuery(con, query)
    if(rs$`MAX(staff_contribution)` == 0){
      query <- paste0("UPDATE project_assignments SET staff_contribution = ", 100,  
                      " WHERE id_staff = ", staff_name_for_unassigned_work$id_staff, 
                      " AND id_project = ", id_project,
                      " AND id_project_phase = ", id_project_phase, ";")
      rs <- RSQLite::dbSendQuery(con, query)
      RSQLite::dbClearResult(rs)
    }
    RSQLite::dbDisconnect(con)
  }
}
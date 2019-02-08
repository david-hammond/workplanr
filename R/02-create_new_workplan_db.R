#' Populate workplanr database with data
#'
#' @param staff Names of staff members
#' @param projects Names of projects
#' @param calendar Dates of work plan
#' @param project_phases List of phases in any project in order of execution
#' @param out_of_office Names of staff that are going to be out of the office
#' @param public_holidays A data frame of dates of public holidays
#' @param time_estimates Time estimates of how long each phase will take in relation 
#' @param staff_name_for_unassigned_work Dummy staff member to assign work to
#' @param db_name name of database to create
#' @return TRUE if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
create_new_workplan_db = function(staff,
                               projects,
                               calendar,
                               project_phases,
                               out_of_office,
                               public_holidays,
                               time_estimates,
                               staff_name_for_unassigned_work = "unassigned",
                               db_name = "my_workplan.sqlite"){
  init_db(db_name)
  if("id_staff" %in% names(staff)){
    staff <- rbind(staff, data.frame(id_staff = nrow(staff) + 1, 
                                     staff_name = staff_name_for_unassigned_work, 
                                     staff_capacity = 0))
  }else{
    staff <- rbind(staff, data.frame(staff_name = staff_name_for_unassigned_work, staff_capacity = 0))
  }
  
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  RSQLite::dbWriteTable(con, "staff", staff, append = T)
  RSQLite::dbWriteTable(con, "projects", projects, append = T)
  RSQLite::dbWriteTable(con, "project_phases", project_phases, append = T)
  RSQLite::dbWriteTable(con, "out_of_office", out_of_office, append = T)
  RSQLite::dbWriteTable(con, "public_holidays", public_holidays, append = T)
  RSQLite::dbWriteTable(con, "calendar", calendar, append = T)
  #TODO: just standardise these initially and then assign and remove
  RSQLite::dbWriteTable(con, "time_estimates", time_estimates, append = T) 
  project_assignments <- expand.grid(id_staff = 1:nrow(staff), id_project_phase = 1:nrow(project_phases), 
                                        id_project = 1:nrow(projects),
                                        KEEP.OUT.ATTRS = FALSE)
  project_assignments$staff_contribution <- 0
  #assign all work to unassigned
  project_assignments$staff_contribution[project_assignments$id_staff == nrow(staff)] <- 100
  RSQLite::dbWriteTable(con, "project_assignments", project_assignments, append = T)
  RSQLite::dbDisconnect(con)
  return(TRUE)
}



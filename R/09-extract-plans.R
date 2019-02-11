
#' Initialise schedule table
#'
#' @param db_name The name of the database to create 
#' @export
extract_project_schedule = function(project, workplan){
  testthat::expect(project %in% workplan@projects@project_name, message = "Project not in workplan")
  new_workplan <- new("workplan")
  for (i in setdiff(slotNames(workplan), c("schedule", "release_schedule", "staff_schedule", "team_schedule"))){
    tmp <- slot(workplan, i)
    if("project_name" %in% slotNames(tmp)){
      pos <- slot(tmp, "project_name") == project
      for (j in slotNames(tmp)){
        slot(tmp, j) <- slot(tmp, j)[pos]
      }
      
    }      
    slot(new_workplan, i) <- tmp
  }
  new_workplan <- calculate_workplan(new_workplan)
  return(new_workplan)
}


#' Initialise schedule table
#'
#' @param db_name The name of the database to create 
#' @export
extract_staff_schedule = function(staff, workplan){
  testthat::expect(staff %in% workplan@staff@staff_name, message = "Staff not in workplan")
  new_workplan <- new("workplan")
  for (i in setdiff(slotNames(workplan), c("schedule", "release_schedule", "staff_schedule", "team_schedule"))){
    tmp <- slot(workplan, i)
    if("staff_name" %in% slotNames(tmp)){
      pos <- slot(tmp, "staff_name") == staff
      for (j in slotNames(tmp)){
        slot(tmp, j) <- slot(tmp, j)[pos]
      }
      
    }      
    slot(new_workplan, i) <- tmp
  }
  new_workplan <- calculate_workplan(new_workplan)
  return(new_workplan)
}
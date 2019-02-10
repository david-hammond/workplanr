#' Create staff schedule from full schedule
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @keywords internal
get_staff_schedule = function(workplan){
  tmp = as.data.frame(workplan@schedule)
  staff_schedule <- tmp %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::summarise(workload = sum(staff_contribution)/max(c(staff_capacity, max(workplan@staff@staff_capacity)), na.rm = T)) %>%
    dplyr::ungroup() 
  projects = tmp %>% 
    dplyr::filter(staff_contribution > 0) %>%
    dplyr::group_by(project_name, staff_name) %>%
    dplyr::filter(date == min(as.Date(date))) %>%
    dplyr::select(project_name, staff_name, date, staff_contribution) %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::distinct() %>%
    dplyr::summarise(project_name = paste(project_name, collapse = ", ")) %>%
    dplyr::ungroup() 
  staff_schedule <- staff_schedule %>%
    dplyr::left_join(projects)
  tmp <- tmp %>%
    dplyr::select(date, staff_name, id_out_of_office, out_of_office, holiday_name) 
  staff_schedule <- staff_schedule %>%
    dplyr::left_join(tmp)
  return(tmp)
}

#' Create team schedule from full schedule
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @keywords internal
get_team_schedule = function(tmp){
  team_capacity <- tmp %>% 
    dplyr::select(staff_name, staff_capacity) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  team_capacity <- sum(team_capacity$staff_capacity)
  tmp <- tmp %>% 
    dplyr::group_by(date, project_confirmed) %>% 
    dplyr::summarise(staff_contribution = sum(staff_contribution, na.rm=TRUE),
                     workload = round(staff_contribution/team_capacity,2)) %>%
    dplyr::select(-staff_contribution) %>%
    dplyr::ungroup() 
  return(tmp)
}

#' Create full schedule from database
#'
#' @param db_name The name of the database to create 
#' @export
get_schedule = function(db_name){
  tmp <- init_schedule(db_name)
  tmp <- add_project_calendar(tmp, db_name)
  tmp <- add_staff_assignment(tmp, db_name)                            
  tmp <- add_staff_out_of_office(tmp, db_name)
  tmp$date = as.Date(tmp$date)
#  tmp <- factor_leave_in_work_allocation(tmp)
  schedule <- list()
  schedule$full_schedule <- tmp
  schedule$staff_schedule <- get_staff_schedule(tmp)
  schedule$team_schedule <- get_team_schedule(tmp)
  return(schedule)
}

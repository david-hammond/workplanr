
#' Initialise schedule table
#'
#' @param db_name The name of the database to create 
#' @keywords internal
init_schedule = function(db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  tmp <- RSQLite::dbReadTable(con, "public_holidays")
  tmp <- dplyr::left_join(RSQLite::dbReadTable(con, "calendar"), tmp)
  tmp <- tmp %>% dplyr::select(-c(id_public_holidays, id_calendar))
  RSQLite::dbDisconnect(con)
  return(tmp)
}
#' Add calendar to schedule
#' @param tmp schedule
#' @param db_name The name of the database to create 
#' @keywords internal
add_project_calendar = function(tmp, db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  phases <- get_project_phases(db_name)
  phases <- phases$project_phase_name
  rs <- get_time_estimates(db_name)
  rs <- rs %>% dplyr::mutate(project_phase_name = factor(project_phase_name, 
                                       levels = phases,
                                       ordered = T)) %>%
  tidyr::spread(project_phase_name, time_estimate)
  
  end = rs 
  my_cal <- get_calendar(db_name)
  bizdays::create.calendar('normal', 
                           weekdays = c('saturday', 'sunday'), 
                           start.date = min(as.Date(my_cal$date)), 
                           end.date = max(as.Date(my_cal$date)))
  
  end[, ncol(end)] = bizdays::offset(end$project_end, end[,ncol(end)], 'normal')
  for (i in  rev(phases)[-1]){
    end[,i] = bizdays::offset(end[,(which(names(end) == i)) +1], rs[,i], 'normal')
  }
  
  start = rs
  for (i in  phases){
    start[,i] = bizdays::offset(end[,i], rs[,i], 'normal')
  }
  start[, phases[1]] = as.Date(apply(data.frame(as.Date(start$project_start), 
                                              as.Date(start[, phases[1]])), 1, min))
  
  rs = list(start = start, end = end)
  rs = lapply(rs, function(x) x %>% dplyr::select(-c(project_start, project_end)) %>% 
                      tidyr::gather(project_phase_name, date, -c(project_name, project_confirmed)) %>%
                      dplyr::mutate(project_phase_name = factor(project_phase_name, phases, ordered = T)))
  rs = dplyr::bind_rows(rs)
  rs = rs %>%
    dplyr::group_by(project_name, project_confirmed, project_phase_name) %>%
    padr::pad() %>%
      dplyr::ungroup() %>%
    dplyr::mutate(date = as.character(date))
  
  tmp = dplyr::left_join(tmp, rs)
  tmp$project_phase_name = as.character(tmp$project_phase_name)
  if(is.na(tmp$project_name[1])){
    tmp <- tmp[-(1:min(which(!is.na(tmp$project_name)))), ]
  }
  if(is.na(tmp$project_name[nrow(tmp)])){
    tmp <- tmp[-(nrow(tmp):max(which(!is.na(tmp$project_name)))), ]
  }
  RSQLite::dbDisconnect(con)
  return(tmp)
}
#' Add staff assignments to schedulke
#'
#' @param db_name The name of the database to create 
#' @param tmp schedule
#' @keywords internal
add_staff_assignment = function(tmp, db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- get_project_assignments(db_name)
  tmp <- dplyr::left_join(tmp, rs)
  RSQLite::dbDisconnect(con)
  return(tmp)
}
#' Add out of office times to schedule
#'
#' @param db_name The name of the database to create 
#' @param tmp schedule
#' @keywords internal
add_staff_out_of_office = function(tmp, db_name){
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rs <- get_out_of_office(db_name)
  rs <- rs %>% 
    tidyr::gather(date_type, date, -c(id_out_of_office, staff_name, work_related)) %>%
    dplyr::group_by(id_out_of_office, staff_name, work_related) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    padr::pad() %>% 
    dplyr::select(-date_type) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::rename(out_of_office = work_related)
  tmp <- dplyr::left_join(tmp, rs)
  RSQLite::dbDisconnect(con)
  return(tmp)
}
#' Re-calculate staff_contributions based on time in office
#'
#' @param tmp schedule
#' @keywords internal
factor_leave_in_work_allocation = function(tmp){
  
  tmp = tmp %>% dplyr::group_by(project_name, project_phase_name, staff_name) %>%
    dplyr::mutate(project_duration = bizdays::bizdays(min(date), max(date), 'normal'), 
           num_holidays = sum(!is.na(unique(holiday_name))),
           holiday_expansion_factor = project_duration/(project_duration-num_holidays),
           num_out_of_office = sum(!is.na(out_of_office)),
           leave_expansion_factor = holiday_expansion_factor * 
             project_duration/(max(project_duration-num_out_of_office, 0.7)), # need to avoid div by 0 error
           leave_adjusted_workload = ifelse(is.na(out_of_office), leave_expansion_factor * staff_contribution, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(staff_name)) %>%
    dplyr::mutate(date = as.Date(date))
    
  tmp$leave_adjusted_workload <- ifelse(is.na(tmp$leave_adjusted_workload), 0, tmp$leave_adjusted_workload)
  
  return(tmp)
}

#' Create staff schedule from full schedule
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @keywords internal
get_staff_schedule = function(tmp){
  staff_schedule = tmp %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::summarise(workload = sum(leave_adjusted_workload)) %>%
    dplyr::ungroup() %>%  
    dplyr::filter(is.finite(workload)) #why inf?
  projects = tmp %>% 
    dplyr::filter(staff_contribution > 0) %>%
    dplyr::group_by(project_name, staff_name) %>%
    dplyr::filter(date == min(as.Date(date))) %>%
    dplyr::select(project_name, staff_name, date, leave_adjusted_workload) %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::distinct() %>%
    dplyr::summarise(project_name = paste(project_name, collapse = ", "),
                     workload = 1) %>%
    dplyr::ungroup() 
  #add leave
  
  leave <- tmp %>% 
    dplyr::group_by(id_out_of_office, staff_name, out_of_office) %>%
    dplyr::summarise(start = min(date), end = max(date), workload = 1) %>%
    dplyr::filter(!is.na(out_of_office))
  
  public_holidays <- tmp %>% 
    dplyr::filter(!is.na(holiday_name))
  
  tmp = list(staff_schedule = staff_schedule, projects = projects,
             leave = leave, public_holidays = public_holidays)
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
    dplyr::summarise(leave_adjusted_workload = sum(leave_adjusted_workload, na.rm=TRUE),
                     workload = round(leave_adjusted_workload/team_capacity,2)) %>%
    dplyr::select(-leave_adjusted_workload) %>%
    dplyr::ungroup() 
  return(tmp)
}

#' Create full schedule from database
#'
#' @param db_name The name of the database to create 
#' @export
get_schedule = function(db_name){
  schedule <- list()
  tmp <- init_schedule(db_name)
  tmp <- add_project_calendar(tmp, db_name)
  tmp <- add_staff_assignment(tmp, db_name) #factor issue                             
  tmp <- add_staff_out_of_office(tmp, db_name)
  tmp <- factor_leave_in_work_allocation(tmp)
  tmp$leave_adjusted_workload <- tmp$leave_adjusted_workload/tmp$staff_capacity
  tmp$staff_capacity <- tmp$staff_capacity/tmp$staff_capacity
  tmp$out_of_office <- ifelse(tmp$out_of_office == 1, "Work", "Vacation")
  schedule$full_schedule <- tmp
  schedule$staff_schedule <- get_staff_schedule(tmp)
  schedule$team_schedule <- get_team_schedule(tmp)
  return(schedule)
}

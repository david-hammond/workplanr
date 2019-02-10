
#' Initialise schedule table
#'
#' @param db_name The name of the database to create 
#' @keywords internal
calculate_start_and_end_dates = function(workplan){
  tmp <- as.data.frame(workplan@projects) %>%
    dplyr::left_join(as.data.frame(workplan@time_estimates))
  
  cals <- bizdays::create.calendar('normal', 
                           weekdays = c('saturday', 'sunday'), 
                           holidays = workplan@public_holidays@date,
                           start.date = min(tmp$project_start) - 2*max(abs(tmp$time_estimate)), 
                           end.date = max(tmp$project_end) + 2*max(abs(tmp$time_estimate)))
  tmp <- tmp %>% 
    tidyr::spread(project_phase_name, time_estimate)
  
  end = tmp 
  
  phases <- as.character(workplan@project_phases@project_phase_name)
  end[, ncol(end)] = bizdays::offset(end$project_end, end[,ncol(end)], 'normal')
  for (i in  rev(phases)[-1]){
    end[,i] = bizdays::offset(end[,(which(names(end) == i)) +1], tmp[,i], 'normal')
  }
  
  start = tmp
  for (i in  phases){
    start[,i] = bizdays::offset(end[,i], tmp[,i], 'normal')
  }
  start[, phases[1]] = as.Date(apply(data.frame(start$project_start,start[, phases[1]]), 1, min))
  
  tmp = list(start = start, end = end)
  tmp = lapply(tmp, function(x) x %>% dplyr::select(-c(project_start, project_end)) %>% 
                 tidyr::gather(project_phase_name, date, -c(project_name, project_confirmed)) %>%
                 dplyr::mutate(project_phase_name = factor(project_phase_name, phases, ordered = T)))
  tmp = dplyr::bind_rows(tmp)
  tmp = tmp %>%
    dplyr::group_by(project_name, project_confirmed, project_phase_name) %>%
    padr::pad() %>%
    dplyr::ungroup()  %>% 
    dplyr::mutate(date = as.Date(date))
  
  tmp <- tmp %>%
    dplyr::left_join(as.data.frame(workplan@public_holidays)) %>%
    dplyr::select(date, project_name, project_confirmed, project_phase_name, holiday_name) %>%
    dplyr::arrange(date)
  bizdays::remove.calendars('normal')
  return(tmp)
}
#' Add staff assignments to schedulke
#'
#' @param db_name The name of the database to create 
#' @param tmp schedule
#' @keywords internal
add_project_assignments = function(schedule, workplan){
    schedule <- schedule %>%
    dplyr::left_join(rbind(as.data.frame(workplan@project_assignments), as.data.frame(workplan@project_unassignments))) %>%
    dplyr::filter(staff_contribution > 0) %>%
      dplyr::mutate(staff_name = as.character(staff_name)) %>%
      dplyr::left_join(as.data.frame(workplan@staff))
  return(schedule)
}
#' Add out of office times to schedule
#'
#' @param db_name The name of the database to create 
#' @param tmp schedule
#' @keywords internal
add_staff_out_of_office = function(schedule, workplan){
  tmp <- as.data.frame(wplan@out_of_office)
  tmp <- tmp %>% 
    tidyr::gather(date_type, date, -c(id_out_of_office, staff_name, work_related)) %>%
    dplyr::group_by(id_out_of_office, staff_name, work_related) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    padr::pad() %>% 
    dplyr::select(-date_type) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::rename(out_of_office = work_related) %>%
    dplyr::mutate(out_of_office = ifelse(as.numeric(out_of_office) == 1, "Work", "Vacation"),
                  date = as.Date(date)) %>%
    dplyr::mutate(staff_name = as.character(staff_name))
  schedule <- schedule %>%
    dplyr::left_join(tmp) 
  return(schedule)
}

#' Create full schedule from database
#'
#' @param db_name The name of the database to create 
#' @export
get_schedule = function(workplan){
  schedule <- calculate_start_and_end_dates(workplan)
  schedule <- add_project_assignments(schedule, workplan)
  schedule <- add_staff_out_of_office(schedule, workplan)
  schedule <- workplanr_schedule(date = schedule$date,
                                 project_name = schedule$project_name,
                                 project_confirmed = schedule$project_confirmed,
                                 project_phase_name = schedule$project_phase_name,
                                 project_role_name = schedule$project_role_name,
                                 staff_name = schedule$staff_name,
                                 staff_capacity = schedule$staff_capacity,
                                 staff_contribution = schedule$staff_contribution,
                                 id_out_of_office = schedule$id_out_of_office,
                                 out_of_office = schedule$out_of_office,
                                 holiday_name = schedule$holiday_name)
  return(schedule)
}



#' Create staff schedule from full schedule
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @keywords internal
get_staff_schedule = function(workplan){
  tmp <- as.data.frame(workplan@schedule)
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
  
  staff_schedule <- workplanr_staff_schedule(date = as.Date(staff_schedule$date),
                                             staff_name = as.character(staff_schedule$staff_name),
                                             workload = as.numeric(staff_schedule$workload),
                                             project_name = as.character(staff_schedule$project_name),
                                             id_out_of_office = as.numeric(staff_schedule$id_out_of_office),
                                             out_of_office = as.character(staff_schedule$out_of_office),
                                             holiday_name = as.character(staff_schedule$holiday_name))
  return(staff_schedule)
}

#' Create team schedule from full schedule
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @keywords internal
get_team_schedule = function(workplan){
  tmp <- as.data.frame(workplan@schedule)
  team_capacity <- tmp %>% 
    dplyr::select(staff_name, staff_capacity) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  team_capacity <- max(c(sum(team_capacity$staff_capacity, na.rm = T), workplan@staff@staff_capacity))
  tmp <- tmp %>% 
    dplyr::group_by(date, project_confirmed) %>% 
    dplyr::summarise(staff_contribution = sum(staff_contribution, na.rm=TRUE),
                     workload = round(staff_contribution/team_capacity,2)) %>%
    dplyr::select(-staff_contribution) %>%
    dplyr::ungroup() 
  tmp <- workplanr_team_schedule(date = as.Date(tmp$date),
                                 project_confirmed = as.logical(tmp$project_confirmed),
                                 workload = as.numeric(tmp$workload))
  return(tmp)
}

calculate_workplan = function(workplan){
  workplan@schedule <- get_schedule(workplan)
  workplan@staff_schedule <- get_staff_schedule(workplan)
  workplan@team_schedule <- get_team_schedule(workplan)
  return(workplan)
}


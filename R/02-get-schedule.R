get_phases = function(){
  tmp <- RSQLite::dbSendQuery(con, "SELECT project_phase_name FROM project_phases")
  tmp <- RSQLite::dbFetch(tmp)
  tmp <- tmp$project_phase_name
  return(tmp)
}

init_schedule = function(con){
  tmp <- RSQLite::dbReadTable(con, "public_holidays")
  tmp <- dplyr::left_join(RSQLite::dbReadTable(con, "calendar"), tmp)
  tmp <- tmp %>% dplyr::select(-c(id_public_holidays, id_calendar))
  return(tmp)
}

add_project_calendar = function(tmp, con){
  rs <- RSQLite::dbSendQuery(con, "SELECT project_name, project_phase_name, project_start, project_end, 
                           project_confirmed, time_estimates  FROM time_estimates INNER JOIN projects ON projects.id_project = time_estimates.id_project
                             INNER JOIN project_phases ON project_phases.id_project_phase =  time_estimates.id_project_phase;")
  rs <- RSQLite::dbFetch(rs)
  
  rs <- rs %>% mutate(project_phase_name = factor(project_phase_name, 
                                       levels = get_phases(),
                                       ordered = T)) %>%
  tidyr::spread(project_phase_name, time_estimates)
  
  end = rs 
  
  end[, ncol(end)] = bizdays::offset(end$project_end, end[,ncol(end)], 'normal')
  for (i in  rev(get_phases())[-1]){
    end[,i] = bizdays::offset(end[,(which(names(end) == i)) +1], rs[,i], 'normal')
  }
  
  start = rs
  for (i in  get_phases()){
    start[,i] = bizdays::offset(end[,i], rs[,i], 'normal')
  }
  start[, get_phases()[1]] = as.Date(apply(data.frame(as.Date(start$project_start), 
                                              as.Date(start[, get_phases()[1]])), 1, min))
  
  rs = list(start = start, end = end)
  rs = lapply(rs, function(x) x %>% dplyr::select(-c(project_start, project_end)) %>% 
                      tidyr::gather(project_phase_name, date, -c(project_name, project_confirmed)) %>%
                      dplyr::mutate(project_phase_name = factor(project_phase_name, get_phases(), ordered = T)))
  rs = dplyr::bind_rows(rs)
  rs = rs %>%
    group_by(project_name, project_confirmed, project_phase_name) %>%
    padr::pad() %>%
      dplyr::ungroup() %>%
    dplyr::mutate(date = as.character(date))
  
  tmp = dplyr::left_join(tmp, rs)
  if(is.na(tmp$project_name[1])){
    tmp <- tmp[-(1:min(which(!is.na(tmp$project_name)))), ]
  }
  if(is.na(tmp$project_name[nrow(tmp)])){
    tmp <- tmp[-(nrow(tmp):max(which(!is.na(tmp$project_name)))), ]
  }

  return(tmp)
}

add_staff_assignment = function(tmp, con){
  rs <- RSQLite::dbSendQuery(con, "SELECT project_name, project_phase_name, staff_name, staff_capacity, staff_contribution
                                                              FROM project_assignments 
                             INNER JOIN projects ON projects.id_project = project_assignments.id_project
                             INNER JOIN project_phases ON project_phases.id_project_phase =  project_assignments.id_project_phase
                             INNER JOIN staff ON staff.id_staff =  project_assignments .id_staff;")
  rs <- RSQLite::dbFetch(rs)
  tmp <- dplyr::left_join(tmp, rs)
  return(tmp)
}

add_staff_out_of_office = function(tmp, con){
  rs <- RSQLite::dbSendQuery(con, "SELECT id_out_of_office, staff_name, 
                             out_of_office_start, 
                             out_of_office_end, 
                             work_related 
                             FROM out_of_office 
                             INNER JOIN staff ON staff.id_staff = out_of_office.id_staff;")
  rs <- RSQLite::dbFetch(rs)
  rs <- rs %>% 
    tidyr::gather(date_type, date, -c(id_out_of_office, staff_name, work_related)) %>%
    dplyr::group_by(id_out_of_office, staff_name, work_related) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    padr::pad() %>% 
    dplyr::select(-date_type) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::select(-id_out_of_office) %>%
    dplyr::rename(out_of_office = work_related)
  tmp <- dplyr::left_join(tmp, rs)
  return(tmp)
}

factor_leave_in_work_allocation = function(tmp){
  
  tmp = tmp %>% dplyr::group_by(project_name, project_phase_name, staff_name) %>%
    mutate(project_duration = bizdays::bizdays(min(date), max(date), 'normal'), 
           num_holidays = sum(!is.na(unique(holiday_name))),
           holiday_expansion_factor = project_duration/(project_duration-num_holidays),
           num_out_of_office = sum(!is.na(out_of_office)),
           leave_expansion_factor = holiday_expansion_factor * project_duration/(project_duration-num_out_of_office), 
           leave_adjusted_workload = ifelse(is.na(out_of_office), leave_expansion_factor * staff_contribution, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.na(staff_name)) %>%
    dplyr::mutate(date = as.Date(date))
    
  tmp$leave_adjusted_workload <- ifelse(is.na(tmp$leave_adjusted_workload), 0, tmp$leave_adjusted_workload)
  
  return(tmp)
}

schedule = init_schedule(con)
schedule = add_project_calendar(schedule, con)
schedule = add_staff_assignment(schedule, con)                             
schedule = add_staff_out_of_office(schedule, con)
schedule = factor_leave_in_work_allocation(schedule)



#' create a list of employees that are to be assigned to projects
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
#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @param tmp schedule
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
  RSQLite::dbDisconnect(con)
  return(tmp)
}
#' create a list of employees that are to be assigned to projects
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
#' create a list of employees that are to be assigned to projects
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
    dplyr::select(-id_out_of_office) %>%
    dplyr::rename(out_of_office = work_related)
  tmp <- dplyr::left_join(tmp, rs)
  RSQLite::dbDisconnect(con)
  return(tmp)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param tmp schedule
#' @keywords internal
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

#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 
#' @export
get_schedule = function(db_name){
  tmp = init_schedule(db_name)
  tmp = add_project_calendar(tmp, db_name)
  tmp = add_staff_assignment(tmp, db_name)                             
  tmp = add_staff_out_of_office(tmp, db_name)
  tmp = factor_leave_in_work_allocation(tmp)
  return(tmp)
}



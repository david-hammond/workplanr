#' create a daily list of assignments for each staff
#'
#' @param wp Complete workplan object
#' @return A reference table for daily projects schedule
#' @keywords internal
set_full_schedule = function(wp){
  tmp <- as.list(wp)
  schedule <- .get_schedule(tmp)
  schedule <- .get_assignment_schedule(schedule, tmp)
  
  # account for leave
  leave = tmp$leave
  leave = leave %>% tidyr::gather("type", "date", -c(staff, description))
  leave = leave %>%  
    padr::pad(group = "staff", interval = 'day') %>%
    dplyr::select(staff, date, description) %>%
    dplyr::rename(out_of_office = description) %>%
    tidyr::fill(out_of_office)
  schedule = dplyr::left_join(schedule, leave)
  
  # account for public holidays
  holidays = tmp$holidays %>%
    dplyr::select(date, name) %>% 
    dplyr::rename(public_holiday = name)
  schedule = dplyr::left_join(schedule, holidays)
  cal <- bizdays::create.calendar('normal', weekdays = c('saturday', 'sunday'), 
                                  start.date = min(tmp$projects$end)-180, end.date = max(tmp$projects$end) +180)
  schedule = schedule %>% dplyr::group_by(project) %>%
    mutate(project_duration = bizdays::bizdays(min(date), max(date), 'normal'), 
              num_holidays = sum(!is.na(public_holiday)),
              holiday_expansion_factor = project_duration/(project_duration-num_holidays))
  
  schedule = schedule %>% 
    dplyr::group_by(project, staff) %>%
    mutate(num_out_of_office = sum(!is.na(out_of_office)),
           leave_expansion_factor = holiday_expansion_factor * project_duration/(project_duration-num_out_of_office), 
           leave_adjusted_workload = ifelse(is.na(out_of_office), leave_expansion_factor * assigned_capacity, 0)) %>%
    dplyr::ungroup()

  schedule$leave_adjusted_workload <- ifelse(is.na(schedule$leave_adjusted_workload), 0, schedule$leave_adjusted_workload)
  schedule$staff <- ifelse(is.na(schedule$staff), "unassigned", schedule$staff)
  schedule <- full_sched(date = schedule$date, project = schedule$project, phase = schedule$phase, 
                       role = schedule$role, staff = as.character(schedule$staff), assigned_capacity = schedule$assigned_capacity,
                       capacity = schedule$capacity, public_holiday = as.character(schedule$public_holiday), 
                       out_of_office = as.character(schedule$out_of_office), 
                       project_duration = schedule$project_duration, num_holidays = schedule$num_holidays, 
                       holiday_expansion_factor = schedule$holiday_expansion_factor,
                       num_out_of_office = schedule$num_out_of_office, leave_expansion_factor = schedule$leave_expansion_factor,
                       leave_adjusted_workload = schedule$leave_adjusted_workload)
  return(schedule)
}

#' create a daily list of assignments for each staff
#'
#' @param wp Complete workplan object
#' @return A reference table for daily projects schedule
#' @keywords internal
.get_schedule <- function(wp){
   cal <- bizdays::create.calendar('normal', weekdays = c('saturday', 'sunday'), 
                          start.date = min(wp$projects$end)-180, end.date = max(wp$projects$end) +180)
   times = dplyr::left_join(wp$projects, wp$time_estimates)
   end = times
 
   end[, ncol(end)] = bizdays::offset(end$end, end[,ncol(end)], 'normal')
   for (i in  rev(names(wp$time_estimates[, -c(1, ncol(wp$time_estimates))]))){
     end[,i] = bizdays::offset(end[,(which(names(end) == i)) +1], times[,i], 'normal')
   }
   start = times
   for (i in  names(wp$time_estimates[, -1])){
     start[,i] = bizdays::offset(end[,i], times[,i], 'normal')
   }
  first.phase = names(wp$time_estimates[, -c(1, ncol(wp$time_estimates))])[1]
  #TODO: need to document this
   start[, first.phase] <- as.Date(apply(data.frame(start$start, start[,first.phase]), 1, min))
   schedule = list(start = start, end = end)
   schedule = lapply(schedule, function(x) x %>% dplyr::select(-c(probability, start, end)) %>% 
                       tidyr::gather(phase, date, -c(project)) %>%
                       dplyr::mutate(phase = factor(phase, wp$phases$phase, ordered = T)))
   schedule = dplyr::bind_rows(schedule, .id = 'date.type')
   schedule = schedule %>% 
     tidyr::spread(date.type, date) %>% 
     dplyr::filter(start!=end) %>%
     tidyr::gather(date.type, date, -c(project, phase))
   schedule$date.type = factor(schedule$date.type, (c('start', 'end')), ordered = T)

   schedule = schedule %>% 
     dplyr::arrange(project, phase, dplyr::desc(date), date.type) %>% 
     dplyr::select(-date.type)
   return(schedule)
 }
 
#' pad out daily list of assignments for each staff
#'
#' @param schedule A working schedule
#' @return A reference table for daily projects schedule
#' @keywords internal
.get_pad_schedule = function(schedule){
  schedule = schedule %>%  
    padr::pad(group = setdiff(names(schedule), 'date'), interval = 'day') 
  schedule <- schedule %>% padr::pad(group = c("staff", "capacity"), interval = 'day', 
                                     start_val = min(schedule$date), end_val = max(schedule$date)) 
  schedule$assigned_capacity = ifelse(is.na(schedule$assigned_capacity), 0, schedule$assigned_capacity)
  
  return(schedule)
}

#' pad out daily list of assignments for each staff
#'
#' @param schedule A working schedule
#' @param wp Complete workplan object
#' @return A reference table for daily projects schedule
#' @keywords internal
.get_assignment_schedule = function(schedule, wp){
   tmp = wp$responsibilities %>% tidyr::gather('phase', 'needed', -role) %>% 
     dplyr::filter(needed == 1) %>%  dplyr::select(-needed) 
   tmp$phase = factor(tmp$phase, levels = levels(schedule$phase), ordered = T)
   schedule = dplyr::left_join(schedule, tmp)
   schedule = dplyr::left_join(schedule, wp$project_teams)
   schedule = dplyr::left_join(schedule, wp$resources)
   schedule = .get_pad_schedule(schedule)
   return(schedule)
}


#' Create a daily list of assignments for each staff
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of staff per date
#' @keywords internal
set_staff_schedule = function(wp){ 
  daily_plan = as.data.frame(wp@full_schedule)
  daily_plan <- daily_plan %>% 
    dplyr::group_by(date, staff) %>% 
    dplyr::summarise(capacity = min(capacity), 
                leave_adjusted_workload = sum(leave_adjusted_workload),
                workload = round(leave_adjusted_workload/capacity,2)) %>%
    dplyr::select(-capacity, -leave_adjusted_workload) %>%
    dplyr::ungroup()

    full_staff_calendar <- expand.grid(date = unique(daily_plan$date), 
                                     staff = unique(daily_plan$staff),
                                     KEEP.OUT.ATTRS = FALSE) 
 
   daily_plan <- dplyr::left_join(full_staff_calendar, daily_plan) 
   # Add task labels
   tasks <- as.data.frame(wp@full_schedule) %>% 
     dplyr::group_by(staff, project) %>% 
     dplyr::summarise(date = min(date)) %>%
     dplyr::ungroup()
   tasks <- tasks %>% 
     dplyr::group_by(staff, date) %>% 
     dplyr::summarise(project = paste(project, collapse = ', ')) %>%
     dplyr::ungroup()
   daily_plan <- dplyr::left_join(daily_plan, tasks)
   daily_plan <- staff_sched(date = daily_plan$date, staff = as.character(daily_plan$staff), project = daily_plan$project,
                             workload = daily_plan$workload)
  
  return(daily_plan) 
}


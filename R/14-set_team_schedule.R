#' Calculate team workload calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of team per date
#' @keywords internal

set_team_schedule = function(wp){
  daily_plan = as.data.frame(wp@full_schedule)
  team_capacity <- daily_plan %>% 
    dplyr::select(staff, capacity) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  team_capacity <- sum(team_capacity$capacity)
  daily_plan <- daily_plan %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(leave_adjusted_workload = sum(leave_adjusted_workload),
                     workload = round(leave_adjusted_workload/team_capacity,2)) %>%
    dplyr::select(-leave_adjusted_workload) %>%
    dplyr::ungroup()
  
  daily_plan <- team_sched(date = daily_plan$date, workload = daily_plan$workload)
  
  return(daily_plan)
}

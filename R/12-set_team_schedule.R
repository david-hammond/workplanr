#' Calculate team workload calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of team per date
#' @keywords internal

set_team_schedule = function(wp){
  tmp = as.data.frame(wp@full_schedule)
  team_capacity <- tmp %>% 
    dplyr::select(staff, capacity) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  team_capacity <- sum(team_capacity$capacity)
  tmp <- tmp %>% 
    dplyr::group_by(date, probability) %>% 
    dplyr::summarise(leave_adjusted_workload = sum(leave_adjusted_workload, na.rm=TRUE),
                     workload = round(leave_adjusted_workload/team_capacity,2)) %>%
    dplyr::select(-leave_adjusted_workload) %>%
    dplyr::ungroup()
  
  tmp <- team_sched(date = tmp$date, probability = tmp$probability, workload = tmp$workload)
  
  return(tmp)
}

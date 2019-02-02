#' Calculate team workload calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of team per date
#' @examples 
#' @export
#' 

get_team_daily_workload = function(daily_plan){
  
  team_capacity <- daily_plan %>% 
    dplyr::select(staff, capacity) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  team_capacity <- sum(team_capacity$capacity)
  daily_plan <- daily_plan %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(assigned_capacity = sum(assigned_capacity),
                     workload = round(assigned_capacity/team_capacity,2)) %>%
    dplyr::select(-assigned_capacity) %>%
    dplyr::ungroup()
  
  daily_plan$teamload <- ifelse(daily_plan$workload > 1, "Overload", "Covered")
  return(daily_plan)
}

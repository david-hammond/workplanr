#' Create a daily list of assignments for each staff
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of staff per date
#' @examples 
#' @export
get_staff_daily_workload = function(daily_plan){ 
  
  daily_plan <- daily_plan %>% 
    dplyr::group_by(date, staff) %>% 
    dplyr::summarise(capacity = min(capacity), 
                assigned_capacity = sum(assigned_capacity),
                workload = round(assigned_capacity/capacity,2)) %>%
    dplyr::select(-capacity, -assigned_capacity) %>%
    dplyr::ungroup()

    full_staff_calendar <- expand.grid(date = unique(daily_plan$date), 
                                     staff = unique(daily_plan$staff),
                                     KEEP.OUT.ATTRS = FALSE) 
 
   daily_plan = dplyr::left_join(full_staff_calendar, daily_plan) 
  
  return(daily_plan) 
}


#' Create a daily list of assignments for each staff
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of staff per date
#' @examples 
#' library(workplanr)
#' staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
#' capacity <- c(40,60,100,100,100)
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' roles <- c("lead", "researcher", "editor", "design")
#' wp <- get_workplan(staff, capacity, projects, probability, start, end, phases, roles)
#' daily_plan <- get_daily_plan(wp)
#' get_staff_daily_workload(daily_plan)      
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


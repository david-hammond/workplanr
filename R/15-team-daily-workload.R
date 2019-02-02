#' Calculate team workload calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of team per date
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
#' get_team_daily_workload(daily_plan)
#'       
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

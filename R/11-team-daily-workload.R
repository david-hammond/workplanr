#' Calculate team workload calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of team per date
#' @examples 
#' staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
#' capacity <- c(40,60,100,100,100)
#' resources <- set_resources(staff, capacity)
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' projects <- set_projects(projects, probability, start, end)
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' phases <- set_phases(phases)
#' roles <- c("lead", "researcher", "editor", "design")
#' roles <- set_roles(roles)
#' responsibilities <- rbind(lead = rep(1, length(phases$phase)), 
#' researcher = c(1,1,1,0,0,0), editor = c(0,0,1,0,0,0), design = c(0,0,0,1,1,0))
#' responsibilities <- set_responsibilities(roles, phases, responsibilities)
#' time_estimates <- rbind(c(-40,-10,-10,-10,-10,10), c(-10,-10,-10,0,0,0), 
#'                         c(0,0,0,-10,-10,10))
#' time_estimates <- set_time_estimates(projects, phases, time_estimates)
#' project_teams <- expand.grid(project = projects$project, 
#'                              role = roles$role, KEEP.OUT.ATTRS = FALSE)
#' project_teams$staff <- sample(resources$staff, size = nrow(project_teams), 
#'                               replace = TRUE)
#' project_teams$assigned_capacity <- sample(c(25,25,75,100), 
#'                                    size = nrow(project_teams), 
#'                                    replace = TRUE)
#' project_teams <- set_project_team(project_teams)
#' daily_plan <- get_daily_plan(resources, projects, 
#'                phases, time_estimates, 
#'                project_teams, responsibilities)
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

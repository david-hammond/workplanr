#' create a daily list of assignments for each staff
#'
#' @param resources Names of staff
#' @param projects Lists of projects
#' @param phases Phases of a project
#' @param time_estimates Time required for different phases
#' @param project_teams Project teams
#' @param responsibilities A reference table for responsibilities
#' @return A reference table for daily projects schedule
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
#' get_daily_plan(resources, projects, 
#'                phases, time_estimates, 
#'                project_teams, responsibilities)
#' @export
get_daily_plan = function(resources, projects,
                          phases, time_estimates,
                          project_teams, responsibilities){

  schedule = .get_schedule(projects, time_estimates, phases)
  schedule = .get_assignment_schedule(schedule, resources, 
                                      project_teams, responsibilities)
  return(schedule)
}

.get_schedule <- function(projects, time_estimates, phases){
   cal <- bizdays::create.calendar('normal', weekdays = c('saturday', 'sunday'), 
                          start.date = min(projects$end)-180, end.date = max(projects$end) +180)
   times = dplyr::left_join(projects, time_estimates)
   end = times
 
   end[, ncol(end)] = bizdays::offset(end$end, end[,ncol(end)], 'normal')
   for (i in  rev(names(time_estimates[, -c(1, ncol(time_estimates))]))){
     end[,i] = bizdays::offset(end[,(which(names(end) == i)) +1], times[,i], 'normal')
   }
   start = times
   for (i in  names(time_estimates[, -1])){
     start[,i] = bizdays::offset(end[,i], -times[,i], 'normal')
   }
 
   schedule = list(start = start, end = end)
   schedule = lapply(schedule, function(x) x %>% dplyr::select(-c(probability, start, end)) %>% 
                       tidyr::gather(phase, date, -c(project)) %>%
                       dplyr::mutate(phase = factor(phase, phases$phase, ordered = T)))
   schedule = dplyr::bind_rows(schedule, .id = 'date.type')
   schedule = schedule %>% 
     tidyr::spread(date.type, date) %>% 
     dplyr::filter(start!=end) %>%
     tidyr::gather(date.type, date, -c(project, phase))
   schedule$date.type = factor(schedule$date.type, (c('start', 'end')), ordered = T)

   schedule = schedule %>% 
     dplyr::arrange(project, phase, desc(date), date.type) %>% 
     dplyr::select(-date.type)
   return(schedule)
 }
 
.get_pad_schedule = function(schedule){
  schedule = schedule %>%  
    padr::pad(group = setdiff(names(schedule), 'date'), interval = 'day') 
  return(schedule)
}

.get_assignment_schedule = function(schedule, resources, project_teams, responsibilities){
   tmp = responsibilities %>% tidyr::gather('phase', 'needed', -role) %>% 
     dplyr::filter(needed == 1) %>%  dplyr::select(-needed) 
   tmp$phase = factor(tmp$phase, levels = levels(schedule$phase), ordered = T)
   schedule = dplyr::left_join(schedule, tmp)
   schedule = dplyr::left_join(schedule, project_teams)
   schedule = dplyr::left_join(schedule, resources)
   schedule = .get_pad_schedule(schedule)
   return(schedule)
}


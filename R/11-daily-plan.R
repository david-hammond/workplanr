#' create a daily list of assignments for each staff
#'
#' @param resources Names of projects
#' @param projects Probability that the project will go ahead
#' @param phases Expected start date of project
#' @param time_estimates Expected end date of the project
#' @param project_teams Expected end date of the project
#' @param responsibilites Expected end date of the project
#' @return A reference table for projects 
#' @examples 
#' data("resources")
#' data("projects")
#' data("phases")
#' data("roles")
#' data("responsibilities")
#' data("time_estimates")
#' data("project_teams")
#' get_daily_plan(esources, projects, phases, time_estimates, project_teams, responsibilities)
#' @export
get_daily_plan = function(resources, projects,
                          phases, time_estimates,
                          project_teams, responsibilities){

  schedule = .get_schedule(projects, time_estimates, phases)
  schedule = .get_assignment_schedule(schedule, resources, project_teams, responsibilities)
  return(schedule)
}

.get_schedule <- function(projects, time_estimates, phases){
   require(bizdays)
   require(lubridate)
   require(tidyr)

   cal <- create.calendar('normal', weekdays = c('saturday', 'sunday'), 
                          start.date = min(projects$end)-180, end.date = max(projects$end) +180)
   times = left_join(projects, time_estimates)
   end = times
 
   end[, ncol(end)] = offset(end$end, end[,ncol(end)], 'normal')
   for (i in  rev(names(time_estimates[, -c(1, ncol(time_estimates))]))){
     end[,i] = offset(end[,(which(names(end) == i)) +1], times[,i], 'normal')
   }
   start = times
   for (i in  names(time_estimates[, -1])){
     start[,i] = offset(end[,i], -times[,i], 'normal')
   }
 
   schedule = list(start = start, end = end)
   schedule = lapply(schedule, function(x) x %>% select(-c(probability, start, end)) %>% 
                       gather(phase, date, -c(project)) %>%
                       mutate(phase = factor(phase, phases, ordered = T)))
   schedule = bind_rows(schedule, .id = 'date.type')
   schedule = schedule %>% spread(date.type, date) %>% filter(start!=end) %>%
     gather(date.type, date, -c(project, phase))
   schedule$date.type = factor(schedule$date.type, (c('start', 'end')), ordered = T)

   schedule = schedule %>% arrange(project, phase, desc(date), date.type) %>% 
     select(-date.type)
   return(schedule)
 }
 
.get_pad_schedule = function(schedule){
  require(padr)
  schedule = schedule %>%  
  pad(group = setdiff(names(schedule), 'date'), interval = 'day') 
  return(schedule)
}

.get_assignment_schedule = function(schedule, resources, project_teams, responsibilities){
   tmp = responsibilities %>% gather('phase', 'needed', -role) %>% 
     filter(needed == 1) %>%  select(-needed) 
   tmp$phase = factor(tmp$phase, levels = levels(schedule$phase), ordered = T)
   schedule = left_join(schedule, tmp)
   schedule = left_join(schedule, project_teams)
   schedule = left_join(schedule, resources)
   schedule = .get_pad_schedule(schedule)
   return(schedule)
}


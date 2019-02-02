#' create a daily list of assignments for each staff
#'
#' @param plan Completed object of class workplan
#' @return A reference table for daily projects schedule
#' @examples 
#' @export
get_daily_plan = function(wp){

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
     dplyr::arrange(project, phase, dplyr::desc(date), date.type) %>% 
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


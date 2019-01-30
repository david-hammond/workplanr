get.schedule <- function(projects, time.estimates, public.holidays){
  require(bizdays)
  require(lubridate)
  require(dplyr)
  cal <- create.calendar("normal", weekdays = c("saturday", "sunday"), 
                         holidays = public.holidays$date, 
                         start.date = min(projects$launch)-180, end.date = max(projects$launch) +180)
  times = left_join(projects, time.estimates)
  end = times

  end[, ncol(end)] = offset(end$launch, end$`launch-events`, 'normal')
  for (i in  rev(names(time.estimates[, -c(1, ncol(time.estimates))]))){
    end[,i] = offset(end[,(which(names(end) == i)) +1], times[,i], 'normal')
  }
  start = times
  for (i in  names(time.estimates[, -1])){
    start[,i] = offset(end[,i], -times[,i], 'normal')
  }

  schedule = list(start = start, end = end)
  schedule = lapply(schedule, function(x) x %>% select(-launch) %>%
                      gather(phase, date, -c(project, probability)) %>%
                      mutate(phase = factor(phase, names(time.estimates)[-1], ordered = T)))
  schedule = bind_rows(schedule, .id = "date.type")
  schedule = schedule %>% select(2:ncol(schedule), 1)
  schedule$date.type = factor(schedule$date.type, (c("start", "end")), ordered = T)
  schedule = schedule %>% arrange(project, phase, desc(date), date.type) %>% 
    select(-date.type)
  return(schedule)
}

get.padded.schedule = function(schedule){
  require(padr)
  schedule = schedule %>%  
    pad(group = setdiff(names(schedule), "date"), interval = "day") 
  return(schedule)
}

get.assignment.schedule = function(schedule, resources, project.teams, responsibilities){
  require(dplyr)
  tmp = responsibilities %>% gather("phase", "needed", -role) %>% 
    filter(needed == 1) %>% mutate(phase = factor(phase, levels(schedule$phase), ordered = T)) %>%
    select(-needed)
  schedule = left_join(schedule, tmp)
  schedule = left_join(schedule, project.teams)
  schedule = left_join(schedule, resources)
  schedule = get.padded.schedule(schedule)
  return(schedule)
}



adjust.schedule.for.leave = function(schedule, leave){
  require(padr)
  require(dplyr)
  tmp = leave %>% gather("date.type", "date", -c(leave.id, staff))
  tmp = pad(tmp, group=c('leave.id', 'staff')) %>% select(-date.type)
  schedule = left_join(schedule, tmp)
  tmp = schedule %>% group_by(project, phase, staff, role) %>% 
    summarise(total.days = n(), days.on.leave = sum(!is.na(leave.id)), 
              daily.allocated.work = mean(assigned_capacity)) %>% ungroup() %>% 
    mutate(leave_adjusted_assigned_capacity = 
             round((daily.allocated.work*total.days)/(total.days - days.on.leave), 2)) %>%
    select(project, phase, staff, role, leave_adjusted_assigned_capacity)
  schedule = left_join(schedule, tmp)
  pos = !is.na(schedule$leave.id)
  schedule$leave_adjusted_assigned_capacity[pos] = 0
  pos = !is.finite(schedule$leave_adjusted_assigned_capacity)
  if(sum(pos) > 0){
    issues = schedule %>% filter(pos) %>% select(project, phase, staff) %>% 
      distinct()
    for (i in 1:nrow(issues)){
      message = paste("This assignement is impossible due to leave:", 
                      paste(unlist(issues[i,]), collapse = ", "))
      sink(file = "log.log", append = T)
      print(message)
      sink()
    }
  }
  return(schedule)
}

#' Creates a randomised project management problem
#'
#' @param nothing
#'
#'
#'
#' @export
get.daily.plan = function(resources, projects, time.estimates, public.holidays, 
                             project.teams, responsibilities, leave){
  schedule = get.schedule(projects, time.estimates, public.holidays)
  schedule = get.assignment.schedule(schedule, resources, project.teams, responsibilities)
  schedule = adjust.schedule.for.leave(schedule, leave)
  return(schedule)
}

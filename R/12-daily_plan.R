#' create a daily list of assignments for each staff
#'
#' @param plan Complete workplan object
#' @return A reference table for daily projects schedule
#' @examples 
#' library(workplanr)
#' staff <- c("Shelby", "Luis", "Taishawn", "Samantha", "Taylor", "unassigned")
#' capacity <- c(40,60,100,100,100, 100) 
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27"))
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' roles <- c("lead", "researcher", "editor", "design")
#' staff_on_leave <- c("Luis", "Samantha")
#' leave_start <-  as.Date(c("2019-09-23", "2019-02-16"))'leave_end <- leave.start + c(10, 25)
#' leave_description <- c("leave", "work")
#' assigned_staff <- sample(staff, size = length(projects)*length(roles), replace = T)
#' assigned_capacity <- sample(c(25,50,75,100), size = length(projects)*length(roles), replace = T)
#' wp <- get_workplan(staff = staff, staff_capacity = capacity, projects = projects, project_probability =  probability, 
#'                    project_start = start, project_end = end, project_phases = phases, project_roles = roles, 
#'                    staff_on_leave = staff_on_leave, leave_start = leave_start, leave_end = leave_end, 
#'                    leave_description = leave_description, staff_project_assignments = assigned_staff, 
#'                    staff_project_assigned_capacity = assigned_capacity)
#' daily_plan <- get_daily_plan(wp)
#' plot_daily_staff_plan(daily_plan)
#' plot_team_daily_workload(daily_plan)
#' @export
get_daily_plan = function(wp){
  tmp = as.list(wp)
  schedule = .get_schedule(tmp)
  schedule = .get_assignment_schedule(schedule, tmp)
  return(schedule)
}

.get_schedule <- function(wp){
   cal <- bizdays::create.calendar('normal', weekdays = c('saturday', 'sunday'), 
                          start.date = min(wp$projects$end)-180, end.date = max(wp$projects$end) +180)
   times = dplyr::left_join(wp$projects, wp$time_estimates)
   end = times
 
   end[, ncol(end)] = bizdays::offset(end$end, end[,ncol(end)], 'normal')
   for (i in  rev(names(wp$time_estimates[, -c(1, ncol(wp$time_estimates))]))){
     end[,i] = bizdays::offset(end[,(which(names(end) == i)) +1], times[,i], 'normal')
   }
   start = times
   for (i in  names(wp$time_estimates[, -1])){
     start[,i] = bizdays::offset(end[,i], -times[,i], 'normal')
   }
 
   schedule = list(start = start, end = end)
   schedule = lapply(schedule, function(x) x %>% dplyr::select(-c(probability, start, end)) %>% 
                       tidyr::gather(phase, date, -c(project)) %>%
                       dplyr::mutate(phase = factor(phase, wp$phases$phase, ordered = T)))
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

.get_assignment_schedule = function(schedule, wp){
   tmp = wp$responsibilities %>% tidyr::gather('phase', 'needed', -role) %>% 
     dplyr::filter(needed == 1) %>%  dplyr::select(-needed) 
   tmp$phase = factor(tmp$phase, levels = levels(schedule$phase), ordered = T)
   schedule = dplyr::left_join(schedule, tmp)
   schedule = dplyr::left_join(schedule, wp$project_teams)
   schedule = dplyr::left_join(schedule, wp$resources)
   schedule = .get_pad_schedule(schedule)
   return(schedule)
}


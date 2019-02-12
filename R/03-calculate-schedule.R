
#' Initialise schedule table
#'
#' @param workplan The name of the database to create 
#' @keywords internal
calculate_start_and_end_dates = function(workplan){
  tmp <- as.data.frame(workplan@projects) %>%
    dplyr::left_join(as.data.frame(workplan@time_estimates)) %>%
    dplyr::filter(time_estimate != 0)
  
  cals <- bizdays::create.calendar('normal', 
                                   weekdays = c('saturday', 'sunday'), 
                                   holidays = workplan@public_holidays@date,
                                   start.date = min(tmp$project_start) - 2*max(abs(tmp$time_estimate)), 
                                   end.date = max(tmp$project_end) + 2*max(abs(tmp$time_estimate)))
  
  #post end activities
  pos <- tmp$time_estimate > 0
  if(sum(pos) > 0){ #if there are pre and post phases
    post_project_end_phases <- tmp[pos,]
    post_project_end_phases <- post_project_end_phases %>%
      dplyr::group_by(project_name) %>%
      dplyr::mutate(day_shift = match(project_phase_name, rev(workplan@project_phases@project_phase_name)) - 1,
                    time_from_project_end = cumsum(time_estimate) + day_shift,
                    time_from_phase_end = -time_estimate,
                    phase_end = bizdays::offset(project_end, time_from_project_end, 'normal'),
                    phase_start = bizdays::offset(phase_end, time_from_phase_end, 'normal')) %>%
      dplyr::ungroup()
    
    pre_project_end_phases <- tmp[!pos,]
    pre_project_end_phases <- pre_project_end_phases %>%
      dplyr::group_by(project_name) %>% 
      dplyr::arrange(project_name, dplyr::desc(project_phase_name)) %>%
      dplyr::mutate(day_shift = match(project_phase_name, rev(workplan@project_phases@project_phase_name)) - 1,
                    time_from_project_end = cumsum(time_estimate) - day_shift,
                    time_from_phase_end = -time_estimate,
                    phase_start = bizdays::offset(project_end, time_from_project_end, 'normal'),
                    phase_end = bizdays::offset(phase_start, time_from_phase_end, 'normal')) %>%
      dplyr::ungroup()
    tmp <- rbind(pre_project_end_phases, post_project_end_phases[,names(pre_project_end_phases)])
  }else{
    tmp <- tmp %>%
      dplyr::group_by(project_name) %>% 
      dplyr::arrange(project_name, dplyr::desc(project_phase_name)) %>%
      dplyr::mutate(time_from_project_end = cumsum(time_estimate),
                    time_from_phase_end = -time_estimate,
                    phase_start = bizdays::offset(project_end, time_from_project_end, 'normal'),
                    phase_end = bizdays::offset(phase_start, time_from_phase_end, 'normal')) %>%
      dplyr::ungroup()
  }
  
  
  

  #allow for extra time if numbers dont add up
  pos <- tmp$project_phase_name == workplan@project_phases@project_phase_name[1] &
    tmp$phase_start > tmp$project_start
  tmp$phase_start[pos] <-  tmp$project_start[pos]
  
  tmp <- tmp %>%
    dplyr::select(project_name, project_confirmed, project_phase_name, phase_start, phase_end) %>%
    tidyr::gather(date_type, date, -c(project_name, project_confirmed, project_phase_name))
  
  tmp = tmp %>%
    dplyr::group_by(project_name, project_confirmed, project_phase_name) %>%
    padr::pad() %>%
    dplyr::ungroup()  %>% 
    dplyr::mutate(date = as.Date(date))
  
  tmp <- tmp %>%
    dplyr::left_join(as.data.frame(workplan@public_holidays)) %>%
    dplyr::select(date, project_name, project_confirmed, project_phase_name, holiday_name) %>%
    dplyr::arrange(date)
  return(tmp)
}
#' Add staff assignments to schedulke
#'
#' @param schedule The name of the database to create 
#' @param workplan schedule
#' @keywords internal
add_project_assignments = function(schedule, workplan){
    schedule <- schedule %>%
    dplyr::left_join(rbind(as.data.frame(workplan@project_assignments), as.data.frame(workplan@project_unassignments))) %>%
    dplyr::filter(staff_contribution > 0) %>%
      dplyr::mutate(staff_name = as.character(staff_name)) %>%
      dplyr::left_join(as.data.frame(workplan@staff))
  return(schedule)
}
#' Add staff assignments to schedulke
#'
#' @param schedule The name of the database to create 
#' @param workplan schedule
#' @keywords internal
add_staff_out_of_office = function(schedule, workplan){
  tmp <- as.data.frame(workplan@out_of_office)
  tmp <- tmp %>% 
    tidyr::gather(date_type, date, -c(id_out_of_office, staff_name, work_related)) %>%
    dplyr::group_by(id_out_of_office, staff_name, work_related) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    padr::pad() %>% 
    dplyr::select(-date_type) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = as.character(date)) %>%
    dplyr::rename(out_of_office = work_related) %>%
    dplyr::mutate(out_of_office = ifelse(as.numeric(out_of_office) == 1, "Work", "Vacation"),
                  date = as.Date(date)) %>%
    dplyr::mutate(staff_name = factor(staff_name, levels = levels(schedule$staff_name), ordered = TRUE))
  schedule <- schedule %>%
    dplyr::full_join(tmp) 
  return(schedule)
}

#' Add staff assignments to schedulke
#'
#' @param workplan schedule
#' @keywords internal
get_schedule = function(workplan){
  schedule <- calculate_start_and_end_dates(workplan)
  schedule <- add_project_assignments(schedule, workplan)
  schedule <- workplanr_schedule(date = schedule$date,
                                 project_name = schedule$project_name,
                                 project_confirmed = schedule$project_confirmed,
                                 project_phase_name = schedule$project_phase_name,
                                 project_role_name = schedule$project_role_name,
                                 staff_name = schedule$staff_name,
                                 staff_capacity = schedule$staff_capacity,
                                 staff_contribution = schedule$staff_contribution,
                                 holiday_name = schedule$holiday_name)
  return(schedule)
}



#' Add staff assignments to schedulke
#'
#' @param workplan schedule
#' @keywords internal
get_staff_schedule = function(workplan){
  tmp <- as.data.frame(workplan@schedule)
  staff_schedule <- tmp %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::summarise(workload = sum(staff_contribution)/max(c(staff_capacity, max(workplan@staff@staff_capacity)), na.rm = T)) %>%
    dplyr::ungroup() 
  projects = tmp %>% 
    dplyr::filter(staff_contribution > 0) %>%
    dplyr::group_by(project_name, staff_name) %>%
    dplyr::filter(date == min(as.Date(date))) %>%
    dplyr::select(project_name, staff_name, date, staff_contribution) %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::distinct() %>%
    dplyr::summarise(project_name = paste(project_name, collapse = ", ")) %>%
    dplyr::ungroup() 
  staff_schedule <- staff_schedule %>%
    dplyr::left_join(projects)
  
  tmp <- tmp %>%
    dplyr::select(date, holiday_name) 
  staff_schedule <- staff_schedule %>%
    dplyr::left_join(tmp)
  staff_schedule$staff_name <- factor(staff_schedule$staff_name, 
                                      levels = c(rev(workplan@staff@staff_name), unique(workplan@project_unassignments@staff_name)),
                                      ordered = TRUE)
  staff_schedule <- add_staff_out_of_office(staff_schedule, workplan)
  staff_schedule <- staff_schedule %>%
    dplyr::distinct()
  staff_schedule <- workplanr_staff_schedule(date = as.Date(staff_schedule$date),
                                             staff_name = staff_schedule$staff_name,
                                             workload = as.numeric(staff_schedule$workload),
                                             project_name = as.character(staff_schedule$project_name),
                                             id_out_of_office = as.numeric(staff_schedule$id_out_of_office),
                                             out_of_office = as.character(staff_schedule$out_of_office),
                                             holiday_name = as.character(staff_schedule$holiday_name))

  return(staff_schedule)
}

#' Add staff assignments to schedulke
#'
#' @param workplan schedule
#' @keywords internal
get_team_schedule = function(workplan){
  tmp <- as.data.frame(workplan@schedule)
  team_capacity <- tmp %>% 
    dplyr::select(staff_name, staff_capacity) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  team_capacity <- max(c(sum(team_capacity$staff_capacity, na.rm = T), workplan@staff@staff_capacity))
  tmp <- tmp %>% 
    dplyr::group_by(date, project_confirmed) %>% 
    dplyr::summarise(staff_contribution = sum(staff_contribution, na.rm=TRUE),
                     workload = round(staff_contribution/team_capacity,2)) %>%
    dplyr::select(-staff_contribution) %>%
    dplyr::ungroup() 
  tmp <- workplanr_team_schedule(date = as.Date(tmp$date),
                                 project_confirmed = as.logical(tmp$project_confirmed),
                                 workload = as.numeric(tmp$workload))
  return(tmp)
}
#' Add staff assignments to schedulke
#'
#' @param workplan schedule
#' @keywords internal
get_release_schedule = function(workplan){
  tmp <- as.data.frame(workplan@schedule) %>%
    dplyr::select(date, project_name, project_phase_name) %>%
    dplyr::distinct() %>%
    dplyr::mutate(days_until = lubridate::today() - date) %>%
    dplyr::group_by(project_name, project_phase_name) %>%
    dplyr::summarise(start_date = min(date), end_date = max(date)) %>%
    dplyr::arrange(end_date)%>%
    dplyr::ungroup() %>%
    dplyr::mutate(project_name = factor(project_name, rev(unique(project_name)), ordered = T))
  tmp <- workplanr_release_schedule(project_name = tmp$project_name,
                                    project_phase_name = tmp$project_phase_name,
                                    start_date = tmp$start_date,
                                    end_date = tmp$end_date)
  return(tmp)
}

#' Add staff assignments to schedulke
#'
#' @param workplan schedule
#' @keywords internal
get_project_dependencies = function(workplan){
  # testthat::expect(length(unique(workplan@schedule@staff_name))>1,
  #                  message = "Can't currently calulate inter-project-dependencies for only one staff member")
  if(length(unique(workplan@schedule@staff_name))>1){
    tmp <- as.data.frame(workplan@schedule)
    tmp <- tmp %>% 
      dplyr::select(date, project_name, staff_name) %>%
      dplyr::filter(staff_name != unique(workplan@project_unassignments@staff_name)) %>%
      dplyr::group_by(project_name) %>%
      dplyr::mutate(staff_days_assigned_project_a = n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(staff_month = paste(staff_name, lubridate::month(date))) %>%
      dplyr::select(-c(staff_name, date)) %>%
      dplyr::rename(project_a = project_name)
    tmp2 <- tmp %>%
      dplyr::select(-staff_days_assigned_project_a) %>%
      dplyr::rename(project_b = project_a) 
    tmp <- tmp %>%
      dplyr::left_join(tmp2) %>%
      dplyr::filter(project_a != project_b) %>%
      dplyr::group_by(project_a, project_b, 
                      staff_days_assigned_project_a) %>%
      dplyr::summarise(dependence_level = n())
    
    tmp <- workplanr_project_dependencies(project_a = tmp$project_a,
                                          project_b = tmp$project_b,
                                          staff_days_assigned_project_a = tmp$staff_days_assigned_project_a,
                                          dependence_level = tmp$dependence_level)
  }else{
    tmp <- new("project_dependencies")
  }
  
  
  return(tmp)
}
#' Add staff assignments to schedulke
#'
#' @param workplan schedule
#' @keywords internal
get_project_teams = function(workplan){
  tmp <- as.data.frame(my_workplan@schedule)
  tmp <- tmp %>% 
    dplyr::filter(staff_contribution > 0) %>%
    dplyr::select(project_name, project_role_name, staff_name) %>%
    dplyr::distinct() %>%
    dplyr::arrange(project_name, project_role_name)
  
  tmp <- split(tmp, tmp$project_name)
  
  tmp <- lapply(tmp, function(x) {
    x <- apply(x, 2, proper_capitalise)
    i <- paste("Project", unique(x$project_name))
    j <- x$project_role_name[1]
    k <- paste(x$staff_name[x$project_role_name == j], collapse = ", ")
    l <- paste(j, k, sep = ": ")
    i <- paste(i, l, sep = "\n")
    project <- data.tree::Node$new(i)
    x <- x %>%
      dplyr::filter(project_role_name != j)
    
    for (i in unique(x$project_role_name)){
      role <- project$AddChild(i)
      for (j in unique(x$staff_name[x$project_role_name == i]))
        staff <- role$AddChild(j)
    }
    data.tree::SetNodeStyle(project,  shape = "box")  
    return(project)
  }
  )
  tmp <- workplanr_get_project_teams(project_teams = tmp)
  return(tmp)
}
#' Add staff assignments to schedulke
#'
#' @param workplan schedule
#' @keywords internal
calculate_workplan = function(workplan){
  workplan@schedule <- get_schedule(workplan)
  workplan@release_schedule <- get_release_schedule(workplan)
  workplan@staff_schedule <- get_staff_schedule(workplan)
  workplan@team_schedule <- get_team_schedule(workplan)
  workplan@project_dependencies <- get_project_dependencies(workplan)
 # workplan@project_team <- get_project_teams(workplan)
  return(workplan)
}


#' Base Class for workplan resources
#' @slot staff Names of staff members
#' @slot capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @family classes
#' @keywords internal
resc <- setClass("resources", slots = c(staff="character", capacity = "numeric"))

#' Coerce Object resource to a data frame
#'
#' @description Coerce Object resource to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{resource} object.
#' @keywords internal
setMethod("as.data.frame", "resources", definition = function(x){
  x <- data.frame(staff = x@staff, capacity = x@capacity)
  return(x)
  })

#' Base Class for workplan projects
#' @slot projects Names of projects
#' @slot probability Probability that the project will go ahead
#' @slot start Expected start date of project
#' @slot end Expected end date of the project
#' @family classes
#' @keywords internal
proj <- setClass("projects", slots = c(project="ordered", probability="numeric", start = "Date", end = "Date"))

#' Coerce Object project to a data frame
#'
#' @description Coerce Object project to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{projects} object.
#' @keywords internal
setMethod("as.data.frame", "projects", definition = function(x){
  x <- data.frame(project = x@project, probability = x@probability, start = x@start, end = x@end)
  return(x)
})

#' Base Class for workplan phases
#'
#' @slot phases List of phases in any project in order of execution
#' @family classes
#' @keywords internal
phas <- setClass("phases", slots = c(phase="ordered"))

#' Coerce Object phases to a data frame
#'
#' @description Coerce Object phases to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{phases} object.
#' @keywords internal
setMethod("as.data.frame", "phases", definition = function(x){
  x <- data.frame(phase = x@phase)
  return(x)
})

#' Base Class for workplan leave
#' 
#' @slot staff_on_leave Names of staff that are going to be out of the office
#' @slot leave_start Starting date for leave
#' @slot leave_end Ending date for leave
#' @slot leave_description Type of leave, can be user defined but recommend "leave" or "work trip"
#' @family classes
#' @keywords internal
leav <- setClass("leave", slots = c(staff="character", start = "Date", 
                                    end = "Date", description = "character"))

#' Coerce Object leave to a data frame
#'
#' @description Coerce Object leave to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{leave} object.
#' @keywords internal
setMethod("as.data.frame", "leave", definition = function(x){
  x <- data.frame(staff = x@staff, start = x@start, end = x@end, description = x@description)
  return(x)
})

#' Base Class for workplan public_holidays
#' 
#' @slot date Date of public holiday
#' @slot name Name of public holiday
#' @family classes
#' @keywords internal
holi <- setClass("public_holidays", slots = c(date="Date", name = "character"))

#' Coerce Object public_holidays to a data frame
#'
#' @description Coerce Object public_holidays to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{public_holidays} object.
#' @keywords internal
#' @keywords internal
setMethod("as.data.frame", "public_holidays", definition = function(x){
  x <- data.frame(date = x@date, name = x@name)
  return(x)
})

#' Base Class for workplan time_estimates
#'
#' @slot projects List of roles in any project team in order of responsibility
#' @slot phases List of roles in any project team in order of responsibility
#' @slot time_estimates Time estimates of how long each phase will take in relation 
#' to project end, negative = phase will occur before project end, positive = phase will occur after project end
#' @family classes
#' @keywords internal
time <- setClass("time_estimates", slots = c(project = "ordered", phase = "ordered", time_estimate = "numeric"))

#' Coerce Object time_estimates to a data frame
#'
#' @description Coerce Object time_estimates to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{time_estimates} object.
#' @keywords internal
setMethod("as.data.frame", "time_estimates", definition = function(x){
  x <- data.frame(project = x@project, phase = x@phase, time_estimate = x@time_estimate)
  return(x)
})

#' Base Class for workplan project_teams
#'
#' @slot projects List of projects
#' @slot phases List of phases in any project in order of execution
#' @slot staff Names of staff members
#' @slot assigned_capacity Amount of time each staff is expected to dedicate to each [project, phase] 
#' @family classes
#' @keywords internal
team <- setClass("project_teams", slots = c(project = "ordered", phase = "ordered", 
                                            staff = "character", assigned_capacity = "numeric"))

#' Coerce Object project_teams to a data frame
#'
#' @description Coerce Object project_teams to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{project_teams} object.
#' @keywords internal
setMethod("as.data.frame", "project_teams", definition = function(x){
  x <- data.frame(project = x@project, phase = x@phase, staff = x@staff, assigned_capacity = x@assigned_capacity)
  return(x)
})

#' Base Class for workplan full_schedule
#'
#' @slot projects List of projects
#' @slot phases List of phases in any project in order of execution
#' @slot date Day wourk occurs on
#' @slot staff Assigned staff to each [project, phase] combination 
#' @slot assigned_capacity Amount of time each staff is expected to dedicate to each [project, phase] 
#' @slot capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @slot public_holiday = "character"
#' @slot out_of_office = "character" 
#' @slot project_duration = "numeric"
#' @slot num_holidays = "numeric" 
#' @slot holiday_expansion_factor = "numeric"
#' @slot num_out_of_office = "numeric" 
#' @slot leave_expansion_factor = "numeric"
#' @slot leave_adjusted_workload = "numeric"
#' @family classes
#' @keywords internal
full_sched <- setClass("full_schedule", slots = c(date = "Date", project = "ordered", phase = "ordered", 
                                                  staff = "character", assigned_capacity = "numeric",
                                                  capacity = "numeric", public_holiday = "character",
                                                  out_of_office = "character", project_duration = "numeric",
                                                  num_holidays = "numeric", holiday_expansion_factor = "numeric",
                                                  num_out_of_office = "numeric", leave_expansion_factor = "numeric",
                                                  leave_adjusted_workload = "numeric"))

#' Coerce Object full_schedule to a data frame
#'
#' @description Coerce Object full_schedule to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{full_schedule} object.
#' @keywords internal
setMethod("as.data.frame", "full_schedule", definition = function(x){
  x <- data.frame(date = x@date, project = x@project, phase = x@phase,
                  staff = x@staff, assigned_capacity = x@assigned_capacity, capacity = x@capacity,
                  public_holiday = x@public_holiday, out_of_office = x@out_of_office, project_duration = x@project_duration,
                  num_holidays = x@num_holidays, holiday_expansion_factor = x@holiday_expansion_factor,
                  num_out_of_office = x@num_out_of_office, 
                  leave_expansion_factor = x@leave_expansion_factor,
                  leave_adjusted_workload = x@leave_adjusted_workload)
  return(x)
})


#' Base Class for workplan staff_schedule
#'
#' @slot date Day work occurs on
#' @slot staff Assigned staff to each [project, role] combination 
#' @slot projects Names of projects
#' @slot workload Amount of work assigned to staff member 
#' @slot out_of_office Whether a staff member is out of office on a date
#' @slot public_holiday Whether a day is a public holiday
#' @family classes
#' @keywords internal
staff_sched <- setClass("staff_schedule", slots = c(date = "Date", staff = "character", project = "character", workload= "numeric",
                                                    out_of_office = "character", 
                                                    public_holiday = "character"))

#' Coerce Object staff_schedule to a data frame
#'
#' @description Coerce Object staff_schedule to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{staff_schedule} object.
#' @keywords internal
setMethod("as.data.frame", "staff_schedule", definition = function(x){
  x <- data.frame(date = x@date, staff = x@staff, project = x@project, workload = x@workload, 
                  out_of_office = x@out_of_office, public_holiday = x@public_holiday)
  return(x)
})

#' Coerce Object staff_schedule to a ggplot
#'
#' @description Coerce Object full_schedule to ggplot, avoiding using the "slot" notation.
#'
#' @examples 
#' library(workplanr)
#' wp <- build_sample_workplan()
#' plot(wp@staff_schedule)
#' @param x A \code{staff_schedule} object.
setMethod("plot", "staff_schedule", definition = function(x){
  x = as.data.frame(x)
  myPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 'RdGy')[c(6,2)], 
                                           space='Lab')
  p <- ggplot2::ggplot(x, ggplot2::aes(date, staff, fill = workload)) +
    ggplot2::geom_tile(alpha = 0.9) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                          date_breaks = '1 month', 
                          expand = c(0,0)) +
    ggplot2::scale_fill_gradientn(colors = myPalette(100), 
                                  labels = scales::percent, name = 'Workload', na.value = "white") +
    ggplot2::labs(x='', y = '', 
                  title = toupper('STAFF WORKLOAD')) 
  
  p <- p + ggrepel::geom_text_repel(ggplot2::aes(x = date, y = staff, label = project), 
                                    size = 3, hjust = 1, force = 2.5)
  
  #add leave
  
  leave <- x %>% 
    dplyr::group_by(staff, out_of_office, workload) %>%
    dplyr::summarise(start = min(date), end = max(date)) %>%
    dplyr::filter(!is.na(out_of_office))
  
  p <- p + ggplot2::geom_segment(data = leave, ggplot2::aes(x=start, 
                                          xend=end, 
                                          y=staff, 
                                          yend=staff, colour = out_of_office), size=2, alpha = 0.6)
  p <- p + ggplot2::geom_point(data = leave, ggplot2::aes(x=start, 
                                        y=staff, colour = out_of_office), size=3)
  p <- p + ggplot2::geom_point(data = leave, ggplot2::aes(x=end, 
                                        y=staff, colour = out_of_office), size=3)
  public_holidays <- x %>% 
    dplyr::filter(!is.na(public_holiday))
  p <- p + ggplot2::geom_vline(xintercept = public_holidays$date, 
                      linetype = "dashed", colour = grey(0.5), alpha = 0.6)
  return(p)
})

#' Base Class for workplan team_schedule
#'
#' @slot date Day wourk occurs on
#' @slot workload Amount of work assigned to staff member 
#' @family classes
#' @keywords internal
team_sched <- setClass("team_schedule", slots = c(date = "Date", workload= "numeric"))

#' Coerce Object team_schedule to a data frame
#'
#' @description Coerce Object team_schedule to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{team_schedule} object.
#' @keywords internal
setMethod("as.data.frame", "team_schedule", definition = function(x){
  x <- data.frame(date = x@date, workload = x@workload)
  return(x)
})


#' Coerce Object team_schedule to a ggplot
#'
#' @description Coerce Object team_schedule to ggplot, avoiding using the "slot" notation.
#'
#' @examples 
#' library(workplanr)
#' wp <- build_sample_workplan()
#' plot(wp@staff_schedule)
#' @param x A \code{team_schedule} object.
setMethod("plot", "team_schedule", definition = function(x){
  x <- as.data.frame(x)
  x$maxcapacity <- ifelse(x$workload > 1, 1, x$workload)
  x$teamload <- ifelse(x$workload > 1, "Overloaded", "Covered")
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  
  # base layer
  p <- ggplot2::ggplot(x, ggplot2::aes(date, workload, fill = teamload)) +
    ggplot2::geom_bar(stat = 'identity',  show.legend = T) +
    ggplot2::scale_fill_manual(values = c(gg_blue, gg_red)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), date_breaks = '1 month',
                          expand = c(0,0)) +
    ggplot2::labs(x='', y = 'TEAM WORKLOAD', title = 'TEAM WORKLOAD')
  
  
  p <- p + ggplot2::geom_bar(data = x, 
                             ggplot2::aes(date, maxcapacity),
                             stat = 'identity', fill = gg_blue)
  
  return(p)
})




#' Base Class for workplan workplan
#'
#' @slot resources Object of class "resource"
#' @slot projects Object of class "projects" 
#' @slot phases Object of class "phases",
#' @slot leave Object of class  "leave",
#' @slot holidays Object of class  "public_holidays",
#' @slot time_estimates Object of class  "time_estimates",
#' @slot project_teams Object of class  "project_teams"
#' @slot full_schedule Object of class "full_schedule"
#' @slot staff_schedule Object of class "staff_schedule"
#' @slot team_schedule Object of class "team_schedule"
#' @family classes
#' @keywords internal
workplan <- setClass("workplan", slots =  list(resources = "resources", 
                                               projects = "projects", 
                                               phases = "phases",
                                               leave = "leave",
                                               holidays = "public_holidays",
                                               time_estimates = "time_estimates",
                                               project_teams = "project_teams",
                                               full_schedule = "full_schedule",
                                               staff_schedule = "staff_schedule",
                                               team_schedule = "team_schedule"))

#' Coerce Object workplan to a list of data frames
#'
#' @description Coerce Object workplan to a list of data frames, avoiding using the "slot" notation.
#'
#' @param x A \code{workplan} object.
#' @keywords internal
setMethod("as.list", "workplan", definition = function(x){
  x = list(
    resources = as.data.frame(x@resources),
    projects = as.data.frame(x@projects),
    phases = as.data.frame(x@phases),
    leave = as.data.frame(x@leave),
    holidays = as.data.frame(x@holidays),
    time_estimates = as.data.frame(x@time_estimates),
    project_teams = as.data.frame(x@project_teams),
    full_schedule = as.data.frame(x@full_schedule),
    staff_schedule = as.data.frame(x@staff_schedule),
    team_schedule = as.data.frame(x@team_schedule))
  return(x)
})


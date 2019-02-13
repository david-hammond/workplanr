#' Base Class for workplan resources
#' @slot staff_name Names of staff members
#' @slot staff_capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @family classes
#' @keywords internal
workplanr_staff <- setClass("staff", slots = c(staff_name = "character", 
                                    staff_capacity = "numeric"))

#' Coerce Object resource to a data frame
#'
#' @description Coerce Object resource to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{resource} object.
#' @keywords internal
setMethod("as.data.frame", "staff", definition = function(x){
  x <- data.frame(staff_name = x@staff_name, staff_capacity = x@staff_capacity, 
                  stringsAsFactors = FALSE)
  return(x)
})

#' Base Class for workplan projects
#' @slot project_name Names of projects
#' @slot project_confirmed Probability that the project will go ahead
#' @slot project_start Expected start date of project
#' @slot project_end Expected end date of the project
#' @family classes
#' @keywords internal
workplanr_projects <- setClass("projects", slots = c(project_name = "ordered", project_confirmed = "logical", 
                                       project_start = "Date", project_end = "Date"))

#' Coerce Object project to a data frame
#'
#' @description Coerce Object project to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{projects} object.
#' @keywords internal
setMethod("as.data.frame", "projects", definition = function(x){
  x <- data.frame(project_name = x@project_name, project_confirmed = x@project_confirmed, 
                  project_start = x@project_start, project_end = x@project_end)
  return(x)
})

#' Base Class for workplan phases
#'
#' @slot project_phase_name List of phases in any project in order of execution
#' @family classes
#' @keywords internal
workplanr_project_phases <- setClass("project_phases", slots = c(project_phase_name = "ordered"))

#' Coerce Object phases to a data frame
#'
#' @description Coerce Object phases to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{phases} object.
#' @keywords internal
setMethod("as.data.frame", "project_phases", definition = function(x){
  x <- data.frame(project_phase_name = x@project_phase_name)
  return(x)
})

#' Base Class for workplan roles
#'
#' @slot project_role_name List of roles in any project team in order of responsibility
#' @family classes
#' @keywords internal
workplanr_project_roles <- setClass("project_roles", slots = c(project_role_name ="ordered"))

#' Coerce Object roles to a data frame
#'
#' @description Coerce Object roles to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{role} object.
#' @keywords internal
setMethod("as.data.frame", "project_roles", definition = function(x){
  x <- data.frame(project_role_name = x@project_role_name)
  return(x)
})

#' Base Class for workplan roles
#' 
#' @slot id_out_of_office Names of staff that are going to be out of the office
#' @slot staff_name Names of staff that are going to be out of the office
#' @slot out_of_office_start Starting date for leave
#' @slot out_of_office_end Ending date for leave
#' @slot work_related Type of leave, can be user defined but recommend "leave" or "work trip"
#' @family classes
#' @keywords internal
workplanr_out_of_office <- setClass("out_of_office", slots = c(id_out_of_office = "numeric", staff_name = "character", 
                                            out_of_office_start = "Date", 
                                            out_of_office_end = "Date", work_related = "logical"))

#' Coerce Object leave to a data frame
#'
#' @description Coerce Object leave to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{leave} object.
#' @keywords internal
setMethod("as.data.frame", "out_of_office", definition = function(x){
  x <- data.frame(id_out_of_office = x@id_out_of_office, staff_name = x@staff_name, 
                  out_of_office_start = x@out_of_office_start, 
                  out_of_office_end = x@out_of_office_end, work_related = x@work_related,
                  stringsAsFactors = FALSE)
  return(x)
})

#' Base Class for workplan roles
#' 
#' @slot date Date of public holiday
#' @slot name Name of public holiday
#' @family classes
#' @keywords internal
workplanr_holidays <- setClass("public_holidays", slots = c(date="Date", holiday_name = "character"))

#' Coerce Object public_holidays to a data frame
#'
#' @description Coerce Object public_holidays to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{public_holidays} object.
#' @keywords internal
setMethod("as.data.frame", "public_holidays", definition = function(x){
  x <- data.frame(date = x@date, holiday_name = x@holiday_name,
                  stringsAsFactors = FALSE)
  return(x)
})

#' Base Class for workplan responsibilites
#'
#' @slot project_role_name List of roles in any project team in order of responsibility
#' @slot project_phase_name List of roles in any project team in order of responsibility
#' @slot responsibility_span a binary matrix of if role i is in volved in phase j
#' @family classes
#' @keywords internal
workplanr_roles_responsibilities <- setClass("roles_responsibilities", slots = c(project_role_name ="ordered", 
                                                     project_phase_name ="ordered", 
                                                     responsibility_span ="logical"))

#' Coerce Object responsibilities to a data frame
#'
#' @description Coerce Object responsibilities to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{roles_responsibilities} object.
#' @keywords internal
setMethod("as.data.frame", "roles_responsibilities", definition = function(x){
  x <- data.frame(project_role_name = x@project_role_name, 
                  project_phase_name = x@project_phase_name, 
                  responsibility_span = x@responsibility_span)
  return(x)
})

#' Base Class for workplan time_estimates
#'
#' @slot project_name List of roles in any project team in order of responsibility
#' @slot project_phase_name List of roles in any project team in order of responsibility
#' @slot time_estimates Time estimates of how long each phase will take in relation 
#' to project end, negative = phase will occur before project end, positive = phase will occur after project end
#' @family classes
#' @keywords internal
workplanr_time_estimates <- setClass("time_estimates", slots = c(project_name = "ordered", 
                                             project_phase_name = "ordered", 
                                             time_estimate = "numeric"))

#' Coerce Object time_estimates to a data frame
#'
#' @description Coerce Object time_estimates to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{time_estimates} object.
#' @keywords internal
setMethod("as.data.frame", "time_estimates", definition = function(x){
  x <- data.frame(project_name = x@project_name, 
                  project_phase_name = x@project_phase_name, 
                  time_estimate = x@time_estimate)
  return(x)
})

#' Base Class for workplan project_assignments
#'
#' @slot project_name List of projects
#' @slot project_role_name Roles of projects
#' @slot project_phase_name Phases
#' @slot staff_name Assigned staff to each [project, role] combination (needs to be at least length(project) x length(roles) in length)
#' @slot staff_contribution Amount of time each staff is expected to dedicate to each [project, role] 
#' combination (needs to be at least length(project) x length(roles) in length)
#' @family classes
#' @keywords internal
workplanr_project_assignments <- setClass("project_assignments", slots = c(project_name = "ordered", project_role_name = "ordered", 
                                                                           project_phase_name = "ordered", staff_name = "character", 
                                                                           staff_contribution = "numeric"))

#' Coerce Object project_teams to a data frame
#'
#' @description Coerce Object project_teams to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{project_teams} object.
#' @keywords internal
setMethod("as.data.frame", "project_assignments", definition = function(x){
  x <- data.frame(project_name = x@project_name, project_role_name = x@project_role_name, 
                  project_phase_name = x@project_phase_name, staff_name = x@staff_name, staff_contribution = x@staff_contribution)
  return(x)
})

#' Base Class for schedule project_assignments
#'
#' @slot date List of projects
#' @slot project_name List of projects
#' @slot project_confirmed project confirmed
#' @slot project_phase_name Phases
#' @slot project_role_name Roles of projects
#' @slot staff_name Names of staff
#' @slot staff_capacity Capacity of staff
#' @slot staff_contribution Contibution of styaff to project
#' @slot holiday_name names of public holidays
#' combination (needs to be at least length(project) x length(roles) in length)
#' @family classes
#' @keywords internal
workplanr_schedule <- setClass("schedule", slots = c(date = "Date",
                                                     project_name = "ordered", 
                                                     project_confirmed = "logical",
                                                     project_phase_name = "ordered",
                                                     project_role_name = "ordered",
                                                     staff_name = "character",
                                                     staff_capacity = "numeric",
                                                     staff_contribution = "numeric",
                                                     holiday_name = "character"))

#' Coerce Object project_teams to a data frame
#'
#' @description Coerce Object project_teams to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{project_teams} object.
#' @keywords internal
setMethod("as.data.frame", "schedule", definition = function(x){
  x <- data.frame(date = x@date, project_name = x@project_name, project_confirmed = x@project_confirmed, 
                  project_phase_name = x@project_phase_name, project_role_name = x@project_role_name, 
                  staff_name = x@staff_name, staff_capacity = x@staff_capacity, 
                  staff_contribution = x@staff_contribution, holiday_name = x@holiday_name)
  return(x)
})

#' Base Class for schedule project_assignments
#'
#' @slot project_name List of projects
#' @slot project_phase_name Names of phases
#' @slot start_date Start date of phase
#' @slot end_date end date of Phase
#' @family classes
#' @keywords internal
workplanr_release_schedule <- setClass("release_schedule", slots = c(project_name = "ordered",
                                                                   project_phase_name = "ordered",
                                                                   start_date = "Date",
                                                                   end_date = "Date"))

#' Coerce Object project_teams to a data frame
#'
#' @description Coerce Object project_teams to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{project_teams} object.
#' @keywords internal
setMethod("as.data.frame", "release_schedule", definition = function(x){
  x <- data.frame(project_name = x@project_name, project_phase_name = x@project_phase_name,
                  start_date = x@start_date, end_date = x@end_date)
  return(x)
})

#' Base Class for schedule project_assignments
#'
#' @slot date List of projects
#' @slot staff_name List of staff
#' @slot workload Workload
#' @slot project_name Project names
#' @slot id_out_of_office id out of office
#' @slot holiday_name public holidays
#' combination (needs to be at least length(project) x length(roles) in length)
#' @family classes
#' @keywords internal
workplanr_staff_schedule <- setClass("staff_schedule", slots = c(date = "Date",
                                                     staff_name = "ordered",
                                                     workload = "numeric",
                                                     project_name = "character", 
                                                     id_out_of_office = "numeric",
                                                     out_of_office = "character",
                                                     holiday_name = "character"))

#' Coerce Object project_teams to a data frame
#'
#' @description Coerce Object project_teams to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{project_teams} object.
#' @keywords internal
setMethod("as.data.frame", "staff_schedule", definition = function(x){
  x <- data.frame(date = x@date, staff_name = x@staff_name, workload = x@workload,
                  project_name = x@project_name, id_out_of_office = x@id_out_of_office,
                  out_of_office = x@out_of_office,
                  holiday_name = x@holiday_name)
  return(x)
})

#' Base Class for schedule project_assignments
#'
#' @slot date List of projects
#' @slot project_confirmed List of projects
#' @slot workload workload
#' @family classes
#' @keywords internal
workplanr_team_schedule <- setClass("team_schedule", slots = c(date = "Date",
                                                                 project_confirmed = "logical",
                                                                 workload = "numeric"))

#' Coerce Object project_teams to a data frame
#'
#' @description Coerce Object project_teams to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{project_teams} object.
#' @keywords internal
setMethod("as.data.frame", "team_schedule", definition = function(x){
  x <- data.frame(date = x@date, project_confirmed = x@project_confirmed, 
                  workload = x@workload)
  return(x)
})

#' Base Class for project_dependencies
#'
#' @slot project_a List of projects
#' @slot project_b List of projects
#' @slot staff_assigned_to_project_a assignements
#' @slot dependence_level dependence
#' @family classes
#' @keywords internal
workplanr_project_dependencies <- setClass("project_dependencies", slots = c(project_a = "ordered",
                                                               project_b = "ordered",
                                                               staff_days_assigned_project_a = "numeric",
                                                               staff_days_assigned_project_b = "numeric",
                                                               dependence_level = "numeric"))

#' Coerce Object project_teams to a data frame
#'
#' @description Coerce Object project_teams to a data frame, avoiding using the "slot" notation.
#'
#' @param x A \code{project_teams} object.
#' @keywords internal
setMethod("as.data.frame", "project_dependencies", definition = function(x){
  x <- data.frame(project_a = x@project_a, project_b = x@project_b, 
                  staff_days_assigned_project_a = x@staff_days_assigned_project_a,
                  dependence_level = x@dependence_level)
  return(x)
})

#' Base Class for workplan workplan
#'
#' @slot resources Object of class "resource"
#' @slot projects Object of class "projects"
#' @slot phases Object of class "phases",
#' @slot roles Object of class  "roles",
#' @slot leave Object of class  "leave",
#' @slot holidays Object of class  "public_holidays",
#' @slot responsibilities Object of class  "responsibilities",
#' @slot time_estimates Object of class  "time_estimates",
#' @slot project_teams Object of class  "project_teams"
#' @slot full_schedule Object of class "full_schedule"
#' @slot staff_schedule Object of class "staff_schedule"
#' @slot team_schedule Object of class "team_schedule"
#' @slot project_dependencies Object of class "project_dependencies
#' @family classes
workplanr_class <- setClass("workplan", slots =  list(staff = "staff",
                                               projects = "projects",
                                               project_phases = "project_phases",
                                               project_roles = "project_roles",
                                               out_of_office = "out_of_office",
                                               public_holidays = "public_holidays",
                                               roles_responsibilities = "roles_responsibilities",
                                               time_estimates = "time_estimates",
                                               project_assignments = "project_assignments",
                                               project_unassignments = "project_assignments",
                                               schedule = "schedule",
                                               release_schedule = "release_schedule",
                                               staff_schedule = "staff_schedule",
                                               team_schedule = "team_schedule",
                                               project_dependencies = "project_dependencies"))


#' Coerce Object staff_schedule to a ggplot
#'
#' @description Coerce Object full_schedule to ggplot, avoiding using the "slot" notation.
#'
#' @param x A \code{staff_schedule} object.
#' @export
setMethod("plot", "staff_schedule", definition = function(x){
  tmp <- as.data.frame(x)
  tmp <- tmp %>%
    dplyr::filter(date <= max(date[!is.na(workload)]), 
                  !is.na(workload))
  #TODO: make function calc beaks
  bins <- c(0,0.25, 0.5, 0.75, 1, 1.25, 2, max(tmp$workload))
  labels <- c("[0 - 25%]", "[25% - 50%]", "[50% - 75%]", "[75% - 100%]", "[100% - 125%]",
                       "[125% - 200%]", paste0("[200% - ", round(100*max(tmp$workload),0), "%]"))
  work_classes <- classInt::classIntervals(tmp$workload, n = length(bins)-1, style = "fixed",
                                fixedBreaks = bins)
  tmp$workload = cut(tmp$workload, bins, labels = labels)
  tmp$workload = forcats::fct_rev(tmp$workload)
  main_title <- paste0("Planned Staff Workload - ", format(lubridate::today(), "%d %B %Y"))
 # labels = scales::percent, 
  p <- ggplot2::ggplot(data = tmp, 
                       ggplot2::aes(date, staff_name, fill = workload)) +
    ggplot2::geom_tile(alpha = 0.6) +
    ggplot2::scale_fill_manual(values = 
                                 rev(RColorBrewer::brewer.pal(n = max(length(unique(tmp$workload)),3), 
                                                              "Reds")), 
                                  name = 'Workload', na.value = "white") +
    ggplot2::labs(x='', y = '', title = main_title) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                          date_breaks = '1 month', 
                         expand = c(0,0))
  project_labels <- tmp %>% 
    dplyr::select(date, staff_name, project_name, workload) %>%
    dplyr::distinct() %>% 
    dplyr::filter(!is.na(project_name))
  p <- p + ggrepel::geom_text_repel(data = project_labels, 
                                    ggplot2::aes(x = date, y = staff_name, label = project_name), 
                                    size = 3, hjust = 1)   +
    ggplot2::geom_vline(xintercept = lubridate::today(), colour = "red", linetype = "dashed")

  #add leave

  leave <- tmp %>%
    dplyr::group_by(staff_name, id_out_of_office, out_of_office) %>%
    dplyr::summarise(start = min(date), end = max(date), 
                     workload = tmp$workload[1]) %>%
    dplyr::filter(!is.na(out_of_office))

  p <- p + ggplot2::geom_segment(data = leave, ggplot2::aes(x=start, 
                                                                xend=end, 
                                                                y=staff_name, 
                                                                yend=staff_name, colour = out_of_office), size=2, alpha = 0.6)
  p <- p + ggplot2::geom_point(data = leave, ggplot2::aes(x=start, 
                                                              y=staff_name, colour = out_of_office), size=3,
                               show.legend = FALSE)
  p <- p + ggplot2::geom_point(data = leave, ggplot2::aes(x=end, 
                                                              y=staff_name, colour = out_of_office), size=3,
                               show.legend = FALSE)
  p <- p + ggplot2::labs(fill ="Workload" ,colour="Out of Office")
  public_holidays <- tmp %>% 
    dplyr::filter(!is.na(holiday_name)) 
  p <- p + ggplot2::geom_vline(xintercept = public_holidays$date,
                               linetype = "dashed", colour = grey(0.5), alpha = 0.6) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"))
  return(p)
})

#' Coerce Object team_schedule to a ggplot
#'
#' @description Coerce Object team_schedule to ggplot, avoiding using the "slot" notation.
#'
#' @param x A \code{team_schedule} object.
#' @export
setMethod("plot", "team_schedule", definition = function(x){
  tmp <- as.data.frame(x)
  pos <- tmp$workload == 0
  tmp$project_confirmed[pos] <- TRUE
  tmp <- tmp %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(planned_work = sum(workload[project_confirmed]),
                  planned_deficit = ifelse(planned_work > 1, planned_work - 1, 0),
                  potential_work = sum(workload[!project_confirmed]),
                  potential_deficit = ifelse(potential_work  > 1, potential_work - 1, 0),
                  total_potential = potential_work +  potential_deficit) %>%
    dplyr::ungroup()
  pos <- tmp$planned_work > 1
  tmp$planned_work[pos] <- 1
  tmp$potential_deficit[pos] = tmp$potential_deficit[pos] + tmp$potential_work[pos]
  tmp$potential_work[pos] = 0

  pos <- tmp$planned_deficit == 0 & (tmp$planned_work + tmp$total_potential) > 1
  tmp$potential_work[pos] <- 1 - tmp$planned_work[pos] 
  tmp$potential_deficit[pos] <- tmp$total_potential[pos] - tmp$potential_work[pos]
  
  tmp <- tmp %>% 
    dplyr::select(-c(project_confirmed, workload, total_potential)) %>% 
    tidyr::gather(group, value, -date) %>%
    dplyr::distinct()
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  cols = c(scales::alpha(gg_red, 0.5),   scales::alpha(gg_blue, 0.5),gg_red, gg_blue)
  tmp$group <- gsub("_", " ", tmp$group)
  tmp$group <- proper_capitalise(tolower(tmp$group))
  tmp$group = factor(tmp$group, levels = rev(c("Planned Work",
                                           "Planned Deficit",
                                           "Potential Work",
                                           "Potential Deficit")), ordered = T)
  main_title <- paste0("Planned Team workload - ", format(lubridate::today(), "%d %B %Y"))
  # base layer
  p <- ggplot2::ggplot(tmp, ggplot2::aes(x = date, y = value, fill =  group)) +
    ggplot2::geom_bar(stat = 'identity',  show.legend = T) + 
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x='', y = 'TEAM WORKLOAD', title = main_title, fill = "")  +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                          date_breaks = '1 month', 
                          expand = c(0,0)) +
    ggplot2::geom_vline(xintercept = lubridate::today(), colour = "red", linetype = "dashed")
  

  return(p)
})

#' Coerce Object team_schedule to a ggplot
#'
#' @description Coerce Object team_schedule to ggplot, avoiding using the "slot" notation.
#'
#' @param x A \code{team_schedule} object.
#' @export
setMethod("plot", "release_schedule", definition = function(x){
  tmp <- as.data.frame(x)
  ref_dates <- apply(data.frame(lubridate::today(), tmp$start_date), 1, max)
  tmp$days_left <- bizdays::bizdays(ref_dates, tmp$end_date, 'normal')
  tmp$mid <- tmp$start_date +(tmp$end_date - tmp$start_date)/2
  tmp$due <- paste0(format(tmp$end_date, "%d/%m"), " (", tmp$days_left, ")")
  tmp$due <- ifelse(tmp$days_left > 0, tmp$due, NA)
  p <- ggplot2::ggplot(tmp, ggplot2::aes(colour=project_phase_name))
  p <- p + ggplot2::geom_segment(ggplot2::aes(x=start_date, 
                                              xend=end_date, 
                                              y=project_name, 
                                              yend=project_name), 
                                 size=10) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                          date_breaks = '1 month', 
                          expand = c(0,0)) +
    ggplot2::geom_vline(xintercept = lubridate::today(), colour = "red", linetype = "dashed")
  main_title <- paste0("Release schedule (days left in each phase) - ", format(lubridate::today(), "%d %B %Y"))
  p <- p + ggrepel::geom_label_repel(data = tmp[!is.na(tmp$due),], 
                                     ggplot2::aes(x = mid, y = project_name, label = due), force = 5,
                                     show.legend = FALSE, size = 3) +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                   legend.position = c(0.7,0.9), legend.direction = "horizontal",
                   legend.text= ggplot2::element_text(size=8)) +
    ggplot2::labs(title = main_title, x = "", y = "", colour = "")
  

  return(p)
})


#' Coerce Object project_dependencies to a ggplot
#'
#' @description Coerce Object team_schedule to ggplot, avoiding using the "slot" notation.
#'
#' @param x A \code{project_dependencies} object.
#' @export
setMethod("plot", "project_dependencies", definition = function(x){
  tmp <- as.data.frame(x)
  tmp <- tmp %>%
    dplyr::mutate(staff_days_assigned_project_a = 
                    20*(1 + (staff_days_assigned_project_a - 
                       min(staff_days_assigned_project_a))/
                    diff(range(staff_days_assigned_project_a))),
                  dependence_level = 4*(1 + (dependence_level - 
                                        min(dependence_level))/
                    diff(range(dependence_level))))
                  
  edges <- tmp %>%
    dplyr::select(project_a, project_b)
  node_size <- tmp %>% 
    dplyr::group_by(project_a) %>%
    dplyr::summarise(size = mean(staff_days_assigned_project_a))
  g <- igraph::graph_from_data_frame(edges, directed = FALSE)
  igraph::V(g)$size <- node_size$size
  igraph::E(g)$weight <- tmp$dependence_level
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  igraph::V(g)$color <- gg_blue
  main_title <- paste("Project Dependencies based on Shared Resources")
  igraph::plot.igraph(g,  
                      edge.width = igraph::E(g)$weight,
                      main = paste(main_title),
                      edge.curved=0.1)
  
  
  return(TRUE)
})


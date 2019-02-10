
#' Plot staff timeleine
#'
#' @param staff, Scheudle
#' @param schedule Scheudle
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_staff_timeline <- function(staff, schedule){
  tmp <- schedule$full_schedule %>%
    dplyr::filter(staff_name == staff, 
                  dplyr::between(date, 
                                 lubridate::today(), 
                                 lubridate::today() + 30),
                  staff_contribution > 0) %>% 
    dplyr::group_by(project_name, project_phase_name) %>%
    dplyr::summarise(date = max(date), workload = sum(staff_contribution)) %>%
    dplyr::mutate(deadline = paste(project_name, project_phase_name))
  
  tmp$dislocations <- sample(c(-1,-0.5,0.5,1), size = nrow(tmp), replace = TRUE)
  main_title = paste("Milestones for", staff, 
                          "over the next month. Next phase shift in", 
                          as.numeric(min(tmp$date) - lubridate::today()), "days")
  p <- ggplot2::ggplot(tmp) + ggplot2::theme_bw() +
    ggrepel::geom_text_repel( ggplot2::aes(x = date, y=dislocations, label = deadline, 
                                                                colour = project_name), 
                              position="jitter" ) +
    ggplot2::geom_hline( yintercept=0, size=1) + 
    ggplot2::geom_segment(ggplot2::aes(x = date, y=dislocations, 
                                         xend=date, yend=0, alpha=.7, colour = project_name)) +
    ggplot2::theme(legend.position = "none", 
                   axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(title = main_title, x ="", y = "") 
  
  return(p)
}

#' Plot staff schedule
#'
#' @param schedule Schedule
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_phase_days = function(workplan){
  cals <- bizdays::create.calendar('normal', 
                                   weekdays = c('saturday', 'sunday'), 
                                   holidays = workplan@public_holidays@date,
                                   start.date = min(workplan@schedule@date), 
                                   end.date = max(workplan@schedule@date))

  
    p <- ggplot2::ggplot(tmp, ggplot2::aes(colour=project_phase_name))
    p <- p + ggplot2::geom_segment(ggplot2::aes(x=start_date, 
                            xend=due_date, 
                            y=project_name, 
                            yend=project_name), 
                        size=10) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                          date_breaks = '1 month', 
                          expand = c(0,0)) +
    ggplot2::geom_vline(xintercept = lubridate::today(), colour = "red", linetype = "dashed")
    p <- p + ggrepel::geom_label_repel(ggplot2::aes(x = mid, y = project_name, label = days_left), force = 5,
                                       show.legend = FALSE) +
      ggplot2::theme_bw()+
      ggplot2::theme(panel.border = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                     legend.position = c(0.7,0.9), legend.direction = "horizontal") +
      ggplot2::labs(title = "Release schedule and days left in each phase", x = "", y = "", colour = "Project Phases")
  
  bizdays::remove.calendars('normal')
  return(p)
}

#' Get related projects
#'
#' @param project Project of interest
#' @param schedule Schedule
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
get_related_projects = function(project, schedule){
  tmp = schedule$full_schedule
  projectx <- tmp %>% dplyr::filter(project_name == project, staff_contribution>0)
  phases <- get_project_phases()
  tmp <- tmp %>% 
    dplyr::filter(dplyr::between(date, min(projectx$date), max(projectx$date)), 
                  project_name == project | staff_name %in% projectx$staff_name,
                  staff_contribution > 0)
  return(tmp)
}
#' Plot project and related projects
#'
#' @param project Project of interest
#' @param schedule Schedule
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_project_schedule = function(project, schedule){
    tmp <- get_related_projects(project, schedule) %>%
    dplyr::group_by(project_name, project_phase_name) %>%
    dplyr::summarise(start = min(date), end = max(date)) %>%
    dplyr::ungroup() %>%
    tidyr::gather(date_type, task_date, -c(project_name, project_phase_name)) %>%
    dplyr::arrange(date_type, task_date) %>%
    dplyr::mutate(project_phase_name = 
                    factor(project_phase_name, levels=rev(phases$project_phase_name), ordered=TRUE)) %>%
    dplyr::mutate(project_name = 
                    factor(project_name, 
                           levels = rev(c(project, setdiff(unique(tmp$project_name), project))),
                           ordered = TRUE))
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  cols = c(  gg_red, rep(gg_blue, length(levels(tmp$project_name))-1))
  tmp$int <- interaction(tmp$project_name, tmp$project_phase_name)
  tmp$cols <- ifelse(grepl(project, tmp$int), gg_red, gg_blue)
  tmp <- tmp %>% dplyr::arrange(int) 
  cols <- data.frame(levs = levels(tmp$int))
  cols$cols <- ifelse(grepl(project, levels(tmp$int)), gg_red, gg_blue)
  p <- ggplot2::ggplot(tmp, ggplot2::aes(x=int, y=task_date, colour=int)) + 
    ggplot2::geom_line(size=6, alpha = 0.5) +  ggplot2::theme_bw() +
    ggplot2::theme(legend.position='none')+ 
    ggplot2::geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") + 
    ggplot2::guides(colour=ggplot2::guide_legend(title=NULL)) +
    ggplot2::labs(x=NULL, y=NULL) + ggplot2::coord_flip() +
    ggplot2::scale_y_date(date_breaks="2 weeks", labels=scales::date_format("%d %b %y")) +
    ggplot2::labs(title = paste("Project", project, "and related projetcs")) +
    ggplot2::scale_colour_manual(values = tmp$cols) 
  return(p)
}
#' Plot project and related projects network
#'
#' @param project Project of interest
#' @param schedule Schedule
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_project_interactions = function(project, schedule){
  tmp <- get_related_projects(project, schedule) 
  tmp <- tmp %>%
    dplyr::select(project_name, staff_name) 
  tmp2 <- tmp %>% dplyr::rename(project_name_2 = project_name) %>%
    dplyr::left_join(tmp) %>% dplyr::filter(project_name != project_name_2) %>%
    dplyr::distinct() %>% dplyr::select(project_name, project_name_2)
  
  net = igraph::graph_from_edgelist(as.matrix(tmp2))
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  igraph::V(net)$color <- ifelse(names(igraph::V(net)) == project, gg_red, gg_blue)
  igraph::V(net)$size <- 5*igraph::degree(net)
  main_title <- data.frame(projects = names(igraph::V(net)), 
                           influence = igraph::V(net)$size) %>% 
    dplyr::filter(projects != project) %>% dplyr::arrange(dplyr::desc(influence))
  main_title <- paste("Project", project, "can be most affected by Project", 
                      main_title$projects[1], "due to shared resources")
  igraph::plot.igraph(net, edge.arrow.size = 0.3, 
                      main = paste(main_title))
  return(TRUE)
}

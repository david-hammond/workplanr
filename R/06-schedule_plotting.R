#' Plot staff schedule
#'
#' @param tmp Schedule
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_staff_schedule = function(tmp){
  tmp <- tmp$staff_schedule
  myPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 'RdGy')[c(6,2)], 
                                           space='Lab')
  p <- ggplot2::ggplot(data = tmp$staff_schedule, 
                       ggplot2::aes(date, staff_name, fill = workload)) +
    ggplot2::geom_tile(alpha = 0.9) +

    ggplot2::scale_fill_gradientn(colors = myPalette(100), 
                                  labels = scales::percent, name = 'Workload', na.value = "white") +
    ggplot2::labs(x='', y = '', 
                  title = toupper('STAFF WORKLOAD')) +
    bdscale::scale_x_bd(business.dates=tmp$staff_schedule$date, max.major.breaks=20,
                        labels = scales::date_format('%b'), expand = c(0,0))
  
  
  p <- p + ggrepel::geom_text_repel(data = tmp$projects, 
                                    ggplot2::aes(x = date, y = staff_name, label = project_name), 
                                    size = 3, hjust = 1)   
  
  #add leave
  p <- p + ggplot2::geom_segment(data = tmp$leave, ggplot2::aes(x=start, 
                                                            xend=end, 
                                                            y=staff_name, 
                                                            yend=staff_name, colour = out_of_office), size=2, alpha = 0.6)
  p <- p + ggplot2::geom_point(data = tmp$leave, ggplot2::aes(x=start, 
                                                          y=staff_name, colour = out_of_office), size=3)
  p <- p + ggplot2::geom_point(data = tmp$leave, ggplot2::aes(x=end, 
                                                          y=staff_name, colour = out_of_office), size=3)
  p <- p + ggplot2::labs(fill ="Workload" ,colour="Out of Office")
  
  #add holidays

  p <- p + ggplot2::geom_vline(xintercept = tmp$public_holidays$date, 
                               linetype = "dashed", colour = grey(0.5), alpha = 0.6)
  return(p)
}

#' Plot team schedule
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_team_schedule = function(tmp){
  tmp <- tmp$team_schedule
  pos <- tmp$workload == 0
  tmp$project_confirmed[pos] <- 1
  worktypes <- c("Potential", "Planned")
  tmp$project_confirmed = worktypes[tmp$project_confirmed+1]
  tmp$project_confirmed = factor(tmp$project_confirmed, worktypes, ordered = TRUE)
  tmp <- tmp %>% dplyr::group_by(date) %>% 
    dplyr::mutate(total = sum(workload)) %>%
    dplyr::ungroup() 
  tmp <- tmp %>% dplyr::group_by(date, project_confirmed) %>% 
    dplyr::mutate(Work = min(workload, 1), Deficit = ifelse(workload > 1, workload-1, 0)) %>%
    dplyr::ungroup() %>% dplyr::select(-workload) %>%
    tidyr::gather(load, value, -c(date, project_confirmed, total)) %>%
    dplyr::distinct()
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  cols = c(scales::alpha(gg_red, 0.5),   scales::alpha(gg_blue, 0.5),gg_red, gg_blue)
  tmp$group = paste(tmp$project_confirmed, tmp$load)
  
  tmp <- tmp %>% dplyr::select(date, group, value) %>% 
    tidyr::spread(group, value, fill = 0)
  
  pos <- tmp$`Planned Work` == 1
  tmp$`Potential Deficit`[pos] = tmp$`Potential Deficit`[pos] + tmp$`Potential Work`[pos]
  tmp$`Potential Work`[pos] = 0
  
  total_potential <- tmp$`Potential Deficit` + tmp$`Potential Work`
  pos <- tmp$`Planned Deficit` == 0 & (tmp$`Planned Work` + total_potential) > 1
  tmp$`Potential Work`[pos] <- 1 - tmp$`Planned Work`[pos] 
  tmp$`Potential Deficit`[pos] <- total_potential[pos] - tmp$`Potential Work`[pos]
  
  tmp <- tmp %>% tidyr::gather(group, value, -date)
  
  tmp$group = factor(tmp$group, levels = sort(unique(tmp$group))[c(3,4,1,2)], ordered = T)
  
  # base layer
  p <- ggplot2::ggplot(tmp, ggplot2::aes(x = date, y = value, fill =  group)) +
    ggplot2::geom_bar(stat = 'identity',  show.legend = T) + 
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x='', y = 'TEAM WORKLOAD', title = 'TEAM WORKLOAD', fill = "")  +
    bdscale::scale_x_bd(business.dates=tmp$date, max.major.breaks=20,
                        labels = scales::date_format('%b'), expand = c(0,0))
  return(p)
}

#' Plot project calendar
#'
#' @param project, Scheudle
#' @param schedule Scheudle
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_project_calendar = function(project, schedule){
  proj_cal = schedule$full_schedule %>% 
    dplyr::filter(project_name == project) %>%
    dplyr::select(date, project_phase_name)
  proj_cal$day <- factor(strftime(proj_cal$date,format="%a"),
                         levels=rev(c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
  proj_cal$week <- factor(strftime(proj_cal$date,format="%V"))
  proj_cal$month <- factor(strftime(proj_cal$date,format="%B"),
                           levels=unique(strftime(proj_cal$date,format="%B")), 
                           ordered = TRUE)
  proj_cal$ddate <- factor(strftime(proj_cal$date,format="%d"))
  phases <- get_project_phases()
  proj_cal$project_phase_name <- factor(proj_cal$project_phase_name, 
                                        levels = phases$project_phase_name,
                                        ordered = TRUE)
  proj_cal$days_from_today <- proj_cal$date - lubridate::today()
  remaining <- proj_cal %>%
    dplyr::filter(days_from_today > 0)
  pc.time.gone <- scales::percent(round(sum(proj_cal$days_from_today < 0)/nrow(proj_cal),2))
  
  main_title = paste0("Project ", project, ", ", pc.time.gone, " time elsapses. Needs to finish ", 
                     remaining$project_phase_name[1]," in ")
  
  remaining <- remaining %>%
    dplyr::filter(project_phase_name != project_phase_name[1])
  
  main_title = paste0(main_title, remaining$days_from_today[1], " Days")
  status <- proj_cal %>% 
    dplyr::filter(date - lubridate::today() == min(date - lubridate::today()))
  p <- ggplot2::ggplot(proj_cal, ggplot2::aes(x=week,y=day))+
    ggplot2::geom_tile(ggplot2::aes(fill=project_phase_name))+
    ggplot2::geom_text(ggplot2::aes(label=ddate))+
    ggplot2::scale_fill_manual(values=c("#8dd3c7","#ffffb3","#fb8072","#d3d3d3"))+
    ggplot2::geom_point(data = proj_cal[which(as.numeric(abs(proj_cal$days_from_today)) == as.numeric(min(abs(proj_cal$days_from_today))))[1],], 
                        ggplot2::aes(x=week,y=day), 
                        size = 16, alpha = 0.4, colour = "#8dd3c7") +
    ggplot2::facet_grid(~month,scales="free",space="free")+
    ggplot2::labs(x="Week",y="", title = paste(main_title)) +
    ggplot2::theme_bw(base_size=10)+
    ggplot2::theme(legend.title=ggplot2::element_blank(),
                   panel.grid=ggplot2::element_blank(),
                   panel.border=ggplot2::element_blank(),
                   axis.ticks=ggplot2::element_blank(),
                   strip.background=ggplot2::element_blank(),
                   legend.position="top",
                   legend.justification="right",
                   legend.direction="horizontal",
                   legend.key.size=ggplot2::unit(0.3,"cm"),
                   legend.spacing.x=ggplot2::unit(0.2,"cm")) 
  
  
  
  
  return(p)
}
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
plot_phase_days = function(schedule){
  tmp <- schedule$full_schedule %>%
    dplyr::select(date, project_name, project_phase_name) %>%
    dplyr::distinct() %>%
    dplyr::filter(date > lubridate::today()) %>%
    dplyr::mutate(days_until = lubridate::today() - date) %>%
    dplyr::group_by(project_name, project_phase_name) %>%
    dplyr::summarise(due_date = max(date), due_days = as.numeric(max(days_until)), days_left = n()) %>%
    dplyr::ungroup() %>% 
    dplyr::group_by(project_name) %>%
    dplyr::mutate(due_date = max(due_date)) %>%
    dplyr::arrange(due_date)
  
  tmp$project_name = factor(tmp$project_name, levels = unique(tmp$project_name),
                            ordered =T)
  phases <- get_project_phases()
  tmp$project_phase_name <- factor(tmp$project_phase_name, 
                                   levels = phases$project_phase_name,
                                   ordered = TRUE)
  myPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 'RdGy')[c(6,2)], 
                                           space='Lab')
  p <- ggplot2::ggplot(tmp, ggplot2::aes(x = project_phase_name, y = project_name, 
                                fill = due_days, label = days_left)) +
    ggplot2::geom_tile() + ggplot2::geom_text() +
    ggplot2::scale_fill_gradientn(colors = myPalette(100), 
                                  name = 'DDay', na.value =  "white") + 
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), 
          panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::labs(x = "", y = "")
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

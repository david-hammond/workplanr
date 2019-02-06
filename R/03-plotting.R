#' create a list of employees that are to be assigned to projects
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
get_staff_schedule = function(tmp){
  staff_schedule = tmp %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::summarise(workload = sum(leave_adjusted_workload)/100) %>%
    dplyr::ungroup() %>%  
    dplyr::filter(is.finite(workload)) #why inf?
  projects = tmp %>% 
    dplyr::filter(staff_contribution > 0) %>%
    dplyr::group_by(project_name, staff_name) %>%
    dplyr::filter(date == min(as.Date(date))) %>%
    dplyr::select(project_name, staff_name, date, leave_adjusted_workload) %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::summarise(project_name = paste(project_name, collapse = ", "),
                     workload = 1) %>%
    dplyr::ungroup() 
  
  
  tmp = list(staff_schedule = staff_schedule, projects = projects)
  return(tmp)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param tmp Schedule
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_staff_schedule = function(tmp){
  tmp <- get_staff_schedule(tmp)
    myPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 'RdGy')[c(6,2)], 
                                           space='Lab')
  p <- ggplot2::ggplot(data = tmp$staff_schedule, ggplot2::aes(date, staff_name, fill = workload)) +
    ggplot2::geom_tile(alpha = 0.9) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                          date_breaks = '1 month', 
                          expand = c(0,0)) +
    ggplot2::scale_fill_gradientn(colors = myPalette(100), 
                                  labels = scales::percent, name = 'Workload', na.value = "white") +
    ggplot2::labs(x='', y = '', 
                  title = toupper('STAFF WORKLOAD')) 
  
  p <- p + ggrepel::geom_text_repel(data = tmp$projects, 
                                    ggplot2::aes(x = date, y = staff_name, label = project_name), 
                                    size = 3, hjust = 1, force = 2.5)
  return(p)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
get_team_schedule = function(tmp){
  team_capacity <- tmp %>% 
    dplyr::select(staff_name, staff_capacity) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  team_capacity <- sum(team_capacity$staff_capacity)
  tmp <- tmp %>% 
    dplyr::group_by(date, project_confirmed) %>% 
    dplyr::summarise(leave_adjusted_workload = sum(leave_adjusted_workload, na.rm=TRUE),
                     workload = round(leave_adjusted_workload/team_capacity,2)) %>%
    dplyr::select(-leave_adjusted_workload) %>%
    dplyr::ungroup() 
  return(tmp)
}
#' create a list of employees that are to be assigned to projects
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_team_schedule = function(tmp){
  x <- get_team_schedule(tmp)
  pos <- x$workload == 0
  x$project_confirmed[pos] <- 1
  worktypes <- c("Potential", "Planned")
  x$project_confirmed = worktypes[x$project_confirmed+1]
  x$project_confirmed = factor(x$project_confirmed, worktypes, ordered = TRUE)
  x <- x %>% dplyr::group_by(date) %>% 
    dplyr::mutate(total = sum(workload)) %>%
    dplyr::ungroup() 
  x <- x %>% dplyr::group_by(date, project_confirmed) %>% 
    dplyr::mutate(Work = min(workload, 1), Deficit = ifelse(workload > 1, workload-1, 0)) %>%
    dplyr::ungroup() %>% dplyr::select(-workload) %>%
    tidyr::gather(load, value, -c(date, project_confirmed, total)) %>%
    dplyr::distinct()
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  cols = c(scales::alpha(gg_red, 0.5),   scales::alpha(gg_blue, 0.5),gg_red, gg_blue)
  x$group = paste(x$project_confirmed, x$load)
  
  x <- x %>% dplyr::select(date, group, value) %>% 
    tidyr::spread(group, value, fill = 0)
  
  pos <- x$`Planned Work` == 1
  x$`Potential Deficit`[pos] = x$`Potential Deficit`[pos] + x$`Potential Work`[pos]
  x$`Potential Work`[pos] = 0
  
  total_potential <- x$`Potential Deficit` + x$`Potential Work`
  pos <- x$`Planned Deficit` == 0 & (x$`Planned Work` + total_potential) > 1
  x$`Potential Work`[pos] <- 1 - x$`Planned Work`[pos] 
  x$`Potential Deficit`[pos] <- total_potential[pos] - x$`Potential Work`[pos]
  
  x <- x %>% tidyr::gather(group, value, -date)
  
  x$group = factor(x$group, levels = sort(unique(x$group))[c(3,4,1,2)], ordered = T)
  
  # base layer
  p <- ggplot2::ggplot(x, ggplot2::aes(date, value, fill =  group)) +
    ggplot2::geom_bar(stat = 'identity',  show.legend = T) + 
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), date_breaks = '1 month',
                          expand = c(0,0)) +
    ggplot2::labs(x='', y = 'TEAM WORKLOAD', title = 'TEAM WORKLOAD', fill = "")
  return(p)
}



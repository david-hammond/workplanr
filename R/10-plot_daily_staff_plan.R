#' Plot a staff calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of staff per date
#' @examples 
#' staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
#' capacity <- c(40,60,100,100,100)
#' resources <- set_resources(staff, capacity)
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' projects <- set_projects(projects, probability, start, end)
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' phases <- set_phases(phases)
#' roles <- c("lead", "researcher", "editor", "design")
#' roles <- set_roles(roles)
#' responsibilities <- rbind(lead = rep(1, length(phases$phase)), 
#' researcher = c(1,1,1,0,0,0), editor = c(0,0,1,0,0,0), design = c(0,0,0,1,1,0))
#' responsibilities <- set_responsibilities(roles, phases, responsibilities)
#' time_estimates <- rbind(c(-40,-10,-10,-10,-10,10), c(-10,-10,-10,0,0,0), 
#'                         c(0,0,0,-10,-10,10))
#' time_estimates <- set_time_estimates(projects, phases, time_estimates)
#' project_teams <- expand.grid(project = projects$project, 
#'                              role = roles$role, KEEP.OUT.ATTRS = FALSE)
#' project_teams$staff <- sample(resources$staff, size = nrow(project_teams), 
#'                               replace = TRUE)
#' project_teams$assigned_capacity <- sample(c(25,25,75,100), 
#'                                    size = nrow(project_teams), 
#'                                    replace = TRUE)
#' project_teams <- set_project_team(project_teams)
#' daily_plan <- get_daily_plan(resources, projects, 
#'                phases, time_estimates, 
#'                project_teams, responsibilities)
#' plot_staff_calendar(daily_plan)
#'       
#' @export
plot_staff_calendar = function(daily_plan){

  workplan = get_staff_daily_workload(daily_plan)
  # create base plot
  myPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 'RdGy')[c(6,2)], 
                                              space='Lab')
  p <- ggplot2::ggplot(workplan, ggplot2::aes(date, staff, fill = workload)) +
    ggplot2::geom_tile(alpha = 0.9) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                          date_breaks = '1 month', 
                          expand = c(0,0)) +
    ggplot2::scale_fill_gradientn(colors = myPalette(100), 
                                  labels = scales::percent, name = 'Workload') +
    ggplot2::labs(x='', y = '', 
                  title = toupper('STAFF WORKLOAD')) 
  
  # Add task labels
  tasks <- daily_plan %>% 
    dplyr::group_by(staff, project) %>% 
    dplyr::summarise(date = min(date)) %>%
    dplyr::ungroup()
  tasks <- tasks %>% 
    dplyr::group_by(staff, date) %>% 
    dplyr::summarise(project = paste(project, collapse = ', ')) %>%
    dplyr::ungroup()
  tasks <- left_join(tasks, workplan)
  p <- p + ggrepel::geom_text_repel(data = as.data.frame(tasks),
                           ggplot2::aes(x = date, y = staff, label = project), 
                           size = 3, hjust = 1, force = 2.5)
  return(p)
}

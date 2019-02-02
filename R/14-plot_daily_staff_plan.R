#' Plot a staff calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of staff per date
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
#'       
#' @export
plot_daily_staff_plan = function(daily_plan){

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
                                  labels = scales::percent, name = 'Workload', na.value = "white") +
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
  tasks <- dplyr::left_join(tasks, workplan)
  p <- p + ggrepel::geom_text_repel(data = as.data.frame(tasks),
                           ggplot2::aes(x = date, y = staff, label = project), 
                           size = 3, hjust = 1, force = 2.5)
  return(p)
}

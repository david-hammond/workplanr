#' Plot a team workload calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return ggplot of workload of team per date
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
#' 

plot_team_daily_workload = function(daily_plan){
  
  team_workplan <- get_team_daily_workload(daily_plan)
  team_workplan$maxcapacity <- ifelse(team_workplan$workload > 1, 1, team_workplan$workload)
  
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"

  # base layer
  p <- ggplot2::ggplot(team_workplan, ggplot2::aes(date, workload, fill = teamload)) +
    ggplot2::geom_bar(stat = 'identity',  show.legend = T) +
    ggplot2::scale_fill_manual(values = c(gg_blue, gg_red)) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), date_breaks = '1 month',
                          expand = c(0,0)) +
    ggplot2::labs(x='', y = 'TEAM WORKLOAD', title = 'TEAM WORKLOAD')
  
  
  p <- p + ggplot2::geom_bar(data = team_workplan, 
                             ggplot2::aes(date, maxcapacity),
                             stat = 'identity', fill = gg_blue)

  return(p)
}

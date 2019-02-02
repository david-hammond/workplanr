#' Plot a team workload calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return ggplot of workload of team per date
#' @examples 
#' library(workplanr)
#' staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
#' capacity <- c(40,60,100,100,100)
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' roles <- c("lead", "researcher", "editor", "design")
#' wp <- get_workplan(staff, capacity, projects, probability, start, end, phases, roles)
#' daily_plan <- get_daily_plan(wp)
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

#' Plot a team workload calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return ggplot of workload of team per date
#' @examples 
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

#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
get.release.gannt.chart = function(plan, filename = "./graphs/release-gantt-chart.png"){
  require(ggthemes)
  gantt = plan$schedule %>% group_by(project) %>% mutate(finish = max(end)) %>% 
    ungroup() %>% filter(complete.cases(.)) %>% arrange(finish)  
  
  gantt$start = as.POSIXct(gantt$start)  
  gantt$end = as.POSIXct(gantt$end) 
  pos = gantt$probability < 1
  gantt$project[pos] = paste0(gantt$project[pos], "***")
  gantt$project = factor(gantt$project, rev(unique(gantt$project)), ordered = T)
  p <- ggplot(gantt, aes(colour=phase))
  p <- p + geom_segment(aes(x=start, 
                            xend=end, 
                            y=project, 
                            yend=project), 
                        size=2)
  p <- p + geom_point(aes(x=start,
                          y=project),
                      size=3)
  p <- p + geom_point(aes(x=end,
                          y=project),
                      size=3)
  
  p <- p + xlab("")
  p <- p + scale_x_datetime(labels = scales::date_format("%b"), date_breaks = "1 month",
                            expand = c(0,0)) +
    labs(title = "Project Phases and Releases", y = "", x = "", 
         caption = "*** Subject to Successful Contract")
  p <- p + theme_fivethirtyeight() 
  p <- p + theme(legend.position=c(0.7,0.8))
  p = ggsave(p, filename = filename)
  plan$gantt.release = gantt
  
  return(plan)
  
}
#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
team.workload = function(plan, filename = "./graphs/all-team-capacity-bar-plot.png"){
  
  x = plan$resource.allocation.tile.plot
  x$staff = as.character(x$staff)
  pos = x$staff == "unassigned"
  x$staff[!pos] = "assigned"
  tmp = x %>% ungroup() %>% group_by(date, staff) %>% 
    summarise(team_capacity = sum(available_capacity)) %>%
    ungroup()
  tmp$team_capacity = tmp$team_capacity/(sum(plan$resources$capacity[plan$resources$staff != "unassigned"])/100)
  tmp$staff = factor(tmp$staff, rev(unique(sort(tmp$staff))), ordered = T)
  tmp = tmp %>% ungroup() %>% group_by(date) %>% 
    mutate(total = sum(team_capacity), load = ifelse(sum(team_capacity) < 1, "Covered", "Overload"),
           group = paste(staff, team_capacity > 1, load)) %>% ungroup()
  
  plot.data = tmp
  
  plot.data$group[plot.data$total <= 1 & plot.data$load == "Covered" & plot.data$staff == "assigned"] =  "Core"
  plot.data$group[plot.data$total <= 1 & plot.data$load == "Covered" & plot.data$staff == "unassigned"] = "Potential Consulting"
  plot.data$group[plot.data$total <= 1 & plot.data$load == "Overload" & plot.data$staff == "assigned"] = "Core"
  plot.data$group[plot.data$total > 1 & plot.data$load == "Overload" & plot.data$staff == "assigned"] = "Core Overload"
  plot.data$group[plot.data$total <= 1 & plot.data$load == "Overload" &plot.data$staff == "unassigned"] = "Potential Consulting Overload"
  plot.data$group[plot.data$total > 1 & plot.data$load == "Overload" & plot.data$staff == "unassigned"] = "Potential Consulting Overload"
  plot.data$group = as.character(plot.data$group)
  plot.data$group = factor(plot.data$group, rev(c("Core", "Potential Consulting","Core Overload", "Potential Consulting Overload")), ordered = T)
  cols = rev(c(RColorBrewer::brewer.pal(9, "Blues")[c(4,6)],  RColorBrewer::brewer.pal(9, "Reds")[c(4,6)]))
  cols = c(alpha(gg_color_hue(1), 0.5), gg_color_hue(1),  alpha(gg_color_hue(2)[2], 0.5), gg_color_hue(2)[2])
  # base layer
  p <- ggplot(plot.data, aes(date, team_capacity, fill = group)) + 
    geom_bar(stat = "identity",  show.legend = T) + 
    scale_fill_manual(values = cols)
  
  plot.data2 = plot.data[which(plot.data$load == "Overload"),]
  plot.data2 = plot.data2 %>% select(date) %>% distinct() %>% mutate(team_capacity = 1)
  
  p = p + geom_bar(data = plot.data2, aes(date, team_capacity), fill = gg_color_hue(2)[2], stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_datetime(labels = scales::date_format("%b"), date_breaks = "1 month",
                     expand = c(0,0)) +   
    labs(x="", y = "TEAM WORKLOAD", title = "Projected Team Workload 2019")
  p <- p + theme_fivethirtyeight()
  p = p + theme(legend.position = c(0.8, 0.8), legend.direction = "vertical")
  ggsave(p, filename = filename)
  
  plan$team.capacity = plot.data
  
  return(plan)
}
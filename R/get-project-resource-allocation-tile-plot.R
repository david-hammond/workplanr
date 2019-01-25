#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
get.project.resource.allocation.tile.plot = function(plan, filename = "./graphs/all-resource-tile-plot.png"){
  require(RColorBrewer)
  require(ggrepel)
  x = plan$assignments %>% group_by(date, staff, probability) %>% 
    summarise(capacity = min(capacity),
              assigned_capacity = sum(workshare),
              available_capacity = capacity - assigned_capacity)
  tmp = expand.grid(date = unique(x$date), staff = unique(x$staff))
  tmp = left_join(tmp, x %>% dplyr::select(date, staff, available_capacity, probability))
  tmp$available_capacity[is.na(tmp$available_capacity)] = 100
  tmp$probability[is.na(tmp$probability)] = 1
  tmp$available_capacity = 1 - tmp$available_capacity/100
  caps = c(-100,-0.1, 0.2, 1)
  tmp$load = findInterval(tmp$available_capacity, caps, all.inside = T)
  tmp$load = c("Overloaded", "Occupied", "Free")[tmp$load]
  # tmp$load = ifelse(tmp$available_capacity > 0, "Available", "Overloaded")
  # tmp$load = ifelse(tmp$available_capacity > 0, "Available", "Overloaded")
  tmp = tmp %>% ungroup()
  tmp$date = as.POSIXct(tmp$date)
  tmp$staff = factor(tmp$staff, levels = rev(sort(unique((tmp$staff)))), ordered = T)
  
  myPalette <- colorRampPalette(rev(brewer.pal(11, "RdGy")[c(2,6)]), space="Lab")

  p <- ggplot(tmp, aes(date, staff, fill = available_capacity)) + 
    geom_tile(alpha = 0.9) +
    scale_x_datetime(labels = scales::date_format("%b"), date_breaks = "1 month",
                     expand = c(0,0)) +
    scale_fill_gradientn(colors = myPalette(100), labels = scales::percent, name = "Workload") + 
    labs(x="", y = "", title = toupper("Projected Staff Availability and Leave 2019")) + theme_bw()
  tasks = plan$assignments %>% group_by(staff, project) %>% summarise(date = as.POSIXct(min(date))) %>% 
    ungroup()
  tasks = tasks %>% group_by(staff, date) %>% summarise(project = paste(project, collapse = ", ")) %>%
    ungroup() %>% mutate(available_capacity = 1)
  plan$leave$available_capacity = 2
  p <- p + geom_text_repel(data = as.data.frame(tasks), 
                           aes(x = date, y = staff, label = project), size = 3, hjust = 1, force = 2.5)
  p <- p + geom_segment(data = plan$leave, aes(x=start, 
                                               xend=end, 
                                               y=staff, 
                                               yend=staff), size=2, alpha = 0.6, colour = "red")
  p <- p + geom_point(data = plan$leave, aes(x=start, 
                                             y=staff), size=3, colour = "red")
  p <- p + geom_point(data = plan$leave, aes(x=end, 
                                             y=staff), size=3,  colour = "red")
  
  p <- p + geom_vline(xintercept = plan$holidays$holiday.dates, 
                      linetype = "dashed", colour = grey(0.5), alpha = 0.6)
  
  ggsave(p, filename = filename)
  
  plan$resource.allocation.tile.plot = tmp
  
  return(plan)
}
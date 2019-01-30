#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
plot.staff.calendar = function(plan, leave, public.holidays){
  require(RColorBrewer)
  require(ggrepel)

  x = plan %>% group_by(date, staff, probability) %>% 
    summarise(capacity = min(capacity),
              assigned_capacity = sum(leave_adjusted_assigned_capacity),
              available_capacity = assigned_capacity/capacity)
  tmp = expand.grid(date = unique(x$date), staff = unique(x$staff))
  tmp = left_join(tmp, x %>% dplyr::select(date, staff, available_capacity, probability))
  tmp = tmp %>% ungroup()
  tmp$staff = factor(tmp$staff, levels = rev(sort(unique((tmp$staff)))), ordered = T)
  
  myPalette <- colorRampPalette(rev(brewer.pal(11, "RdGy")[c(2,6)]), space="Lab")
  
  p <- ggplot(tmp, aes(date, staff, fill = available_capacity)) + 
    geom_tile(alpha = 0.9) +
    scale_x_date(labels = scales::date_format("%b"), date_breaks = "1 month",
                     expand = c(0,0)) +
    scale_fill_gradientn(colors = myPalette(100), labels = scales::percent, name = "Workload") + 
    labs(x="", y = "", title = toupper("Projected Staff Availability and Leave 2019")) + theme_bw()
  tasks = plan %>% group_by(staff, project) %>% summarise(date = min(date)) %>% 
    ungroup()
  tasks = tasks %>% group_by(staff, date) %>% summarise(project = paste(project, collapse = ", ")) %>%
    ungroup() %>% mutate(available_capacity = 1) 
  p <- p + geom_text_repel(data = as.data.frame(tasks), 
                           aes(x = date, y = staff, label = project), size = 3, hjust = 1, force = 2.5)
  leave <- leave %>%  mutate(available_capacity = 1)
  p <- p + geom_segment(data = leave, aes(x=start, 
                                               xend=end, 
                                               y=staff, 
                                               yend=staff), size=2, alpha = 0.6, colour = "red")
  p <- p + geom_point(data = leave, aes(x=start, 
                                             y=staff), size=3, colour = "red")
  p <- p + geom_point(data = leave, aes(x=end, 
                                             y=staff), size=3,  colour = "red")
  
  p <- p + geom_vline(xintercept = public.holidays$date, 
                      linetype = "dashed", colour = grey(0.5), alpha = 0.6)
  
  return(p)
}
#' Plot a staff calendar
#'
#' @param daily_plan Daily work plan of staff
#' @return Calculated workload of staff per date
#' @examples
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

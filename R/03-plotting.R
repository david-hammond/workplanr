get_staff_schedule = function(tmp){
  staff_schedule = tmp %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::summarise(workload = sum(leave_adjusted_workload)) %>%
    dplyr::ungroup() 
  projects = tmp %>% 
    dplyr::group_by(project_name, staff_name) %>%
    dplyr::filter(date == min(as.Date(date))) %>%
    dplyr::select(project_name, staff_name, date) %>%
    dplyr::group_by(date, staff_name) %>%
    dplyr::summarise(project_name = paste(project_name, collapse = ", "),
                     workload = 1) %>%
    dplyr::ungroup()
  
  
  tmp = list(staff_schedule, projects)
  return(tmp)
}

plot_staff_schedule = function(tmp){

  myPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 'RdGy')[c(6,2)], 
                                           space='Lab')
  p <- ggplot2::ggplot(data = tmp$staff_schedule, ggplot2::aes(date, staff_name, fill = workload)) +
    ggplot2::geom_tile(alpha = 0.9) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), 
                          date_breaks = '1 month', 
                          expand = c(0,0)) +
    ggplot2::scale_fill_gradientn(colors = myPalette(100), 
                                  labels = scales::percent, name = 'Workload', na.value = "white") +
    ggplot2::labs(x='', y = '', 
                  title = toupper('STAFF WORKLOAD')) 
  
  p <- p + ggrepel::geom_text_repel(data = tmp$projects, 
                                    ggplot2::aes(x = date, y = staff_name, label = project_name), 
                                    size = 3, hjust = 1, force = 2.5)
}

get_team_schedule = function(tmp){
  team_capacity <- tmp %>% 
    dplyr::select(staff_name, staff_capacity) %>%
    dplyr::distinct() %>%
    dplyr::ungroup()
  team_capacity <- sum(team_capacity$staff_capacity)
  tmp <- tmp %>% 
    dplyr::group_by(date, project_confirmed) %>% 
    dplyr::summarise(leave_adjusted_workload = sum(leave_adjusted_workload, na.rm=TRUE),
                     workload = round(leave_adjusted_workload/team_capacity,2)) %>%
    dplyr::select(-leave_adjusted_workload) %>%
    dplyr::ungroup()
  return(tmp)
}

plot_team_schedule = function(tmp){
  x <- tmp
  pos <- x$workload == 0
  x$project_confirmed[pos] <- 1
  worktypes <- c("Potential", "Committed")
  x$project_confirmed = worktypes[x$project_confirmed+1]
  x$probability = factor(x$project_confirmed, worktypes, ordered = TRUE)
  x <- x %>% dplyr::group_by(date) %>% 
    dplyr::mutate(total = sum(workload)) %>%
    dplyr::ungroup() 
  x <- x %>% dplyr::group_by(date, probability) %>% 
    dplyr::mutate(maximum = min(workload, 1), deficit = ifelse(workload > 1, workload-1, 0)) %>%
    dplyr::ungroup() %>% dplyr::select(-workload) %>%
    tidyr::gather(load, value, -c(date, probability, total))
  pos <- x$probability == "Potatential"
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  cols = c(scales::alpha(gg_red, 0.5),   scales::alpha(gg_blue, 0.5),gg_red, gg_blue)
  x$group = paste(x$probability, x$load)
  
  x <- x %>% dplyr::select(date, group, value) %>% 
    tidyr::spread(group, value, fill = 0)
  
  pos <- x$`Committed deficit` > 0
  x$`Potential deficit`[pos] = x$`Potential deficit`[pos] + x$`Potential maximum`[pos]
  x$`Potential maximum`[pos] = 0
  
  total_potential <- x$`Potential deficit` + x$`Potential maximum`
  pos <- x$`Committed deficit` == 0 & (x$`Committed maximum` + total_potential) > 1
  x$`Potential maximum`[pos] <- 1 - x$`Committed maximum`[pos] 
  x$`Potential deficit`[pos] <- total_potential[pos] - x$`Potential maximum`[pos]
  
  x <- x %>% tidyr::gather(group, value, -date)
  
  
  x$group = factor(x$group, levels = rev(sort(unique(x$group))[c(2,1,4,3)]), ordered = T)
  
  # base layer
  p <- ggplot2::ggplot(x, ggplot2::aes(date, value, fill =  group)) +
    ggplot2::geom_bar(stat = 'identity',  show.legend = T) + 
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_date(labels = scales::date_format('%b'), date_breaks = '1 month',
                          expand = c(0,0)) +
    ggplot2::labs(x='', y = 'TEAM WORKLOAD', title = 'TEAM WORKLOAD', fill = "")
}



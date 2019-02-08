
#' Plot staff schedule
#'
#' @param tmp Schedule
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_staff_schedule = function(tmp){
  tmp <- tmp$staff_schedule
  myPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(11, 'RdGy')[c(6,2)], 
                                           space='Lab')
  p <- ggplot2::ggplot(data = tmp$staff_schedule, ggplot2::aes(date, staff_name, fill = workload)) +
    ggplot2::geom_tile(alpha = 0.9) +
    # ggplot2::scale_x_date(labels = scales::date_format('%b'), 
    #                       date_breaks = '1 month', 
    #                       expand = c(0,0)) +
    ggplot2::scale_fill_gradientn(colors = myPalette(100), 
                                  labels = scales::percent, name = 'Workload', na.value = "white") +
    ggplot2::labs(x='', y = '', 
                  title = toupper('STAFF WORKLOAD')) 
  
  p <- p + ggrepel::geom_text_repel(data = tmp$projects, 
                                    ggplot2::aes(x = date, y = staff_name, label = project_name), 
                                    size = 3, hjust = 1) +
    bdscale::scale_x_bd(business.dates=tmp$staff_schedule$date, max.major.breaks=20,
                        labels = scales::date_format('%b'), expand = c(0,0))
  
  #add leave
  p <- p + ggplot2::geom_segment(data = tmp$leave, ggplot2::aes(x=start, 
                                                            xend=end, 
                                                            y=staff_name, 
                                                            yend=staff_name, colour = out_of_office), size=2, alpha = 0.6)
  p <- p + ggplot2::geom_point(data = tmp$leave, ggplot2::aes(x=start, 
                                                          y=staff_name, colour = out_of_office), size=3)
  p <- p + ggplot2::geom_point(data = tmp$leave, ggplot2::aes(x=end, 
                                                          y=staff_name, colour = out_of_office), size=3)
  p <- p + ggplot2::labs(fill ="Workload" ,colour="Out of Office")
  
  #add holidays

  p <- p + ggplot2::geom_vline(xintercept = tmp$public_holidays$date, 
                               linetype = "dashed", colour = grey(0.5), alpha = 0.6)
  return(p)
}

#' Plot team schedule
#'
#' @param tmp Scheudle
#' @return staff_schedule if script exectues completely
#' @examples 
#' library(workplanr)
#' @export
plot_team_schedule = function(tmp){
  tmp <- tmp$team_schedule
  pos <- tmp$workload == 0
  tmp$project_confirmed[pos] <- 1
  worktypes <- c("Potential", "Planned")
  tmp$project_confirmed = worktypes[tmp$project_confirmed+1]
  tmp$project_confirmed = factor(tmp$project_confirmed, worktypes, ordered = TRUE)
  tmp <- tmp %>% dplyr::group_by(date) %>% 
    dplyr::mutate(total = sum(workload)) %>%
    dplyr::ungroup() 
  tmp <- tmp %>% dplyr::group_by(date, project_confirmed) %>% 
    dplyr::mutate(Work = min(workload, 1), Deficit = ifelse(workload > 1, workload-1, 0)) %>%
    dplyr::ungroup() %>% dplyr::select(-workload) %>%
    tidyr::gather(load, value, -c(date, project_confirmed, total)) %>%
    dplyr::distinct()
  gg_red <- "#F8766D"
  gg_blue <- "#00BFC4"
  cols = c(scales::alpha(gg_red, 0.5),   scales::alpha(gg_blue, 0.5),gg_red, gg_blue)
  tmp$group = paste(tmp$project_confirmed, tmp$load)
  
  tmp <- tmp %>% dplyr::select(date, group, value) %>% 
    tidyr::spread(group, value, fill = 0)
  
  pos <- tmp$`Planned Work` == 1
  tmp$`Potential Deficit`[pos] = tmp$`Potential Deficit`[pos] + tmp$`Potential Work`[pos]
  tmp$`Potential Work`[pos] = 0
  
  total_potential <- tmp$`Potential Deficit` + tmp$`Potential Work`
  pos <- tmp$`Planned Deficit` == 0 & (tmp$`Planned Work` + total_potential) > 1
  tmp$`Potential Work`[pos] <- 1 - tmp$`Planned Work`[pos] 
  tmp$`Potential Deficit`[pos] <- total_potential[pos] - tmp$`Potential Work`[pos]
  
  tmp <- tmp %>% tidyr::gather(group, value, -date)
  
  tmp$group = factor(tmp$group, levels = sort(unique(tmp$group))[c(3,4,1,2)], ordered = T)
  
  # base layer
  p <- ggplot2::ggplot(tmp, ggplot2::aes(x = date, y = value, fill =  group)) +
    ggplot2::geom_bar(stat = 'identity',  show.legend = T) + 
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x='', y = 'TEAM WORKLOAD', title = 'TEAM WORKLOAD', fill = "")  +
    bdscale::scale_x_bd(business.dates=tmp$date, matmp.major.breaks=20,
                        labels = scales::date_format('%b'), etmppand = c(0,0))
  return(p)
}



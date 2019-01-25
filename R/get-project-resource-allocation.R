#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
get.project.resource.allocation = function(plan, filename = "./graphs/resource-allocation-waterfall.png"){
  
  tmp = plan$assignments %>% group_by(project) %>%
    summarise(total = sum(workshare), days = n(), fte = length(unique(staff))) %>%
    ungroup() %>% mutate(pc = total/sum(total)) %>% arrange(desc(total))
  
  p = waterfall(tmp, "project", "total") + 
    labs(title = "Planned Project Resource Allocation for 2019", x = "TOTAL RESOURCE FOR 2019", y = "") 
  p <- p + theme_fivethirtyeight()
  tmp$x = c(cumsum(tmp$pc))
  tmp$pc = scales::percent(round(tmp$pc, 2))
  p <- p + geom_text(data = tmp, aes(x=x, y = project, label = pc))
  p = ggsave(p, filename = filename)
  plan$resource.allocation = tmp
  return(plan)
  
}
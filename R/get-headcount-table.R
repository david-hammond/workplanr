#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
get.headcount.table = function(plan, filename = "./graphs/headcount-table.png"){
  headcount = plan$resources %>% filter(staff != "unassigned") %>%
    group_by(team, contract) %>% summarise(FTE = sum(capacity)/100) %>%
    arrange(desc(FTE)) %>% ungroup() %>% mutate(contract = toupper(contract), team = toupper(team))
  headcount = rbind(headcount, data.frame(team = "SYDNEY", contract = "ALL", "FTE" = sum(headcount$FTE)))
  png(filename, width = 297/2, height = 210/2, units = "mm", res = 200)
  p<-tableGrob(headcount, rows = NULL)
  grid.arrange(p)
  dev.off()
  
  plan$headcount = headcount
  
  return(plan)
}
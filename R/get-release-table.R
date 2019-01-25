#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
get.release.table = function(plan, filename = "./graphs/release-table.png"){
  require(tidyverse)
  require(gridExtra)
  release.table = plan$projects %>% select(project, probability, launch)
  release.table = release.table %>% dplyr::mutate(launch = format(as.Date(launch), "%A, %d-%b")) %>% arrange(launch) 
  release.table$probability = scales::percent(round(release.table$probability, 1), accuracy = 1)
  png(filename, width = 297, height = 210, units = "mm", res = 100)
  p<-tableGrob(release.table, rows = NULL)
  grid.arrange(p)
  dev.off()
  plan$release.table = release.table
  return(plan)
}
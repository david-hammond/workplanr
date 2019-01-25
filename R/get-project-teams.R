#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
get.project.teams = function(plan, filename = "./graphs/project-team-table.png"){
  require(tidyverse)
  require(gridExtra)
  project.teams = plan$teams %>% group_by(project) %>% 
    summarise(team = paste(unique(staff), collapse = ", ")) 
  project.teams = left_join(plan$release.table, project.teams)
  project.leads = plan$teams %>% filter(role == "lead") %>% select(project, staff) %>%
    rename(lead = staff)
  project.teams = left_join(project.teams, project.leads)
  #  project.teams$team = sapply(project.teams$team, pathview::wordwrap, 50)
  png(filename, width = 297, height = 210, units = "mm", res = 100)
  p<-tableGrob(project.teams, rows = NULL)
  grid.arrange(p)
  dev.off()
  plan$project.teams = project.teams
  return(plan)
}
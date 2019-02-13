#' Add staff assignments to schedulke
#'
#' @param workplan schedule
#' @export
get_project_teams = function(project, workplan){
  tmp <- as.data.frame(my_workplan@schedule)
  tmp <- tmp %>% 
    dplyr::filter(staff_contribution > 0, project_name == project) %>%
    dplyr::select(project_name, project_role_name, staff_name) %>%
    dplyr::distinct() %>%
    dplyr::arrange(project_name, project_role_name)
  
  i <- unique(tmp$project_name)
  tmp <- tmp[tmp$project_name == i,]
  j <- tmp$project_role_name[1]
  k <- paste(tmp$staff_name[tmp$project_role_name == j], collapse = ", ")
  i <- paste(j, k, sep = ": ")
  teams <- data.tree::Node$new(i)
  tmp <- tmp %>%
      dplyr::filter(project_role_name != j)
  for (i in unique(tmp$project_role_name)){
    role <- teams$AddChild(i)
    for (j in unique(tmp$staff_name[tmp$project_role_name == i]))
      staff <- role$AddChild(j)
  }
  data.tree::SetNodeStyle(teams,  shape = "box")

  return(teams)
}
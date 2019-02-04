#' Assigns staff roles on a project
#'
#' @param projects List of projects
#' @param phases List of phases in any project in order of execution
#' @param staff Assigned staff to each [project, role] combination (needs to be at least length(project) x length(roles) in length)
#' @param assigned_capacity Amount of time each staff is expected to dedicate to each [project, role] 
#' @param randomise Randomise project assignments
#' combination (needs to be at least length(project) x length(roles) in length)
#' @return A reference table project teams and roles
#' @keywords internal

set_project_team = function(projects, phases, staff, assigned_capacity = 0, 
                            randomise = TRUE){
  
  tmp <- expand.grid(project = projects, 
                     phase = phases,
                     staff = staff,
                     KEEP.OUT.ATTRS = FALSE)
  tmp$assigned_capacity <- ifelse(randomise, sample(x = seq(0,100, by = 10),
                                                   size = nrow(tmp),
                                                   replace = T), assigned_capacity)
  tmp <- team(project = tmp$project, 
              phase = tmp$phase, 
              staff = as.character(tmp$staff),
              assigned_capacity = tmp$assigned_capacity)
  return(tmp)
}


#' creates time estimates for each phases of each project
#'
#' @param projects List of roles in any project team in order of responsibility
#' @param phases List of roles in any project team in order of responsibility
#' @param time_estimates Time estimates of how long each phase will take in relation to project end, negative = phase will occur before project end, positive = phase will occur after project end
#' @return A reference table for time estimates of each phase of each project
#' @keywords internal

set_time_estimates <- function(projects, phases, time_estimates = 10){ 
  
  tmp <- expand.grid(project = projects, phase = phases, KEEP.OUT.ATTRS = FALSE)
  
  tmp$time_estimates = time_estimates
  
  tmp <- time(project = tmp$project, 
              phase = tmp$phase, 
              time_estimate = tmp$time_estimates)
  
  return(tmp)
}

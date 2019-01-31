#' creates time estimates for each phases of each project
#'
#' @param projects List of roles in any project team in order of responsibility
#' @param phases List of roles in any project team in order of responsibility
#' @param days_from_end Time estimates of how long each phase will take in relation to project end, negative = phase will occur before project end, positive = phase will occur after project end
#' @return A reference table for time estimates of each phase of each project
#' @examples 
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' projects <- set_projects(projects, probability, start, end)
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' phases <- set_phases(phases)
#' time_estimates <- rbind(c(-40,-10,-10,-10,-10,10), c(-10,-10,-10,0,0,0), c(0,0,0,-10,-10,10))
#' time_estimates <- set_time_estimates(projects, phases, time_estimates)
#' @export

set_time_estimates <- function(projects, phases, days_from_end){ 
  
  colnames(time_estimates) <- phases
  
  time_estimates <- data.frame(project = projects$project, time_estimates)
  
  .db.save(time_estimates)
  
  return(time_estimates)
}

#' Retrieve time estimates
#'
#' @examples 
#' get_time_estimates()
#' @export
get_time_estimates <- function() {
  
  return(.db.get("time_estimates"))
  
}
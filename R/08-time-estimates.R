#' creates time estimates for each phases of each project
#'
#' @param projects List of roles in any project team in order of responsibility
#' @param phases List of roles in any project team in order of responsibility
#' @param days_from_end Time estimates of how long each phase will take in relation to project end, negative = phase will occur before project end, positive = phase will occur after project end
#' @return A reference table for time estimates of each phase of each project
#' @examples 
#' n <- 3
#' projects <- LETTERS[1:n]
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' time_estimates <- rbind(A = c(-40,-10,-10,-10,-10,10), B = c(-10,-10,-10,0,0,0), C = c(0,0,0,-10,-10,10))
#' time_estimates <- create_time_estimates(projects, phases, time_estimates)
#' @export

create_time_estimates <- function(projects, phases, days_from_end){ 
  colnames(time_estimates) <- phases
  time_estimates <- data.frame(project = projects, time_estimates)
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
#' create a list of phases in any project
#'
#' @param phases List of phases in any project in order of execution
#' @return A reference table for phases 
#' @examples 
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' phases <- set_phases(phases)
#' @export
set_phases <- function(phases) {
  
    phases <- data.frame(phase = factor(phases, levels = phases, ordered = TRUE))
    
    return(phases)
}

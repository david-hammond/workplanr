#' create a list of phases in any project
#'
#' @param phases List of phases in any project in order of execution
#' @return A reference table for phases 
#' @examples 

set_phases <- function(phases) {
  
    phases <- phas(phase = factor(phases, levels = phases, ordered = TRUE))
    
    return(phases)
}

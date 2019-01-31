#' create a list of phases in any project
#'
#' @param phases List of phases in any project in order of execution
#' @return A reference table for phases 
#' @examples 
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' phases <- create_phases(phases)
#' @export
create_phases <- function(phases) {
    phases <- factor(phases, levels = phases, ordered = TRUE)
    
    .db.save(phases)
    
    return(phases)
}

#' Retrieve phase list
#'
#' @examples 
#' get_phases()
#' @export
get_phases <- function() {
  
  return(.db.get("phases"))
  
}

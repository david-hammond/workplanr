#' creates a span responbsibilities of of roles across project phases
#'
#' @param roles List of roles in any project team in order of responsibility
#' @param phases List of roles in any project team in order of responsibility
#' @param responsibilities a binary matrix of if role i is in volved in phase j
#' @return A reference table for responsibilities
#' @keywords internal

set_responsibilities <- function(roles, phases, responsibilities = 1){ 
  
  tmp <- expand.grid(role = roles, phase = phases, KEEP.OUT.ATTRS = FALSE)
  
  tmp <- resp(role = tmp$role, 
              phase = tmp$phase, 
              responsibilities = responsibilities)
  
  return(tmp)
}


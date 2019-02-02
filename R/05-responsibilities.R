#' creates a span responbsibilities of of roles across project phases
#'
#' @param roles List of roles in any project team in order of responsibility
#' @param phases List of roles in any project team in order of responsibility
#' @param responsibilities a binary matrix of if role i is in volved in phase j
#' @return A reference table for responsibilities
#' @examples 
#' roles <- c("lead", "researcher", "editor", "design")
#' roles <- set_roles(roles)
#' phases <- c("research", "drafting", "editing", "design", "print", "events")
#' phases <- set_phases(phases)
#' responsibilities <- rbind(lead = rep(1, length(phases$phase)), 
#' researcher = c(1,1,1,0,0,0), editor = c(0,0,1,0,0,0), design = c(0,0,0,1,1,0))
#' responsibilities <- set_responsibilities(roles, phases, responsibilities)
#' @export
set_responsibilities <- function(roles, phases, responsibilities = 1){ 
  
  tmp <- expand.grid(role = roles, phase = phases, KEEP.OUT.ATTRS = FALSE)
  
  tmp <- resp(role = tmp$role, 
              phase = tmp$phase, 
              responsibilities = responsibilities)
  
  return(tmp)
}


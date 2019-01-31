#' creates a span responbsibilities of of roles across project phases
#'
#' @param roles List of roles in any project team in order of responsibility
#' @param phases List of roles in any project team in order of responsibility
#' @param responsibilities a binary matrix of if role i is in volved in phase j
#' @return A reference table for responsibilities
#' @examples 
#' data("roles")
#' data("phases")
#' responsibilities <- rbind(lead = rep(1, length(phases)), researcher = c(1,1,1,0,0,0), editor = c(0,0,1,0,0,0), design = c(0,0,0,1,1,0))
#' responsibilities <- set_responsibilities(roles, phases, responsibilities)
#' @export
set_responsibilities <- function(roles, phases, responsibilities){ 
  
  colnames(responsibilities) <- phases
  
  responsibilities <- data.frame(role = roles, responsibilities)
  
  .db.save(responsibilities)
  
  return(responsibilities)
}

#' Retrieve responsibilities
#'
#' @examples 
#' get_responsibilities()
#' @export
get_responsibilities <- function() {
  
  return(.db.get("responsibilities"))
  
}
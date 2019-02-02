#' create a list of roles in any project team
#'
#' @param roles List of roles in any project team in order of responsibility
#' @return A reference table for roles
#' @examples 
#' roles <- c("lead", "researcher", "editor", "design")
#' roles <- set_roles(roles)
#' @export
set_roles <- function(roles){

  roles <- data.frame(role = factor(roles, levels = roles, ordered = TRUE))
  
  return(roles)
}


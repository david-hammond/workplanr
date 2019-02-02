#' create a list of roles in any project team
#'
#' @param roles List of roles in any project team in order of responsibility
#' @return A reference table for roles
#' @keywords internal

set_roles <- function(roles){

  roles <- role(role = factor(roles, levels = roles, ordered = TRUE))
  
  return(roles)
}


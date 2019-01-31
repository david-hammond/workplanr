#' create a list of employees that are to be assigned to projects
#'
#' @param staff Names of staff members
#' @param capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @return A reference table for staff
#' @examples 
#' staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
#' capacity <- c(40,60,100,100,100)
#' resources <- set_resources(staff, capacity)
#' @export
set_resources <- function(staff, capacity) {
    
    resources <- data.frame(staff = staff, capacity = capacity)
    
    .db.save(resources)
    
    return(resources)
}

#' Retrieve resource list
#'
#' @examples 
#' get_resources()
#' @export
get_resources <- function() {
  
  return(.db.get("resources"))
  
}

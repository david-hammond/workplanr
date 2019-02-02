#' create a list of employees that are to be assigned to projects
#'
#' @param staff Names of staff members
#' @param capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @return A reference table for staff
#' @keywords internal

set_resources <- function(staff, capacity) {
  resources <- resc(staff = staff, capacity = capacity)
  return(resources)
}


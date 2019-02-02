#' create a list of employees that are to be assigned to projects
#'
#' @param staff Names of staff members
#' @param capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @return A reference table for staff

set_resources <- function(staff, capacity, dummy_resource = "unassigned") {
  resources <- resc(staff = c(staff, dummy_resource), capacity = c(capacity, max(capacity)))
  return(resources)
}


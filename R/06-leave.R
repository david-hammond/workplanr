#' create a list of phases in any project
#'
#' @param phases List of phases in any project in order of execution
#' @return A reference table for phases 
#' @keywords internal

set_leave <- function(staff_on_leave, leave_start, leave_end, leave_description) {
  
    leave <- leav(staff = staff_on_leave, start = leave_start, end = leave_end, description = leave_description)
    
    return(leave)
}

#' create a list of phases in any project
#'
#' @param phases List of phases in any project in order of execution
#' @return A reference table for phases 

set_leave <- function(staff_on_leave, leave_start, leave_end, leave_description) {
  
    leave <- leav(staff = staff.on.leave, start = leave.start, end = leave.end, description = leave_description)
    
    return(leave)
}

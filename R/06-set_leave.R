#' Create an object of class leave
#'
#' @param staff_on_leave Names of staff that are going to be out of the office
#' @param leave_start Starting date for leave
#' @param leave_end Ending date for leave
#' @param leave_description Type of leave, can be user defined but recommend "leave" or "work trip"
#' @return A reference table for staff leave 
#' @keywords internal

set_leave <- function(staff_on_leave, leave_start, leave_end, leave_description) {
  
    leave <- leav(staff = staff_on_leave, start = leave_start, 
                  end = leave_end, description = leave_description)
    
    return(leave)
}

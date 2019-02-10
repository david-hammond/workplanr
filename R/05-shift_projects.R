#' Assign staff member to project phase
#'
#' @param project The name of the database to create 
#' @param by staff name
#' @param new_start_date project name
#' @param new_end_date project_phase_name
#' @param workplan wp
#' @export
shift_project = function(project, by = NULL, new_start_date = NULL, new_end_date = NULL, workplan){
  if(is.null(by) & is.null(new_start_date) & is.null(new_end_date)){
    message("You need to enter either a 'by' value, a 'new_start_date' value or a 'new_end_date' value")
    break()
  }else{
    cals <- bizdays::create.calendar('normal', 
                                     weekdays = c('saturday', 'sunday'), 
                                     holidays = workplan@public_holidays@date,
                                     start.date = min(workplan@schedule@date), 
                                     end.date = max(workplan@schedule@date))
    for (i in 1:length(projects)){
      pos <- which(workplan@projects@project_name == project[i])
      project_duration <- bizdays::bizdays(workplan@projects@project_start[pos],
                                           workplan@projects@project_end[pos],
                                           'normal')
      if(!is.null(by)){
        workplan@projects@project_start[pos] <- bizdays::offset(workplan@projects@project_start, by, 'normal')
        workplan@projects@project_end[pos] <- bizdays::offset(workplan@projects@project_end, by, 'normal')
        bizdays::remove.calendars('normal')
        return(workplan)
      }
      if(!is.null(new_start_date)){
        workplan@projects@project_start[pos] <- new_start_date[i]
        workplan@projects@project_end[pos] <- bizdays::offset(workplan@projects@project_start, project_duration, 'normal')
        bizdays::remove.calendars('normal')
        return(workplan)
      }
      if(!is.null(new_end_date)){
        workplan@projects@project_end[pos] <- new_end_date[i]
        workplan@projects@project_start[pos] <- bizdays::offset(workplan@projects@project_end, -project_duration, 'normal')
        bizdays::remove.calendars('normal')
        return(workplan)
      }
    }
  }
}

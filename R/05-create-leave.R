create.leave = function(resources, projects, n = 3){
  require(bizdays)
  require(lubridate)
  cal <- create.calendar("normal", weekdays = c("saturday", "sunday"))
  leave = data.frame(leave.id = 1:n, staff = sample(resources$staff, size = n, replace = T),
                     start = sample(seq(as.Date(today()), as.Date(today() + 365), by="day"), 
                                    size = n, replace = T))
  leave$end = offset(leave$start, sample(seq(5,15, by = 5), size = n, replace = T), 'normal')
  return(leave)
}
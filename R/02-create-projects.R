create.projects = function(n = 10){
  require(bizdays)
  require(lubridate)
  require(dplyr)
  cal <- create.calendar("normal", weekdays = c("saturday", "sunday"))
  projects <- data.frame(project = sample(LETTERS, size = n, replace = F), 
                         probability = sample(c(0.50, 1, 1, 1, 1), size = n, replace = T),
                         launch = sample(seq(as.Date(today() + 60), as.Date(today() + 365), by="day"), 
                                         size = n, replace = T))
  projects = projects %>% arrange(launch) %>% mutate(project = factor(project, project, ordered = T))
  return(projects)                       
}

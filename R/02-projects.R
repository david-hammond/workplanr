#' create a list of employees that are to be assigned to projects
#'
#' @param projects Names of projects
#' @param probability Probability that the project will go ahead
#' @param start Expected start date of project
#' @param end Expected end date of the project
#' @return A reference table for projects 
#' @examples 
#' projects <- LETTERS[1:3]
#' probability <- c(50, 100, 100)
#' start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
#' end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
#' projects <- set_projects(projects, probability, start, end)
#' @export
set_projects <- function(projects, probability = 1, start, end) {
    require(dplyr)
  
    projects <- data.frame(project = projects, probability = probability, start = start, end = end)
    
    projects <- projects %>% arrange(start) %>% mutate(project = factor(project, project, ordered = TRUE))
    
    .db.save(projects)
    
    return(projects)
}

#' Retrieve projects list
#'
#' @examples 
#' get_projects()
#' @export
get_projects <- function() {
  
  return(.db.get("projects"))
  
}

#' create a list of employees that are to be assigned to projects
#'
#' @param projects Names of projects
#' @param probability Probability that the project will go ahead
#' @param start Expected start date of project
#' @param end Expected end date of the project
#' @return A reference table for projects 
#' @examples 
#' library(lubridate)
#' n <- 3
#' projects <- LETTERS[1:n]
#' probability <- sample(c(50, 75, 100), size = n, replace = TRUE)
#' start <- sample(seq(as.Date(today() + 60), as.Date(today() + 365), by='day'), size = n, replace = FALSE) 
#' end <- start + sample(c(30,40,50,60), size = n, replace = TRUE)
#' projects <- create_projects(projects, probability, start, end)
#' @export
create_projects <- function(projects, probability = 1, start, end) {
    
    projects <- data.frame(project = projects, probaility = probability, start = start, end = end)
    
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

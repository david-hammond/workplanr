#' create a list of employees that are to be assigned to projects
#'
#' @param projects Names of projects
#' @param probability Probability that the project will go ahead
#' @param start Expected start date of project
#' @param end Expected end date of the project
#' @return A reference table for projects 
#' @examples 

set_projects <- function(projects, probability = 1, start, end, plan = new("workplan")) {
    projects <- data.frame(project = projects, 
                           probability = probability, 
                           start = start, end = end)
    
    projects <- projects %>% 
      dplyr::arrange(start) %>% 
      dplyr::mutate(project = factor(project, project, ordered = TRUE))
    
    projects <- proj(project = projects$project, 
                     probability = projects$probability,
                     start = projects$start,
                     end = projects$end)
    
    return(projects)
}

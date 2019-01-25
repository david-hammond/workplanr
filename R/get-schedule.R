#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
get.schedule <- function(schedule){
  require(tidyverse)
  require(pbapply)
  
  tmp = schedule %>% dplyr::filter(!is.na(launch)) %>%
    gather("phase", "days", -c(project, launch, kickoff, probability))
  
  tmp = split(tmp, factor(tmp$project))
  
  tmp = pblapply(tmp, backcast)
  
  tmp = bind_rows(tmp) %>% group_by(project) %>% mutate(final = max(end)) %>% 
    mutate(task = paste(project, phase, sep = ": ")) %>%
    arrange(final)
  
  return(as.data.frame(tmp))
}


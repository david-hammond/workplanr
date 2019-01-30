create.time.estimates <- function(projects, phases){
  require(tidyr)
  estimates <- expand.grid(project = projects$project, phases = phases$phase, 
                           value = -10) %>% spread(phases, value)
  estimates[,ncol(estimates)] = -estimates[,ncol(estimates)]
  return(estimates)
}
create.responsibilities <- function(roles, phases){
  require(tidyr)
  responsibilities <- expand.grid(role = roles$role, phases = phases$phase, value = 1) %>% 
    spread(phases, value)
  return(responsibilities)
}
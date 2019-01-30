create.roles <- function(){
  require(dplyr)
  roles <- data.frame(role = c("lead", "researcher", "editor", "design")) %>%
    mutate(role = factor(role, role, ordered = T))
  return(roles)                       
}
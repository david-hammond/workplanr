#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
create.project.plan <- function(plan, my.plan){
  plan$teams = expand.grid(project = plan$projects$project, staff = plan$resources$staff, 
                           role = NA, workshare = NA, KEEP.OUT.ATTRS = F) %>% arrange(project, staff) 
  plan$responsibilities = expand.grid(roles = plan$roles$roles, 
                                      phases = plan$phases$phases, 
                                      value = NA, KEEP.OUT.ATTRS = F) %>% 
    spread(phases, value)
  plan$time.estimates = expand.grid(project = plan$projects$project, 
                                    phases = plan$phases$phases, 
                                    value = NA, KEEP.OUT.ATTRS = F) %>% 
    spread(phases, value)
  export(plan$teams, file = my.plan, which = "teams")
  export(plan$responsibilities, file = my.plan, which = "responsibilities")
  export(plan$time.estimates, file = my.plan, which = "time.estimates")
  
  return(plan)
}

#' Creates a randomised project management problem
#'
#' @param nothing
#'
#'
#'
#' @export
create.random.project = function(){
  resources <<- create.resources()
  projects <<- create.projects()
  project.phases <<- create.phases()
  roles <<- create.roles()
  public.holidays <<- create.public.holidays()
  
  leave <<- create.leave(resources, projects)
  responsibilities <<- create.responsibilities(roles, project.phases)
  time.estimates <<- create.time.estimates(projects, project.phases)
  project.teams <<- create.project.teams(resources, 
                                         projects, 
                                         roles, 
                                         time.estimates, 
                                         responsibilities,
                                         randomise = T)
  #project.teams$staff[1:8] = NA
  #project.teams$assigned_capacity[1:8] = NA
  daily.plan <<- get.daily.plan(resources, projects, time.estimates, public.holidays, 
                                  project.teams, responsibilities, leave)
  daily.staff.plan <<- get.daily.staff.plan(daily.plan)
  daily.team.plan <<- get.team.daily.plan(daily.plan)
}
























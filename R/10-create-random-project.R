#' Creates a randomised project management problem
#'
#' @param nothing
#'
#'
#'
#' @export
create.random.project = function(){
  project <<- create.project.template()
  project <<- create.project.plan(project)
  daily.plan <<- get.daily.plan(project$resources, project$projects, project$time.estimates, 
                                project$public.holidays, 
                                project$project.teams, project$responsibilities, project$leave)
  daily.staff.plan <<- get.daily.staff.plan(daily.plan)
  daily.team.plan <<- get.team.daily.plan(daily.plan)
}



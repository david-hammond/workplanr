#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
project.vignette = function(){
  workplanr::create.random.project()
  print(workplanr::staff.calendar(daily.plan, project$leave, project$public.holidays))
  workplanr::team.workload(daily.plan)
}

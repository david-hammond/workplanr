#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
project.vignette = function(){
  workplanr::create.random.project()
  print(workplanr::staff.calendar(daily.plan, leave, public.holidays))
  workplanr::team.workload(daily.plan)
}

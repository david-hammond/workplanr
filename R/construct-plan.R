#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
construct.plan = function(plan, my.plan = "my-plan2.xlsx"){
  for (i in names(plan)){
    plan[[i]] = import(my.plan, which = i) #lost factors
  }
  
  #Need to test projects have lead
  
  plan$teams = plan$teams %>% filter(!is.na(role))
  plan$schedule = left_join(plan$projects, plan$time.estimates)
  
  plan$schedule = get.schedule(plan$schedule)
  
  plan$daily.schedule = get.daily.schedule(plan$schedule)
  
  tmp = plan$responsibilities %>% gather("phase", "value", -role) %>% 
    filter(!is.na(value)) %>% select(-value)
  plan$assignments = left_join(plan$daily.schedule, tmp)
  plan$assignments = left_join(plan$assignments, plan$teams) %>% filter(!is.na(staff))
  plan$assignments = left_join(plan$assignments, plan$resources)
  
  return(plan)
}
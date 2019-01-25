#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
load.project.inputs = function(filename = "./data/base-input.xlsx"){
  #TODO: Make RData
  require(rio)
  require(lubridate)
  require(tidyverse)
  plan = list()
  plan$resources = import(filename, which = "resources") %>% arrange(staff)
  plan$resources$staff = factor(plan$resources$staff, plan$resources$staff, ordered = T)
  plan$projects = import(filename, which = "projects") %>% 
    mutate(kickoff = as.POSIXct(ymd(kickoff)),
           launch = as.POSIXct(ymd(launch))) %>%
    arrange(launch)
  plan$projects$project = factor(plan$projects$project, plan$projects$project, ordered = T)
  plan$phases = import(filename, which = "phases")
  plan$roles = import(filename, which = "roles")
  plan$phases$phases = factor(plan$phases$phases, 
                              plan$phases$phases[order(plan$phases$dependencies)], 
                              ordered = T)
  plan$leave = import(filename, which = "leave") %>% mutate(start = as.POSIXct(ymd(start)),
                                                            end = as.POSIXct(ymd(end)))
  plan$holidays = import(filename, which = "public holidays")
  plan$holidays = plan$holidays %>% 
    mutate(holiday.dates = as.POSIXct(dmy(holiday.dates)))
  my.plan = "my-plan.xlsx"
  if(file.exists(my.plan)){
    file.remove(my.plan)
  }
  for (i in names(plan)){
    export(plan[[i]], my.plan, which = i)
  }
  return(plan)
}
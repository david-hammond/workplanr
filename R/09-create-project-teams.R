assign.staff <- function(tmp, resources){
  assignments <- NULL
  question = paste0("Do you want to add staff to Project ", tmp$project[1], "?")
  add = menu(c("yes", "no"), title = question)
  while(add == 1){
    question = paste("Who do you want to add to Project", tmp$project[1])
    person = menu(resources$staff, title = question)
    question = "and what will they be doing?"
    role = menu(tmp$role, title = question)
    question = paste("and how much of their time will be devoted to ", tmp$project[1], "?")
    capacities = seq(1,0.25, by=-.25)
    capacity = menu(percent(capacities, accuracy = 1), title = question)
    assignments <- rbind(assignments, data.frame(project = tmp$project[1], role = roles$role[role],
                                                 staff = resources$staff[person], assigned_capacity = capacities[capacity]))
    question = paste0("Do you want to add staff to Project ", tmp$project[1], "?")
    add = menu(c("yes", "no"), title = question)
  }
  if(is.null(assignments)){
    assignments <- data.frame(tmp, staff = NA, assigned_capacity = NA)
  }else if(nrow(assignments) < nrow(tmp)){
    assignments = left_join(tmp, assignments)
  }
  return(assignments)
}
create.project.teams <- function(resources, projects, roles, time.estimates, 
                                 responsibilities, randomise = F){
  require(tidyr)
  require(dplyr)
  require(scales)
  project.phases = time.estimates %>% gather("phase", "time", - project) %>%
    filter(time > 0)
  project.roles = responsibilities %>% gather("phase", "needed", -role) %>%
    filter(needed == 1)
  project.team = left_join(project.phases, project.roles) %>% select(project, role) %>%
    distinct()
  project.team = left_join(project.team, roles)
  if(randomise){
    project.team$staff = sample(resources$staff, size = nrow(project.team), replace = T)
    project.team$assigned_capacity = sample(seq(1,0.25, by=-.25), size = nrow(project.team), replace = T)
  }else{
    project.team = split(project.team, project.team$project)
    project.team = lapply(project.team, assign.staff, resources)
    project.team = bind_rows(project.team)
  }
  
  return(project.team)
}
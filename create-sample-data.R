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
  my.staff.plan <<- create.work.plan(resources, projects, time.estimates, public.holidays, 
                                  project.teams, responsibilities, leave)
}



create.resources <- function(n = 5){
  require(randomNames)
  resources <- data.frame(staff = randomNames(n), 
                          capacity = sample(c(50, 100, 100, 100, 100)/100, size = n, replace = T),
                          team = sample(LETTERS[1:3], size = n, replace = T),
                          contract = sample(c("FTE", "INTERN"), size = n, replace = T))
  return(resources)                       
}

create.projects <- function(n = 10){
  require(bizdays)
  require(lubridate)
  require(dplyr)
  cal <- create.calendar("normal", weekdays = c("saturday", "sunday"))
  projects <- data.frame(project = sample(LETTERS, size = n, replace = F), 
                          probability = sample(c(0.50, 1, 1, 1, 1), size = n, replace = T),
                          launch = sample(seq(as.Date(today() + 60), as.Date(today() + 365), by="day"), 
                                           size = n, replace = T))
  projects = projects %>% arrange(launch) %>% mutate(project = factor(project, project, ordered = T))
  return(projects)                       
}

projects = create.projects()

create.phases <- function(){

  phases <- data.frame(phase = c("research", "drafting", "editing", "design", "print", "launch-events"), 
                         sequencing = 1:6) %>% mutate(phase = factor(phase, phase[sequencing], ordered = T))
  return(phases)                       
}

create.roles <- function(){
  require(dplyr)
  roles <- data.frame(role = c("lead", "researcher", "editor", "design")) %>%
    mutate(role = factor(role, role, ordered = T))
  return(roles)                       
}

create.leave = function(resources, projects, n = 3){
  require(bizdays)
  require(lubridate)
  cal <- create.calendar("normal", weekdays = c("saturday", "sunday"))
  leave = data.frame(leave.id = 1:n, staff = sample(resources$staff, size = n, replace = T),
                     start = sample(seq(as.Date(today()), as.Date(today() + 365), by="day"), 
                                    size = n, replace = T))
  leave$end = offset(leave$start, sample(seq(5,15, by = 5), size = n, replace = T), 'normal')
  return(leave)
}

create.public.holidays <- function(file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"){
  require(lubridate)
  require(dplyr)
  public.holidays = read.csv(file)
  public.holidays$Date = as.Date(ymd(public.holidays$Date))
  public.holidays = public.holidays %>% filter(Jurisdiction == "nsw") %>%
    select(Date, Holiday.Name, Information)
  names(public.holidays) = tolower(names(public.holidays))
  return(public.holidays)
}

create.responsibilities <- function(roles, phases){
  require(tidyr)
  require(editData)
  responsibilities <- expand.grid(role = roles$role, phases = phases$phase, value = 1) %>% 
    spread(phases, value)
  return(responsibilities)
}

create.time.estimates <- function(projects, phases){
  require(tidyr)
  estimates <- expand.grid(project = projects$project, phases = phases$phase, 
                           value = -10) %>% spread(phases, value)
  estimates[,ncol(estimates)] = -estimates[,ncol(estimates)]
  return(estimates)
}

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



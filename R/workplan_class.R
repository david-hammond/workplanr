resc <- setClass("resources", slots = c(staff="character", capacity = "numeric"))

setMethod("as.data.frame", "resources", function(x){
  x <- data.frame(staff = x@staff, capacity = x@capacity)
  return(x)
  })

proj <- setClass("projects", slots = c(project="ordered", probability="numeric", start = "Date", end = "Date"))

setMethod("as.data.frame", "projects", function(x){
  x <- data.frame(project = x@project, probability = x@probability, start = x@start, end = x@end)
  return(x)
})

phas <- setClass("phases", slots = c(phase="ordered"))

setMethod("as.data.frame", "phases", function(x){
  x <- data.frame(phase = x@phase)
  return(x)
})

role <- setClass("roles", slots = c(role="ordered"))

setMethod("as.data.frame", "roles", function(x){
  x <- data.frame(role = x@role)
  return(x)
})

resp <- setClass("responsibilities", slots = c(role ="ordered", phase ="ordered", responsibilities ="numeric"))

setMethod("as.data.frame", "responsibilities", function(x){
  x <- data.frame(role = x@role, phase = x@phase, responsibilities = x@responsibilities)
  x <- x %>% tidyr::spread(phase, responsibilities)
  return(x)
})

time <- setClass("time_estimates", slots = c(project = "ordered", phase = "ordered", time_estimate = "numeric"))

setMethod("as.data.frame", "time_estimates", function(x){
  x <- data.frame(project = x@project, phase = x@phase, time_estimate = x@time_estimate)
  x <- x %>% tidyr::spread(phase, time_estimate)
  return(x)
})

team <- setClass("project_teams", slots = c(project = "ordered", role = "ordered", assigned_staff = "character", assigned_capacity = "numeric"))

setMethod("as.data.frame", "project_teams", function(x){
  x <- data.frame(project = x@project, role = x@role, assigned_staff = x@assigned_staff, assigned_capacity = x@assigned_capacity)
  return(x)
})

workplan <- setClass("workplan", slots =  list(resources = "resources", 
                                               projects = "projects", 
                                               phases = "phases",
                                               roles = "roles",
                                               responsibilities = "responsibilities",
                                               time_estimates = "time_estimates",
                                               project_teams = "project_teams"))

setMethod("as.list", "workplan", function(wp){
  tmp = list(
    resources = as.data.frame(wp@resources),
    projects = as.data.frame(wp@projects),
    phases = as.data.frame(wp@phases),
    roles = as.data.frame(wp@roles),
    responsibilities = as.data.frame(wp@responsibilities),
    time_estimates = as.data.frame(wp@time_estimates),
    project_teams = as.data.frame(wp@project_teams))
  return(tmp)
})


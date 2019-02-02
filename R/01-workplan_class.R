resc <- setClass("resources", slots = c(staff="character", capacity = "numeric"))

setMethod("as.data.frame", "resources", definition = function(x){
  x <- data.frame(staff = x@staff, capacity = x@capacity)
  return(x)
  })

proj <- setClass("projects", slots = c(project="ordered", probability="numeric", start = "Date", end = "Date"))

setMethod("as.data.frame", "projects", definition = function(x){
  x <- data.frame(project = x@project, probability = x@probability, start = x@start, end = x@end)
  return(x)
})

phas <- setClass("phases", slots = c(phase="ordered"))

setMethod("as.data.frame", "phases", definition = function(x){
  x <- data.frame(phase = x@phase)
  return(x)
})

role <- setClass("roles", slots = c(role="ordered"))

setMethod("as.data.frame", "roles", definition = function(x){
  x <- data.frame(role = x@role)
  return(x)
})

resp <- setClass("responsibilities", slots = c(role ="ordered", phase ="ordered", responsibilities ="numeric"))

setMethod("as.data.frame", "responsibilities", definition = function(x){
  x <- data.frame(role = x@role, phase = x@phase, responsibilities = x@responsibilities)
  x <- x %>% tidyr::spread(phase, responsibilities)
  return(x)
})

time <- setClass("time_estimates", slots = c(project = "ordered", phase = "ordered", time_estimate = "numeric"))

setMethod("as.data.frame", "time_estimates", definition = function(x){
  x <- data.frame(project = x@project, phase = x@phase, time_estimate = x@time_estimate)
  x <- x %>% tidyr::spread(phase, time_estimate)
  return(x)
})

team <- setClass("project_teams", slots = c(project = "ordered", role = "ordered", staff = "character", assigned_capacity = "numeric"))

setMethod("as.data.frame", "project_teams", definition = function(x){
  x <- data.frame(project = x@project, role = x@role, staff = x@staff, assigned_capacity = x@assigned_capacity)
  return(x)
})

workplan <- setClass("workplan", slots =  list(resources = "resources", 
                                               projects = "projects", 
                                               phases = "phases",
                                               roles = "roles",
                                               responsibilities = "responsibilities",
                                               time_estimates = "time_estimates",
                                               project_teams = "project_teams"))

setMethod("as.list", "workplan", definition = function(x){
  x = list(
    resources = as.data.frame(x@resources),
    projects = as.data.frame(x@projects),
    phases = as.data.frame(x@phases),
    roles = as.data.frame(x@roles),
    responsibilities = as.data.frame(x@responsibilities),
    time_estimates = as.data.frame(x@time_estimates),
    project_teams = as.data.frame(x@project_teams))
  return(x)
})

#' Create Excel file for project inputs
#' 
#' This function creates an excel file that can be used to create a new project
#' @param plan File name for project inputs
#' @param excel_file_name File name for project inputs
#' @return NULL
#' @examples 
#' create_project_file()
#' @export 
create_project_file = function(plan = utils::data("example_project", envir = environment()), excel_file_name = "my_project.xlsx"){
  for (i in names(plan)){
    rio::export(example_project[[i]], file = excel_file_name, which = i)
  }
  return(NULL)
}


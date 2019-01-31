#' #' ggplot colours
#' #'
#' #' @param p a ggplot
#' #'
#' #'
#' #'
#' #' @export
#' 
#' create.project.template = function(file = 'my-project-template-inputs.xlsx'){
#'   project = list()
#'   project$resources <- create.resources()
#'   project$projects <- create.projects()
#'   project$project.phases <- create.phases()
#'   project$roles <- create.roles()
#'   project$public.holidays <- create.public.holidays()
#'   export.xlsx(project, file)
#'   return(project)
#' }
#' 
#' create.project.plan = function(project, file = 'my-project-template-all-plan.xlsx'){
#'   project$leave <- create.leave(project$resources, project$projects)
#'   project$responsibilities <- create.responsibilities(project$roles, project$project.phases)
#'   project$time.estimates <- create.time.estimates(project$projects, project$project.phases)
#'   project$project.teams <- create.project.teams(project$resources, 
#'                                               project$projects, 
#'                                               project$roles, 
#'                                               project$time.estimates, 
#'                                               project$responsibilities,
#'                                               randomise = T)
#'   export.xlsx(project, file = file)
#'   return(project)
#' }
#' 
#' import.xlsx = function(file){
#'   require(openxlsx)
#'   wb = loadWorkbook(file)
#'   i = openxlsx::sheets(wb)
#'   bwb = lapply(i, function(i) readWorkbook(wb, sheet = i))
#'   names(wb) = i
#'   return(wb)
#' }
#' 
#' export.xlsx = function(wb.list, file){
#'   require(rio)
#'   sheetnames = names(wb.list)
#'   file.remove(file)
#'   sapply(sheetnames, function(i) export(wb.list[[i]], file, which = i))
#' }

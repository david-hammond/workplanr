#' Create Excel file for project inputs
#' 
#' This function creates an excel file that can be used to create a new project
#' @param wp
#' @param excel_file_name File name for project inputs
#' @return NULL
#' @examples 
#' library(workplanr)
#' @export 
export_workplan = function(wp, excel_file_name = "my_project.xlsx"){
  tmp = as.list(wp)
  tmp$time_estimates <- tmp$time_estimates %>% tidyr::spread(phase, time_estimate)
  tmp$project_teams <- tmp$project_teams %>% tidyr::spread(phase, assigned_capacity)
  if(file.exists(excel_file_name)){
    file.remove(excel_file_name)
  }
  for (i in names(tmp)){
    rio::export(wp[[i]], file = excel_file_name, which = i)
  }
  return(NULL)
}

#' Create Excel file for project inputs
#' 
#' This function creates an excel file that can be used to create a new project
#' @param excel_file_name File name for project inputs
#' @return NULL
#' @examples 
#' library(workplanr)
#' @export 
import_workplan = function(excel_file_name = "my_project.xlsx"){
  tmp <- read_excel_allsheets(excel_file_name)
  tmp$projects <- tmp$projects %>% dplyr(arrange(start))
  tmp$time_estimates <- tmp$time_estimates %>% 
    tidyr::gather(phase, time_estimate, -project)
  tmp$project_teams <- tmp$project_teams %>% 
    tidyr::gather(phase, assigned_capacity, -c(project,staff))
  wp = new("workplan")
  tmp <- correct_classes(staff = tmp$resources$staff, 
                         staff_capacity = tmp$resources$capacity, 
                         projects = tmp$projects$project, 
                         project_probability = tmp$projects$probability, 
                         project_start = tmp$projects$start, 
                         project_end = tmp$projects$end, 
                         project_phases = tmp$phases$phase, 
                         project_time_estimates = tmp$time_estimates$time_estimate, 
                         staff_on_leave = tmp$leave$staff, 
                         leave_start = tmp$leave$start, 
                         leave_end = tmp$leave$end, 
                         leave_description = tmp$leave$description, 
                         public_holidays_date = tmp$holidays$date, 
                         public_holidays_name = tmp$holidays$name,
                         staff_project_assigned_capacity = tmp$project_teams$assigned_capacity)
  
  wp <- init_workplan(tmp, randomise = FALSE)
  wp <- get_workplan(wp)
  return(wp)
  
}

#' Create Excel file for project inputs
#' 
#' This function creates an excel file that can be used to create a new project
#' @param excel_file_name File name for project inputs
#' @return NULL
#' @examples 
#' library(workplanr)
#' @keywords internal
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


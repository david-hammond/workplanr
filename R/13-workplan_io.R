#' Create Excel file for project inputs
#' 
#' This function creates an excel file that can be used to create a new project
#' @param wp A workplan object
#' @param excel_file_name File name for project inputs
#' @return NULL
#' @examples 
#' library(workplanr)
#' wp <- build_sample_workplan()
#' ## ----export_workplan, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' export_workplan(wp, excel_file_name = "my-workplan.xlsx")
#' ## ----import_workplan, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' wp <- import_workplan(excel_file_name = "my-workplan.xlsx")
#' @export 
export_workplan = function(wp, excel_file_name = "my_workplan.xlsx"){
  tmp = as.list(wp)
  tmp$time_estimates <- tmp$time_estimates %>% tidyr::spread(phase, time_estimate)
  tmp$project_teams <- tmp$project_teams %>% tidyr::spread(phase, assigned_capacity)
  if(file.exists(excel_file_name)){
    file.remove(excel_file_name)
  }
  for (i in names(tmp)){
    rio::export(tmp[[i]], file = excel_file_name, which = i)
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
#' wp <- build_sample_workplan()
#' ## ----export_workplan, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' export_workplan(wp, excel_file_name = "my-workplan.xlsx")
#' ## ----import_workplan, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' wp <- import_workplan(excel_file_name = "my-workplan.xlsx")
#' @export 
import_workplan = function(excel_file_name = "my_workplan.xlsx"){
  tmp <- read_excel_allsheets(excel_file_name)
  tmp$projects <- tmp$projects %>% dplyr::arrange(start)
  tmp$time_estimates <- tmp$time_estimates %>% 
    tidyr::gather(phase, time_estimate, -project)
  tmp$project_teams <- tmp$project_teams %>% 
    tidyr::gather(phase, assigned_capacity, -c(project,staff))
  wp = new("workplan")
  wp <- get_workplan(staff = tmp$resources$staff, 
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
                     staff_project_assignment_capacity = tmp$project_teams$assigned_capacity)
  return(wp)
  
}

#' Create Excel file for project inputs
#' 
#' This function creates an excel file that can be used to create a new project
#' @param excel_file_name File name for project inputs
#' @param tibble Read in a tibble?
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

#' Create a random workplan
#' 
#' This function creates an excel file that can be used to create a new project
#' @param excel_file_name File name for project inputs
#' @return NULL
#' @examples 
#' library(workplanr)
#' @export 
build_sample_workplan = function(excel_file_name = "my-workplan.xlsx"){
  staff <- c("Shelby", "Luis", "Taishawn", "Samantha", "Taylor", "unassigned")
  staff_capacity <- c(40,60,100,100,100, 100)
  projects <- LETTERS[1:3]
  project_probability <- c(50, 100, 100)
  project_start <- as.Date(c("2019-01-25", "2019-05-17", "2019-06-27"))
  project_end <- as.Date(c("2019-06-03", "2019-06-16", "2019-09-27"))
  project_phases <- c("research", "drafting", "editing", "design", "print", "events")
  project_time_estimates  <- c(c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,10,10,10))
  staff_on_leave <- c("Luis", "Samantha")
  leave_start <-  as.Date(c("2019-07-23", "2019-05-16"))
  leave_end <- leave_start + c(20, 25)
  leave_description <- c("leave", "work trip")
  url <- "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"
  public_holidays <- utils::read.csv(url, stringsAsFactors = FALSE)
  names(public_holidays) <- tolower(names(public_holidays))
  public_holidays$date <- as.Date(lubridate::ymd(public_holidays$date))
  public_holidays <- public_holidays %>% dplyr::filter(jurisdiction == "nsw") %>%
    dplyr::select(date, holiday.name) %>% dplyr::rename(name = holiday.name)
  public_holidays_date <- public_holidays$date
  public_holidays_name = public_holidays$name
  #staff project assignments
  staff_project_assignment_capacity <- sample(c(0,25,50,75,100), size = length(projects)*length(project_phases)*length(staff), replace = TRUE)
  wp <- get_workplan(staff = staff, 
                     staff_capacity = staff_capacity,
                     projects = projects, 
                     project_probability =  project_probability, 
                     project_start = project_start, 
                     project_end = project_end, 
                     project_phases = project_phases, 
                     project_time_estimates = project_time_estimates, 
                     staff_on_leave = staff_on_leave, 
                     leave_start = leave_start, 
                     leave_end = leave_end, 
                     leave_description = leave_description, 
                     public_holidays_date = public_holidays_date,
                     public_holidays_name = public_holidays_name,
                     staff_project_assignment_capacity = staff_project_assignment_capacity)
  export_workplan(wp, excel_file_name)
  message("Opening setup vignette and writing sample workplan to my-workplan.xlsx, please use this to enter and manage your project information")
  message("Happy Planning!")
  return(wp)
}


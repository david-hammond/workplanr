#' Create Excel file for project inputs
#' 
#' This function creates an excel file that can be used to create a new project
#' @param db_name A workplan object
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
export_workplan_to_xlsx = function(db_name = "my_workplan.sqlite", excel_file_name = "my_workplan.xlsx"){
  if(file.exists(excel_file_name)){
    file.remove(excel_file_name)
  }
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  rio::export(get_staff(db_name), file = excel_file_name, which = "staff")
  rio::export(get_projects(db_name), file = excel_file_name, which = "projects")
  rio::export(get_project_phases(db_name), file = excel_file_name, which = "project_phases")
  rio::export(get_out_of_office(db_name), file = excel_file_name, which = "out_of_office")
  rio::export(get_public_holidays(db_name), file = excel_file_name, which = "public_holidays")
  tmp <- get_time_estimates(db_name)
  phases <- get_project_phases(db_name)
  phases <- phases$project_phase_name
  tmp$project_phase_name <- factor(tmp$project_phase_name, levels = phases, ordered = T)
  tmp <- tmp %>% tidyr::spread(project_phase_name, time_estimate)
  rio::export(tmp, file = excel_file_name, which = "time_estimates")
  tmp <- get_project_assignments(db_name)
  phases <- get_project_phases(db_name)
  phases <- phases$project_phase_name
  tmp$project_phase_name <- factor(tmp$project_phase_name, levels = phases, ordered = T)
  tmp <- tmp %>% tidyr::spread(project_phase_name, staff_contribution)
  rio::export(tmp, file = excel_file_name, which = "project_assignments")
  rio::export(get_calendar(db_name), file = excel_file_name, which = "calendar")
  RSQLite::dbDisconnect(con)
  return(TRUE)
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
import_workplan_from_xlsx = function(db_name = "my_workplan.sqlite", excel_file_name = "my_workplan.xlsx"){
  tmp <- read_excel_allsheets(excel_file_name)
  tmp$time_estimates <- format_time_estimates(tmp$time_estimates)
  tmp <- normalise_time_estimates(tmp)
  tmp$project_assignments <- format_project_assignments(tmp$project_assignments)
  tmp <- normalise_project_assignments(tmp)
  tmp <- normalise_out_of_office(tmp)
  
  create_new_workplan_db(staff = tmp$staff,
                         projects = tmp$projects,
                         calendar = tmp$calendar,
                         project_phases = tmp$project_phases,
                         out_of_office = tmp$out_of_office,
                         public_holidays = tmp$public_holidays,
                         time_estimates = tmp$time_estimates,
                         project_assignments = tmp$project_assignments,
                         db_name = db_name)
  return(TRUE)
  
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

format_time_estimates <- function(tmp){
  tmp <- tmp %>%
    tidyr::gather(project_phase_name, time_estimate, -c(project_name, project_start, project_end, project_confirmed))
  return(tmp)
}

format_project_assignments <- function(tmp){
  tmp <- tmp %>%
    tidyr::gather(project_phase_name, staff_contribution, -c(project_name, staff_name, staff_capacity)) %>%
    dplyr::select(c(-staff_capacity))
  return(tmp)
}

normalise_time_estimates = function(tmp){
  rs <-  tmp$time_estimates
  rs <- dplyr::left_join(rs, tmp$projects %>% select(id_project, project_name)) %>%
    dplyr::select(-project_name)
  rs <- dplyr::left_join(rs, tmp$project_phases %>% select(id_project_phase, project_phase_name)) %>%
    dplyr::select(-project_phase_name)
  rs <- rs %>% dplyr::select(-c(project_start, project_end, project_confirmed))
  tmp$time_estimates <- rs
  return(tmp)
}

normalise_project_assignments = function(tmp){
  rs <-  tmp$project_assignments 
  rs <- dplyr::left_join(rs, tmp$projects %>% select(id_project, project_name)) %>%
    dplyr::select(-project_name)
  rs <- dplyr::left_join(rs, tmp$project_phases %>% select(id_project_phase, project_phase_name)) %>%
    dplyr::select(-project_phase_name)
  rs <- dplyr::left_join(rs, tmp$staff %>% select(id_staff, staff_name)) %>%
    dplyr::select(-staff_name)
  tmp$project_assignments <- rs
  return(tmp)
}

normalise_out_of_office = function(tmp){
  rs <- tmp$out_of_office
  rs <- dplyr::left_join(rs, tmp$staff %>% select(id_staff, staff_name)) %>%
    dplyr::select(-staff_name)
  tmp$out_of_office <- rs
  return(tmp)
}

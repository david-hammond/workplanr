#' create a list of employees that are to be assigned to projects
#'
#' @param staff Names of staff members
#' @param staff_capacity Number of units of work per staff, for example 100 for full time equivalents, 40 for staff who work only 2 days per week
#' @param projects Names of projects
#' @param project_probability Probability that the project will go ahead
#' @param project_start Expected start date of project
#' @param project_end Expected end date of the project
#' @param project_phases List of phases in any project in order of execution
#' @param project_time_estimates Time estimates of how long each phase will take in relation 
#' @param staff_on_leave Names of staff that are going to be out of the office
#' @param leave_start Starting date for leave
#' @param leave_end Ending date for leave
#' @param public_holidays_date A data frame of dates of public holidays
#' @param public_holidays_name A data frame of names of public holidays
#' @param leave_description Type of leave, can be user defined but recommend "leave" or "work trip"
#' @param staff_project_assignment_capacity Amount of time each staff is expected to dedicate to each [project, phase] 
#' @return A reference table for staff
#' @examples 
#' ## ----library, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' library(workplanr)
#' ## ----resources, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' staff <- c("Shelby", "Luis", "Taishawn", "Samantha", "Taylor", "unassigned")
#' staff_capacity <- c(40,60,100,100,100, 100)
#' ## ----projects, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' projects <- LETTERS[1:3]
#' project_probability <- c(50, 100, 100)
#' project_start <- as.Date(c("2019-01-25", "2019-05-17", "2019-06-27"))
#' project_end <- as.Date(c("2019-06-03", "2019-06-16", "2019-09-27"))
#' project_phases <- c("research", "drafting", "editing", "design", "print", "events")
#' project_time_estimates  <- c(c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,10,10,10)) 
#' ## ----leave, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' staff_on_leave <- c("Luis", "Samantha")
#' leave_start <-  as.Date(c("2019-07-23", "2019-05-16"))
#' leave_end <- leave_start + c(20, 25)
#' leave_description <- c("leave", "work trip")
#' ## ----holidays, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' url <- "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"
#' public_holidays <- utils::read.csv(url, stringsAsFactors = FALSE)
#' names(public_holidays) <- tolower(names(public_holidays))
#' public_holidays$date <- as.Date(lubridate::ymd(public_holidays$date))
#' public_holidays <- public_holidays %>% filter(jurisdiction == "nsw") %>%
#'   select(date, holiday.name) %>% rename(name = holiday.name)
#' public_holidays_date <- public_holidays$date
#' public_holidays_name = public_holidays$name
#' ## ----assignments, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' staff_project_assignment_capacity <- sample(c(0,25,50,75,100), size = length(projects)*length(project_phases)*length(staff), replace = TRUE)
#' ## ----get_workplan, include = TRUE, results='hide', message=FALSE, warning=FALSE----
#' wp <- get_workplan(staff = staff, 
#'                    staff_capacity = staff_capacity,
#'                    projects = projects, 
#'                    project_probability =  project_probability, 
#'                    project_start = project_start, 
#'                    project_end = project_end, 
#'                    project_phases = project_phases, 
#'                    project_time_estimates = project_time_estimates, 
#'                    staff_on_leave = staff_on_leave, 
#'                    leave_start = leave_start, 
#'                    leave_end = leave_end, 
#'                    leave_description = leave_description, 
#'                    public_holidays_date = public_holidays_date,
#'                    public_holidays_name = public_holidays_name,
#'                    staff_project_assignment_capacity = staff_project_assignment_capacity)
#' print(wp)
#' 
#' @export
get_workplan <- function(staff, staff_capacity, projects, project_probability, 
                         project_start, project_end, project_phases, 
                         project_time_estimates, staff_on_leave, 
                         leave_start, leave_end, leave_description, 
                         public_holidays_date, public_holidays_name,
                         staff_project_assignment_capacity){
  x <- correct_classes(staff, staff_capacity, projects, project_probability, 
                       project_start, project_end, project_phases, 
                       project_time_estimates, staff_on_leave, 
                       leave_start, leave_end, leave_description, 
                       public_holidays_date, public_holidays_name,
                       staff_project_assignment_capacity)
  wp <- init_workplan(x)
  
  #calculate actual schedules
  wp@full_schedule <- set_full_schedule(wp)
  wp@staff_schedule <- set_staff_schedule(wp)
  wp@team_schedule <- set_team_schedule(wp)
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
correct_classes <- function(staff, staff_capacity, projects, project_probability, 
                            project_start, project_end, project_phases, 
                            project_time_estimates, staff_on_leave, 
                            leave_start, leave_end, leave_description, 
                            public_holidays_date, public_holidays_name,
                            staff_project_assignment_capacity) {
  x <- list()
  x$staff <- as.character(staff)
  x$staff_capacity <- as.numeric(staff_capacity)
  x$projects <- factor(unique(projects), unique(projects), ordered = T)
  x$project_probability <- as.numeric(project_probability)
  x$project_start <- as.Date(project_start)
  x$project_end <- as.Date(project_end)
  x$project_phases <- factor(unique(project_phases), unique(project_phases), ordered = T)
  x$project_time_estimates <- as.numeric(project_time_estimates)
  x$staff_on_leave <- as.character(staff_on_leave)
  x$leave_start <- as.Date(leave_start)
  x$leave_end <- as.Date(leave_end)
  x$leave_description <- as.character(leave_description)
  x$public_holidays_date <- as.Date(public_holidays_date)
  x$public_holidays_name <- as.character(public_holidays_name)
  x$staff_project_assignment_capacity <- as.numeric(staff_project_assignment_capacity)
  
  return(x)
  
}



init_workplan <- function(x) {
  

  
  wp <- new("workplan")
  
  wp@resources <- set_resources(x$staff, x$staff_capacity)
  wp@projects <- set_projects(x$projects, x$project_probability, x$project_start, x$project_end)
  wp@phases <- set_phases(x$project_phases)
  wp@leave <- set_leave(x$staff_on_leave, x$leave_start, x$leave_end, x$leave_description)
  wp@holidays <- set_public_holidays(x$public_holidays_date, x$public_holidays_name)
  wp@time_estimates <- set_time_estimates(wp@projects@project, wp@phases@phase, x$project_time_estimates)
  wp@project_teams <- set_project_team(wp@projects@project, wp@phases@phase, 
                                       wp@resources@staff, 
                                       x$staff_project_assignment_capacity) 
  wp@full_schedule <- full_sched(date = lubridate::today(), project = wp@projects@project[1], 
                                 phase = wp@phases@phase[1], 
                                 staff = wp@resources@staff[1], 
                                 assigned_capacity = wp@project_teams@assigned_capacity[1],
                                 capacity = wp@resources@capacity[1],
                                 public_holiday = "dummy",
                                 out_of_office = "dummy", project_duration = 1,
                                 num_holidays = 1, holiday_expansion_factor = 1,
                                 num_out_of_office = 1, leave_expansion_factor = 1,
                                 leave_adjusted_workload = wp@project_teams@assigned_capacity[1])
  wp@staff_schedule <- staff_sched(date = lubridate::today(), 
                                   staff = wp@resources@staff[1], 
                                   project = as.character(wp@projects@project[1]),
                                   workload = wp@project_teams@assigned_capacity[1], 
                                   out_of_office = "dummy", 
                                   public_holiday = "dummy")
  wp@team_schedule <- team_sched(date = lubridate::today(), 
                                 workload = wp@project_teams@assigned_capacity[1])
  return(wp)
}






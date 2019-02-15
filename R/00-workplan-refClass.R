library(workplanr)
data("staff", package = "workplanr")
data("projects", package = "workplanr")
data("project_phases", package = "workplanr")
data("project_roles", package = "workplanr")
data("out_of_office", package = "workplanr")
data("public_holidays", package = "workplanr")
data("time_estimates", package = "workplanr")
data("roles_responsibilities", package = "workplanr")

workplan <- R6Class("workplan", list(
  staff = NA,
  addStaff = function(staff_name, staff_capacity) {
    self$staff <- data.frame(staff_name, staff_capacity) 
    self_check(self)
    invisible(self)
  },
  projects = NA,
  addProjects = function(project_name, project_confirmed,
                         project_start, project_end){
    self$projects <- add_projects(project_name, project_confirmed,
                                  project_start, project_end)
    self_check(self)
    invisible(self)
  },
  project_phases = NA,
  addPhases = function(project_phase_name){
    project_phase_name <- factor(project_phase_name, project_phase_name, ordered = T)
    self$project_phases <- data.frame(project_phase_name)
    self_check(self)
    invisible(self)
  },
  project_roles = NA,
  addRoles = function(project_role_name){
    project_roles <- factor(project_role_name, project_role_name, ordered = T)
    self$project_roles <- data.frame(project_role_name)
    self_check(self)
    invisible(self)
  },
  out_of_office = NA,
  add_Out_Of_Office = function(staff_name, out_of_office_start, 
                               out_of_office_end, work_related){
    self$out_of_office <- add_OOO(staff_name, out_of_office_start, 
                                  out_of_office_end, work_related)
    self_check(self)
    invisible(self)
  },
  public_holidays = NA,
  addHolidays = function(date, holiday_name){
    self$public_holidays <- add_ph(date, holiday_name)
    self_check(self)
    invisible(self)
  },
  roles_responsibilities = NA,
  addResponsibilites = function(project_role_name, 
                                project_phase_name, 
                                responsibility_span){
    self$roles_responsibilities <- add_rr(project_role_name, 
                                          project_phase_name, 
                                          responsibility_span)
    self$roles_responsibilities$project_role_name <- factor(self$roles_responsibilities$project_role_name,
                                                            levels = self$project_roles$project_role_name,
                                                            ordered = T)
    self$roles_responsibilities$project_phase_name <- factor(self$roles_responsibilities$project_phase_name,
                                                             levels = self$project_phases$project_phase_name,
                                                             ordered = T)
    self_check(self)
    invisible(self)
  },
  time_estimates = NA,
  addTimeEstimates = function(project_name, 
                              project_phase_name, 
                              time_estimate){
    self$time_estimates <- add_te(project_name, 
                                  project_phase_name, 
                                  time_estimate)
    self$time_estimates$project_name <- factor(self$time_estimates$project_name,
                                               levels = self$projects$project_name,
                                               ordered = T)
    self$time_estimates$project_phase_name <- factor(self$time_estimates$project_phase_name,
                                                     levels = self$project_phases$project_phase_name,
                                                     ordered = T)
    self_check(self)
    invisible(self)
  })
)


tmp <- workplan$new()
tmp$addStaff(staff$staff_name, staff$staff_capacity)
tmp$addProjects(projects$project_name, projects$project_confirmed,
                projects$project_start, projects$project_end)
tmp$addPhases(project_phases$project_phase_name)
tmp$addRoles(project_roles$project_role_name)
tmp$add_Out_Of_Office(out_of_office$staff_name,
                      out_of_office$out_of_office_start,
                      out_of_office$out_of_office_end,
                      out_of_office$work_related)
tmp$addHolidays(public_holidays$date, public_holidays$holiday_name)
tmp$addResponsibilites(roles_responsibilities$project_role_name, 
                       roles_responsibilities$project_phase_name,
                       roles_responsibilities$responsibility_span)
tmp$addTimeEstimates(time_estimates$project_name, time_estimates$project_phase_name,
                     time_estimates$time_estimate)

add_projects = function(project_name, project_confirmed,
                        project_start, project_end){
  project_name <- factor(project_name, ordered = TRUE)
  project_confirmed <- as.logical(project_confirmed)
  project_start <- as.Date(project_start)
  project_end <- as.Date(project_end)
  tmp <- data.frame(project_name, project_confirmed,
                    project_start, project_end)
}

add_OOO = function(staff_name, out_of_office_start, 
                   out_of_office_end, work_related){
  id_out_of_office <- 1:length(staff_name)
  staff_name <- as.character(staff_name)
  out_of_office_start <- as.Date(out_of_office_start)
  out_of_office_end <- as.Date(out_of_office_end)
  work_related <- as.logical(work_related)
  tmp <- data.frame(id_out_of_office,
                    staff_name,
                    out_of_office_start,
                    out_of_office_end,
                    work_related)
  return(tmp)
}

add_ph = function(date, holiday_name){
  date <- as.Date(date)
  holiday_name <- as.character(holiday_name)
  tmp <- data.frame(date, holiday_name)
  return(tmp)
}

add_rr = function(project_role_name, 
                  project_phase_name, 
                  responsibility_span){
  project_role_name <- as.character(project_role_name)
  project_phase_name <- as.character(project_phase_name)
  responsibility_span <- as.logical(responsibility_span)
  tmp <- data.frame(project_role_name, 
                    project_phase_name, 
                    responsibility_span)
  return(tmp)
}

add_te = function(project_name, 
                  project_phase_name, 
                  time_estimate){
  project_name <- as.character(project_name)
  project_phase_name <- as.character(project_phase_name)
  time_estimate <- as.numeric(time_estimate)
  tmp <- data.frame(project_name, 
                    project_phase_name, 
                    time_estimate)
  return(tmp)
}

self_check = function(wp){
  inputs <- c("staff",
              "projects",
              "project_phases",
              "project_roles",
              "out_of_office",
              "public_holidays",
              "roles_responsibilities",
              "time_estimates")
  pos <- is.na(mget(inputs, wp))
  message(paste0("You still need to input:\n", paste(inputs[pos], collapse = "\n"), 
          "\nbefore project schedules can be calculated"))
}

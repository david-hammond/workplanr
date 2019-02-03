library(workplanr)
staff <- c("Shelby", "Luis", "Taishawn", "Samantha", "Taylor", "unassigned")
capacity <- c(40,60,100,100,100, 100)
projects <- LETTERS[1:3]
probability <- c(50, 100, 100)
start <- as.Date(c("2019-01-25", "2019-05-17", "2019-06-27"))
end <- as.Date(c("2019-06-03", "2019-06-16", "2019-09-27"))
phases <- c("research", "drafting", "editing", "design", "print", "events")
roles <- c("lead", "researcher", "editor", "design")
staff_on_leave <- c("Luis", "Samantha")
leave_start <-  as.Date(c("2019-07-23", "2019-05-16"))
leave_end <- leave_start + c(20, 25)
leave_description <- c("leave", "work trip")
assigned_staff <- c("Samantha",   "Taylor", "unassigned", "unassigned",
                    "Taishawn",   "unassigned", "Taishawn",   "Taishawn", 
                    "unassigned", "Shelby", "Luis", "Shelby")
time_estimates <- c(c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,10,10,10)) 

#sample(staff, size = length(projects)*length(roles), replace = T)
assigned_capacity <- sample(c(25,50,75,100), size = length(projects)*length(roles), replace = T)
wp <- get_workplan(staff = staff, staff_capacity = capacity, projects = projects, project_probability =  probability, 
                   project_start = start, project_end = end, project_phases = phases, project_roles = roles,
                   project_time_estimates = time_estimates, staff_on_leave = staff_on_leave, leave_start = leave_start, 
                   leave_end = leave_end, leave_description = leave_description, 
                   staff_project_assignments = assigned_staff, staff_project_assigned_capacity = assigned_capacity)
tmp <- as.list(wp)
x <- tmp$full_schedule

plot(wp@staff_schedule)
plot(wp@team_schedule)

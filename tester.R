library(workplanr)
staff <- c("Shelby", "Luis", "Taishawn", "Samantha", "Taylor", "unassigned")
capacity <- c(40,60,100,100,100, 100)
projects <- LETTERS[1:3]
probability <- c(50, 100, 100)
start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27"))
end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
phases <- c("research", "drafting", "editing", "design", "print", "events")
roles <- c("lead", "researcher", "editor", "design")
staff_on_leave <- c("Luis", "Samantha")
leave_start <-  as.Date(c("2019-09-23", "2019-02-16"))
leave_end <- leave.start + c(10, 25)
leave_description <- c("leave", "work")
assigned_staff <- sample(staff, size = length(projects)*length(roles), replace = T)
assigned_capacity <- sample(c(25,50,75,100), size = length(projects)*length(roles), replace = T)
wp <- get_workplan(staff = staff, staff_capacity = capacity, projects = projects, project_probability =  probability, 
                   project_start = start, project_end = end, project_phases = phases, project_roles = roles,
                   staff_on_leave = staff_on_leave, leave_start = leave_start, leave_end = leave_end, leave_description = leave_description, 
                   staff_project_assignments = assigned_staff, staff_project_assigned_capacity = assigned_capacity)
daily_plan <- get_daily_plan(wp)
plot_daily_staff_plan(daily_plan)
plot_team_daily_workload(daily_plan)

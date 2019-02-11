#lapply(list.files("./R/", full.names = T), source)
library(workplanr)
data("staff", package = "workplanr")
data("projects", package = "workplanr")
data("project_phases", package = "workplanr")
data("project_roles", package = "workplanr")
data("out_of_office", package = "workplanr")
data("public_holidays", package = "workplanr")
data("time_estimates", package = "workplanr")
data("roles_responsibilities", package = "workplanr")
my_workplan = create_new_workplan(staff = staff,
                       projects = projects,
                       project_phases = project_phases,
                       project_roles = project_roles,
                       out_of_office = out_of_office,
                       public_holidays = public_holidays,
                       time_estimates = time_estimates,
                       roles_responsibilities = roles_responsibilities,
                       staff_name_for_unassigned_work = "unassigned")
plot(my_workplan@release_schedule)
plot(my_workplan@staff_schedule)
plot(my_workplan@team_schedule)
data("project_assignments", package = "workplanr")
my_workplan <- assign_staff(workplan = my_workplan, 
                             staff_name = project_assignments$staff_name,
                             project_name = project_assignments$project_name,
                             project_role_name = project_assignments$project_role_name,
                             staff_contribution = project_assignments$staff_contribution)

plot(my_workplan@release_schedule)
plot(my_workplan@staff_schedule)
plot(my_workplan@team_schedule)
plot(my_workplan@project_dependencies)

project_workplan <- extract_project_schedule(project = "C", my_workplan)
plot(project_workplan@release_schedule)
plot(project_workplan@staff_schedule)
plot(project_workplan@team_schedule)

irene_workplan <- extract_staff_schedule("Irene", my_workplan)
plot(irene_workplan@staff_schedule)
plot(irene_workplan@team_schedule)
plot(irene_workplan@release_schedule)

plot(my_workplan@release_schedule)
my_workplan <- shift_project(project = "D", by = 20, workplan = my_workplan) #does not work
plot(my_workplan@release_schedule)
plot(my_workplan@staff_schedule)
plot(my_workplan@team_schedule)

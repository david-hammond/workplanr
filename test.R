library(workplanr)
data("staff", package = "workplanr")
data("projects", package = "workplanr")
data("project_phases", package = "workplanr")
data("project_roles", package = "workplanr")
data("out_of_office", package = "workplanr")
data("public_holidays", package = "workplanr")
data("time_estimates", package = "workplanr")
data("roles_responsibilities", package = "workplanr")
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

data("project_assignments", package = "workplanr")
tmp$assignStaff(project_name = project_assignments$project_name,
                project_role_name = project_assignments$project_role_name,
                staff_name = project_assignments$staff_name,
                staff_contribution = project_assignments$staff_contribution)


tmp$plotTeamSchedule()
tmp$plotStaffSchedule()
#Warning message:
#  Factor `out_of_office` contains implicit NA, consider using `forcats::fct_explicit_na` 
tmp$plotReleaseSchedule()





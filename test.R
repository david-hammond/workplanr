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
data("project_assignments", package = "workplanr")
my_workplan2 <- assign_staff(workplan = my_workplan, 
             staff_name = project_assignments$staff_name,
             project_name = project_assignments$project_name,
             project_role_name = project_assignments$project_role_name,
             staff_contribution = project_assignments$staff_contribution)
plot(my_workplan2@staff_schedule)
plot(my_workplan2@team_schedule)
plot_project_calendar(project = "A", schedule)


  

tmp <- schedule$full_schedule %>%
  dplyr::filter(staff_contribution > 0) %>%
  select(project_name, staff_name) 
tmp2 <- tmp %>% dplyr::rename(project_name_2 = project_name) %>%
  dplyr::left_join(tmp) %>% dplyr::filter(project_name != project_name_2) %>%
  distinct() %>% dplyr::select(project_name, project_name_2)

g = igraph::graph_from_edgelist(as.matrix(tmp2))
igraph::V(g)$size = igraph::degree(g, mode = "out")
plot(g)


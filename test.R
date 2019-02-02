library(workplanr)
staff <- c('Shelby', 'Luis', 'Taishawn', 'Samantha', 'Taylor')
capacity <- c(40,60,100,100,100)
resources <- set_resources(staff, capacity)
projects <- LETTERS[1:3]
probability <- c(50, 100, 100)
start <- as.Date(c("2019-07-25", "2019-05-17", "2019-09-27")) 
end <- as.Date(c("2019-09-03", "2019-06-16", "2019-10-27"))
projects <- set_projects(projects, probability, start, end)
phases <- c("research", "drafting", "editing", "design", "print", "events")
phases <- set_phases(phases)
roles <- c("lead", "researcher", "editor", "design")
roles <- set_roles(roles)
responsibilities <- rbind(lead = rep(1, length(phases)), 
researcher = c(1,1,1,0,0,0), editor = c(0,0,1,0,0,0), design = c(0,0,0,1,1,0))
responsibilities <- set_responsibilities(roles, phases, responsibilities)
time_estimates <- rbind(c(-40,-10,-10,-10,-10,10), c(-10,-10,-10,0,0,0), 
                         c(0,0,0,-10,-10,10))
time_estimates <- set_time_estimates(projects, phases, time_estimates)
project_teams <- expand.grid(project = projects$project, 
                            role = roles, KEEP.OUT.ATTRS = FALSE)
project_teams$staff <- sample(resources$staff, size = nrow(project_teams), 
                             replace = TRUE)
project_teams$assigned_capacity <- sample(c(25,25,75,100), 
                                  size = nrow(project_teams), 
                                  replace = TRUE)
project_teams <- set_project_team(project_teams)
daily_plan <- get_daily_plan(resources, projects, 
              phases, time_estimates, 
              project_teams, responsibilities)
staff_workplan <- get_staff_daily_workload(daily_plan)
plot_staff_calendar(daily_plan)
team_workplan <- get_team_daily_workload(daily_plan)
plot_team_daily_workload(daily_plan)
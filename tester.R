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
file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"
public_holidays <- utils::read.csv(file, stringsAsFactors = FALSE)
names(public_holidays) <- tolower(names(public_holidays))
public_holidays$date <- as.Date(lubridate::ymd(public_holidays$date))
public_holidays <- public_holidays %>% filter(jurisdiction == "nsw") %>%
  select(date, holiday.name) %>% rename(name = holiday.name)
assigned_staff <- c("Samantha",   "Taylor", "unassigned", "unassigned",
                    "Taishawn",   "unassigned", "Taishawn",   "Taishawn", 
                    "unassigned", "Shelby", "Luis", "Shelby")
time_estimates <- c(c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,10,10,10)) 

#sample(staff, size = length(projects)*length(roles), replace = T)
assigned_capacity <- sample(c(25,50,75,100), size = length(projects)*length(roles), replace = T)
wp <- get_workplan(staff = staff, staff_capacity = capacity, projects = projects, project_probability =  probability, 
                   project_start = start, project_end = end, project_phases = phases, project_roles = roles,
                   project_time_estimates = time_estimates, staff_on_leave = staff_on_leave, leave_start = leave_start, 
                   leave_end = leave_end, leave_description = leave_description, public_holidays = public_holidays,
                   staff_project_assignments = assigned_staff, staff_project_assigned_capacity = assigned_capacity)
plot(wp@staff_schedule)
plot(wp@team_schedule)

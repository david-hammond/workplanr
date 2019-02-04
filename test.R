library(workplanr)
#resources
staff <- c("Shelby", "Luis", "Taishawn", "Samantha", "Taylor", "unassigned")
staff_capacity <- c(40,60,100,100,100, 100)
#projects
projects <- LETTERS[1:3]
project_probability <- c(50, 100, 100)
project_start <- as.Date(c("2019-01-25", "2019-05-17", "2019-06-27"))
project_end <- as.Date(c("2019-06-03", "2019-06-16", "2019-09-27"))
project_phases <- c("research", "drafting", "editing", "design", "print", "events")
project_time_estimates  <- c(c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,10,10,10)) 
#leave
staff_on_leave <- c("Luis", "Samantha")
leave_start <-  as.Date(c("2019-07-23", "2019-05-16"))
leave_end <- leave_start + c(20, 25)
leave_description <- c("leave", "work trip")
#public holidays
file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"
public_holidays <- utils::read.csv(file, stringsAsFactors = FALSE)
names(public_holidays) <- tolower(names(public_holidays))
public_holidays$date <- as.Date(lubridate::ymd(public_holidays$date))
public_holidays <- public_holidays %>% filter(jurisdiction == "nsw") %>%
  select(date, holiday.name) %>% rename(name = holiday.name)
public_holidays_date <- public_holidays$date
public_holidays_name = public_holidays$name
#staff project assignments
staff_project_assignment_capacity <- sample(c(0,25,50,75,100), size = length(projects)*length(phases)*length(staff), replace = T)
wp <- get_workplan(staff = staff, 
                    staff_capacity = staff_capacity,
                    projects = projects, 
                    project_probability =  project_probability, 
                    project_start = project_start, 
                    project_end = project_end, 
                    project_phases = project_phases, 
                    project_time_estimates = project_time_estimates, 
                    staff_on_leave = staff_on_leave, 
                    leave_start = leave_start, 
                    leave_end = leave_end, 
                    leave_description = leave_description, 
                    public_holidays_date = public_holidays_date,
                    public_holidays_name = public_holidays_name,
                    staff_project_assignment_capacity = staff_project_assignment_capacity)

plot(wp@staff_schedule) + ggplot2::theme_bw()
plot(wp@team_schedule) + ggthemes::theme_fivethirtyeight()
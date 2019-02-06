## create staff dataframe
staff = data.frame(staff_name = c("Shelby", "Luis", "Taishawn", "Samantha", "Taylor", "unassigned"),
                   staff_capacity = c(40,60,100,100,100, 0))
## create project dataframe
projects = data.frame(project_name = LETTERS[1:3],
                      project_confirmed = c(FALSE, TRUE, TRUE),
                      project_start = c("2019-01-25", "2019-05-17", "2019-06-27"),
                      project_end = c("2019-06-03", "2019-06-16", "2019-09-27"))
## create calendar dataframe
bizdays::create.calendar('normal', 
                         weekdays = c('saturday', 'sunday'), 
                         start.date = min(as.Date(projects$project_start)-30), 
                         end.date = max(as.Date(projects$project_start)+30))
calendar <- data.frame(date = as.character(bizdays::bizseq(from = min(as.Date(projects$project_start)-30), 
                                                           to = max(as.Date(projects$project_start)+30), 'normal')))
## create phases dataframe
project_phases <- data.frame(project_phase_name = c("research", "drafting", "editing", "design", "print", "events"))
## create out_of_office dataframe
out_of_office <- data.frame(id_staff = match(c("Luis", "Samantha"), staff$staff_name),
                            out_of_office_start =  c("2019-07-23", "2019-05-16"),
                            out_of_office_end = as.character(as.Date(c("2019-07-23", "2019-05-16")) + c(20, 25)),
                            work_related = c(TRUE, FALSE))
## create public_holidays dataframe
url <- "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"
public_holidays <- utils::read.csv(url, stringsAsFactors = FALSE)
names(public_holidays) <- tolower(names(public_holidays))
public_holidays$date <- as.Date(lubridate::ymd(public_holidays$date))
public_holidays <- public_holidays %>% filter(jurisdiction == "nsw") %>%
  select(date, holiday.name) %>% rename(holiday_name = holiday.name) %>%
  mutate(date = as.character(date))
## create time_estimates dataframe
time_estimates <- expand.grid(id_project_phase = 1:nrow(project_phases), id_project = 1:nrow(projects), 
                              KEEP.OUT.ATTRS = FALSE)
time_estimates$time_estimates <- rep(c(-10,-10,-10,-10,-10,10), nrow(projects))
## create project_assignments dataframe

project_assignments <- expand.grid(id_staff = 1:nrow(staff), id_project_phase = 1:nrow(project_phases), 
                                   id_project = 1:nrow(projects),
                                   KEEP.OUT.ATTRS = FALSE)

project_assignments$staff_contribution <- sample(c(0,25,50,75,100), 
                                                 size = nrow(projects)*nrow(project_phases)*nrow(staff), 
                                                 replace = T)

## create new workplan SQLite database 
db_name <- "my_workplan.sqlite"
create_new_workplan_db(staff = staff,
                       projects = projects,
                       calendar = calendar,
                       project_phases = project_phases,
                       out_of_office = out_of_office,
                       public_holidays = public_holidays,
                       time_estimates = time_estimates,
                       project_assignments = project_assignments,
                       db_name = "my_workplan.sqlite")

schedule = get_schedule(db_name)
plot_staff_schedule(schedule)
plot_team_schedule(schedule)

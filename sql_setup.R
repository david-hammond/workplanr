## ----library, include = TRUE, results='hide', message=FALSE, warning=FALSE----
library(workplanr)

## ----resources, include = TRUE, results='hide', message=FALSE, warning=FALSE----
db_name = "my_workplan.sqlite"
init_workplan_db(db_name)
con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)






staff = data.frame(staff_name = c("Shelby", "Luis", "Taishawn", "Samantha", "Taylor", "unassigned"),
                  staff_capacity = c(40,60,100,100,100, 100))
RSQLite::dbWriteTable(con, "staff", staff, append = T)
RSQLite::dbReadTable(con, "staff")
## ----projects, include = TRUE, results='hide', message=FALSE, warning=FALSE----
projects = data.frame(project_name = LETTERS[1:3],
project_confirmed = c(FALSE, TRUE, TRUE),
project_start = c("2019-01-25", "2019-05-17", "2019-06-27"),
project_end = c("2019-06-03", "2019-06-16", "2019-09-27"))
RSQLite::dbWriteTable(con, "projects", projects, append = T)
RSQLite::dbReadTable(con, "projects")


project_phases <- data.frame(project_phase_name = c("research", "drafting", "editing", "design", "print", "events"))

RSQLite::dbWriteTable(con, "project_phases", project_phases, append = T)
RSQLite::dbReadTable(con, "project_phases")

project_time_estimates  <- c(c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,-10,-10,-10), c(-10,-10,-10,10,10,10)) 

## ----leave, include = TRUE, results='hide', message=FALSE, warning=FALSE----
out_of_office <- data.frame(id_staff = match(c("Luis", "Samantha"), staff$name),
out_of_office_start =  c("2019-07-23", "2019-05-16"),
out_of_office_end = as.character(as.Date(leave_start) + c(20, 25)),
work_related = c(TRUE, FALSE))
RSQLite::dbWriteTable(con, "out_of_office", out_of_office, append = T)
RSQLite::dbReadTable(con, "out_of_office")
#lost holidays
## ----holidays, include = TRUE, results='hide', message=FALSE, warning=FALSE----
url <- "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"
public_holidays <- utils::read.csv(url, stringsAsFactors = FALSE)
names(public_holidays) <- tolower(names(public_holidays))
public_holidays$date <- as.Date(lubridate::ymd(public_holidays$date))
public_holidays <- public_holidays %>% filter(jurisdiction == "nsw") %>%
  select(date, holiday.name) %>% rename(holiday_name = holiday.name) %>%
  mutate(date = as.character(date))
RSQLite::dbWriteTable(con, "public_holidays", public_holidays, append = T)
RSQLite::dbReadTable(con, "public_holidays")

#add calendar
bizdays::create.calendar('normal', 
                         weekdays = c('saturday', 'sunday'), 
                         start.date = "2018-1-1", 
                         end.date = "2019-12-31",
                         holidays = public_holidays$date)
calendar <- data.frame(date = as.character(bizdays::bizseq(from = "2018-1-1", to = "2019-12-31", 'normal')))

RSQLite::dbWriteTable(con, "calendar", calendar, append = T)
RSQLite::dbReadTable(con, "calendar")

time_estimates <- expand.grid(id_project_phase = 1:nrow(project_phases), id_project = 1:nrow(projects), 
                             KEEP.OUT.ATTRS = FALSE)

time_estimates$time_estimates <- rep(c(-10,-10,-10,-10,-10,10), nrow(projects))

RSQLite::dbWriteTable(con, "time_estimates", time_estimates, append = T)
RSQLite::dbReadTable(con, "time_estimates")

project_assignments <- expand.grid(id_staff = 1:nrow(staff), id_project_phase = 1:nrow(project_phases), 
                                   id_project = 1:nrow(projects),
                              KEEP.OUT.ATTRS = FALSE)

project_assignments$contribution <- sample(c(0,25,50,75,100), 
                                             size = nrow(projects)*nrow(project_phases)*nrow(staff), 
                                             replace = T)

RSQLite::dbWriteTable(con, "project_assignments", project_assignments, append = T)
RSQLite::dbReadTable(con, "project_assignments")

schedule <- RSQLite::dbReadTable(con, "calendar")
schedule <- dplyr::left_join(schedule, RSQLite::dbReadTable(con, "public_holidays"))

#get project start and end dates 
rs <- RSQLite::dbReadTable(con, "projects") %>% 
  dplyr::select(-id_project) %>% 
  tidyr::gather("date_type", "date", -c(project_name, project_confirmed)) %>%
  dplyr::select(-date_type) %>% 
  dplyr::mutate(date = as.Date(date)) %>%
  padr::pad(group = c("project_name", "project_confirmed"), interval = 'day') %>%
  dplyr::mutate(date = as.character(date))
                                   
schedule <- dplyr::left_join(schedule, rs)                                 





phases <- RSQLite::dbSendQuery(con, "SELECT project_phase_name FROM project_phases")
phases <- RSQLite::dbFetch(phases)

rs <- RSQLite::dbSendQuery(con, "SELECT project_name, project_phase_name, project_start, project_end, 
project_confirmed, time_estimates  FROM time_estimates INNER JOIN projects ON projects.id_project = time_estimates.id_project
                           INNER JOIN project_phases ON project_phases.id_project_phase =  time_estimates.id_project_phase;")
rs <- RSQLite::dbFetch(rs)
 
rs <- rs %>% 
  mutate(project_phase_name = factor(project_phase_name, 
                                        levels = phases$project_phase_name,
                                        ordered = T)) %>%
           tidyr::spread(project_phase_name, time_estimates)

end = times

end[, ncol(end)] = bizdays::offset(end$project_end, end[,ncol(end)], 'normal')
for (i in  rev(phases$project_phase_name)[-1]){
  end[,i] = bizdays::offset(end[,(which(names(end) == i)) +1], times[,i], 'normal')
}
start = times
for (i in  phases$project_phase_name[ -1]){
  start[,i] = bizdays::offset(end[,i], times[,i], 'normal')
}
first.phase = names(phases$project_phase_name[-c(1, ncol(tmp$time_estimates))])[1]
#TODO: need to document this
start[, first.phase] <- as.Date(apply(data.frame(start$start, start[,first.phase]), 1, min))
schedule = list(start = start, end = end)
schedule = lapply(schedule, function(x) x %>% dplyr::select(-c(start, end)) %>% 
                    tidyr::gather(phase, date, -c(project, probability)) %>%
                    dplyr::mutate(phase = factor(phase, tmp$phases$phase, ordered = T)))
schedule = dplyr::bind_rows(schedule, .id = 'date.type')
schedule = schedule %>% 
  tidyr::spread(date.type, date) %>% 
  dplyr::filter(start!=end) %>%
  tidyr::gather(date.type, date, -c(project, probability, phase))
schedule$date.type = factor(schedule$date.type, (c('start', 'end')), ordered = T)

schedule = schedule %>% 
  dplyr::arrange(project, phase, dplyr::desc(date), date.type) %>% 
  dplyr::select(-date.type)

rs <- rs %>% tidyr::gather("date.type", "date") %>%
  select(-date_type) %>% padr::pad(group = c("project_name", interval = 'day')


schedule <- dplyr::left_join(schedule , )
rs <- RSQLite::dbSendQuery(con, "SELECT project_name, project_phase_name, staff_name, contribution
FROM project_assignments 
                           INNER JOIN projects ON projects.id_project = project_assignments.id_project
                           INNER JOIN project_phases ON project_phases.id_project_phase =  project_assignments.id_project_phase
                           INNER JOIN staff ON staff.id_staff =  project_assignments .id_staff;")
schedule <- dplyr::left_join(schedule , RSQLite::dbFetch(rs))

#left_join_project+phase dates

rs <- RSQLite::dbSendQuery(con, "SELECT project_name, project_phase_name, project_start, project_end, 
project_confirmed, time_estimates  FROM time_estimates INNER JOIN projects ON projects.id_project = time_estimates.id_project
INNER JOIN project_phases ON project_phases.id_project_phase =  time_estimates.id_project_phase;")
RSQLite::dbFetch(rs)

rs <- RSQLite::dbSendQuery(con, "SELECT project_name, project_phase_name, staff_name, contribution
FROM project_assignments 
INNER JOIN projects ON projects.id_project = project_assignments.id_project
INNER JOIN project_phases ON project_phases.id_project_phase =  project_assignments.id_project_phase
INNER JOIN staff ON staff.id_staff =  project_assignments .id_staff;")
RSQLite::dbFetch(rs)
#left_join_staff assignemtns

#left_join staff_on_leave










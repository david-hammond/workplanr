#' create a list of employees that are to be assigned to projects
#'
#' @param db_name The name of the database to create 

#' @keywords interal
init_db = function(db_name = "my_workplan.sqlite"){
  
  if(file.exists(db_name)) file.remove(db_name)
  
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname= db_name)
  
  RSQLite::dbSendQuery(db, statement = "CREATE TABLE `calendar` 
            ( `id_calendar` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                       `date` TEXT)")  
  
  RSQLite::dbSendQuery(db, statement = "CREATE TABLE `staff` 
            ( `id_staff` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
              `staff_name` TEXT, `staff_capacity` INTEGER, `joined` TEXT, `departed` TEXT )")
  
  RSQLite::dbSendQuery(db, statement = "CREATE TABLE `projects` 
              ( `id_project` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
              `project_name` TEXT, 
              `project_confirmed` INTEGER, 
              `project_start` TEXT, 
              `project_end` TEXT )")
  
  RSQLite::dbSendQuery(db, statement = "CREATE TABLE `project_phases` 
              ( `id_project_phase` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
                `project_phase_name` TEXT)")
  
  RSQLite::dbSendQuery(db, statement = "CREATE TABLE `out_of_office` 
              ( `id_out_of_office` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
              `id_staff` INTEGER, `out_of_office_start` TEXT, `out_of_office_end` TEXT, 
              `work_related` INTEGER, FOREIGN KEY(`id_staff`) REFERENCES `staff`(`id_staff`) )")
  
  RSQLite::dbSendQuery(db, statement = "CREATE TABLE `public_holidays` 
              ( `id_public_holidays` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
              `date` TEXT, `holiday_name` TEXT )")
  
  RSQLite::dbSendQuery(db, statement = "CREATE TABLE `time_estimates` 
              ( `id_time_estimates` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
              `id_project` INTEGER, `id_project_phase` INTEGER, 
              `time_estimate` INTEGER, FOREIGN KEY(`id_project_phase`) REFERENCES `project_phases`(`id_project_phase`) )")
  
  RSQLite::dbSendQuery(db, statement = "CREATE TABLE `project_assignments` 
              ( `id_project_assignment` INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, 
              `id_project` INTEGER, `id_project_phase` INTEGER, 
              `id_staff` INTEGER, `staff_contribution` INTEGER, 
              FOREIGN KEY(`id_staff`) REFERENCES `staff`(`id_staff`), 
              FOREIGN KEY(`id_project`) REFERENCES `projects`(`id_project`), 
              FOREIGN KEY(`id_project_phase`) REFERENCES `project_phases`(`id_project_phase`) )")
  
  RSQLite::dbDisconnect(db)
}
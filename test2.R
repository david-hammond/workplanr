##initial plot
db_name <- "my_workplan.sqlite"
workplanr::import_workplan_from_xlsx(excel_file_name = "iep_workplan.xlsx")
schedule = workplanr::get_schedule(db_name)
workplanr::plot_staff_schedule(schedule)
workplanr::plot_team_schedule(schedule)
##secondary plot
assign_staff(db_name, staff_name = "steve", project_name = "CEF", project_phase_name = "editing", 
             staff_contribution = 100)
schedule = workplanr::get_schedule(db_name)
workplanr::plot_staff_schedule(schedule)
workplanr::plot_team_schedule(schedule)
##teritary plot
remove_staff(db_name, staff_name = "steve", project_name = "CEF", project_phase_name = "editing")
schedule = workplanr::get_schedule(db_name)
workplanr::plot_staff_schedule(schedule)
workplanr::plot_team_schedule(schedule)

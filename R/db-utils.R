#' Create Excel file for project inputs
#' 
#' This function creates an excel file that can be used to create a new project
#'
#' @param excel_file_name File name for project inputs
#' @return NULL
#' @example 
#' create_project_file()
#' @export 
create_project_file = function(excel_file_name = "my_project.xlsx"){
  data("example_project")
  for (i in names(example_project)){
    rio::export(example_project[[i]], file = excel_file_name, which = i)
  }
  return(NULL)
}
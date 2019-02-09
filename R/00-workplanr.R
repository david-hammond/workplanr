#' \code{workplanr} package
#'
#'
#' The \code{workplanr} package is a SQLite dabase wrapper to plan concurrent projects with phases and deadlines.
#' It in intended for project manaers who need to track dealdlines and stff loads across multiple projects.
#' 
#' It is aimed to be a lightweight simple to use projec management tool to avoid the need for small teams to invest 
#' in large expensive project management tool infrastructure.
#' 
#' To build a workplan, the user needs to set the following project inputs
#' \itemize{
#' \item \strong{staff} A data frame of staff_name and staff capacity
#' \item Producing finalised charts for layout
#' \item Producing maps in IEP colours
#' }
#' 
#' 
#' These functions are intended to produce pdfs that are importable into illustrator
#' making the layout for comms much easier.
#' 
#' @section Installation:
#' 
#' #' # Then you need devtools installed
#' 
#' \code{install.packages('devtools')}
#' 
#' # Now you can install from github 
#' 
#' \code{devtools::install_github('hammond/workplar')}
#' 
#' \code{library(workplanr)}
#' 
#'
#' @name workplanr-package
#' @aliases iep.charts-package
#' @title IEP charting functions for R
#' @author David Hammond \email{anotherdavidhammond@gmail.com}
#' @name workplanr
#' @importFrom dplyr "%>%"
#' @importFrom grDevices grey
#' @importFrom utils vignette
#' @keywords package
NULL

if(getRversion() >= "2.15.1")  utils::globalVariables(c( "date_type",
                                                         "end",
                                                         "group",
                                                         "holiday_name",
                                                         "id_calendar",
                                                         "id_out_of_office",
                                                         "id_public_holidays",
                                                         "project_confirmed",
                                                         "project_end",
                                                         "project_name",
                                                         "project_phase_name",
                                                         "project_start",
                                                         "staff_capacity",
                                                         "staff_contribution",
                                                         "staff_name",
                                                         "start",
                                                         "total",
                                                         "value",
                                                         "work_related",
                                                         "holiday_expansion_factor",
                                                         "leave_adjusted_workload",
                                                         "leave_expansion_factor",
                                                         "num_holidays",
                                                         "num_out_of_office",
                                                         "out_of_office",
                                                         "project_duration",
                                                         "time_estimate",
                                                         "workload",
                                                         "id_project",
                                                         "id_project_phase",
                                                         "id_project_role",
                                                         "id_staff",
                                                         "n",
                                                         "project_role_name",
                                                         "responsibility_span",
                                                         "roles_responsibilities"), add = FALSE)

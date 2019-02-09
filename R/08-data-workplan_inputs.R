#' Exmaple list of staff
#'
#' A dataset staff_names and staff_capacity
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{staff_name}{Names of staff}
#'   \item{staff_capacity}{Capacity, can be on any scale but easiest to use scale of 100 for full time equivalent, 
#'   40 for someone who works two days a week, etc.}
#'   ...
#' }
"staff"

#' Data frame of proejct information
#'
#' A dataset of projects, whether they are confirmed, start and end dates
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{project_name}{Project Name}
#'   \item{project_confirmed}{Whether the project is confirmed TRUE = confirmed, FALSE = potential work, allows project manager to plan for unconfirmed work}
#'   \item{project_start}{Date as character for project start}
#'   \item{project_end}{Date of end of project}
#'   ...
#' }
"projects"

#' Project phases
#'
#' A dataset containing the the project phases
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{project_phase_name}{Phases of a project}
#'   ...
#' }
"project_phases"

#' Project roles
#'
#' A dataset containing the the project phases
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{project_role_name}{Project roles}
#'   ...
#' }
"project_roles"

#' A dataset of periods where staff are out of the office
#'
#' A dataset containing periods where staff are out of the office
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{staff_name}{Staff who will be out of the office.}
#'   \item{out_of_office_start}{Date period starts.}
#'   \item{out_of_office_end}{Date of period end.}
#'   \item{work_related}{Whether out of office period is for work (TRUE) or vacation (FALSE)}
#'   ...
#' }
#' @source \url{http://www.diamondse.info/}
"out_of_office"

#' Public Holidays
#'
#' A dataset containing the dates of public holidays in New South Wales, Australia in 2019.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{date}{The date of the public holiday}
#'   \item{holiday_name}{The name of the holiday}
#'   ...
#' }
"public_holidays"

#' Time Estimates
#'
#' A dataset containing the estimates of time for each pahse in each project.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{project_name}{The project}
#'   \item{project_phase_name}{The pahse of the project}
#'   \item{time_estimate}{Amount of time that each phase is expected to be in each project. 
#'   A negative number means that the phase occurs before the end date of the project, 
#'   a positive number means it happens after the end date of a project, 
#'   and 0 means phase will not occur in the project}
#'   ...
#' }
"time_estimates"

#' Roles and responsibilities
#'
#' A dataset containing the resposibilities across phases that each project role has
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{project_role_name}{Project Role}
#'   \item{project_phase_name}{Project phase}
#'   \item{responsibility_span}{Whether the role in involved in that phase of the project, 1 = yes, 0 = no.}
#'   ...
#' }
"roles_responsibilities"

#' Project Assignments
#'
#' A dataset containing the staff assigned to roles on projects
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{project_name}{Name of project}
#'   \item{project_role_name}{Project roles}
#'   \item{staff_name}{Names of staff}
#'   \item{staff_contribution}{Approximate amount of time staff will dedicate to performing that role. Needs to be the same scale as staff_capacity in data("staff")}
#'   ...
#' }
"project_assignments"
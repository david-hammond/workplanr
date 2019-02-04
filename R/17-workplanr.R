#' \code{workplanr} package
#'
#' Package for work plans and resource balancing
#'
#'
#' @docType package
#' @name workplanr
#' @importFrom dplyr "%>%"
#' @importFrom grDevices grey
#' @importFrom stats end na.omit start
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("assigned_capacity", "capacity", "date.type", "description",
                                                          "holiday_expansion_factor", "leave_adjusted_workload",
                                                          "leave_expansion_factor", "maxcapacity", "mutate", "name", "needed", 
                                                          "new", "num_holidays", "num_out_of_office", "out_of_office", "phase", "probability",
                                                          "project", "project_duration", "public_holiday", "responsibilities", "staff",
                                                          "teamload", "time_estimate", "workload"), add = FALSE)
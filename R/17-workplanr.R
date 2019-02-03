#' \code{workplanr} package
#'
#' Package for work plans and resource balancing
#'
#'
#' @docType package
#' @name workplanr
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("x"), add = FALSE)
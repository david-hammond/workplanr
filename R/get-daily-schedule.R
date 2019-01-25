#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
get.daily.schedule <- function(schedule){
  require(padr)
  x = schedule %>% 
    dplyr::select(project, phase, task, probability, start, end) %>% 
    gather("type", "date", -c(project, phase, task, probability)) %>%
    mutate(date = as.Date(date))
  x = pad(x, group = 'task', interval = "day") %>% select(-type)
  x = x %>% fill(project, phase, probability)
  return(x)
}
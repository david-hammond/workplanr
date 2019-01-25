#' ggplot colours
#'
#' @param p a ggplot
#'
#'
#'
#' @export
backcast <- function(tmp){
  require(bizdays)
  require(lubridate)

  cal <- create.calendar("normal", weekdays = c("saturday", "sunday"), 
                         holidays = plan$holidays$holiday.dates)
  tmp$start = as.POSIXct(tmp$launch[nrow(tmp)])
  tmp$end = as.POSIXct(bizdays::offset(tmp$launch[nrow(tmp)], tmp$days[nrow(tmp)], 'normal'))
  
  for (i in (nrow(tmp)-1):1){
    tmp$end[i] = tmp$start[i+1]
    tmp$start[i] = bizdays::offset(tmp$end[i], -tmp$days[i], 'normal')
  }

  y = tmp[1,]
  y$phase = "research"
  y$start = y$kickoff[1]
  y$end = tmp$start[1]
  y$days = bizdays(y$start, y$end, 'normal')
  tmp = rbind(y, tmp) %>% dplyr::select(-c(launch, kickoff))
  tmp = tmp %>% dplyr::filter(days > 0)
  tmp = as.data.frame(tmp)
  return(tmp)
}


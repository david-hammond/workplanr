#' create a list of phases in any project
#'
#' @param file file with public holiday information
#' @return A reference table for phases 

set_public_holidays <- function(file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"){
  public_holidays <- utils::read.csv(file, stringsAsFactors = FALSE)
  names(public_holidays) <- tolower(names(public_holidays))
  public_holidays$date <- as.Date(lubridate::ymd(public_holidays$date))
  public_holidays <- public_holidays %>% filter(jurisdiction == "nsw") %>%
    select(date, holiday.name, information) 
  public_holidays <- holi(date = public_holidays$date, 
                          name = public_holidays$holiday.name,
                          info = public_holidays$information)
  return(public_holidays)
}

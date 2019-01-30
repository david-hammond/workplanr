create.public.holidays <- function(file = "https://data.gov.au/data/dataset/b1bc6077-dadd-4f61-9f8c-002ab2cdff10/resource/31eec35e-1de6-4f04-9703-9be1d43d405b/download/australian_public_holidays_2019.csv"){
  require(lubridate)
  require(dplyr)
  public.holidays = read.csv(file)
  public.holidays$Date = as.Date(ymd(public.holidays$Date))
  public.holidays = public.holidays %>% filter(Jurisdiction == "nsw") %>%
    select(Date, Holiday.Name, Information)
  names(public.holidays) = tolower(names(public.holidays))
  return(public.holidays)
}
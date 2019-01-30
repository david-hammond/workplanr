get.team.daily.plan = function(daily.plan){
  
  x = get.daily.staff.plan(daily.plan)
  pos = is.na(x$available_capacity)
  x$available_capacity[pos] = 1
  x$probability[pos] = 1
  x$staff = as.character(x$staff)
  pos = x$staff == "unassigned"
  x$staff[!pos] = "assigned"
  tmp = x %>% ungroup() %>% group_by(date, staff) %>% 
    summarise(team_capacity = sum(available_capacity)) %>%
    ungroup()
  team = daily.plan %>% select(staff, capacity) %>% distinct() %>%
    filter(staff != "unassigned")
  tmp$team_capacity = tmp$team_capacity/(sum(team$capacity))
  tmp = tmp %>% ungroup() %>% group_by(date) %>% 
     mutate(total = sum(team_capacity), load = ifelse(sum(team_capacity) < 1, "Covered", "Overload"),
            group = paste(staff, team_capacity > 1, load)) %>% ungroup()
  return(tmp)
}
get.team.daily.plan = function(daily.plan){
  
  x = get.daily.staff.plan(daily.plan)
  x$staff = as.character(x$staff)
  pos = is.na(x$staff)
  x$staff[pos] = "unassigned"
  x$staff[!pos] = "assigned"
  tmp = x %>% ungroup() %>% group_by(date, staff) %>% 
    summarise(team_capacity = sum(available_capacity)) %>%
    ungroup()
  # tmp$team_capacity = tmp$team_capacity/(sum(plan$resources$capacity[plan$resources$staff != "unassigned"])/100)
  # tmp$staff = factor(tmp$staff, rev(unique(sort(tmp$staff))), ordered = T)
  # tmp = tmp %>% ungroup() %>% group_by(date) %>% 
  #   mutate(total = sum(team_capacity), load = ifelse(sum(team_capacity) < 1, "Covered", "Overload"),
  #          group = paste(staff, team_capacity > 1, load)) %>% ungroup()
  return(tmp)
}
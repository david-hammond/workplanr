get.daily.staff.plan = function(plan){
  require(dplyr)
  x = plan %>% group_by(date, staff, probability) %>% 
    summarise(capacity = min(capacity),
              assigned_capacity = sum(leave_adjusted_assigned_capacity),
              available_capacity = assigned_capacity/capacity)
  tmp = expand.grid(date = unique(x$date), staff = unique(x$staff))
  tmp = left_join(tmp, x %>% dplyr::select(date, staff, available_capacity, probability))
  tmp = tmp %>% ungroup()
  tmp$staff = factor(tmp$staff, levels = rev(sort(unique((tmp$staff)))), ordered = T)
  return(tmp)
}


activity1 %>% 
    group_by(date) %>% 
    mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
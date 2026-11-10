do_daily_data <- function(data){
  data_daily <- data %>% 
    mutate(dt_event = DT_SIN_PRI) %>%
    group_by(dt_event) %>%
    summarize(Cases = n())
  return(data_daily)
}

do_weekly_data <- function(data){
  data_weekly <- data %>% mutate(epiweek = epiweek(DT_SIN_PRI),
                                 epiyear = epiyear(DT_SIN_PRI)) %>%
    mutate(dt_event = MMWRweek::MMWRweek2Date(epiyear, epiweek)) %>%
    group_by(dt_event) %>%
    summarize(Cases = n())
  return(data_weekly)
}

do_nowcaster <- function(data){
  res_nowcaster <- nowcasting_inla(dataset = data, 
                                   date_onset = "DT_SIN_PRI", 
                                   date_report = "DT_DIGITA", 
                                   data.by.week = T)
  return(res_nowcaster$total)
}
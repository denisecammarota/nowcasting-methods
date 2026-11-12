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

do_nowcaster_inla <- function(data){
  res_nowcaster <- nowcasting_inla(dataset = data, 
                                   date_onset = "DT_SIN_PRI", 
                                   date_report = "DT_DIGITA", 
                                   data.by.week = T,
                                   trim.data = 0)
  return(res_nowcaster$total)
}

do_nowcaster_mgcv <- function(data){
  res_nowcaster <- nowcasting_mgcv(dataset = data, 
                                   date_onset = "DT_SIN_PRI", 
                                   date_report = "DT_DIGITA", 
                                   data.by.week = T,
                                   trim.data = 0)
  return(res_nowcaster$total)
}

do_baselinenowcast <- function(data, temporal_scale = c("weekly", "daily"),
                               max_delay, draws = 1000) {
  
  unit <- match.arg(temporal_scale)
  
  data <- data %>% filter(!is.na(DT_SIN_PRI), !is.na(DT_DIGITA))
  
  if (unit == "weekly") {
    # same cutoff as nowcasting_inla(use.epiweek = TRUE):
    # keep reports up to the last Saturday before the week of max(DT_DIGITA)
    dmax   <- max(data$DT_DIGITA)
    cutoff <- dmax - (as.integer(format(dmax, "%w")) + 1)
    
    data <- data %>%
      filter(DT_DIGITA <= cutoff) %>%
      mutate(DT_SIN_PRI = MMWRweek::MMWRweek2Date(epiyear(DT_SIN_PRI), epiweek(DT_SIN_PRI)),
             DT_DIGITA  = MMWRweek::MMWRweek2Date(epiyear(DT_DIGITA),  epiweek(DT_DIGITA)))
 
  }
  
  step        <- if (unit == "weekly") 7 else 1
  delays_unit <- if (unit == "weekly") "weeks" else "days"
  
  counts <- data %>%
    count(DT_SIN_PRI, DT_DIGITA, name = "cases") %>%
    mutate(delay = as.integer(DT_DIGITA - DT_SIN_PRI) / step) %>%
    filter(delay >= 0)                       # nowcaster drops negative delays
  
  rep_tri <- counts %>%
    as_reporting_triangle(delays_unit = delays_unit,
                          reference_date = "DT_SIN_PRI",
                          report_date = "DT_DIGITA",
                          count = "cases") %>%
    truncate_to_delay(max_delay = max_delay)
  
  baselinenowcast(rep_tri, draws = draws)
}
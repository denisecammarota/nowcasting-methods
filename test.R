library(arrow)
library(tidyverse)
library(lubridate)
library(baselinenowcast)

source('source.R')

# Importing the file that will be used ###########

path_file <- 'dengue_all_sp.parquet'
data <- read_parquet(path_file) %>%
  mutate(SG_UF = as.numeric(SG_UF)) %>%
  filter(SG_UF == 35) %>%
  select('DT_SIN_PRI', 'DT_DIGITA') %>%
  mutate(
  DT_SIN_PRI = as.Date(DT_SIN_PRI),
  DT_DIGITA = as.Date(DT_DIGITA)
  ) %>%
  filter(!is.na(DT_DIGITA)) %>%
  filter(!is.na(DT_SIN_PRI)) %>%
  filter(DT_SIN_PRI <= DT_DIGITA)

colnames(data)

# No nowcasting baseline #########################
# For both weekly and daily time granularity

data_weekly <- do_weekly_data(data)
data_daily <- do_daily_data(data)

# Nowcasting by base nowcaster ###################
# Only for weekly time granularity

res_nowcaster <- do_nowcaster(data)

# Nowcasting by baselinenowcast ##################
# For both daily and weekly time granularity

## Processing data for the baselinenowcast

process_baselinenowcast <- function(data, temporal_scale = c('weekly', 'daily')){
  
  unit <- match.arg(temporal_scale)
  
  if(temporal_scale = 'weekly'){
    data <- data %>% mutate(epiweek_dt_sin_pri = epiweek(DT_SIN_PRI),
                    epiyear_dt_sin_pri = epiyear(DT_SIN_PRI),
                    epiweek_dt_digita = epiweek(DT_DIGITA),
                    epiyear_dt_digita = epiyear(DT_DIGITA)) %>%
      mutate(DT_SIN_PRI = MMWRweek::MMWRweek2Date(epiyear_dt_sin_pri, epiweek_dt_sin_pri),
             DT_DIGITA = MMWRweek::MMWRweek2Date(epiyear_dt_digita, epiweek_dt_digita))    
  }
  

}





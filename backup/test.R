library(arrow)
library(tidyverse)
library(lubridate)
library(nowcaster)
library(baselinenowcast)

source('source.R')

# Importing the file that will be used ###########

path_file <- 'dengue-sp-july.parquet'
data <- read_parquet(path_file) %>%
  mutate(SG_UF = as.numeric(SG_UF)) %>%
  filter(SG_UF == 35) %>%
  select('DT_SIN_PRI', 'DT_DIGITA') %>%
  mutate(
  DT_SIN_PRI = as.Date(DT_SIN_PRI),
  DT_DIGITA = as.Date(DT_DIGITA)
  ) %>%
  filter(epiyear(DT_SIN_PRI) >= 2025) %>%
  filter(epiyear(DT_SIN_PRI) <= 2026) 

path_file <- 'dengue-sp-nov.parquet'
data_gt <- read_parquet(path_file) %>%
  mutate(SG_UF = as.numeric(SG_UF)) %>%
  filter(SG_UF == 35) %>%
  select('DT_SIN_PRI', 'DT_DIGITA') %>%
  mutate(
    DT_SIN_PRI = as.Date(DT_SIN_PRI),
    DT_DIGITA = as.Date(DT_DIGITA)
  ) %>%
  filter(epiyear(DT_SIN_PRI) >= 2025) %>%
  filter(epiyear(DT_SIN_PRI) <= 2026) 

# No nowcasting baseline #########################
# For both weekly and daily time granularity

data_weekly <- do_weekly_data(data)
data_daily <- do_daily_data(data)
data_weekly_gt <- do_weekly_data(data_gt)
data_daily_gt <- do_daily_data(data_gt)
#data_weekly_gt <- data_weekly_gt %>% filter(dt_event <= max(data_weekly$dt_event))
#data_daily_gt <- data_daily_gt %>% filter(dt_event <= max(data_daily$dt_event))

# Nowcasting by base nowcaster ###################
# Only for weekly time granularity

res_nowcaster_inla <- do_nowcaster_inla(data)
res_nowcaster_mgcv <- do_nowcaster_mgcv(data)

# Nowcasting by baselinenowcast ##################
# For both daily and weekly time granularity

res_baseline_weekly <- do_baselinenowcast(data, temporal_scale = "weekly",
                                          max_delay = 15, draws = 1000)

# Plotting results so far for all methods ##################

## Combine all methods ######################

nc_inla <- res_nowcaster_inla |>
  transmute(dt_event, median = Median, lower = LI, upper = LS, method = "nowcaster (INLA)")

nc_mgcv <- res_nowcaster_mgcv |>
  transmute(dt_event, median = Median, lower = LI, upper = LS, method = "nowcaster (mgcv)")

nc_bnc <- res_baseline_weekly |>
  as_tibble() |>
  group_by(dt_event = reference_date) |>
  summarise(median = median(pred_count),
            lower  = quantile(pred_count, 0.025),
            upper  = quantile(pred_count, 0.975),
            .groups = "drop") |>
  mutate(method = "baselinenowcast")
max_bnc <- sort(nc_bnc$dt_event)[dim(nc_bnc)[1] - 14]
nc_bnc <- nc_bnc %>% filter(dt_event >= max_bnc)

nowcasts <- bind_rows(nc_inla, nc_mgcv, nc_bnc)

## Plot ####################################

max_time <- max(nowcasts$dt_event)
x_from   <- max_time - 7 * 20
y_max    <- max(c(nowcasts$upper[nowcasts$dt_event >= x_from],
                  data_weekly$Cases[data_weekly$dt_event >= x_from]), na.rm = TRUE)

epiweek_lab <- function(x) sprintf("%02d/%02d", epiweek(x), epiyear(x) %% 100)

ggplot(nowcasts, aes(x = dt_event)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = method), alpha = 0.2) +
  geom_line(aes(y = median, colour = method)) +
  geom_line(data = data_weekly, aes(y = Cases), colour = "black") +
  geom_point(data = data_weekly, aes(y = Cases), colour = "black", size = 0.8) +
  geom_line(data = data_weekly_gt, aes(y = Cases), colour = "red") +
  geom_point(data = data_weekly_gt, aes(y = Cases), colour = "red", size = 0.8) +
  scale_x_date(breaks = seq(x_from, max_time, by = "2 weeks"),
               labels = epiweek_lab, name = "Epiweek") +
  coord_cartesian(xlim = c(x_from, max_time), ylim = c(0, y_max * 1.05)) +
  labs(y = "Nº Cases", colour = NULL, fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5))


# convert data exported from the CHI program to iCal format (.ics) to be imported to calandar apps
#   by Chat Wacharamanotham
#   Tested with https://programs.sigchi.org/chi/2021/ version 3.3.7

library(tidyverse)
source("R/tidy_schedule.R")

path_schedule_csv <- "input/CHI2021_my_schedule.csv"
exclude_before_ymd <- "2021-05-08"

#===============================================================================

df_target_tz <- tidy_schedule(path_schedule_csv, "Europe/Zurich")

df_target_tz %>%   
  write_ics("output/all_schedule.ics")

df_target_tz %>%   
  filter(start_date_target > ymd(exclude_before_ymd)) %>% 
  write_ics("output/filtered_schedule.ics")

df_target_tz %>%   
  mutate(start_ymd = str_c(year(start_date_target), month(start_date_target), day(start_date_target), sep = "-")) %>%
  nest(schedule = !start_ymd) %>% 
  pwalk(function(start_ymd , schedule) {
    path_ics <- file.path("output", sprintf("%s.ics", start_ymd))
    schedule %>% 
      write_ics(path_ics)
  })

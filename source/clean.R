library(tidyverse)
library(janitor)
library(lubridate)
library(stringr)
library(hms)
library(zoo)
library(scales)
library(extrafont)
library(RColorBrewer)
library(treemapify)
library(ggalluvial)

#load
audit_data <- read_csv(here("data/Rapid_Recovery.csv"))

#flatten

audit_data <- clean_names(audit_data)

#Clean


#remove ?
audit_data$mob_time_24h <- str_remove(audit_data$mob_time_24h, pattern = "\\?")
audit_data$mob_time_24h_2 <- str_remove(audit_data$mob_time_24h_2, pattern = "\\?")


audit_data <- audit_data %>% 
  mutate(surgeon = as.factor(surgeon)) %>%
  mutate(anaethetist = as.factor(anaethetist)) %>%
  mutate(l_or_r = factor(l_or_r)) %>%
  mutate(op_type = factor(op_type)) %>% 
  #mutate(date_of_surgery = as.Date(dmy(date_of_surgery))) %>% 
  #mutate(time_to_theatre = strftime(time_to_theatre, format = "%H:%M:%OS", tz = "GMT")) %>% 
  #mutate(time_on_ward = strftime(time_on_ward, format = "%H:%M:%OS", tz = "GMT")) %>% 
  mutate(day1_eating = ifelse(day1_eating %in% c("Yes"), TRUE, FALSE)) %>% 
  mutate(day1_drinking = ifelse(day1_drinking %in% c("Yes"), TRUE, FALSE)) %>% 
  mutate(day1_mobilising = ifelse(day1_mobilising %in% c("Yes"), TRUE, FALSE)) %>% 
  mutate(eras_protocol = ifelse(eras_protocol %in% c("Yes"), TRUE, FALSE)) %>% 
  mutate(pain_day_0 = factor(pain_day_0, 
                             levels = c("None", "Mild", "Moderate", "Severe"),
                             ordered=TRUE)) %>% 
  mutate(pain_activity_day_0 = factor(pain_activity_day_0, 
                             levels = c("None", "Mild", "Moderate", "Severe"),
                             ordered=TRUE)) %>% 
  mutate(n_v_day_0 = ifelse(n_v_day_0 %in% c("Yes"), TRUE, FALSE)) %>%
  mutate(anti_emetic = ifelse(anti_emetic %in% c("Yes"), TRUE, FALSE)) %>%
  mutate(day_0_mob = ifelse(day_0_mob %in% c("Yes"), TRUE, FALSE)) %>%
  #mutate(mob_time_24h = str_replace_all(mob_time_24h, "?", "")) %>% 
  mutate(mob_time_24h = na_if(mob_time_24h,"")) %>% 
  #mutate(mob_time_24h = strftime(mob_time_24h, format = "%H:%M:%OS", tz = "GMT")) %>% 
  mutate(mob_by = factor(mob_by)) %>% 
  mutate(pain_day_1 = factor(pain_day_1, 
                             levels = c("None", "Mild", "Moderate", "Severe"),
                             ordered=TRUE)) %>% 
  mutate(pain_day_1 = factor(pain_day_1, 
                             levels = c("None", "Mild", "Moderate", "Severe"),
                             ordered=TRUE)) %>% 
  mutate(n_v_day_1 = ifelse(n_v_day_1 %in% c("Yes"), TRUE, FALSE)) %>%
  mutate(anti_emetic2 = ifelse(anti_emetic2 %in% c("Yes"), TRUE, FALSE)) %>%
  mutate(day_1_mob = ifelse(day_1_mob %in% c("Yes"), TRUE, FALSE)) %>%
 # mutate(mob_time_24h_3 = strftime(mob_time_24h_3, format = "%H:%M:%OS", tz = "GMT")) %>% 
  #mutate(date_1st_therapy_mob = as.Date(dmy(date_1st_therapy_mob))) %>% 
  #mutate(time_1st_therapy_mob = strftime(time_1st_therapy_mob, format = "%H:%M:%OS", tz = "GMT")) %>% 
  #mutate(date_discharge_from_therapy = as.Date(dmy(date_discharge_from_therapy))) %>% 
  #mutate(time_discharge_from_therapy = strftime(time_discharge_from_therapy, format = "%H:%M:%OS", tz = "GMT")) %>%
  #mutate(date_x_ray = as.Date(dmy(date_x_ray))) %>%  
  #mutate(time_x_ray = strftime(time_x_ray, format = "%H:%M:%OS", tz = "GMT")) %>%
  #mutate(date_tta_ready = as.Date(dmy(date_tta_ready))) %>%  
  #mutate(time_tta_ready = strftime(time_tta_ready, format = "%H:%M:%OS", tz = "GMT")) %>%
  #mutate(date_pt_left_ward = as.Date(dmy(date_pt_left_ward))) %>%  
  #mutate(time_pt_left_ward = strftime(time_pt_left_ward, format = "%H:%M:%OS", tz = "GMT")) %>%
  mutate(pain_severity_on_dc = factor(pain_severity_on_dc, 
                             levels = c("None", "Mild", "Moderate", "Severe"),
                             ordered=TRUE)) %>% 
  mutate(pain_activity_on_dc = factor(pain_activity_on_dc, 
                                      levels = c("None", "Mild", "Moderate", "Severe"),
                                      ordered=TRUE)) %>% 
  mutate(n_v_on_dc = ifelse(n_v_on_dc %in% c("Yes"), TRUE, FALSE)) %>%
  mutate(anti_emetic6 = ifelse(anti_emetic6 %in% c("Yes"), TRUE, FALSE))
  

#sort out date and times, remerge 

audit_data <-audit_data %>% 
  mutate(datetime_of_surgery = dmy_hms((str_c(date_of_surgery," ",time_to_theatre)), tz = "GMT")) %>%
  mutate(datetime_on_ward = dmy_hms((str_c(date_of_surgery," ",time_on_ward)), tz = "GMT")) %>%
  mutate(datetime_1st_therapy_mob = dmy_hms((str_c(date_1st_therapy_mob," ",time_1st_therapy_mob)), tz = "GMT")) %>%
  mutate(datetime_discharge_therapy = dmy_hms((str_c(date_discharge_from_therapy," ",time_discharge_from_therapy)), tz = "GMT")) %>%
  mutate(datetime_of_xray = dmy_hms((str_c(date_x_ray," ",time_x_ray)), tz = "GMT")) %>%
  mutate(datetime_tta = dmy_hms((str_c(date_tta_ready," ",time_tta_ready)), tz = "GMT")) %>%
  mutate(datetime_of_discharge = dmy_hms((str_c(date_pt_left_ward," ",time_pt_left_ward)), tz = "GMT")) %>% 
  mutate(los = difftime(datetime_of_discharge, datetime_of_surgery, units = "days")) %>% 
  mutate(mob_time_24h = as_hms(mob_time_24h)) %>% 
  mutate(mob_time_24h_2 = as_hms(mob_time_24h_2))
  

# Generate month year

audit_data <- 
  audit_data %>% 
  mutate(procedure_month = as.yearmon(as.Date(dmy(audit_data$date_of_surgery)), format = "%B %Y")) %>% 
  mutate(episode_id = row_number())
  
audit_data <- audit_data %>% 
  mutate(theatre_hour = as.numeric(format(datetime_of_surgery, format="%H")))

time_hist <- 
  audit_data %>% 
  mutate(theatre_hour = as.numeric(format(datetime_of_surgery, format="%H"))) %>% 
  mutate(ward_hour = as.numeric(format(datetime_on_ward, format="%H"))) %>%
  select(theatre_hour, ward_hour)
 # select(theatre_hour, ward_hour) %>% 
  #gather(cases, "year",  1:2) 

theatre_hist <-
  ggplot(time_hist, aes(x=theatre_hour))+
  geom_histogram(color="lightblue", fill="#0571b0", binwidth=1) +
  theme_PQIP() +
  scale_x_continuous(name = "start hour", breaks=seq(0,19,1))



ward_hist <-
  ggplot(time_hist, aes(x=ward_hour))+
  geom_histogram(color="lightblue", fill="#0571b0", binwidth=1, ) +
  theme_PQIP() +
  scale_x_continuous(name = "start hour", breaks=seq(0,21,1))
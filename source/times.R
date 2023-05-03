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

LOS <- 
  
  audit_data %>% 
  select(surgeon, op_type, los) %>% 
  group_by(surgeon, op_type) %>% 
  summarise(n=n(),
            median_LOS = median(los, na.rm = TRUE)) %>% 
  arrange(median_LOS) %>%
  ggplot( aes(x=surgeon, y=median_LOS)) +
 # geom_segment( aes(xend=surgeon, yend=0)) +
#  geom_point( size=4, color="orange") +
  geom_bar(stat = "identity", fill="#0571b0") +
  theme_PQIP() +
  theme(axis.text.y =element_blank())+
  facet_wrap(~op_type, scale="fixed") +
  labs(x = "Surgeon", y = "Median Length of stay (days),  Labels = number of patients") +
  scale_y_continuous(breaks = seq(0, 4, by = 1))+
  geom_text(aes(label = n), size=3,hjust=1.5, colour = "white") +
  coord_flip() 


LOS_anaes <- 
  
  audit_data %>% 
  select(anaethetist, op_type, los) %>% 
  group_by(anaethetist, op_type) %>% 
  summarise(n=n(),
            median_LOS = median(los, na.rm = TRUE)) %>% 
  arrange(median_LOS) %>%
  ggplot( aes(x=anaethetist, y=median_LOS)) +
   geom_bar(stat = "identity", fill="#0571b0") +
  theme_PQIP() +
  theme(axis.text.y =element_blank())+
  facet_wrap(~op_type, scale="fixed") +
  labs(x = "Anaesthetist", y = "Median Length of stay (days),  Labels = number of patients") +
  scale_y_continuous(breaks = seq(0, 4, by = 1))+
  geom_text(aes(label = n), size=3,hjust=1.5, colour = "white") +
  coord_flip() 

  
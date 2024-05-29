dreaming <-
  audit_data %>% 
  select(drinking_on_day_1, eating_on_day_1, mobilising_on_day_1, op_type, procedure_month) %>% 
  mutate(dream = ifelse(drinking_on_day_1 == TRUE & eating_on_day_1 == TRUE & mobilising_on_day_1 == TRUE, TRUE, FALSE)) 

perc_drink <- signif(sum(dreaming$drinking_on_day_1)/nrow(dreaming)*100,  digits = 3)
perc_eat <-  signif(sum(dreaming$eating_on_day_1)/nrow(dreaming)*100,  digits = 3)
perc_mobile <- signif(sum(dreaming$mobilising_on_day_1)/nrow(dreaming)*100,  digits = 3)
perc_dream <- signif(sum(dreaming$dream)/nrow(dreaming)*100,  digits = 3)

drinking <- dreaming %>% 
  group_by(procedure_month, drinking_on_day_1) %>% 
  summarise(n=n()) %>% 
  mutate(Perc = n/sum(n)*100) 

drink_plot <-
  ggplot(drinking, aes(x=as.Date(procedure_month), y=n, fill= drinking_on_day_1))+
  geom_bar(stat="identity") +
  theme_PQIP_legend() + 
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) + 
  scale_fill_manual(breaks=c(TRUE, FALSE), values = c("#0571b0", "#ca0020"),
                    labels=c("Drinking on Day 1", "Not Drinking on Day 1")) +
  guides(fill=guide_legend(title=NULL)) + 
  scale_y_continuous(name="Number of patients")+
  labs(x=NULL) 

eating <- dreaming %>% 
  group_by(procedure_month, eating_on_day_1) %>% 
  summarise(n=n()) %>% 
  mutate(Perc = n/sum(n)*100) 

eat_plot <-
  ggplot(eating, aes(x=as.Date(procedure_month), y=n, fill= eating_on_day_1))+
  geom_bar(stat="identity") +
  theme_PQIP_legend() + 
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) + 
  scale_fill_manual(breaks=c(TRUE, FALSE), values = c("#0571b0", "#ca0020"),
                    labels=c("Eating on Day 1", "Not Eating on Day 1")) +
  guides(fill=guide_legend(title=NULL)) + 
  scale_y_continuous(name="Number of patients")+
  labs(x=NULL) 


mobilising <- dreaming %>% 
  group_by(procedure_month, mobilising_on_day_1) %>% 
  summarise(n=n()) %>% 
  mutate(Perc = n/sum(n)*100)


mobile_plot <-
  ggplot(mobilising, aes(x=as.Date(procedure_month), y=n, fill= mobilising_on_day_1))+
  geom_bar(stat="identity") +
  theme_PQIP_legend() + 
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) + 
  scale_fill_manual(breaks=c(TRUE, FALSE), values = c("#0571b0", "#ca0020"),
                    labels=c("Mobilised on Day 1", "Not Mobilised on Day 1")) +
  guides(fill=guide_legend(title=NULL)) + 
  scale_y_continuous(name="Number of patients")+
  labs(x=NULL) 


dreaming <- dreaming %>% 
  group_by(procedure_month, dream) %>% 
  summarise(n=n()) %>% 
  mutate(Perc = n/sum(n)*100)



dream_plot <-
ggplot(dreaming, aes(x=as.Date(procedure_month), y=n, fill= dream))+
  geom_bar(stat="identity") +
  theme_PQIP_legend() + 
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) + 
  scale_fill_manual(breaks=c(TRUE, FALSE), values = c("#0571b0", "#ca0020"),
                    labels=c("DrEaMing", "Not DrEaMing")) +
  guides(fill=guide_legend(title=NULL)) + 
  scale_y_continuous(name="Number of patients")+
  labs(x=NULL) 


#pain

pain <- audit_data %>% 
  
  select(pain_day_0, pain_day_1, pain_severity_on_dc) %>% 
  filter(complete.cases(.)) %>% 
  group_by(pain_day_0, pain_day_1, pain_severity_on_dc) %>% 
  summarise(n=n()) 
 # mutate(perc=n/sum(n)*100) %>%
  #complete(pain_day_0, pain_day_1, pain_severity_on_dc) %>%
  #mutate(difference = (as.integer(pain_day_0) - as.integer(pain_day_1))) %>%
  #replace_na(., list(perc=0, n=0)) %>%
#  mutate(fill=ifelse(pain_day_1 > pain_day_0, "red", 
 #                    ifelse(pain_day_1 == pain_day_0, "orange", "green")))

pain_alluvial <-


ggplot(pain, aes(y = n, axis1 = pain_day_0 , axis2 = pain_day_1 , axis3 =pain_severity_on_dc)) +
  geom_alluvium(aes(fill = pain_day_0), width = 1/12) +
  geom_stratum(width = 1/4, reverse = TRUE, color = "grey" ) +
  geom_label(stat = "stratum", aes(label = after_stat(stratum)))+
  geom_text(aes(x = 1, y = -10, label = "Day 0"), color = "black", size = 3) +  # Label for Axis 1 (adjust position as needed)
  geom_text(aes(x = 2, y = -10, label = "Day 1"), color = "black", size = 3) +  # Label for Axis 2 (adjust position as needed)
  geom_text(aes(x = 3, y = -10, label = "At discharge"), color = "black", size = 3) +  # Label for Axis 3 (adjust position as needed)
  guides(fill=guide_legend(title="Pain on Day 0"))+
  labs(x = "Post operative day", y = "Number of patients") +
  scale_x_discrete(labels = c("Pre-operative", "Day 0", "Day 1"))

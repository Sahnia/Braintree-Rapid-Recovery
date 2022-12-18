dreaming <-
  audit_data %>% 
  select(day1_drinking, day1_eating, day1_mobilising, op_type, procedure_month) %>% 
  mutate(dream = ifelse(day1_drinking == TRUE & day1_eating == TRUE & day1_mobilising == TRUE, TRUE, FALSE)) 

perc_drink <- signif(sum(dreaming$day1_drinking)/nrow(dreaming)*100,  digits = 3)
perc_eat <-  signif(sum(dreaming$day1_eating)/nrow(dreaming)*100,  digits = 3)
perc_mobile <- signif(sum(dreaming$day1_mobilising)/nrow(dreaming)*100,  digits = 3)
perc_dream <- signif(sum(dreaming$dream)/nrow(dreaming)*100,  digits = 3)

drinking <- dreaming %>% 
  group_by(procedure_month, day1_drinking) %>% 
  summarise(n=n()) %>% 
  mutate(Perc = n/sum(n)*100) 

drink_plot <-
  ggplot(drinking, aes(x=as.Date(procedure_month), y=n, fill= day1_drinking))+
  geom_bar(stat="identity") +
  theme_PQIP_legend() + 
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) + 
  scale_fill_manual(breaks=c(TRUE, FALSE), values = c("#0571b0", "#ca0020"),
                    labels=c("Drinking on Day 1", "Not Drinking on Day 1")) +
  guides(fill=guide_legend(title=NULL)) + 
  scale_y_continuous(name="Number of patients")+
  labs(x=NULL) 

eating <- dreaming %>% 
  group_by(procedure_month, day1_eating) %>% 
  summarise(n=n()) %>% 
  mutate(Perc = n/sum(n)*100) 

eat_plot <-
  ggplot(eating, aes(x=as.Date(procedure_month), y=n, fill= day1_eating))+
  geom_bar(stat="identity") +
  theme_PQIP_legend() + 
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) + 
  scale_fill_manual(breaks=c(TRUE, FALSE), values = c("#0571b0", "#ca0020"),
                    labels=c("Eating on Day 1", "Not Eating on Day 1")) +
  guides(fill=guide_legend(title=NULL)) + 
  scale_y_continuous(name="Number of patients")+
  labs(x=NULL) 


mobilising <- dreaming %>% 
  group_by(procedure_month, day1_mobilising) %>% 
  summarise(n=n()) %>% 
  mutate(Perc = n/sum(n)*100)


mobile_plot <-
  ggplot(mobilising, aes(x=as.Date(procedure_month), y=n, fill= day1_mobilising))+
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
  group_by(pain_day_0, pain_day_1, pain_severity_on_dc) %>% 
  summarise(n=n())%>% 
 # mutate(perc=n/sum(n)*100) %>%
  #complete(pain_day_0, pain_day_1, pain_severity_on_dc) %>%
  #mutate(difference = (as.integer(pain_day_0) - as.integer(pain_day_1))) %>%
  replace_na(., list(perc=0, n=0)) %>%
  mutate(fill=ifelse(pain_day_1 > pain_day_0, "red", 
                     ifelse(pain_day_1 == pain_day_0, "orange", "green")))

is_alluvia_form(as.data.frame(UCBAdmissions), axes = 1:3, silent = TRUE)


ggplot(pain,
      aes(y = n, axis1 = pain_day_0 , axis2 = pain_day_1 , axis3 =pain_severity_on_dc     )) +
  geom_alluvium(aes(fill = pain_day_0), width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("Gender", "Dept"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  ggtitle("UC Berkeley admissions and rejections, by sex and department")
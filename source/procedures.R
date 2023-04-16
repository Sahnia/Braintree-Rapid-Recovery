procedures <- audit_data %>% 
  select(episode_id, procedure_month) %>% 
 
  group_by(procedure_month) %>% 
  summarise(n=n()) %>% 
  mutate(procedure_month = as.Date(procedure_month)) %>% 
  drop_na(procedure_month)

procedure_bar <- ggplot(data = procedures, mapping = aes(x=as.Date(procedure_month), y=n)) +
  geom_bar(stat="identity", position = "dodge", fill= "#0571B0") +
  labs(x = "Month", y = "Number of arthroplasties performed") +
  theme_PQIP() +
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) +
  scale_y_continuous(breaks = seq(0,max(procedures$n)+10, by=5))




tkr <- audit_data %>% 
  filter(op_type == "Total Knee") %>% 
  select(episode_id, procedure_month) %>% 
  distinct(episode_id, .keep_all = TRUE) %>% 
  group_by(procedure_month) %>% 
  summarise(n=n()) %>% 
  mutate(procedure_month = as.Date(procedure_month)) %>% 
  drop_na(procedure_month)

breakdown <- 
  audit_data %>% 
  select(episode_id, op_type, procedure_month) %>%
  group_by(procedure_month, op_type) %>% 
  summarise(n=n()) %>% 
  mutate(procedure_month = as.Date(procedure_month)) %>% 
  drop_na(procedure_month)

procedure_break <- 
  ggplot(breakdown, aes(fill=op_type, y=n, x=procedure_month)) + 
  geom_bar(position="dodge", stat="identity", colour = "black") +
  scale_fill_manual(name = "Operation type",
    values = c("#054C70","#05C3DE", "#2c7fb8"))+
  labs(x = "Month", y="Number of Patients") +
    theme_PQIP() +
  theme(legend.position="right") +
  guides(fill = guide_legend(reverse=FALSE)) +
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) +
  #scale_y_continuous(breaks = NULL) +
  theme(axis.title.y = element_blank()) 


tkr_bar <- ggplot(data = tkr, mapping = aes(x=as.Date(procedure_month), y=n)) +
  geom_bar(stat="identity", position = "dodge", fill= "#0571B0") +
  labs(x = "Month", y = "Number of total knee replacements performed") +
  theme_PQIP() +
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) +
  scale_y_continuous(breaks = seq(0,max(procedures$n)+10, by=5))


anaes <- audit_data %>% 
  select(episode_id, anaethetist, procedure_month) %>%
  group_by(procedure_month, anaethetist) %>% 
  summarise(n=n()) %>% 
  mutate(procedure_month = as.Date(procedure_month)) %>% 
  drop_na(procedure_month)


anaes_facet <-
  ggplot(anaes, aes(x=procedure_month, y=n, group=anaethetist, fill=anaethetist)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~anaethetist, scale="fixed") +
  theme_PQIP() +
  theme(panel.margin=unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        strip.background = element_rect(color = "black", size = 0.5),
        strip.text.x = element_blank()) +
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) +
  labs(x = "Month", y = "Number of anaesthetics performed")

surgeons <-
  audit_data %>% 
  select(episode_id, surgeon, procedure_month) %>%
  group_by(procedure_month, surgeon) %>% 
  summarise(n=n()) %>% 
  mutate(procedure_month = as.Date(procedure_month)) %>% 
  drop_na(procedure_month)

#ggplot(surgeons, aes(x = procedure_month, y = surgeon, fill = surgeon)) +
#  geom_density_ridges() +
#  theme_ridges() + 
 # theme(legend.position = "none")


Surgeon_facet <-
ggplot(surgeons, aes(x=procedure_month, y=n, group=surgeon, fill=surgeon)) + 
  geom_bar(stat = "identity") +
  facet_wrap(~surgeon, scale="fixed") +
  theme_PQIP() +
  theme(panel.margin=unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
        strip.background = element_rect(color = "black", size = 0.5),
        strip.text.x = element_blank()) +
  scale_x_date(labels = date_format("%b %Y"), breaks = date_breaks("1 month")) +
  labs(x = "Month", y = "Number of procedures performed")
  

work <- 
  audit_data %>% 
  group_by(surgeon, anaethetist) %>% 
  summarise(n=n())%>% 
  filter(!is.na(surgeon))
  
 work_tree <-  ggplot(work, aes(area = n, fill = surgeon, label = surgeon,
                  subgroup = surgeon, subgroup2 = anaethetist)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup2_border() +
  theme_PQIP() +
   geom_treemap_subgroup_border(colour = "white", size = 5) +
   geom_treemap_subgroup2_border(colour = "white", size = 2)

library(redcapAPI)
library(stringr)
library(dplyr)
source("tokens.R")
api.url <- maternal_api
#togo_bl_token
api.token <- togo_bl_token

#rodion excel:
#penta2, penta3, rr1 and passive detection complete or not, numbers inside soc or multiply cohort
# recruitment_complete, id_complete, clinical_history_complete, vaccination_status_complete, intervention_complete,
# tests_complete, health_facility_complete, community_complete, withdrawal_complete, death_complete

rcon <- redcapConnection(api.url, api.token)

my.fields <- c('record_id', 'screening_district', 'screening_hf', 'screening_study_number_cohort','screening_child_number',
               'recruitment_complete', 'id_complete', 'clinical_history_complete', 'vaccination_status_complete',
               'intervention_complete', 'tests_complete', 'health_facility_complete', 'community_complete',
               'withdrawal_complete', 'death_complete')
               
               # 'death_complete','withdrawal_complete', 'wdrawal_reason', 'community_complete','health_facility_complete',
               # 'morb_malaria_result', 'unsch_malaria_blood_result', 'unsch_malaria_rdt_result', 'tests_complete', 'rdt_malaria_result',
               # 'his_where', 'screening_dob_weeks', 'int_sp', 'his_fill_date')
my.events <- c('penta2ipti1_3_mont_arm_1',
               'penta3ipti_2_4_mon_arm_1',
               'mrv1ipti_3_9_month_arm_1',
               'mrv2ipti_4_15_mont_arm_1',
               'active_detection_arm_1',
               'passive_detection_arm_1',
               'end_of_fu_arm_1')

data_rod <- data.frame()

data_rod <- exportRecords(
  rcon,
  factors = F,
  fields = my.fields,
  events = my.events,
  form_complete_auto = F
  
)

data_clean <- data_rod
data_clean$study_number <- paste('COH',data_rod$screening_district,data_rod$screening_hf,
                                 data_rod$screening_study_number_cohort,
                                 str_pad(data_clean$screening_child_number, 3,side = 'left', pad = "0"),
                                 sep = '-')




# Data separated in different arms. we separate into different dataframes by its arm name
penta2_3m <- data_clean[data_clean$redcap_event_name == 'penta2ipti1_3_mont_arm_1',]
penta3_4m <- data_clean[data_clean$redcap_event_name == 'penta3ipti_2_4_mon_arm_1',]
mrv1_9m <- data_clean[data_clean$redcap_event_name == 'mrv1ipti_3_9_month_arm_1',]
mrv2_15m <- data_clean[data_clean$redcap_event_name == 'mrv2ipti_4_15_mont_arm_1',]
active_detection <- data_clean[data_clean$redcap_event_name == 'active_detection_arm_1',]
passive_detection <- data_clean[data_clean$redcap_event_name == 'passive_detection_arm_1',]
#end_fu <- data[data_clean$redcap_event_name == 'end_of_fu_arm_1',]

#passive detection ironically not detected correctly


# add cohort value to other events than penta2_3m:
penta2_soc <- penta2_3m[penta2_3m$screening_study_number_cohort ==1 & !is.na(penta2_3m$screening_child_number),]
penta2_multi <- penta2_3m[penta2_3m$screening_study_number_cohort ==2 & !is.na(penta2_3m$screening_child_number),]

#penta3
penta3_4m_cohort <- penta3_4m
penta3_4m_cohort$cohort <- ifelse(penta3_4m_cohort$record_id %in% penta2_soc$record_id, 1, 2)

#mrv1
mrv1_9m_cohort <- mrv1_9m
mrv1_9m_cohort$cohort <- ifelse(mrv1_9m_cohort$record_id %in% penta2_soc$record_id, 1, 2)

#mrv2
mrv2_15m_cohort <- mrv2_15m
mrv2_15m_cohort$cohort <- ifelse(mrv2_15m_cohort$record_id %in% penta2_soc$record_id, 1, 2)

#passive detection crf:
passive_detection_cohort <- passive_detection
passive_detection_cohort$cohort <- ifelse(passive_detection_cohort$record_id %in% penta2_soc$record_id, 1, 2)




###penta2 complete crf:

#soc cohort
data_clean_penta2_soc <- penta2_3m %>%
  filter(recruitment_complete == 2 & clinical_history_complete == 2 & vaccination_status_complete == 2 &
           intervention_complete == 2 & screening_study_number_cohort == 1) #254

#multiply cohort
data_clean_penta2_multi <- penta2_3m %>%
  filter(recruitment_complete == 2 & clinical_history_complete == 2 & vaccination_status_complete == 2 &
           intervention_complete == 2 & screening_study_number_cohort == 2) #257



###penta3 complete crf

#soc cohort
data_clean_penta3_soc <- penta3_4m_cohort %>%
  filter(clinical_history_complete == 2 & vaccination_status_complete == 2 &
           intervention_complete == 2 & cohort == 1) #119

#multiply cohort
data_clean_penta3_multi <- penta3_4m_cohort %>%
  filter(clinical_history_complete == 2 & vaccination_status_complete == 2 &
           intervention_complete == 2 & cohort == 2) #0

###mrv1 complete crf

#soc cohort
data_clean_mrv1_soc <- mrv1_9m_cohort %>%
  filter(clinical_history_complete == 2 & vaccination_status_complete == 2 &
           intervention_complete == 2 & cohort == 1) #0

#multiply cohort
data_clean_mrv1_multi <- mrv1_9m_cohort %>%
  filter(clinical_history_complete == 2 & vaccination_status_complete == 2 &
           intervention_complete == 2 & cohort == 2) #0


###mrv2 complete crf

#soc cohort
data_clean_mrv2_soc <- mrv2_15m_cohort %>%
  filter(clinical_history_complete == 2 & vaccination_status_complete == 2 &
           intervention_complete == 2 & cohort == 1) #4

#multiply cohort
data_clean_mrv2_multi <- mrv2_15m_cohort %>%
  filter(clinical_history_complete == 2 & vaccination_status_complete == 2 &
           intervention_complete == 2 & cohort == 2) #0

#updatea el track changes PALETO
###passive detection crf:
data_clean_passive_soc <- passive_detection_cohort %>%
  filter((health_facility_complete == 2 & cohort == 1) | (community_complete == 2 & cohort == 1)) #0

#multiply cohort
data_clean_passive_multi <- passive_detection_cohort %>%
  filter((health_facility_complete == 2 & cohort == 2)| (community_complete == 2 & cohort == 2)) #0


#update to stratify by HF

#before create df that checks the id and HF of each ID
data_clean_hf <- data_clean %>% 
  select(record_id,screening_hf) %>% 
  filter(!is.na(screening_hf))


#remove screening_hf from all the dfs that have them as missing and merge with the data_clean_hf so they are correctly labelled
data_clean_penta2_soc <- data_clean_penta2_soc %>% 
  select(-screening_hf)
data_clean_penta2_multi <- data_clean_penta2_multi %>% 
  select(-screening_hf)
data_clean_penta3_soc <- data_clean_penta3_soc %>% 
  select(-screening_hf)
data_clean_penta3_multi <- data_clean_penta3_multi %>% 
  select(-screening_hf)
data_clean_mrv1_soc <- data_clean_mrv1_soc %>% 
  select(-screening_hf)
data_clean_mrv1_multi <- data_clean_mrv1_multi %>% 
  select(-screening_hf)
data_clean_mrv2_soc <- data_clean_mrv2_soc %>% 
  select(-screening_hf)
data_clean_mrv2_multi <- data_clean_mrv2_multi %>% 
  select(-screening_hf)
data_clean_passive_soc <- data_clean_passive_soc %>% 
  select(-screening_hf)
data_clean_passive_multi <- data_clean_passive_multi %>% 
  select(-screening_hf)


data_clean_penta2_soc <- merge(data_clean_penta2_soc, data_clean_hf, by= 'record_id')
data_clean_penta3_soc <- merge(data_clean_penta3_soc, data_clean_hf, by= 'record_id')  
data_clean_mrv1_soc <- merge(data_clean_mrv1_soc, data_clean_hf, by= 'record_id')
data_clean_mrv2_soc <- merge(data_clean_mrv2_soc, data_clean_hf, by= 'record_id')
data_clean_passive_soc <- merge(data_clean_passive_soc, data_clean_hf, by= 'record_id')

data_clean_penta2_multi <- merge(data_clean_penta2_multi, data_clean_hf, by= 'record_id')
data_clean_penta3_multi <- merge(data_clean_penta3_multi, data_clean_hf, by= 'record_id')  
data_clean_mrv1_multi <- merge(data_clean_mrv1_multi, data_clean_hf, by= 'record_id')
data_clean_mrv2_multi <- merge(data_clean_mrv2_multi, data_clean_hf, by= 'record_id')
data_clean_passive_multi <- merge(data_clean_passive_multi, data_clean_hf, by= 'record_id')

#hf1 wahala
penta2_soc_hf1 <- data_clean_penta2_soc %>% 
  filter(screening_hf == '1')

penta2_multi_hf1 <- data_clean_penta2_multi %>% 
  filter(screening_hf == '1')

penta3_soc_hf1 <- data_clean_penta3_soc %>% 
  filter(screening_hf == '1')

penta3_multi_hf1 <- data_clean_penta3_multi %>% 
  filter(screening_hf == '1')

mrv1_soc_hf1 <- data_clean_mrv1_soc %>% 
  filter(screening_hf == '1')

mrv1_multi_hf1 <- data_clean_mrv1_multi %>% 
  filter(screening_hf == '1')

mrv2_soc_hf1 <- data_clean_mrv2_soc %>% 
  filter(screening_hf == '1')

mrv2_multi_hf1 <- data_clean_mrv2_multi %>% 
  filter(screening_hf == '1')

passive_soc_hf1 <- data_clean_passive_soc %>% 
  filter(screening_hf == '1')

passive_multi_hf1 <- data_clean_passive_multi %>% 
  filter(screening_hf == '1')

# hf2 Amakpape
penta2_soc_hf2 <- data_clean_penta2_soc %>% 
  filter(screening_hf == '2')

penta2_multi_hf2 <- data_clean_penta2_multi %>% 
  filter(screening_hf == '2')

penta3_soc_hf2 <- data_clean_penta3_soc %>% 
  filter(screening_hf == '2')

penta3_multi_hf2 <- data_clean_penta3_multi %>% 
  filter(screening_hf == '2')

mrv1_soc_hf2 <- data_clean_mrv1_soc %>% 
  filter(screening_hf == '2')

mrv1_multi_hf2 <- data_clean_mrv1_multi %>% 
  filter(screening_hf == '2')

mrv2_soc_hf2 <- data_clean_mrv2_soc %>% 
  filter(screening_hf == '2')

mrv2_multi_hf2 <- data_clean_mrv2_multi %>% 
  filter(screening_hf == '2')

passive_soc_hf2 <- data_clean_passive_soc %>% 
  filter(screening_hf == '2')

passive_multi_hf2 <- data_clean_passive_multi %>% 
  filter(screening_hf == '2')

# hf3 Hahomegbe
penta2_soc_hf3 <- data_clean_penta2_soc %>% 
  filter(screening_hf == '3')

penta2_multi_hf3 <- data_clean_penta2_multi %>% 
  filter(screening_hf == '3')

penta3_soc_hf3 <- data_clean_penta3_soc %>% 
  filter(screening_hf == '3')

penta3_multi_hf3 <- data_clean_penta3_multi %>% 
  filter(screening_hf == '3')

mrv1_soc_hf3 <- data_clean_mrv1_soc %>% 
  filter(screening_hf == '3')

mrv1_multi_hf3 <- data_clean_mrv1_multi %>% 
  filter(screening_hf == '3')

mrv2_soc_hf3 <- data_clean_mrv2_soc %>% 
  filter(screening_hf == '3')

mrv2_multi_hf3 <- data_clean_mrv2_multi %>% 
  filter(screening_hf == '3')

passive_soc_hf3 <- data_clean_passive_soc %>% 
  filter(screening_hf == '3')

passive_multi_hf3 <- data_clean_passive_multi %>% 
  filter(screening_hf == '3')

# hf4 Tetetou
penta2_soc_hf4 <- data_clean_penta2_soc %>% 
  filter(screening_hf == '4')

penta2_multi_hf4 <- data_clean_penta2_multi %>% 
  filter(screening_hf == '4')

penta3_soc_hf4 <- data_clean_penta3_soc %>% 
  filter(screening_hf == '4')

penta3_multi_hf4 <- data_clean_penta3_multi %>% 
  filter(screening_hf == '4')

mrv1_soc_hf4 <- data_clean_mrv1_soc %>% 
  filter(screening_hf == '4')

mrv1_multi_hf4 <- data_clean_mrv1_multi %>% 
  filter(screening_hf == '4')

mrv2_soc_hf4 <- data_clean_mrv2_soc %>% 
  filter(screening_hf == '4')

mrv2_multi_hf4 <- data_clean_mrv2_multi %>% 
  filter(screening_hf == '4')

passive_soc_hf4 <- data_clean_passive_soc %>% 
  filter(screening_hf == '4')

passive_multi_hf4 <- data_clean_passive_multi %>% 
  filter(screening_hf == '4')

## count!

#penta2soc
nrow(data_clean_penta2_soc)
#penta2multi
nrow(data_clean_penta2_multi)
#penta3soc
nrow(data_clean_penta3_soc)
#penta3multi
nrow(data_clean_penta3_multi)
##rr1soc
nrow(data_clean_mrv1_soc)
#rr13multi
nrow(data_clean_mrv1_multi)
#rr2soc
nrow(data_clean_mrv2_soc)
#rr2multi
nrow(data_clean_mrv2_multi)

#rr2soc

#rr2multi

##passivesoc
nrow(data_clean_passive_soc)
#passivemulti
nrow(data_clean_passive_multi)









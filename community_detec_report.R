library(redcapAPI)
library(stringr)
library(dplyr)
library(openxlsx)
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
               'recruitment_complete','cd_date', 'fever', 'malaria_rdt', 'community_detection_complete')

# 'death_complete','withdrawal_complete', 'wdrawal_reason', 'community_complete','health_facility_complete',
# 'morb_malaria_result', 'unsch_malaria_blood_result', 'unsch_malaria_rdt_result', 'tests_complete', 'rdt_malaria_result',
# 'his_where', 'screening_dob_weeks', 'int_sp', 'his_fill_date')
my.events <- c('penta2ipti1_3_mont_arm_1',
               'community_detectio_arm_1')

data_comm <- data.frame()

data_comm <- exportRecords(
  rcon,
  factors = F,
  fields = my.fields,
  events = my.events,
  form_complete_auto = F
  
)

data_clean_comm <- data_comm
data_clean_comm$study_number <- paste('COH',data_comm$screening_district,data_comm$screening_hf,
                                      data_comm$screening_study_number_cohort,
                                 str_pad(data_comm$screening_child_number, 3,side = 'left', pad = "0"),
                                 sep = '-')
# Data separated in different arms. we separate into different dataframes by its arm name
penta2_3m_comm <- data_clean_comm[data_clean_comm$redcap_event_name == 'penta2ipti1_3_mont_arm_1',]
comm_detection <- data_clean_comm[data_clean_comm$redcap_event_name == 'community_detectio_arm_1',]


# add cohort value to other events than penta2_3m:
penta2_soc_comm <- penta2_3m_comm[penta2_3m_comm$screening_study_number_cohort ==1 & !is.na(penta2_3m_comm$screening_child_number),]
penta2_multi_comm <- penta2_3m_comm[penta2_3m_comm$screening_study_number_cohort ==2 & !is.na(penta2_3m_comm$screening_child_number),]


#community detection
comm_detection_cohort <- comm_detection
comm_detection_cohort$cohort <- ifelse(comm_detection_cohort$record_id %in% penta2_soc_comm$record_id, 1, 2)



###penta2 complete crf:

#soc cohort
penta2_3m_comm_soc <- penta2_3m_comm %>%
  filter(recruitment_complete == 2 & screening_study_number_cohort == 1) #254

#multiply cohort
penta2_3m_comm_multi <- penta2_3m_comm %>%
  filter(recruitment_complete == 2 & screening_study_number_cohort == 2) #257


###community detectioncomplete crf:

#soc cohort
comm_detection_soc <- comm_detection_cohort %>%
  filter(community_detection_complete == 2 & cohort == 1)

#multiply cohort
comm_detection_multi <- comm_detection_cohort %>%
  filter(community_detection_complete == 2 & cohort == 2)


#update to stratify by HF

#before create df that checks the id and HF of each ID
data_clean_hf <- data_comm %>% 
  select(record_id,screening_hf) %>% 
  filter(!is.na(screening_hf))


#remove screening_hf from all the dfs that have them as missing and merge with the data_clean_hf so they are correctly labelled
penta2_3m_comm_soc <- penta2_3m_comm_soc %>% 
  select(-screening_hf)
penta2_3m_comm_multi <- penta2_3m_comm_multi %>% 
  select(-screening_hf)

# comm detection
comm_detection_soc <- comm_detection_soc %>% 
  select(-screening_hf)
comm_detection_multi <- comm_detection_multi %>% 
  select(-screening_hf)



#merge
penta2_3m_comm_soc <- merge(penta2_3m_comm_soc, data_clean_hf, by= 'record_id')
penta2_3m_comm_multi <- merge(penta2_3m_comm_multi, data_clean_hf, by= 'record_id') 

comm_detection_soc <- merge(comm_detection_soc, data_clean_hf, by= 'record_id')
comm_detection_multi <- merge(comm_detection_multi, data_clean_hf, by= 'record_id')





#hf1 wahala
# penta2_soc_hf1 <- penta2_3m_comm_soc %>% 
#   filter(screening_hf == '1')

# penta2_multi_hf1 <- data_clean_penta2_multi %>% 
#   filter(screening_hf == '1')


#hf1 wahala
comm_detection_soc_hf1 <- comm_detection_soc %>% 
  filter(screening_hf == '1')

comm_detection_multi_hf1 <- comm_detection_multi %>% 
  filter(screening_hf == '1')



# hf2 Amakpape
comm_detection_soc_hf2 <- comm_detection_soc %>% 
  filter(screening_hf == '2')

comm_detection_multi_hf2 <- comm_detection_multi %>% 
  filter(screening_hf == '2')

# hf3 Hahomegbe


comm_detection_soc_hf3 <- comm_detection_soc %>% 
  filter(screening_hf == '3')

comm_detection_multi_hf3 <- comm_detection_multi %>% 
  filter(screening_hf == '3')


# hf4 Tetetou


comm_detection_soc_hf4 <- comm_detection_soc %>% 
  filter(screening_hf == '4')

comm_detection_multi_hf4 <- comm_detection_multi %>% 
  filter(screening_hf == '4')


## count!
# ara toca separar per mesos:

comm_detection_soc_hf1[months(comm_detection_soc_hf1$cd_date) %in% month.name[6:9],]


hf1_jan <- comm_detection_soc %>% 
  filter(cd_date >= '2023-01-01' & 
           cd_date <= '2023-01-31')  #afegint esto tenim el num de visites de gener %>% nrow()

#la idea es crear la table en compte de posar hf1_jan, fer tot el filtro dplyr i m ahorre crear totes les variables prèviament






#create automated table (it's about time my man)







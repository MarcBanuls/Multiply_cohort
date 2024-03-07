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
#remove comm detection that was not done (empty row)
comm_detection_cohort <- comm_detection_cohort %>% 
  filter(community_detection_complete != 0)

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

comm_detection_cohort <- comm_detection_cohort %>% 
  select(-screening_hf)
comm_detection_soc <- comm_detection_soc %>% 
  select(-screening_hf)
comm_detection_multi <- comm_detection_multi %>% 
  select(-screening_hf)



#merge
penta2_3m_comm_soc <- merge(penta2_3m_comm_soc, data_clean_hf, by= 'record_id')
penta2_3m_comm_multi <- merge(penta2_3m_comm_multi, data_clean_hf, by= 'record_id') 

comm_detection_cohort <- merge(comm_detection_cohort, data_clean_hf, by= 'record_id')
comm_detection_soc <- merge(comm_detection_soc, data_clean_hf, by= 'record_id')
comm_detection_multi <- merge(comm_detection_multi, data_clean_hf, by= 'record_id')






#hf1 wahala
# penta2_soc_hf1 <- penta2_3m_comm_soc %>% 
#   filter(screening_hf == '1')

# penta2_multi_hf1 <- data_clean_penta2_multi %>% 
#   filter(screening_hf == '1')


#hf1 wahala

comm_detection_hf1 <- comm_detection_cohort %>% 
  filter(screening_hf == '1')

comm_detection_soc_hf1 <- comm_detection_soc %>% 
  filter(screening_hf == '1')

comm_detection_multi_hf1 <- comm_detection_multi %>% 
  filter(screening_hf == '1')



# hf2 Amakpape

comm_detection_hf2 <- comm_detection_cohort %>% 
  filter(screening_hf == '2')

comm_detection_soc_hf2 <- comm_detection_soc %>% 
  filter(screening_hf == '2')

comm_detection_multi_hf2 <- comm_detection_multi %>% 
  filter(screening_hf == '2')

# hf3 Hahomegbe

comm_detection_hf3 <- comm_detection_cohort %>% 
  filter(screening_hf == '3')

comm_detection_soc_hf3 <- comm_detection_soc %>% 
  filter(screening_hf == '3')

comm_detection_multi_hf3 <- comm_detection_multi %>% 
  filter(screening_hf == '3')


# hf4 Tetetou

comm_detection_hf4 <- comm_detection_cohort %>% 
  filter(screening_hf == '4')

comm_detection_soc_hf4 <- comm_detection_soc %>% 
  filter(screening_hf == '4')

comm_detection_multi_hf4 <- comm_detection_multi %>% 
  filter(screening_hf == '4')




# we create rowcounts for each value of total and the 4 hfs

#### TOTAL ####
total_com <- c(
  #jan23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_cohort %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_cohort %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_cohort %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_cohort %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_cohort %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

total_com_soc <- c(
  #jan23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc %>% 
                 filter(fever == 1 & cd_date >= '2023-01-01' & 
                          cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-01-30') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_soc %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_soc %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_soc %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
               )

#multi
total_com_multi <- c(
  #jan23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-01-30') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_multi %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_multi %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_multi %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)


#### HF1 ####

total_com_hf1 <- c(
  #jan23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_hf1 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_hf1 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_hf1 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

total_com_soc_hf1 <- c(
  #jan23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-01-30') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_soc_hf1 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

#multi
total_com_multi_hf1 <- c(
  #jan23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-01-30') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_multi_hf1 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi_hf1 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

#### HF2 ####


total_com_hf2 <- c(
  #jan23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_hf2 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_hf2 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_hf2 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

total_com_soc_hf2 <- c(
  #jan23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-01-30') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_soc_hf2 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

#multi
total_com_multi_hf2 <- c(
  #jan23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-01-30') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_multi_hf2 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi_hf2 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)








#### HF3 ####

total_com_hf3 <- c(
  #jan23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_hf3 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_hf3 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_hf3 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

total_com_soc_hf3 <- c(
  #jan23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-01-30') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_soc_hf3 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

#multi
total_com_multi_hf3 <- c(
  #jan23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-01-30') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_multi_hf3 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi_hf3 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)
#### HF4 ####

total_com_hf4 <- c(
  #jan23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_hf4 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_hf4 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_hf4 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

total_com_soc_hf4 <- c(
  #jan23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_soc_hf4 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_soc_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

#multi
total_com_multi_hf4 <- c(
  #jan23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-01-01' & 
             cd_date <= '2023-01-31') %>% nrow(),
  
  #feb23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-02-01' & 
             cd_date <= '2023-02-28') %>% nrow(),
  
  #mar23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-03-01' & 
             cd_date <= '2023-03-31') %>% nrow(),
  
  #apr23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-04-01' & 
             cd_date <= '2023-04-30') %>% nrow(),
  
  #may23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-05-01' & 
             cd_date <= '2023-05-31') %>% nrow(),
  
  #jun23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-06-01' & 
             cd_date <= '2023-06-30') %>% nrow(),
  
  #jul23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-07-01' & 
             cd_date <= '2023-07-31') %>% nrow(),
  
  #aug23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-08-01' & 
             cd_date <= '2023-08-31') %>% nrow(),
  
  #sept23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-09-01' & 
             cd_date <= '2023-09-30') %>% nrow(),
  
  #oct23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-10-01' & 
             cd_date <= '2023-10-31') %>% nrow(),
  
  #nov23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-11-01' & 
             cd_date <= '2023-11-30') %>% nrow(),
  
  #dec23
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2023-12-01' & 
             cd_date <= '2023-12-31') %>% nrow(),
  
  #jan24
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-01-01' & 
             cd_date <= '2024-01-31') %>% nrow(),
  
  #feb24
  comm_detection_multi_hf4 %>% 
    filter(cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(fever == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow(),
  comm_detection_multi_hf4 %>% 
    filter(malaria_rdt == 1 & cd_date >= '2024-02-02' & 
             cd_date <= '2024-02-28') %>% nrow()
  
  
)

#### save files ####

# 
# #create automated table (it's about time my man)
# rownames <- c('PENTA 2', 'PENTA 3', 'RR 1', 'RR 2', 'PASSIVE DETECTION', 'ACTIVE DETECTION')
# track_changes <- data.frame(rownames,total,total_soc,total_multi,wahala_total,wahala_soc,wahala_multi,amakpape_total,amakpape_soc,amakpape_multi,
#                             hahomegbe_total,hahomegbe_soc,hahomegbe_multi,tetetou_total,tetetou_soc,tetetou_multi)


#for table names, we can repeat the "visits" "fever" and "malaria" 3 times per month ergo 42 times at the moment (14 months)
sample_names = c("Visits" ,"Fever", "Malaria_rdt")
r_names_table <- c(rep(sample_names,  times=14))
months<- c('Jan 23','Feb 23','Mar 23','Apr 23','May 23','Jun 23','Jul 23','Aug 23','Sep 23','Oct 23','Nov 23',' Dec 23',
           'Jan 24','Feb 24')
months <- c(rep(months,  each=3))
#add months as suffix
r_names_table <- paste(r_names_table,months)



final_df <- data.frame(r_names_table, total_com, total_com_soc, total_com_multi,
                       total_com_hf1, total_com_soc_hf1, total_com_multi_hf1,
                       total_com_hf2, total_com_soc_hf2, total_com_multi_hf2,
                       total_com_hf3, total_com_soc_hf3, total_com_multi_hf3,
                       total_com_hf4, total_com_soc_hf4, total_com_multi_hf4)

#change columns  

#added space on first column and no row names put, add them directly as a column
c_names_table_comm <- c(' ','Total-All', 'Total-SOC', 'Total-MULTIPLY','Wahala-All','Wahala-SOC', 'Wahala-MULTIPLY',
                        'Amakpape-All', 'Amakpape-SOC', 'Amakpape-MULTIPLY','Hahomegbe-All', 'Hahomegbe-SOC', 'Hahomegbe-MULTIPLY',
                        'Tetetou-All', 'Tetetou-SOC', 'Tetetou-MULTIPLY')


colnames(final_df) <- c_names_table_comm
#rownames(track_changes) <- r_names_table

#let's do it pretty
#http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r#write-data-to-an-excel-file
wb <- createWorkbook()
addWorksheet(wb,'comm_detection')
writeDataTable(wb,'comm_detection',final_df, startCol = 3,startRow = 3,colNames = TRUE,rowNames = FALSE,
               withFilter = FALSE)
saveWorkbook(wb,file = paste0('G:/Mi unidad/MULTIPLY/1.Cohorts/togo/reports/community_detection/','community_detection_',Sys.Date(),'.xlsx'))






















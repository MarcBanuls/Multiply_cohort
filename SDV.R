library(redcapAPI)
library(stringr)
library(dplyr)
source("tokens.R")
api.url <- maternal_api
#togo_bl_token
api.token <- togo_bl_token

# By the way, as you remember, we have discussed the set of indicators that need to be generated automatically for monitoring purpose. 
# Please note that we are interested in the following indicators:
#   
#   Number of children currently followed in each cohort (screenning_study_number_cohort =1 (soc) or =2 (multiply)) ADDED
# Number of LTF (number of children who missed the scheduled visit as of xx). DONT KNOW YET
# Number of deaths (death_complete == 2) ADDED
# Number of withdrawals (wdrawal_complete == 2) ADDED
# Number of children moving out of the area (wdrawal_reason == 3) ADDED
# Number of morbidity events (all causes) (community_complete == 2) ADDED
# Number of malaria cases (morb_malaria_result == 1, his_malaria_confirmed ==1, rdt_malaria_result == 1, unsch_malaria_rdt_result == 1,
#                         unsch_malaria_blood_result ==1, unsch_haemoglobin_result ==1) PENDING
# Number of hospitalisation (all causes) (unsch_hosp ==1 or unsch_ref_disc_hosp == 1??) PENDING
# Number of hospitalisation (due to malaria) (unsch_malaria_rdt_result == 1 or unsch_malaria_blood_result ==1 or unsch_haemoglobin_result ==1 AND 
#                                           (unsch_hosp ==1 or unsch_ref_disc_hosp == 1??)) PENDING

# I don't really know the complexity of generating such indicators via REDCap.
# We can discuss it in more details if you have any questions. Please let me know. 

rcon <- redcapConnection(api.url, api.token)

my.fields <- c('record_id', 'screening_district', 'screening_hf', 'screening_study_number_cohort','screening_child_number')
my.events <- c('penta2ipti1_3_mont_arm_1')

data <- data.frame()

data <- exportRecords(
  rcon,
  factors = F,
  fields = my.fields,
  events = my.events,
  form_complete_auto = F
  
)

data_clean <- data
data_clean$study_number <- paste('COH',data$screening_district,data$screening_hf,
                                 data$screening_study_number_cohort,
                                 str_pad(data_clean$screening_child_number, 3,side = 'left', pad = "0"),
                                 sep = '-')

data_study <- data_clean[,c('record_id','study_number')]


sample_30 <- data_study %>% sample_frac(.3)
write.csv(sample_30,file = "togo_cohort_penta2_sample_30.csv",row.names = FALSE)





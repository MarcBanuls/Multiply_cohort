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

my.fields <- c('record_id', 'screening_district', 'screening_hf', 'screening_study_number_cohort','screening_child_number',
               'int_sp', 'his_fill_date')
my.events <- c('penta2ipti1_3_mont_arm_1',
               'penta3ipti_2_4_mon_arm_1',
               'mrv1ipti_3_9_month_arm_1',
               'mrv2ipti_4_15_mont_arm_1',
               'active_detection_arm_1',
               'passive_detection_arm_1',
               'end_of_fu_arm_1')

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

data_clean <- data_clean[,-c(3:4)]


# Data separated in different arms. we separate into different dataframes by its arm name
penta2_3m <- data_clean[data_clean$redcap_event_name == 'penta2ipti1_3_mont_arm_1',]
penta3_4m <- data_clean[data_clean$redcap_event_name == 'penta3ipti_2_4_mon_arm_1',]
mrv1_9m <- data_clean[data_clean$redcap_event_name == 'mrv1ipti_3_9_month_arm_1',]
mrv2_15m <- data_clean[data_clean$redcap_event_name == 'mrv2ipti_4_15_mont_arm_1',]
active_detection <- data_clean[data_clean$redcap_event_name == 'active_detection_arm_1',]
passive_detection <- data_clean[data_clean$redcap_event_name == 'passive_detection_arm_1',]
end_fu <- data[data_clean$redcap_event_name == 'end_of_fu_arm_1',]


#######
#check when the "two weeks between recruitment and ipti administration is correct or not
#but if 5 weeks after penta 2 not show
#join penta2 and penta3

penta2_3 <- merge(penta2_3m,penta3_4m, by = 'record_id')


weektimes_penta2_3 <- as.data.frame(as.numeric(difftime(strptime(penta2_3$his_fill_date.y, format = "%Y-%m-%d"),
                                                                                             strptime(penta2_3$his_fill_date.x, format = "%Y-%m-%d"),units="weeks")))
weektimes_penta2_3 <- cbind(penta2_3$record_id,weektimes_penta2_3)
names(weektimes_penta2_3) <- c('record_id','difftime_weeks')

#pick the ones that have more than 2 weeks (14)
penta2_3_2w <- weektimes_penta2_3[weektimes_penta2_3$difftime_weeks > 14,]
# check which ones have more than 2 weeks (14) but less than 5 (35)
penta2_3_5w <- weektimes_penta2_3[weektimes_penta2_3$difftime_weeks > 14 & weektimes_penta2_3$difftime_weeks < 35,]

# check which ones have more than 5 weeks (35)
penta2_3_m5w <- weektimes_penta2_3[weektimes_penta2_3$difftime_weeks > 35,]





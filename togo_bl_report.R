library(redcapAPI)
library(stringr)
library(dplyr)
source("tokens.R")
api.url <- maternal_api
#togo_bl_token
api.token <- testing_cohort_token

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
               'death_complete','withdrawal_complete', 'wdrawal_reason', 'community_complete','health_facility_complete',
               'morb_malaria_result', 'unsch_malaria_blood_result', 'unsch_malaria_rdt_result', 'tests_complete', 'rdt_malaria_result')
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

-----


# number of cohort childs:
sum(data_clean$screening_study_number_cohort == 1, na.rm = TRUE)
sum(data_clean$screening_study_number_cohort == 2, na.rm = TRUE)

# number of fu for death:
sum(data_clean$death_complete == 2, na.rm = TRUE)

# number of fu for withdrawal:
sum(data_clean$withdrawal_complete == 2, na.rm = TRUE)

# number of wdrawal for migration:
sum(data_clean$wdrawal_reason == 3, na.rm = TRUE)

# Number of morbidity events (all causes) (community_complete == 2)
sum(data_clean$community_complete == 2, na.rm = TRUE)

#test malaria cases
# we want to know number of malaria cases. if a child has in ANY of the visits some of the malaria variables the "1" value,
# this child is counted as malaria positive.then we can have several "1" in one child in different rows. how to compute by each
# child that "if one record id has a 1 in the new column, add a 1 to the counter ONLY once"
# other option:make directly a "count" function that sums 1 only one time if the child is positive in any of the variables.

# check if a record_id has any of the variables wanted as 1. check by group?
#data_clean$malaria_positive <- ifelse(data_clean)
test <- data_clean %>%
  group_by(record_id)
test2 <- test %>%
  count(death_complete)

test %>% tally()
data_clean %>%
  count(record_id,rdt_malaria_result)

# Number of malaria cases
sum(data_clean$morb_malaria_result == 1, na.rm = TRUE)
sum(data_clean$unsch_malaria_blood_result == 1, na.rm = TRUE)
sum(data_clean$unsch_malaria_rdt_result == 1, na.rm = TRUE)
malariacases <- length(which(data_clean$redcap_event_name == 'penta2ipti1_3_mont_arm_1' & data_clean$rdt_malaria_result == 1))



----------
# NEEDED TO LINK RDT MALARIA EVENT WITH HHS before filtering by district.

# Data separated in different arms. we separate into different dataframes by its arm name
penta2_3m <- data_clean[data_clean$redcap_event_name == 'penta2ipti1_3_mont_arm_1',]
penta3_4m <- data_clean[data_clean$redcap_event_name == 'penta3ipti_2_4_mon_arm_1',]
mrv1_9m <- data_clean[data_clean$redcap_event_name == 'mrv1ipti_3_9_month_arm_1',]
mrv2_15m <- data_clean[data_clean$redcap_event_name == 'mrv2ipti_4_15_mont_arm_1',]
active_detection <- data_clean[data_clean$redcap_event_name == 'active_detection_arm_1',]
passive_detection <- data_clean[data_clean$redcap_event_name == 'passive_detection_arm_1',]
end_fu <- data[data_clean$redcap_event_name == 'end_of_fu_arm_1',]

# Remove the variables for each df that are empty (and arm name to avoid interference) CHANGED due to change of variables in TOGO
penta2_3m <- penta2_3m[,-c(2:4,9:11)]
end_fu <- end_fu[,-c(2:8)]

# Merge the two dataframes to have all data in one row per record_id
# (Named hhs_data due to reuse of tiptop functions that only worked with hhs data, not rdt)
cohort_data <- merge(recruitment, end_fu, by = "record_id", all.x = TRUE)

# Filter by district. Only 1 district in Togo, line not usable.
#hhs_data = hhs_data[hhs_data$district == study_area_id, ]
cohort_data = cohort_data[!is.na(cohort_data$record_id), ]

#number of withdrawal ids:
table(!is.na(cohort_data$wdrawal_reported_date))

#number of deaths ids:
table(!is.na(cohort_data$death_reported_date))

#number of children moving out of the area?? TO DO

#number of morbidity events (all causes) TO DO

#number of malaria cases TODO

#number of hospitalisation (allcauses) TODO

#number of hospitalisation (only malaria) TODO

#use reduce to merge more than 2 df in one function
Reduce(function(...) merge(..., all=TRUE), list(df1, df2, df3))


# use %in% to count number of values that appear in each df?
#sum(few %in% alot)


# ------------
#pass to excel

#create excel with:
# 
#   
# Number of children currently followed in each cohort (screenning_study_number_cohort =1 (soc) or =2 (multiply)) ADDED
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


#Number of SOC cohort
#Number of MULTIPLY cohort
#Number of missed visits
#Number of deaths
#Number of withdrawals
#Number of migrations
#Number of morbidity events
#Number of malaria cases
#Number of hospitalization all
#Number of hospitalization for malaria
indicators <- c('Number of SOC cohort', 'Number of MULTIPLY cohort', 'Number of missed visits',
                'Number of deaths', 'Number of withdrawals', 'Number of migrations', 'Number of morbidity events',
                'Number of malaria cases', 'Number of hospitalization all', 'Number of hospitalization for malaria')
number <- c(sum(data_clean$screening_study_number_cohort == 1, na.rm = TRUE), sum(data_clean$screening_study_number_cohort == 2, na.rm = TRUE),
            0, sum(data_clean$death_complete == 2, na.rm = TRUE), sum(data_clean$withdrawal_complete == 2, na.rm = TRUE), 
            sum(data_clean$wdrawal_reason == 3, na.rm = TRUE), 0, 0, 0, 0)
report <- data.frame(Indicators = indicators, N = number )

write.csv(report, file = paste0('multiply_togo_cohort_indicators_', Sys.Date(), '.csv'), row.names = F)











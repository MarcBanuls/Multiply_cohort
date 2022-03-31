library(redcapAPI)
library(stringr)
library(dplyr)
source("tokens.R")
api.url <- maternal_api
api.token <- togo_bl_token

# By the way, as you remember, we have discussed the set of indicators that need to be generated automatically for monitoring purpose. 
# Please note that we are interested in the following indicators:
#   
#   Number of children currently followed in each cohort (screenning_study_number_cohort =1 (soc) or =2 (multiply))
# Number of LTF (number of children who missed the scheduled visit as of xx). DONT KNOW YET
# Number of deaths (death_complete == 2)
# Number of withdrawals (wdrawal_complete == 2)
# Number of children moving out of the area (wdrawal_reason == 3)
# Number of morbidity events (all causes) (community_complete == 2)
# Number of malaria cases (morb_malaria_result == 1, his_malaria_confirmed ==1, rdt_malaria_result == 1, unsch_malaria_rdt_result == 1,
#                         unsch_malaria_blood_result ==1, unsch_haemoglobin_result ==1)
# Number of hospitalisation (all causes) (unsch_hosp ==1 or unsch_ref_disc_hosp == 1??)
# Number of hospitalisation (due to malaria) (unsch_malaria_rdt_result == 1 or unsch_malaria_blood_result ==1 or unsch_haemoglobin_result ==1 AND 
#                                           (unsch_hosp ==1 or unsch_ref_disc_hosp == 1??))

# I don't really know the complexity of generating such indicators via REDCap.
# We can discuss it in more details if you have any questions. Please let me know. 

rcon <- redcapConnection(api.url, api.token)

my.fields <- c('record_id', 'screening_district', 'screening_hf', 'screening_study_number_cohort','screening_child_number', 'wdrawal_reported_date','death_know','death_reported_date')
my.events <- c('penta2ipti1_3_mont_arm_1', 'end_of_fu_arm_1')

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


# NEEDED TO LINK RDT MALARIA EVENT WITH HHS before filtering by district.

# Data separated in two arms. we separate in two dataframes by its arm name
recruitment <- data_clean[data_clean$redcap_event_name == 'penta2ipti1_3_mont_arm_1',]
end_fu <- data[data_clean$redcap_event_name == 'end_of_fu_arm_1',]

# Remove the variables for each df that are empty (and arm name to avoid interferences) CHANGED due to change of variables in TOGO
recruitment <- recruitment[,-c(2:4,9:11)]
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





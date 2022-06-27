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
               'death_complete','withdrawal_complete', 'wdrawal_reason', 'community_complete','health_facility_complete',
               'morb_malaria_result', 'unsch_malaria_blood_result', 'unsch_malaria_rdt_result', 'tests_complete', 'rdt_malaria_result',
               'his_where', 'screening_dob_weeks', 'int_sp', 'his_fill_date')
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
cohort_data <- merge(penta2_3m, end_fu, by = "record_id", all.x = TRUE)

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
#Reduce(function(...) merge(..., all=TRUE), list(df1, df2, df3))


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

#####description cohort togo #####
# pick screening_dob_weeks, screening_hf and his_where cols 6,7,11
#his_where: 1=HF 2=outreach, screening_dob_weeks: 10-14weeks or 15-20weeks
#screening_hf:1	CMS Wahala 2	USP Amakpape 3	USP Hahomegbe 4	USP Tetetou
desc_cohort <- data_clean[,c(6,7,11)]
#filter from first desc_cohort, testing
#screening_hf=1,screening_dob_weeks>=10 and <=14, his_where =1
#wahala 10-14 fixed
nrow(desc_cohort[desc_cohort$screening_hf ==1 & desc_cohort$screening_dob_weeks >= 10 &
                             desc_cohort$screening_dob_weeks <=14 & desc_cohort$his_where ==1 &
                             !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                             !is.na(desc_cohort$his_where),])
#wahala 10-14 outreach
nrow(desc_cohort[desc_cohort$screening_hf ==1 & desc_cohort$screening_dob_weeks >= 10 &
                   desc_cohort$screening_dob_weeks <=14 & desc_cohort$his_where ==2 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#wahala 15-20 fixed
nrow(desc_cohort[desc_cohort$screening_hf ==1 & desc_cohort$screening_dob_weeks >= 15 &
                   desc_cohort$screening_dob_weeks <=20 & desc_cohort$his_where ==1 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#wahala 15-20 outreach
nrow(desc_cohort[desc_cohort$screening_hf ==1 & desc_cohort$screening_dob_weeks >= 15 &
                   desc_cohort$screening_dob_weeks <=20 & desc_cohort$his_where ==2 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#amakpape 10-14 fixed
nrow(desc_cohort[desc_cohort$screening_hf ==2 & desc_cohort$screening_dob_weeks >= 10 &
                   desc_cohort$screening_dob_weeks <=14 & desc_cohort$his_where ==1 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])
#amakpape 10-14 outreach
nrow(desc_cohort[desc_cohort$screening_hf ==2 & desc_cohort$screening_dob_weeks >= 10 &
                   desc_cohort$screening_dob_weeks <=14 & desc_cohort$his_where ==2 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#amakpape 15-20 fixed
nrow(desc_cohort[desc_cohort$screening_hf ==2 & desc_cohort$screening_dob_weeks >= 15 &
                   desc_cohort$screening_dob_weeks <=20 & desc_cohort$his_where ==1 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#amakpape 15-20 outreach
nrow(desc_cohort[desc_cohort$screening_hf ==2 & desc_cohort$screening_dob_weeks >= 15 &
                   desc_cohort$screening_dob_weeks <=20 & desc_cohort$his_where ==2 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])


#Hahomégbé 10-14 fixed
nrow(desc_cohort[desc_cohort$screening_hf ==3 & desc_cohort$screening_dob_weeks >= 10 &
                   desc_cohort$screening_dob_weeks <=14 & desc_cohort$his_where ==1 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])
#Hahomégbé 10-14 outreach
nrow(desc_cohort[desc_cohort$screening_hf ==3 & desc_cohort$screening_dob_weeks >= 10 &
                   desc_cohort$screening_dob_weeks <=14 & desc_cohort$his_where ==2 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#Hahomégbé 15-20 fixed
nrow(desc_cohort[desc_cohort$screening_hf ==3 & desc_cohort$screening_dob_weeks >= 15 &
                   desc_cohort$screening_dob_weeks <=20 & desc_cohort$his_where ==1 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#Hahomégbé 15-20 outreach
nrow(desc_cohort[desc_cohort$screening_hf ==3 & desc_cohort$screening_dob_weeks >= 15 &
                   desc_cohort$screening_dob_weeks <=20 & desc_cohort$his_where ==2 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])




#Tététou 10-14 fixed
nrow(desc_cohort[desc_cohort$screening_hf ==4 & desc_cohort$screening_dob_weeks >= 10 &
                   desc_cohort$screening_dob_weeks <=14 & desc_cohort$his_where ==1 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])
#Tététou 10-14 outreach
nrow(desc_cohort[desc_cohort$screening_hf ==4 & desc_cohort$screening_dob_weeks >= 10 &
                   desc_cohort$screening_dob_weeks <=14 & desc_cohort$his_where ==2 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#Tététou 15-20 fixed
nrow(desc_cohort[desc_cohort$screening_hf ==4 & desc_cohort$screening_dob_weeks >= 15 &
                   desc_cohort$screening_dob_weeks <=20 & desc_cohort$his_where ==1 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

#Tététou 15-20 outreach
nrow(desc_cohort[desc_cohort$screening_hf ==4 & desc_cohort$screening_dob_weeks >= 15 &
                   desc_cohort$screening_dob_weeks <=20 & desc_cohort$his_where ==2 &
                   !is.na(desc_cohort$screening_hf) & !is.na(desc_cohort$screening_dob_weeks) &
                   !is.na(desc_cohort$his_where),])

###### remove penta3 visits to check duplicate codes
data_clean_coh<-data_clean[!data_clean$redcap_event_name=="penta3ipti_2_4_mon_arm_1",]
#duplicates?
n_occur <- data.frame(table(data_clean_coh$study_number))
n_occur[n_occur$Freq > 1,]
#site1
data_clean_coh1 <- data_clean_coh[grep("COH-1-1-1-",data_clean_coh$study_number),]
data_clean_coh1 <- data_clean_coh1[,c('record_id','study_number')]

data_clean_coh2 <- data_clean_coh[grep("COH-1-2-1-",data_clean_coh$study_number),]
data_clean_coh2 <- data_clean_coh2[,c('record_id','study_number')]

data_clean_coh3 <- data_clean_coh[grep("COH-1-3-1-",data_clean_coh$study_number),]
data_clean_coh3 <- data_clean_coh3[,c('record_id','study_number')]

data_clean_coh4 <- data_clean_coh[grep("COH-1-4-1-",data_clean_coh$study_number),]
data_clean_coh4 <- data_clean_coh4[,c('record_id','study_number')]

write.csv(data_clean_coh1, 'study_numbers_wahala.csv', row.names = F)
write.csv(data_clean_coh2, 'study_numbers_amakpape.csv', row.names = F)
write.csv(data_clean_coh3, 'study_numbers_hahomegbe.csv', row.names = F)
write.csv(data_clean_coh4, 'study_numbers_tetetou.csv', row.names = F)


#######
#######
#######
#check when the "two weeks between recruitment and ipti administration is correct or not
#but if 5 weeks after penta 2 not show
#join penta2 and penta3
penta2_3 <- merge(penta2_3m,penta3_4m, by = 'record_id')

weektimes <- as.data.frame(as.numeric(difftime(strptime(penta3_4m$int_date, format = "%Y-%m-%d"),
         strptime(penta2_3m$int_date, format = "%Y-%m-%d"),units="weeks")))

weektimes <- as.data.frame(c(record_id = penta2_3$record_id, diffweeks = as.numeric(difftime(strptime(penta2_3$int_date.y, format = "%Y-%m-%d"),
                                               strptime(penta2_3$int_date.x, format = "%Y-%m-%d"),units="weeks"))))



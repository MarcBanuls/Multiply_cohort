library(redcapAPI)
library(stringr)
source("tokens.R")
api.url <- maternal_api
api.token <- togo_bl_token


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

library(redcapAPI)
source("tokens.R")
api.url <- maternal_api
api.token <- togo_bl_token


rcon <- redcapConnection(api.url, api.token)

my.fields <- c('record_id', 'screening_study_number_cohort')
my.events <- c('penta2ipti1_3_mont_arm_1')

data <- exportRecords(
  rcon,
  factors = F,
  fields = my.fields,
  events = my.events,
  form_complete_auto = F
  
)

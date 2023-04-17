library(dplyr)

nodup <- read.csv('MULTIPLYHHSBaselineTogo_DATA_WITH_NO_DUPS_2022-05-17_1603.csv')
gps <- read.csv('MultiplyHHS_GPS_Togo.csv',sep = ';')

#take nodups where rdt = '1'
nodup_rdt <- nodup %>% 
  filter(rdt == '1') %>% 
  select(record_id)
#merge to add lat and lon
nodup_gps <- merge(nodup_rdt,gps)
#write csv
write.csv(nodup_gps, 'MultiplyHHS_gps_togo.csv',row.names = F)

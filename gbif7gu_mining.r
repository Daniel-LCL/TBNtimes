library(data.table)
library(dplyr)
library(magrittr)
gbif_7gu_event <- fread('C:/Users/user/Desktop/TBRI_project_lu/TBN/TBNtimes/sample data/event.txt', encoding = 'UTF-8')
gbif_7gu_occ <- fread('C:/Users/user/Desktop/TBRI_project_lu/TBN/TBNtimes/sample data/occurrence.txt', encoding = 'UTF-8')
gbif_7gu_measure <- fread('C:/Users/user/Desktop/TBRI_project_lu/TBN/TBNtimes/sample data/extendedmeasurementorfact.txt', encoding = 'UTF-8')
colnames(gbif_7gu_event)
colnames(gbif_7gu_occ)
colnames(gbif_7gu_measure)

gbif_7gu_event$eventID
gbif_7gu_occ$occurrenceID

## tbn open api
library(httr)
library(jsonlite)
res <- GET('https://www.tbn.org.tw/api/v25/occurrence?datasetUUID=b6ca9b0b-b6eb-46c5-b623-cd479a71e566&limit=26000')
res
rawToChar(res$content)
tbn_api <- fromJSON(rawToChar(res$content))
names(tbn_api)
tbn_api$data
tbn_api$meta
tbn_api$links
tbnocc %>% colnames 
tbn_api$data %>% colnames
tbn_api$data %>% View
tbn_api$data

# groupBy eventID
time7gu <- data.frame(eventID = gbif_7gu_event$eventID)
habitat_7gu <- data.frame(verbatimSiteNames = unique(gbif_7gu_event$locationID), targetHabitatScope = NA)
habitat_7gu$targetHabitatScope <- c('Tidal Mudflat', 'Tidal Mudflat', 'Windbreak', 'Windbreak',
                                    'Fish pond', 'Abandoned Salt Pan', 'Abandoned Salt Pan',
                                    'Agricultural Field', 'Fish Pond', 'Windbreak', 'Windbreak', 
                                    'Abandoned Salt Pan', 'Abandoned Salt Pan', 'Windbreak')

dwc_7gu <- 
  left_join(gbif_7gu_event[,c('eventID', 'locationID', 
                            'sampleSizeValue', 'sampleSizeUnit', 
                            'samplingEffort', 
                            'eventDate')], time7gu, by = 'eventID') %>% 
  left_join(., 
            gbif_7gu_occ[!duplicated(eventID),c('eventID', 'recordedBy')] %>% 
              mutate(num_observer = 
                       strsplit(recordedBy, '、') %>% 
                       lapply(., uniqueN) %>% 
                       do.call(rbind, .) %>% .[,1]) %>% 
              .[,c('eventID', 'num_observer')],
            by = 'eventID'
            ) %>% 
  left_join(., 
            habitat_7gu, by = c('locationID' = 'verbatimSiteNames'))
dwc_7gu
# transform to humboltExtension
dwc_7gu %<>% 
  mutate(year = strsplit(eventDate, '/') %>% do.call(rbind, .) %>% .[,1] %>% as.numeric,
         month = strsplit(eventDate, '/') %>% do.call(rbind, .) %>% .[,2] %>% as.numeric,
         day = strsplit(eventDate, '/') %>% do.call(rbind, .) %>% .[,3] %>% as.numeric, 
         samplingEffortValue = strsplit(samplingEffort, ' observer-minutes') %>% do.call(rbind,.) %>% .[,1] %>% as.numeric %>% as.vector,
         samplingEffortUnit = 'observer-minutes') %>% 
  .[,list(eventID, 
          'sampleAreaID' = locationID,
          'verbatimSiteNames' = locationID,
          'targetTaxonomicScope' = 'Aves',
          'excludedTaxonomicSope' = 'Passeriformes',
          'geospatialScopeAreaValue' = sampleSizeValue,
          'geospatialScopeAreaUnit' = sampleSizeUnit, 
          'totalAreaSampledValue' = sampleSizeValue,
          'totalAreaSampledUnit' = sampleSizeUnit,
          'eventDurationValue' = samplingEffortValue/num_observer, 
          'eventDurationUnit' = 'minutes',
          year,
          month,
          day,
          samplingEffortValue,
          samplingEffortUnit,
          targetHabitatScope,
          'excludedHabitatScope' = NA)]
dwc_7gu[verbatimSiteNames %in% c('J2A', 'J2B'),'sampleAreaID'] <- 'J2'
dwc_7gu[verbatimSiteNames %in% c('D1A'), 'sampleAreaID'] <- 'D1'
dwc_7gu[verbatimSiteNames %in% c('D2A'), 'sampleAreaID'] <- 'D2'
# fwrite(dwc_7gu, 'C:/Users/user/Desktop/TBRI_project_lu/TBN/TBNtimes/sample data/humbolt/dwchumbolt_7gu.csv')

# 檢查是否每個樣區都有做幾年幾次
## 理論上每月2次，每年24次，共14年度
dwc_7gu %>% 
  group_by(year, month, sampleAreaID) %>% 
  summarise(n_survey = uniqueN(eventID)) %>% 
  View

dwc_7gu %>% 
  group_by(year, month) %>% 
  summarise(n_survey = uniqueN(eventID), 
            n_sampleAreaID = uniqueN(sampleAreaID),
            n_site = uniqueN(verbatimSiteNames)) %>% 
  View

dwc_7gu %>% 
  group_by(year, sampleAreaID) %>% 
  summarise(n_survey = uniqueN(eventID),
            n_month = uniqueN(month),
            start_month = min(month)) %>% 
  View

dwc_7gu %>% 
  group_by(sampleAreaID) %>% 
  summarise(n_year = uniqueN(year)) %>% 
  View
# trend index
### 串occurrence
dwc_7gu %>% 
  left_join(., gbif_7gu_occ[,c('eventID', 'occurrenceID', 'individualCount', 'scientificName', 'vernacularName')], by = 'eventID')


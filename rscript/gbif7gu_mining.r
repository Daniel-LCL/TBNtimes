library(data.table)
library(dplyr)
library(magrittr)
`%out%` <- function(a,b){!a %in% b}
gbif_7gu_event <- fread('data/dwc/event.txt', encoding = 'UTF-8')
gbif_7gu_occ <- fread('data/dwc/occurrence.txt', encoding = 'UTF-8')
gbif_7gu_measure <- fread('data/dwc/extendedmeasurementorfact.txt', encoding = 'UTF-8')

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
  group_by(sampleAreaID) %>% 
  summarise(n_year = uniqueN(year)) %>% 
  View
# trend index
dwc_7gu_times <- 
  dwc_7gu %>% 
  # 接上occurrence
  left_join(., gbif_7gu_occ[,c('eventID', 'occurrenceID', 'individualCount', 'scientificName', 'vernacularName')], by = 'eventID') %>% 
  mutate(projectName = 'Long-term Bird Census in Cigu 2004-2017') %>% 
  # 合併計算trend index
  group_by(year, sampleAreaID, scientificName, vernacularName) %>% 
  summarise(rawEventID = paste0(eventID, collapse = ';'), 
            numOfSurvey = uniqueN(eventID),
            numOfMonth = uniqueN(month),
            trendIndexValue = sum(individualCount)/numOfSurvey, 
            trendIndexType = 'Individual') %>% 
  # 接上新的eventID、occurrenceID、以及計算單一物種是否超過三年紀錄的timeID
  mutate(eventID = paste0(year,'_',sampleAreaID), 
         occID = paste0(year,'_',sampleAreaID,'_',vernacularName),
         timeID = paste0(sampleAreaID,'_',vernacularName)) %>% 
  data.table

##################################################################
# 嘗試做圖
library(ggplot2)
library(ggpubr)
### 檢視單一樣區單一物種的年份數 (趨勢線比較有機會漂亮的單位)
dwc_7gu_times %>% 
  group_by(vernacularName, sampleAreaID) %>% 
  summarise(n_year = uniqueN(year)) %>% 
  data.frame

### 統計各 樣區_物種 代碼的紀錄次數 (>=3次可繪製趨勢圖)
sp_count3 <- 
  table(dwc_7gu_times$timeID) %>% 
  data.table %>% 
  .[N >= 3,]
sp_count3

#### 每年資料多的物種
species <- '青足鷸'
site <- 'D1'
#### 資料稍少的物種
species <- '紅冠水雞'
site <- 'E3'
#### 每年資料少的物種
species <- '中杓鷸'
site <- 'D1'

# 做圖
if(paste0(site,'_',species) %in% sp_count3$V1){
  print(paste0('物種 //',species,'// 於 //',site,'// 樣區內紀錄至少涵蓋3年度，可繪製趨勢圖'))
  # 補足每年缺值為0
  a <- 
    data.table(year = min(dwc_7gu_times$year):max(dwc_7gu_times$year), 
               sampleAreaID = rep(site, 14),
               vernacularName = species, 
               occID = paste0(min(dwc_7gu_times$year):max(dwc_7gu_times$year),'_',site,'_',species)
               ) %>% 
    left_join(., dwc_7gu_times[,c('eventID','scientificName','rawEventID',
                                  'trendIndexValue','trendIndexType', 'occID')], by = 'occID') %>% 
    data.table
  a[is.na(a$trendIndexValue),'trendIndexValue'] <- 0
  a %<>% mutate(trendIndexPercentage = trendIndexValue/a[a$year %in% min(a[trendIndexValue %out% 0,'year']),]$trendIndexValue*100)
  # 畫圖
  #### trendindex直出 a
  A <- 
    ggplot(a) +
    geom_line(aes(x = year, y = trendIndexValue)) +
    annotate('text', 
             y = max(a[(vernacularName %in% species) &
                       (sampleAreaID %in% site),'trendIndexValue']
             ), 
             x = 2006,
             label = 'y: mean individual per year') +
    ggtitle(species)
  print('trendIndex直出繪製完畢')
  print(A)
  }else{
    print(paste0('物種 //',species,'// 於 //',site,'// 樣區內紀錄不足3年度，無法繪製趨勢圖'))
  }

# 對該物種所有樣區做圖
#### 每年資料多的物種
species <- '青足鷸'
site <- 'D1'
#### 資料稍少的物種
species <- '紅冠水雞'
site <- 'E3'
#### 資料不足的物種
species <- '中杓鷸'
site <- 'D1'
## 每個樣區分別檢查該物種是否能畫線
i <- 1
species <- '紅冠水雞'
trend_sites <- 
  lapply(1:length(unique(dwc_7gu_times$sampleAreaID)), function(i){
    site <- unique(dwc_7gu_times$sampleAreaID) %>% .[i]
    if(paste0(site,'_',species) %in% sp_count3$V1){
      print(paste0('物種 //',species,'// 於 //',site,'// 樣區內紀錄至少涵蓋3年度，可繪製趨勢圖'))
      # 補足每年缺值為0
      a <- 
        data.table(year = min(dwc_7gu_times$year):max(dwc_7gu_times$year), 
                   sampleAreaID = site,
                   vernacularName = species, 
                   occID = paste0(min(dwc_7gu_times$year):max(dwc_7gu_times$year),'_',site,'_',species)
        ) %>% 
        left_join(., dwc_7gu_times[,c('eventID','scientificName','rawEventID',
                                      'trendIndexValue','trendIndexType', 'occID')], by = 'occID') %>% 
        data.table
      a[is.na(a$trendIndexValue),'trendIndexValue'] <- 0
      a %<>% mutate(trendIndexPercentage = trendIndexValue/a[a$year %in% min(a[trendIndexValue %out% 0,'year']),]$trendIndexValue*100)
      a
    }else{
      print(paste0('物種 //',species,'// 於 //',site,'// 樣區內紀錄不足3年度，無法繪製趨勢圖'))
      a <- 
        data.table(year = min(dwc_7gu_times$year):max(dwc_7gu_times$year), 
                   sampleAreaID = site, 
                   vernacularName = species, 
                   occID = paste0(min(dwc_7gu_times$year):max(dwc_7gu_times$year),'_',site,'_',species),
                   eventID = NA, scientificName = NA, rawEventID = NA, 
                   trendIndexValue = NA, trendIndexType = NA, trendIndexPercentage = NA
                   ) %>% 
        data.table
      a$trendIndexValue <- as.numeric(a$trendIndexValue)
      a[is.na(a$trendIndexValue),'trendIndexValue'] <- 0
      a %<>% mutate(trendIndexPercentage = 0)
      a
    }
  }) %>% 
    do.call(rbind, .)
# 作圖
  ggplot(trend_sites) +
  geom_line(aes(x = year, y = trendIndexPercentage, 
                group = sampleAreaID, color = sampleAreaID), 
            linewidth = 2) +
  annotate('text', 
           y = max(trend_sites$trendIndexPercentage), 
           x = 2006,
           label = 'y: relative mean individual per year since 2004') +
  ggtitle(species)
trend_sites[trendIndexPercentage > 100,]
##########################

# 統計整個計畫每個物種幾個樣區上升，幾個樣區下降 (高/低於100%者超過一半，都沒有則無差別)







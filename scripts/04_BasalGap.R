# MOJA Basal Gap Analysis v. 1
# Author: Frankie Gerraty (fgerraty@ucsc.edu/frankiegeraty@gmail.com)
# -----------------------------------------------------------------------------

## Import and Prepare Basal Gap Datasets ####

BasalGap <- read_csv("MOJA_Analysis/data/processed/BasalGap.csv")

#Calculate gap size, proportion of transect, and make variables to determine whether the gap fits lumped gap size groupings
BasalGap <- BasalGap %>% 
  mutate(gap_size = end_m - start_m) %>% #calculate gap size
  mutate(portion_transect = gap_size/50) %>% #calculation portion of the transect in gap
  mutate(gap_size_25cm = if_else(gap_size >= 0.25, TRUE, FALSE)) %>% #TRUE if gap size >= .25m
  mutate(gap_size_50cm = if_else(gap_size >= 0.5, TRUE, FALSE)) %>% #TRUE if gap size >= .5m
  mutate(gap_size_1m = if_else(gap_size >= 1, TRUE, FALSE)) %>% #TRUE if gap size >= 1m
  mutate(gap_size_2m = if_else(gap_size >= 2, TRUE, FALSE))  #TRUE if gap size >= 2m


#Summarize proportion of each transect with gaps >25cm
BasalGap_25cm <- BasalGap %>%   
  group_by(well, point, transect,distance, gap_size_25cm) %>% 
  summarize(percent_transect_25cm_gaps = sum(portion_transect)) %>% 
  filter(gap_size_25cm == TRUE) %>% 
  select(-gap_size_25cm)

#Summarize proportion of each transect with gaps >50cm
BasalGap_50cm <- BasalGap %>%   
  group_by(well, point, transect, distance, gap_size_50cm) %>% 
  summarize(percent_transect_50cm_gaps = sum(portion_transect)) %>% 
  filter(gap_size_50cm == TRUE) %>% 
  select(-gap_size_50cm) %>% 
  ungroup()

#Summarize proportion of each transect with gaps >1m
BasalGap_1m <- BasalGap %>%   
  group_by(well, point, transect, distance, gap_size_1m) %>% 
  summarize(percent_transect_1m_gaps = sum(portion_transect)) %>% 
  filter(gap_size_1m == TRUE) %>% 
  select(-gap_size_1m) %>% 
  ungroup()

#Summarize proportion of each transect with gaps >2m
BasalGap_2m <- BasalGap %>%   
  group_by(well, point, transect, distance, gap_size_2m) %>% 
  summarize(percent_transect_2m_gaps = sum(portion_transect)) %>% 
  filter(gap_size_2m == TRUE) %>% 
  select(-gap_size_2m) %>% 
  ungroup()

#Bring all dataframes together #NOTE! ERRORS IN DATA COLLECTION = MORE THAN 50M TRANSECTS
BasalGapSummary <- Reduce(merge, list(BasalGap_25cm, BasalGap_50cm, BasalGap_1m, BasalGap_2m))

#Clean workspace
rm(BasalGap_25cm,BasalGap_50cm,BasalGap_1m,BasalGap_2m)


#messing around---remove latere
temp <- BasalGapSummary %>% 
  mutate(percent_transect_25cm_gaps_beta = if_else(percent_transect_25cm_gaps >= 1,
                                                   ((199-1) + 0.05)/199,
                                                   percent_transect_25cm_gaps*(199-1) + 0.05)/199,
         distance = distance/100,
         zif_beta = if_else(percent_transect_25cm_gaps >= 1,
                            0, 1-percent_transect_25cm_gaps))



#25cm gap glmm 
bg1 <- glmmTMB(zif_beta ~ distance + 
                 (1|well/point), #Random effects of well, point
               ziformula=~1,
               family = beta_family(),
               data = temp)
summary(bg1)


# Check bg1 assumptions with DHARMa package
bg1_res = simulateResiduals(bg1, quantreg=T)
plot(bg1_res, rank = T)
testDispersion(bg1_res)
plotResiduals(bg1, form = BasalGapSummary$distance, main=NULL)

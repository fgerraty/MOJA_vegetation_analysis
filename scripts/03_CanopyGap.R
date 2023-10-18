# MOJA Canopy Gap Analysis v. 1
# Author: Frankie Gerraty (fgerraty@ucsc.edu/frankiegeraty@gmail.com)
# -----------------------------------------------------------------------------

## Import and Prepare Canopy Gap Datasets ####
CanopyGap <- read_csv("MOJA_Analysis/data/processed/CanopyGap.csv")

#Calculate gap size, proportion of transect, and make variables to determine whether the gap fits lumped gap size groupings
CanopyGap <- CanopyGap %>% 
  mutate(gap_size = end_m - start_m) %>% #calculate gap size
  mutate(portion_transect = gap_size/50) %>% #calculation portion of the transect in gap
  mutate(gap_size_25cm = if_else(gap_size >= 0.25, TRUE, FALSE)) %>% #TRUE if gap size >= .25m
  mutate(gap_size_50cm = if_else(gap_size >= 0.5, TRUE, FALSE)) %>% #TRUE if gap size >= .5m
  mutate(gap_size_1m = if_else(gap_size >= 1, TRUE, FALSE)) %>% #TRUE if gap size >= 1m
  mutate(gap_size_2m = if_else(gap_size >= 2, TRUE, FALSE))  #TRUE if gap size >= 2m


#Summarize proportion of each transect with gaps >25cm
CanopyGap_25cm <- CanopyGap %>%   
  group_by(well, point, transect,distance, gap_size_25cm) %>% 
  summarize(percent_transect_25cm_gaps = sum(portion_transect)) %>% 
  filter(gap_size_25cm == TRUE) %>% 
  select(-gap_size_25cm)

#Summarize proportion of each transect with gaps >50cm
CanopyGap_50cm <- CanopyGap %>%   
  group_by(well, point, transect, distance, gap_size_50cm) %>% 
  summarize(percent_transect_50cm_gaps = sum(portion_transect)) %>% 
  filter(gap_size_50cm == TRUE) %>% 
  select(-gap_size_50cm) %>% 
  ungroup()

#Summarize proportion of each transect with gaps >1m
CanopyGap_1m <- CanopyGap %>%   
  group_by(well, point, transect, distance, gap_size_1m) %>% 
  summarize(percent_transect_1m_gaps = sum(portion_transect)) %>% 
  filter(gap_size_1m == TRUE) %>% 
  select(-gap_size_1m) %>% 
  ungroup()

#Summarize proportion of each transect with gaps >2m
CanopyGap_2m <- CanopyGap %>%   
  group_by(well, point, transect, distance, gap_size_2m) %>% 
  summarize(percent_transect_2m_gaps = sum(portion_transect)) %>% 
  filter(gap_size_2m == TRUE) %>% 
  select(-gap_size_2m) %>% 
  ungroup()

#Bring all dataframes together
CanopyGapSummary <- Reduce(merge, list(CanopyGap_25cm, CanopyGap_50cm, CanopyGap_1m, CanopyGap_2m))

#Clean workspace
rm(CanopyGap_25cm,CanopyGap_50cm,CanopyGap_1m,CanopyGap_2m)

## Analyze Canopy Gap Dataset ####

CanopyGapSummary <- CanopyGapSummary %>% 
  # transform data to remove 0 and 1 values as per Smithson, M., & Verkuilen, J. (2006)
  # https://doi.org/10.1037/1082-989X.11.1.54
  mutate(percent_transect_2m_gaps = (percent_transect_2m_gaps * (199-1) + 0.05)/199, 
         percent_transect_1m_gaps = (percent_transect_1m_gaps * (199-1) + 0.05)/199,
         percent_transect_50cm_gaps = (percent_transect_50cm_gaps * (199-1) + 0.05)/199,
         percent_transect_25cm_gaps = (percent_transect_25cm_gaps * (199-1) + 0.05)/199,
         
         distance = distance/100)


#2m gap glmm : NOTE model errors
cg1 <- glmmTMB(percent_transect_2m_gaps ~ distance + 
                 (1|well/point), #Random effects of well, point
               family = beta_family(),
               data = CanopyGapSummary)
summary(cg1)

# Check cg1 assumptions with DHARMa package
cg1_res = simulateResiduals(cg1)
plot(cg1_res, rank = T)
testDispersion(cg1_res)
plotResiduals(cg1, form = CanopyGapSummary$distance, main=NULL)
#NOTE: DHARMA throwing error


# 1m gap GLMM
cg2 <- glmmTMB(percent_transect_1m_gaps ~ distance + 
                 (1|well/point), #Random effects of well, point
               family = beta_family(),
               data = CanopyGapSummary)
summary(cg2)


# Check cg2 assumptions with DHARMa package
cg2_res = simulateResiduals(cg2)
plot(cg2_res, rank = T)
testDispersion(cg2_res)
plotResiduals(cg2, form = CanopyGapSummary$distance, main=NULL)
#NOTE: DHARMA throwing error


# 50cm gap GLMM
cg3 <- glmmTMB(percent_transect_50cm_gaps ~ distance + 
                 (1|well/point), #Random effects of well, point
               family = beta_family(),
               data = CanopyGapSummary)
summary(cg3)


# Check cg3 assumptions with DHARMa package
cg3_res = simulateResiduals(cg3)
plot(cg3_res, rank = T)
testDispersion(cg3_res)
plotResiduals(cg3, form = CanopyGapSummary$distance, main=NULL)
#NOTE: DHARMA throwing error

# 25cm gap GLMM
cg4 <- glmmTMB(percent_transect_25cm_gaps ~ distance + 
                 (1|well/point), #Random effects of well, point
               family = beta_family(),
               data = CanopyGapSummary)
summary(cg4)

# Check cg4 assumptions with DHARMa package
cg4_res = simulateResiduals(cg4)
plot(cg4_res, rank = T)
testDispersion(cg4_res)
plotResiduals(cg4, form = CanopyGapSummary$distance, main=NULL)
#NOTE: DHARMA throwing error


ggplot(CanopyGapSummary, aes(x=percent_transect_2m_gaps))+
  geom_histogram()

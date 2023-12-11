# MOJA Canopy Gap Analysis v. 2
# Author: Frankie Gerraty (fgerraty@ucsc.edu/frankiegeraty@gmail.com)
# -----------------------------------------------------------------------------

## Import and Prepare Canopy Gap Datasets ####
CanopyGap <- read_csv("data/processed/CanopyGap.csv")

## Analyze Canopy Gap Dataset ####

CanopyGap <- CanopyGap %>% 
  mutate(percent_transect_2m_gaps = 1-percent_transect_2m_gaps, 
         percent_transect_1m_gaps = 1-percent_transect_1m_gaps,
         percent_transect_50cm_gaps = 1-percent_transect_50cm_gaps,
         percent_transect_25cm_gaps = 1-percent_transect_25cm_gaps)


#2m gap glmm : NOTE model errors
cg1 <- glmmTMB(percent_transect_2m_gaps ~ distance + 
                 (1|well/point), #Random effects of well, point
               ziformula= ~1,
               family = beta_family(),
               data = CanopyGap)
summary(cg1)

# Check cg1 assumptions with DHARMa package
cg1_res = simulateResiduals(cg1)
plot(cg1_res, rank = T)
testDispersion(cg1_res)
plotResiduals(cg1, form = CanopyGap$distance, main=NULL)
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

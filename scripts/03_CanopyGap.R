# MOJA Canopy Gap Analysis v. 2
# Author: Frankie Gerraty (fgerraty@ucsc.edu/frankiegeraty@gmail.com)
# -----------------------------------------------------------------------------

## Import and Prepare Canopy Gap Datasets ####
CanopyGap <- read_csv("data/processed/CanopyGap.csv")

## Analyze Canopy Gap Dataset ####

CanopyGap <- CanopyGap %>% 
  # transform data to remove 0 and 1 values as per Smithson, M., & Verkuilen, J. (2006)
  # https://doi.org/10.1037/1082-989X.11.1.54
  mutate(percent_transect_2m_gaps = (percent_transect_2m_gaps * (199-1) + 0.05)/199, 
         percent_transect_1m_gaps = (percent_transect_1m_gaps * (199-1) + 0.05)/199,
         percent_transect_50cm_gaps = (percent_transect_50cm_gaps * (199-1) + 0.05)/199,
         percent_transect_25cm_gaps = (percent_transect_25cm_gaps * (199-1) + 0.05)/199)
  

#First, take a look at the distribution of the data
ggplot(CanopyGap, aes(x=percent_transect_2m_gaps))+
  geom_histogram()
ggplot(CanopyGap, aes(x=percent_transect_1m_gaps))+
  geom_histogram()
ggplot(CanopyGap, aes(x=percent_transect_50cm_gaps))+
  geom_histogram()
ggplot(CanopyGap, aes(x=percent_transect_25cm_gaps))+
  geom_histogram()






#2m gap glmm : NOTE model errors
cg1 <- glmmTMB(percent_transect_2m_gaps ~ distance, + 
#              (1|well/point), #Random effects of well, point
#              ziformula= ~1,
               family = Gamma,
               data = CanopyGap)
summary(cg1)

# Check cg1 assumptions with DHARMa package
cg1_res = simulateResiduals(cg1)
plot(cg1_res, rank = T)
testDispersion(cg1_res)
plotResiduals(cg1, form = CanopyGap$distance, main=NULL)
#NOTE: DHARMA throwing error


# 1m gap GLMM
cg2 <- glmmTMB(percent_transect_1m_gaps ~ distance,# + 
#                 (1|well/point), #Random effects of well, point
               family = beta_family(),
               data = CanopyGap)
summary(cg2)


# Check cg2 assumptions with DHARMa package
cg2_res = simulateResiduals(cg2)
plot(cg2_res, rank = T)
testDispersion(cg2_res)
plotResiduals(cg2, form = CanopyGapSummary$distance, main=NULL)
#NOTE: DHARMA throwing error


# 50cm gap GLMM
cg3 <- glmmTMB(percent_transect_50cm_gaps ~ distance,# + 
             #    (1|well/point), #Random effects of well, point
               family = beta_family(),
               data = CanopyGap)
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

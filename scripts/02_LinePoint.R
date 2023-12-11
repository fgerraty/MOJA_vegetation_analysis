##########################################################################
# MOJA Vegetation Analysis Project #######################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 2: LinePoint Analyses ###########################################
# -----------------------------------------------------------------------------

# PART 0: Import Line Point Intercept Data ####

LinePoint <- read_csv("data/processed/LinePoint.csv")

# PART 1: Percent Cover of Litter ---------------------------------------------

# Question: Does the percent cover of leaf litter vary with distance to well (water source)?

#First, take a look at the distribution of the data and check for normality
ggplot(LinePoint, aes(x=litter_cover))+
  geom_histogram()
shapiro.test(LinePoint$litter_cover) #Yes, it is just within the threshold of normality


#Create a linear mixed effects model (normal distribution)
f1 <- lme(litter_cover ~ distance, 
          random = ~1|well/point, #Random effects of well, point (nested)
          data = LinePoint)
summary(f1)


#Check assumptions
plot(f1)
qqnorm(f1, ~ranef(., level=1))
qqnorm(f1, ~ranef(., level=2))
qqnorm(resid(f1))


# PART 2: Percent Cover of woody plants ----------------------------------------











# PART 3: Percent Cover of B. rubens & Schizmus --------------------------------



#First, take a look at the distribution of the data and check for normality
ggplot(LinePoint, aes(x=selected_plant_cover))+
  geom_histogram()

LinePoint <- LinePoint %>% 
  mutate(distance_rescaled = distance/100)


#Create model f3. 
f3 <- glmer.nb(selected_plant_cover ~ distance_rescaled + 
                (1|well/point), #Random effects of well, point, transect (nested)
              data = LinePoint)
summary(f3)


# Check f3 assumptions with DHARMa package = ERRORS
f3_res = simulateResiduals(f3, n=1000)
plot(f3_res, rank = T) 
testDispersion(f3_res)
plotResiduals(f3, form = LinePoint$selected_plant_cover, main=NULL)


# PART 4: Percent Cover of all invasive plants --------------------------------

#First, take a look at the distribution of the data and check for normality
ggplot(LinePoint, aes(x=invasive_plant_cover))+
  geom_histogram()


#Create model f4.
f4 <- glmmTMB(invasive_plant_cover ~ distance + 
                (1|well/point), #Random effects of well, point (nested)
              ziformula=~1,
              family=nbinom2, #binomial distribution (0/1) data
              data = LinePoint)
summary(f4)


# Check f4 assumptions with DHARMa package
f4_res = simulateResiduals(f4)
plot(f4_res, rank = T) 
testDispersion(f4_res)
plotResiduals(f4, form = LinePoint$distance, main=NULL)


# PART 5: % cover of native grasses ---------------------------------------









#PART 6: Percent cover of canopy vegetation ------------------------------------

#Create variable that includes all the codes for dead vegetation
dead_canopy_codes <- c(NA,"DS","DPG","DC","DPF","DAG","DAF")
#Create variable that includes all the codes for surface types
surface_codes <- c(NA, "L","LIC","M","R","G","S","CC")


CanopyCover <- LinePoint %>%  
  select(well:transect, distance, interval, species, code1:code4) %>% #select for only relevant columns
  group_by(well, point, transect, distance, interval) %>% #Group 
  #Score as 1 or 0 based on presence/absence of living canopy vegetation
  mutate(species = na_if(species, "none")) %>% 
  mutate(CanopyCover_presence = if_else(species %in% c(dead_canopy_codes, surface_codes) &
                                          code1 %in% c(dead_canopy_codes, surface_codes) &
                                          code2 %in% c(dead_canopy_codes, surface_codes) &
                                          code3 %in% c(dead_canopy_codes, surface_codes) &
                                          code4 %in% c(dead_canopy_codes, surface_codes),
                                        0, 1, missing = 0)) %>% 
  mutate(distance = distance/100)%>% #Divide by to rescale variables for GLMM analyses
  ungroup() #Drop groups

#Create model f6. NEED TO RECTIFY MODEL FIT ERRORS
f6 <- glmmTMB(CanopyCover_presence ~ distance + 
                (1|well/point/transect), #Random effects of well, point, transect (nested)
              family = "binomial", #binomial distribution (0/1) data
              data = CanopyCover)
summary(f6)

# Check f6 assumptions with DHARMa package
f6_res = simulateResiduals(f6, n=1000)
plot(f6_res, rank = T) 
testDispersion(f6_res)
plotResiduals(f6, form = CanopyCover$distance, main=NULL)
#NOTE: DHARMa throws an error that there are may be problems with model fit.  


# PART 7: Total percent cover of vegetation (dead or alive) --------------------

TotalCover <- LinePoint %>%  
  select(well:transect, distance, interval, species, code1:code4) %>% #select for only relevant columns
  group_by(well, point, transect, distance, interval) %>% #Group 
  #Score as 1 or 0 based on presence/absence of any vegetation (dead or alive)
  mutate(species = na_if(species, "none")) %>% 
  mutate(TotalCover_presence = if_else(species %in% surface_codes &
                                         code1 %in% surface_codes &
                                         code2 %in% surface_codes &
                                         code3 %in% surface_codes &
                                         code4 %in% surface_codes,
                                       0, 1, missing = 0)) %>% 
  mutate(distance = distance/100)%>% #Divide by to rescale variables for GLMM analyses
  ungroup() #Drop groups

#Create model f7. Need to check distribution assumptions, double check, etc.
f7 <- glmer(TotalCover_presence ~ distance + 
              (1|well/point/transect), #Random effects of well, point, transect (nested)
            family = "binomial", #binomial distribution (0/1) data
            data = TotalCover)
summary(f7)


# Check f7 assumptions with DHARMa package
f7_res = simulateResiduals(f7, n=1000)
plot(f7_res, rank = T) 
testDispersion(f7_res)
plotResiduals(f7, form = TotalCover$distance, main=NULL)
#NOTE: DHARMa throws an error that there are may be problems with model fit.  

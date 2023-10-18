##########################################################################
# MOJA Vegetation Analysis Project #######################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# LinePoint Preliminary Analysis v. 1 
# -----------------------------------------------------------------------------

# PART 0: Import Line Point Intercept Data ####

LinePoint <- read_csv("MOJA_Analysis/data/processed/LinePoint.csv")

# PART 1: Percent Cover of Litter ---------------------------------------------

# Question: Does the percent cover of leaf litter vary with distance to well (water source)?

## Create new dataframe "litter" for litter analysis
Litter <- LinePoint %>%  
  select(well:transect, distance, interval, code1:code4) %>% #select for only relevant columns
  group_by(well, point, transect, distance, interval) %>% #Group by well, transect, point, interval
  #Score as 1 or 0 based on presence/absence of litter at any of the levels (code 1-4)
  mutate(litter_presence = if_else(code1 == "L" |
                                     code2 == "L" |
                                     code3 == "L" |
                                     code4 == "L", 1, 0, missing = 0)) %>% 
  mutate(distance = distance/100)%>% #Divide by to rescale variables for GLMM analyses
  ungroup() #Drop groups

#Turn into dataframe (required for glmer)
Litter <- data.frame(Litter)


#Create model f1. 
f1 <- glmmTMB(litter_presence ~ distance + 
              (1|well/point/transect), #Random effects of well, point, transect (nested)
            family = "binomial", #binomial distribution (0/1) data
            data = Litter)
summary(f1)

# Check f1 assumptions with DHARMa package
f1_res = simulateResiduals(f1)
plot(f1_res, rank = T)
testDispersion(f1_res)
plotResiduals(f1, form = Litter$distance, main=NULL)



# PART 2: Percent Cover of woody plants ----------------------------------------











# PART 3: Percent Cover of B. rubens & Schizmus --------------------------------

SelectPlants <- LinePoint %>%  #Create new dataframe "SelectPlants" for percent cover analysis
  select(well:transect, distance, interval, species, code1:code4) %>% #select for only relevant columns
  group_by(well, point, transect, distance, interval) %>% #Group 
  #Score as 1 or 0 based on presence/absence of B. rubens & Schizmus at any of the levels (code 1-4)
  mutate(SelectPlant_presence = if_else(species %in% c("Schismus barbatus", "Bromus madridentis") |
                                          code1 %in% c("Schismus barbatus", "Bromus madridentis") |
                                          code2 %in% c("Schismus barbatus", "Bromus madridentis") |
                                          code3 %in% c("Schismus barbatus", "Bromus madridentis") |
                                          code4 %in% c("Schismus barbatus", "Bromus madridentis"), 1, 0, missing = 0)) %>%   
  mutate(distance = distance/100)%>% #Divide by to rescale variables for GLMM analyses
  ungroup() #Drop groups


#Create model f3. 
f3 <- glmmTMB(SelectPlant_presence ~ distance + 
              (1|well/point/transect), #Random effects of well, point, transect (nested)
            family = "binomial", #binomial distribution (0/1) data
            data = SelectPlants)
summary(f3)


# Check f3 assumptions with DHARMa package
f3_res = simulateResiduals(f3, n=1000)
plot(f3_res, rank = T) 
testDispersion(f3_res)
plotResiduals(f3, form = SelectPlants$distance, main=NULL)


# PART 4: Percent Cover of all invasive plants --------------------------------

InvasivePlants <- LinePoint %>%  #Create new dataframe "InvasivePlants" for percent cover analysis
  select(well:transect, distance, interval, species, code1:code4) %>% #select for only relevant columns
  group_by(well, point, transect, distance, interval) %>% #Group 
  #Score as 1 or 0 based on presence/absence of invasive plants (Descurainia spp., Schismus barbatus, Bromus madritentis, Erodium cicutarium) at any of the levels (code 1-4)
  mutate(InvasivePlant_presence = 
           if_else(species %in% c("Descurainia species", "Descurainia (pinnata)",
                                  "Schismus barbatus", "Bromus madridentis", 
                                  "Erodium cicutarium") |
                     code1 %in% c("Descurainia species", "Descurainia (pinnata)",
                                  "Schismus barbatus", "Bromus madridentis", 
                                  "Erodium cicutarium") |
                     code2 %in% c("Descurainia species", "Descurainia (pinnata)",
                                  "Schismus barbatus", "Bromus madridentis", 
                                  "Erodium cicutarium") |
                     code3 %in% c("Descurainia species", "Descurainia (pinnata)",
                                  "Schismus barbatus", "Bromus madridentis", 
                                  "Erodium cicutarium") |
                     code4 %in% c("Descurainia species", "Descurainia (pinnata)",
                                  "Schismus barbatus", "Bromus madridentis", 
                                  "Erodium cicutarium"), 1, 0, missing = 0)) %>%  
  mutate(distance = distance/100) #Divide by 100 to rescale variables for GLMM analyses
.groups = "drop"#Drop groups


#Create model f4.
f4 <- glmmTMB(InvasivePlant_presence ~ distance + 
              (1|well/point/transect), #Random effects of well, point (nested)
            family = binomial(), #binomial distribution (0/1) data
            data = InvasivePlants)
summary(f4)


# Check f4 assumptions with DHARMa package
f4_res = simulateResiduals(f4)
plot(f4_res, rank = T) 
testDispersion(f4_res)
plotResiduals(f4, form = InvasivePlants$distance, main=NULL)

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

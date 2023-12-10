##########################################################################
# MOJA Project ###########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Import and Clean Datasets ###################################
#-------------------------------------------------------------------------

###################################
## PART 1: Import Raw Datasets ####
###################################

LinePoint_Raw <- read_excel("data/raw/MOJA_VegData_2022_LinePoint.xlsx", 
                            skip = 9) 
CanopyGap_Raw <- read_excel("data/raw/MOJA_VegData_2022_CanopyGap.xlsx", 
                            skip = 5)
BasalGap_Raw <- read_excel("data/raw/MOJA_VegData_2022_BasalGap.xlsx", 
                           skip = 5)
PerennialDens_Raw <- read_excel("data/raw/MOJA_VegData_2022_PerennialDens.xlsx", 
                                skip = 5)
Annuals_Raw <- read_excel("data/raw/MOJA_VegData_2022_Annuals.xlsx", 
                          skip = 7)


####################################################
# PART 2: Clean and Summarize LinePoint Dataset ####
####################################################

# PART 2A: Create objects for key species groupings ---------------------------

#Create variable that includes the codes for two plants of interest
selected_plant <- c("Schismus barbatus", "Bromus madridentis")

#Create variable for five invasive plants
invasive_plant <- c("Descurainia species", "Descurainia (pinnata)",
                     "Schismus barbatus", "Bromus madridentis", 
                     "Erodium cicutarium")

#Create variable that includes all the codes for dead vegetation
dead_canopy_codes <- c(NA,"DS","DPG","DC","DPF","DAG","DAF")

#Create variable that includes all the codes for surface types
surface_codes <- c(NA, "L","LIC","M","R","G","S","CC")


#PART 2B: Clean LinePoint dataset ---------------------------------------------
LinePoint <- LinePoint_Raw %>% 
  clean_names() %>% #clean names
  select(-date, -observer, -recorder, -notes, -x17) %>% #remove non-relevant columns
  
  # Manipulate data to summarize key attributes
  
  #Score as 1 or 0 based on presence/absence of litter at any of the levels (code 1-4)
  mutate(litter_presence = if_else(code1 == "L" |
                                     code2 == "L" |
                                     code3 == "L" |
                                     code4 == "L", 1, 0, missing = 0),
         
         #Score as 1 or 0 based on the presence/absence of Schismus or Bromus at any of the levels
         selected_plant_presence = if_else(species %in% selected_plant |
                                        code1 %in% selected_plant |
                                        code2 %in% selected_plant |
                                        code3 %in% selected_plant |
                                        code4 %in% selected_plant , 
                                        1, 0, missing = 0),
         
        #Score as 1 or 0 based on presence/absence of invasive plants (Descurainia spp., Schismus barbatus, Bromus madritentis, Erodium cicutarium) at any of the levels (code 1-4)
        invasive_plant_presence = if_else(species %in% invasive_plant |
                     code1 %in% invasive_plant |
                     code2 %in% invasive_plant |
                     code3 %in% invasive_plant |
                     code4 %in% invasive_plant, 1, 0, missing = 0),
        
        #Replace "none" with N/A in species column
        species = na_if(species, "none"),
        
        #Score as 1 or 0 based on presence/absence of living canopy vegetation
        canopy_cover_presence = if_else(species %in% c(dead_canopy_codes, surface_codes) &
                                       code1 %in% c(dead_canopy_codes, surface_codes) &
                                       code2 %in% c(dead_canopy_codes, surface_codes) &
                                       code3 %in% c(dead_canopy_codes, surface_codes) &
                                       code4 %in% c(dead_canopy_codes, surface_codes),
                                       0, 1, missing = 0),
        
        #Score as 1 or 0 based on presence/absence of any vegetation (dead or alive)
        all_cover_presence = if_else(species %in% surface_codes &
                                          code1 %in% surface_codes &
                                          code2 %in% surface_codes &
                                          code3 %in% surface_codes &
                                          code4 %in% surface_codes,
                                        0, 1, missing = 0)) %>% 
  
  # Group by transect and key variables
  group_by(well, point, transect) %>% #Group by well, transect, point, interval
  
  #Summarize key attributes
  
  summarize(litter_cover = sum(litter_presence),
            selected_plant_cover = sum(selected_plant_presence), 
            invasive_plant_cover = sum(invasive_plant_presence),
            canopy_cover = sum(canopy_cover_presence),
            all_cover = sum(all_cover_presence),
            .groups = "drop") %>% 
          
  #Add distance from well based on point name
  mutate(distance = if_else(point == "A" | point == "B" | 
                            point =="C" | point == "A(X)" | 
                            point == "C(X)", 100, #100m away points
                          if_else(point == "D" | point =="E" |
                                    point =="F" | point == "F(X)", 400, #400m away points
                                  if_else(point == "G" | point =="H" |  
                                            point =="I" | point == "I(X)" |
                                            point == "J(X)" | point == "H(2)", 1600, #1600m away points
                                          0))))

#Export summarized LinePoint dataset
write_csv(LinePoint, "data/processed/LinePoint.csv")

####################################################
# PART 3: Clean and Summarize CanopyGap Dataset ####
####################################################

CanopyGap <- CanopyGap_Raw %>% 
  clean_names() %>% #clean names
  select(well:end_m) %>% #remove non-relevant columns
  mutate(distance = if_else(point == "A" | point == "B" | 
                              point =="C" | point == "A(X)" | 
                              point == "C(X)", 100, #100m away points
                    if_else(point == "D" | point =="E" |
                              point =="F" | point == "F(X)", 400, #400m away points
                    if_else(point == "G" | point =="H" | 
                              point =="I" | point == "I(X)" |
                              point == "J(X)" | point == "H(2)", 1600, 0)))) #1600m away points

write_csv(CanopyGap, "data/processed/CanopyGap.csv")

### Clean BasalGap dataset and export ####
BasalGap <- BasalGap_Raw %>% 
  clean_names() %>%  #clean names
  select(well:end_m) %>% #remove non-relevant columns
  mutate(distance = if_else(point == "A" | point == "B" | 
                              point =="C" | point == "A(X)" | 
                              point == "C(X)", 100, #100m away points
                    if_else(point == "D" | point =="E" |
                              point =="F" | point == "F(X)", 400, #400m away points
                    if_else(point == "G" | point =="H" | 
                              point =="I" | point == "I(X)" |
                              point == "J(X)" | point == "H(2)", 1600, 0)))) #1600m away points

write_csv(BasalGap, "data/processed/BasalGap.csv")



### Clean PerennialDens dataset and export ####
PerennialDens <- PerennialDens_Raw %>% 
  clean_names()

### Clean Annuals dataset and export ####
Annuals <- Annuals_Raw %>% 
  clean_names()




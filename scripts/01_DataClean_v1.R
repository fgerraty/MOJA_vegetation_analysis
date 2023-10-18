##########################################################################
# MOJA Project ###########################################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 00: Import and Clean Dataseta ###################################
#-------------------------------------------------------------------------


## Import Raw Datasets ####

LinePoint_Raw <- read_excel("MOJA_Analysis/data/raw/MOJA_VegData_2022_LinePoint.xlsx", 
                            skip = 9) 
CanopyGap_Raw <- read_excel("MOJA_Analysis/data/raw/MOJA_VegData_2022_CanopyGap.xlsx", 
                            skip = 5)
BasalGap_Raw <- read_excel("MOJA_Analysis/data/raw/MOJA_VegData_2022_BasalGap.xlsx", 
                           skip = 5)
PerennialDens_Raw <- read_excel("MOJA_Analysis/data/raw/MOJA_VegData_2022_PerennialDens.xlsx", 
                                skip = 5)
Annuals_Raw <- read_excel("MOJA_Analysis/data/raw/MOJA_VegData_2022_Annuals.xlsx", 
                          skip = 7)

## Clean datasets and move to "processed" data folder ####

### Clean LinePoint dataset and export ####
LinePoint <- LinePoint_Raw %>% 
  clean_names() %>% #clean names
  select(-date, -observer, -recorder, -notes, -x17) %>% #remove non-relevant columns
  #Add distance from well based on point name
  mutate(distance = if_else(point == "A" | point == "B" | 
                              point =="C" | point == "A(X)" | 
                              point == "C(X)", 100, #100m away points
                            if_else(point == "D" | point =="E" |
                                      point =="F" | point == "F(X)", 400, #400m away points
                                    if_else(point == "G" | point =="H" | 
                                              point =="I" | point == "I(X)" |
                                              point == "J(X)" | point == "H(2)", 1600, 0)))) #1600m away points


write_csv(LinePoint, "MOJA_Analysis/data/processed/LinePoint.csv")


### Clean CanopyGap dataset and export ####
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

write_csv(CanopyGap, "MOJA_Analysis/data/processed/CanopyGap.csv")

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

write_csv(BasalGap, "MOJA_Analysis/data/processed/BasalGap.csv")



### Clean PerennialDens dataset and export ####
PerennialDens <- PerennialDens_Raw %>% 
  clean_names()

### Clean Annuals dataset and export ####
Annuals <- Annuals_Raw %>% 
  clean_names()




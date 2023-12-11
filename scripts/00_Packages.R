##########################################################################
# MOJA Vegetation Analysis Project #######################################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 00: Load packages ###############################################
#-------------------------------------------------------------------------

# Load packages
packages<- c("tidyverse", "readxl", "janitor", "lme4", "glmmTMB", "DHARMa", "nlme")

pacman::p_load(packages, character.only = TRUE)

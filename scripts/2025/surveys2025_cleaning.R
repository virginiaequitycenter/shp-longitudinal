##############################################################
# 2025 Survey Scales- Autonomy, Future Forward Thinking, Resilience, Social Support         
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-06-18
# Summary: A document that cleans the data derived from 
#          surveys collected on June 16 and 17, 2025
#          completed by Starr Hill Pathways students
#          entering 7th, 8th, and 9th grade. 
##############################################################

##############################################################
# Call Libraries
##############################################################

library(readxl)
library(writexl)
library(dplyr)
library(tidyverse)
library(janitor)

##############################################################
# Call and Clean Datasets
##############################################################

# Call dataset of students with completed surveys  
shp_survey <- read_xlsx("raw_data/2025/Summer_2025_SHP_Surveys_ Autonomy_FFT_Resilience_Social Support.xlsx") %>% 
  clean_names() %>%
  subset(!is.na(survey_language))

# Modify Gender Categories
shp_survey <- shp_survey %>% mutate(gender=ifelse(gender!="Boy" & gender!="Girl","Other",gender))

# Factorize Relevant Variables

shp_survey <- shp_survey %>% mutate_at(c("grade","grad_year","gender","race_category","school"),
                                       as.factor)

# Set Relevant Variables to Numeric

shp_survey <- shp_survey %>% mutate_at(c("aa_1","aa_2","aa_3","aa_4","aa_5",                    
                                         "ea_1","ea_2","ea_3","ea_4","ea_5",
                                         "fa_1","fa_2","fa_3","fa_4","fa_5",
                                         "aftrs_1","aftrs_2","aftrs_3","aftrs_4",
                                         "aftrs_6","aftrs_7","aftrs_8","aftrs_9","aftrs_10",                
                                         "aftrs_11","aftrs_12","aftrs_13","aftrs_14","aftrs_15",               
                                         "cyrm_1","cyrm_3","cyrm_4","cyrm_5","cyrm_7",
                                         "cyrm_8","cyrm_9","cyrm_10","cyrm_11","cyrm_12",
                                         "cyrm_13","cyrm_14","cyrm_15","cyrm_16",                  
                                         "ssrs_1","ssrs_2","ssrs_3","ssrs_4","ssrs_5","ssrs_6",                  
                                         "ssrs_12","ssrs_13","ssrs_14","ssrs_15","ssrs_16"),
                                      as.numeric)

##############################################################
# Flip Score of Negative Questions                   
##############################################################

shp_survey <- shp_survey %>% mutate(`aa_1`= 5- `aa_1`+1)
shp_survey <- shp_survey %>% mutate(`aa_3`= 5- `aa_3`+1)
shp_survey <- shp_survey %>% mutate(`aa_5`= 5- `aa_5`+1)
shp_survey <- shp_survey %>% mutate(`ea_1`= 5- `ea_1`+1)
shp_survey <- shp_survey %>% mutate(`ea_2`= 5- `ea_2`+1)
shp_survey <- shp_survey %>% mutate(`ea_4`= 5- `ea_4`+1)
shp_survey <- shp_survey %>% mutate(`ea_5`= 5- `ea_5`+1)
shp_survey <- shp_survey %>% mutate(`fa_2`= 5- `fa_2`+1)
shp_survey <- shp_survey %>% mutate(`aftrs_3`= 5- `aftrs_3`+1)
shp_survey <- shp_survey %>% mutate(`aftrs_6`= 5- `aftrs_6`+1)
shp_survey <- shp_survey %>% mutate(`aftrs_8`= 5- `aftrs_8`+1)
shp_survey <- shp_survey %>% mutate(`aftrs_10`= 5- `aftrs_10`+1)
shp_survey <- shp_survey %>% mutate(`aftrs_11`= 5- `aftrs_11`+1)
shp_survey <- shp_survey %>% mutate(`aftrs_13`= 5- `aftrs_13`+1)
shp_survey <- shp_survey %>% mutate(`aftrs_14`= 5- `aftrs_14`+1)
shp_survey <- shp_survey %>% mutate(`aftrs_15`= 5- `aftrs_15`+1)

##############################################################
# Create Subscale Averages/ Total Averages
##############################################################

shp_survey$aa_sub <- rowMeans(shp_survey[,12:16], na.rm=TRUE)
shp_survey$ea_sub <- rowMeans(shp_survey[,17:21], na.rm=TRUE)
shp_survey$fa_sub <- rowMeans(shp_survey[,22:26], na.rm=TRUE)
shp_survey$avg_autonomy <- rowMeans(shp_survey[,12:26], na.rm=TRUE)

shp_survey$future_pos_sub <- rowMeans(shp_survey[,c(27,28,30,37)], na.rm=TRUE)
shp_survey$maladaptive_sub <- rowMeans(shp_survey[,c(29,31,33,38,39,40)], na.rm=TRUE)
shp_survey$vis_sub <- rowMeans(shp_survey[,c(32,34,35,36)], na.rm=TRUE)
shp_survey$avg_aftrs <- rowMeans(shp_survey[,27:40], na.rm=TRUE)

shp_survey$caregiver_res_sub <- rowMeans(shp_survey[,c(43,44,46,49,53)], na.rm=TRUE)
shp_survey$personal_res_sub <- rowMeans(shp_survey[,c(41,42,45,47,48,50,51,53,54)], na.rm=TRUE)
shp_survey$avg_res <- rowMeans(shp_survey[,41:54], na.rm=TRUE)

shp_survey$valued_sub <- rowMeans(shp_survey[,c(55,56,57,58,59,60)], na.rm=TRUE)
shp_survey$mentoring_sub <- rowMeans(shp_survey[,c(61,62,63,64,65)], na.rm=TRUE)
shp_survey$avg_ssrs <- rowMeans(shp_survey[,55:65], na.rm=TRUE)

shp_survey %>% write_xlsx("data/2025/2025_summer_shp_clean.xlsx")


##############################################################
# 2025 Survey Scales- Autonomy, Future Forward Thinking Change Dataset         
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-08-125
# Summary: A document that creates a dataset that examines the change in 
#          autonomy and future forward thinking in 7-8th and 8-9th grades. 
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
# Call Datasets
##############################################################

# Call 2024 and 2025 autonomy and future-forward thinking data
 
a_fft_24 <- read_xlsx("data/2024/autonomy_fft_clean.xlsx") %>% clean_names() %>%
            select(!c("grade_10","user_language","age","school","grade_4","vdoe_race",
                      "self_race","gender"))  
a_fft_25 <- read_xlsx("data/2025/2025_summer_shp_clean.xlsx") %>% clean_names() %>%  
            select(!contains("ssrs")) 
a_fft_25 <- a_fft_25 %>% select(!contains("cyrm"))
a_fft_25 <- a_fft_25 %>% select(!c("avg_res","valued_sub","mentoring_sub",
                                   "first_name","last_name","survey_language",
                                   "caregiver_res_sub","personal_res_sub","school","grade",
                                   "race_category","race_self_id","gender"))

##############################################################
# Merge to Create Change Dataset
##############################################################

autonomy_change_24 <- a_fft_24 %>% select(!contains("aftrs"))  
autonomy_change_24 <- autonomy_change_24 %>% select(!c("future_pos_sub","maladaptive_sub","vis_sub"))  
colnames(autonomy_change_24) <- c("name","campminder_id",
                             "24_aa_1","24_aa_2","24_aa_3","24_aa_4","24_aa_5","24_ea_1","24_ea_2","24_ea_3",
                             "24_ea_4","24_ea_5","24_fa_1","24_fa_2","24_fa_3","24_fa_4","24_fa_5",
                             "24_aa_sub","24_ea_sub","24_fa_sub","24_avg_autonomy")

autonomy_change_25 <- a_fft_25 %>% select(!contains("aftrs"))  
autonomy_change_25 <- autonomy_change_25 %>% select(!c("future_pos_sub","maladaptive_sub","vis_sub"))  
colnames(autonomy_change_25) <- c("name","campminder_id","grad_year",
                               "25_aa_1","25_aa_2","25_aa_3","25_aa_4","25_aa_5","25_ea_1","25_ea_2","25_ea_3",
                               "25_ea_4","25_ea_5","25_fa_1","25_fa_2","25_fa_3","25_fa_4","25_fa_5",
                               "25_aa_sub","25_ea_sub","25_fa_sub","25_avg_autonomy")

autonomy_change <- merge(x=autonomy_change_24,y=autonomy_change_25)

##############################################################
# Create Change Values
##############################################################

autonomy_change <- autonomy_change %>% add_column(aa_1_c=0)
autonomy_change <- autonomy_change %>% mutate(aa_1_c=`25_aa_1`-`24_aa_1`)
autonomy_change <- autonomy_change %>% add_column(aa_2_c=0)
autonomy_change <- autonomy_change %>% mutate(aa_2_c=`25_aa_2`-`24_aa_2`)
autonomy_change <- autonomy_change %>% add_column(aa_3_c=0)
autonomy_change <- autonomy_change %>% mutate(aa_3_c=`25_aa_3`-`24_aa_3`)
autonomy_change <- autonomy_change %>% add_column(aa_4_c=0)
autonomy_change <- autonomy_change %>% mutate(aa_4_c=`25_aa_4`-`24_aa_4`)
autonomy_change <- autonomy_change %>% add_column(aa_5_c=0)
autonomy_change <- autonomy_change %>% mutate(aa_5_c=`25_aa_5`-`24_aa_5`)

autonomy_change <- autonomy_change %>% add_column(ea_1_c=0)
autonomy_change <- autonomy_change %>% mutate(ea_1_c=`25_ea_1`-`24_ea_1`)
autonomy_change <- autonomy_change %>% add_column(ea_2_c=0)
autonomy_change <- autonomy_change %>% mutate(ea_2_c=`25_ea_2`-`24_ea_2`)
autonomy_change <- autonomy_change %>% add_column(ea_3_c=0)
autonomy_change <- autonomy_change %>% mutate(ea_3_c=`25_ea_3`-`24_ea_3`)
autonomy_change <- autonomy_change %>% add_column(ea_4_c=0)
autonomy_change <- autonomy_change %>% mutate(ea_4_c=`25_ea_4`-`24_ea_4`)
autonomy_change <- autonomy_change %>% add_column(ea_5_c=0)
autonomy_change <- autonomy_change %>% mutate(ea_5_c=`25_ea_5`-`24_ea_5`)

autonomy_change <- autonomy_change %>% add_column(fa_1_c=0)
autonomy_change <- autonomy_change %>% mutate(fa_1_c=`25_fa_1`-`24_fa_1`)
autonomy_change <- autonomy_change %>% add_column(fa_2_c=0)
autonomy_change <- autonomy_change %>% mutate(fa_2_c=`25_fa_2`-`24_fa_2`)
autonomy_change <- autonomy_change %>% add_column(fa_3_c=0)
autonomy_change <- autonomy_change %>% mutate(fa_3_c=`25_fa_3`-`24_fa_3`)
autonomy_change <- autonomy_change %>% add_column(fa_4_c=0)
autonomy_change <- autonomy_change %>% mutate(fa_4_c=`25_fa_4`-`24_fa_4`)
autonomy_change <- autonomy_change %>% add_column(fa_5_c=0)
autonomy_change <- autonomy_change %>% mutate(fa_5_c=`25_fa_5`-`24_fa_5`)

autonomy_change <- autonomy_change %>% add_column(aa_sub_c=0)
autonomy_change <- autonomy_change %>% mutate(aa_sub_c=`25_aa_sub`-`24_aa_sub`)
autonomy_change <- autonomy_change %>% add_column(ea_sub_c=0)
autonomy_change <- autonomy_change %>% mutate(ea_sub_c=`25_ea_sub`-`24_ea_sub`)
autonomy_change <- autonomy_change %>% add_column(fa_sub_c=0)
autonomy_change <- autonomy_change %>% mutate(fa_sub_c=`25_fa_sub`-`24_fa_sub`)
autonomy_change <- autonomy_change %>% add_column(avg_autonomy_c=0)
autonomy_change <- autonomy_change %>% mutate(avg_autonomy_c=`25_avg_autonomy`-`24_avg_autonomy`)


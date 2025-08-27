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
# Merge to Create Autonomy Change Dataset
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
# Create Autonomy Change Values
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

autonomy_change <- autonomy_change %>% select(c("name","campminder_id","24_aa_sub","24_ea_sub","24_fa_sub",
                                                "24_avg_autonomy","25_aa_sub","25_ea_sub","25_fa_sub","25_avg_autonomy",
                                                "aa_1_c","aa_2_c","aa_3_c","aa_4_c","aa_5_c","ea_1_c","ea_2_c",
                                                "ea_3_c","ea_4_c","ea_5_c","fa_1_c","fa_2_c","fa_3_c","fa_4_c","fa_5_c",
                                                "aa_sub_c","ea_sub_c","fa_sub_c","avg_autonomy_c"))

write_xlsx(autonomy_change, 'data/2025/autonomy_change_24-25.xlsx')

##############################################################
# Merge to Create Future Forward Thinking Change Dataset
##############################################################

fft_change_24 <- a_fft_24 %>% select(!contains(c("aa","ea","fa","autonomy","aftrs_5")))
colnames(fft_change_24) <- c("name","campminder_id","24_aftrs_1","24_aftrs_2","24_aftrs_3",
                             "24_aftrs_4","24_aftrs_6","24_aftrs_7","24_aftrs_8","24_aftrs_9",
                             "24_aftrs_10","24_aftrs_11","24_aftrs_12","24_aftrs_13","24_aftrs_14",
                             "24_aftrs_15","24_future_pos_sub","24_maladaptive_sub",
                             "24_vis_sub","24_avg_aftrs")


fft_change_25 <- a_fft_25 %>% select(!contains(c("aa","ea","fa","autonomy")))
colnames(fft_change_25) <- c("name","campminder_id","25_aftrs_1","25_aftrs_2","25_aftrs_3",
                             "25_aftrs_4","25_aftrs_6","25_aftrs_7","25_aftrs_8","25_aftrs_9",
                             "25_aftrs_10","25_aftrs_11","25_aftrs_12","25_aftrs_13","25_aftrs_14",
                             "25_aftrs_15","25_future_pos_sub","25_maladaptive_sub",
                             "25_vis_sub","25_avg_aftrs")

fft_change <- merge(x=fft_change_24,y=fft_change_25)

##############################################################
# Create FFT Change Values
##############################################################

fft_change <- fft_change %>% add_column(aftrs_1_c=0)
fft_change <- fft_change %>% mutate(aftrs_1_c=`25_aftrs_1`-`24_aftrs_1`)
fft_change <- fft_change %>% add_column(aftrs_2_c=0)
fft_change <- fft_change %>% mutate(aftrs_2_c=`25_aftrs_2`-`24_aftrs_2`)
fft_change <- fft_change %>% add_column(aftrs_3_c=0)
fft_change <- fft_change %>% mutate(aftrs_3_c=`25_aftrs_3`-`24_aftrs_3`)
fft_change <- fft_change %>% add_column(aftrs_4_c=0)
fft_change <- fft_change %>% mutate(aftrs_4_c=`25_aftrs_4`-`24_aftrs_4`)
fft_change <- fft_change %>% add_column(aftrs_6_c=0)
fft_change <- fft_change %>% mutate(aftrs_6_c=`25_aftrs_6`-`24_aftrs_6`)
fft_change <- fft_change %>% add_column(aftrs_7_c=0)
fft_change <- fft_change %>% mutate(aftrs_7_c=`25_aftrs_7`-`24_aftrs_7`)
fft_change <- fft_change %>% add_column(aftrs_8_c=0)
fft_change <- fft_change %>% mutate(aftrs_8_c=`25_aftrs_8`-`24_aftrs_8`)
fft_change <- fft_change %>% add_column(aftrs_9_c=0)
fft_change <- fft_change %>% mutate(aftrs_9_c=`25_aftrs_9`-`24_aftrs_9`)
fft_change <- fft_change %>% add_column(aftrs_10_c=0)
fft_change <- fft_change %>% mutate(aftrs_10_c=`25_aftrs_10`-`24_aftrs_10`)
fft_change <- fft_change %>% add_column(aftrs_11_c=0)
fft_change <- fft_change %>% mutate(aftrs_11_c=`25_aftrs_11`-`24_aftrs_11`)
fft_change <- fft_change %>% add_column(aftrs_12_c=0)
fft_change <- fft_change %>% mutate(aftrs_12_c=`25_aftrs_12`-`24_aftrs_12`)
fft_change <- fft_change %>% add_column(aftrs_13_c=0)
fft_change <- fft_change %>% mutate(aftrs_13_c=`25_aftrs_13`-`24_aftrs_13`)
fft_change <- fft_change %>% add_column(aftrs_14_c=0)
fft_change <- fft_change %>% mutate(aftrs_14_c=`25_aftrs_14`-`24_aftrs_14`)
fft_change <- fft_change %>% add_column(aftrs_15_c=0)
fft_change <- fft_change %>% mutate(aftrs_15_c=`25_aftrs_15`-`24_aftrs_15`)

fft_change <- fft_change %>% add_column(future_pos_sub_c=0)
fft_change <- fft_change %>% mutate(future_pos_sub_c=`25_future_pos_sub`-`24_future_pos_sub`)
fft_change <- fft_change %>% add_column(maladaptive_sub_c=0)
fft_change <- fft_change %>% mutate(maladaptive_sub_c=`25_maladaptive_sub`-`24_maladaptive_sub`)
fft_change <- fft_change %>% add_column(vis_sub_c=0)
fft_change <- fft_change %>% mutate(vis_sub_c=`25_vis_sub`-`24_vis_sub`)
fft_change <- fft_change %>% add_column(avg_aftrs_c=0)
fft_change <- fft_change %>% mutate(avg_aftrs_c=`25_avg_aftrs`-`24_avg_aftrs`)

fft_change <- fft_change %>% select(c("name","campminder_id","24_future_pos_sub","24_maladaptive_sub","24_vis_sub",
                                      "24_avg_aftrs","25_future_pos_sub","25_maladaptive_sub","25_vis_sub",
                                      "25_avg_aftrs","aftrs_1_c","aftrs_2_c","aftrs_3_c","aftrs_4_c","aftrs_6_c",
                                      "aftrs_7_c","aftrs_8_c","aftrs_9_c","aftrs_10_c","aftrs_11_c","aftrs_12_c",
                                      "aftrs_13_c","aftrs_14_c","aftrs_15_c","future_pos_sub_c",
                                      "maladaptive_sub_c","vis_sub_c","25_avg_aftrs"))

write_xlsx(fft_change, 'data/2025/fft_change_24-25.xlsx')

# Next steps must take place in IVY using protected student data. 


                                      
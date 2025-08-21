##############################################################
# 7th and 8th grade Survey Scales- Autonomy and Future Forward Thinking          
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-07-31
# Summary: A document that analyzes the data from the Autonomy and FFT 
#          Surveys implemented on Starr Hill Pathways students 
#          during the summer of 2024
##############################################################

##############################################################
# Library Intros                               
##############################################################

library(tidyverse)
library(readxl)
library(dplyr)
library(writexl)

##############################################################
# Pull in Data                    
##############################################################

a_fft_78 <- read_excel("raw_data/2024/Autonomy and Future Forward Thinking 7th and 8th Survey Responses_.xlsx")

##############################################################
# Change Negatives                    
##############################################################

a_fft_78 <- a_fft_78 %>% subset(!is.na(`FA-1`))
a_fft_78 <- a_fft_78 %>% mutate(`FA-1` = as.numeric(`FA-1`))
a_fft_78 <- a_fft_78 %>% mutate(`FA-2` = as.numeric(`FA-2`))
a_fft_78 <- a_fft_78 %>% mutate(`FA-3` = as.numeric(`FA-3`))
a_fft_78 <- a_fft_78 %>% mutate(`FA-4` = as.numeric(`FA-4`))
a_fft_78 <- a_fft_78 %>% mutate(`FA-5` = as.numeric(`FA-5`))


a_fft_78 <- a_fft_78 %>% mutate(`AA-1`= 5- `AA-1`)
a_fft_78 <- a_fft_78 %>% mutate(`AA-3`= 5- `AA-3`)
a_fft_78 <- a_fft_78 %>% mutate(`AA-5`= 5- `AA-5`)
a_fft_78 <- a_fft_78 %>% mutate(`EA-1`= 5- `EA-1`)
a_fft_78 <- a_fft_78 %>% mutate(`EA-2`= 5- `EA-2`)
a_fft_78 <- a_fft_78 %>% mutate(`EA-4`= 5- `EA-4`)
a_fft_78 <- a_fft_78 %>% mutate(`EA-5`= 5- `EA-5`)
a_fft_78 <- a_fft_78 %>% mutate(`FA-2`= 5- `FA-2`)
a_fft_78 <- a_fft_78 %>% mutate(`AFTRS-3`= 5- `AFTRS-3`)
a_fft_78 <- a_fft_78 %>% mutate(`AFTRS-6`= 5- `AFTRS-6`)
a_fft_78 <- a_fft_78 %>% mutate(`AFTRS-8`= 5- `AFTRS-8`)
a_fft_78 <- a_fft_78 %>% mutate(`AFTRS-10`= 5- `AFTRS-10`)
a_fft_78 <- a_fft_78 %>% mutate(`AFTRS-11`= 5- `AFTRS-11`)
a_fft_78 <- a_fft_78 %>% mutate(`AFTRS-13`= 5- `AFTRS-13`)
a_fft_78 <- a_fft_78 %>% mutate(`AFTRS-14`= 5- `AFTRS-14`)
a_fft_78 <- a_fft_78 %>% mutate(`AFTRS-15`= 5- `AFTRS-15`)

##############################################################
# Create Subscale Averages/ Total Averages
##############################################################

a_fft_78$AA_Sub <- rowMeans(a_fft_78[,11:15], na.rm=TRUE)
a_fft_78$EA_Sub <- rowMeans(a_fft_78[,16:20], na.rm=TRUE)
a_fft_78$FA_Sub <- rowMeans(a_fft_78[,21:25], na.rm=TRUE)
a_fft_78$avg_autonomy <- rowMeans(a_fft_78[,11:25], na.rm=TRUE)

mean_a_avg <- mean(a_fft_78$avg_autonomy,na.rm=TRUE)
mean_aa <- mean(a_fft_78$AA_Sub,na.rm=TRUE)
mean_ea <- mean(a_fft_78$EA_Sub,na.rm=TRUE)
mean_fa <- mean(a_fft_78$FA_Sub,na.rm=TRUE)

a_fft_78$future_pos_sub <- rowMeans(a_fft_78[,c(26,27,29,37)], na.rm=TRUE)
a_fft_78$maladaptive_sub <- rowMeans(a_fft_78[,c(28,31,33,38,39,40)], na.rm=TRUE)
a_fft_78$vis_sub <- rowMeans(a_fft_78[,c(32,34,35,36)], na.rm=TRUE)
a_fft_78$avg_aftrs <- rowMeans(a_fft_78[,c(26,27,28,29,31,32,33,34,35,36,37,38,39,40)], na.rm=TRUE)

mean_fft_avg <- mean(a_fft_78$avg_aftrs,na.rm=TRUE)
mean_future_pos <- mean(a_fft_78$future_pos_sub,na.rm=TRUE)
mean_mal <- mean(a_fft_78$maladaptive_sub,na.rm=TRUE)
mean_vis <- mean(a_fft_78$vis_sub,na.rm=TRUE)

write_xlsx(a_fft_78, 'data/2024/autonomy_fft_clean.xlsx')

##############################################################
# Plot Autonomy Subscales
##############################################################

ggplot() + 
  geom_point(data=a_fft_78, aes(y=avg_autonomy, x=-1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=AA_Sub, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=EA_Sub, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=FA_Sub, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= a_fft_78, aes(y=mean_a_avg, yend=mean_a_avg, x=-1.4, xend=-.6), linewidth=1, color ="black") +
  geom_segment(data= a_fft_78, aes(y=mean_aa, yend=mean_aa, x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= a_fft_78, aes(y=mean_ea, yend=mean_ea, x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= a_fft_78, aes(y=mean_fa, yend=mean_fa, x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="Autonomy Subscale", y="Autonomy Score", 
       title="SHP Average Autonomy Scores by Subscale") +
  scale_x_continuous(breaks = -1:2,
                     labels = c("Composite","Attitudinal", "Emotional", "Functional"))

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`AA-1`, x=-1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`AA-2`, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`AA-3`, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`AA-4`, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_point(data=a_fft_78, aes(y=`AA-5`, x=3), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AA-1`,na.rm=TRUE), yend=mean(a_fft_78$`AA-1`,na.rm=TRUE), x=-1.4, xend=-.6), linewidth=1, color ="black") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AA-2`,na.rm=TRUE), yend=mean(a_fft_78$`AA-2`,na.rm=TRUE), x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AA-3`,na.rm=TRUE), yend=mean(a_fft_78$`AA-3`,na.rm=TRUE), x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AA-4`,na.rm=TRUE), yend=mean(a_fft_78$`AA-4`,na.rm=TRUE), x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AA-5`,na.rm=TRUE), yend=mean(a_fft_78$`AA-5`,na.rm=TRUE), x=2.6, xend=3.4), linewidth=1, color ="#139E02") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="Autonomy Subscale", y="Autonomy Score", 
       title="SHP Attitudinal Autonomy Scores by Question") +
  scale_x_continuous(breaks = -1:3,
                     labels = c("AA-1","AA-2", "AA-3", "AA-4", "AA-5"))


ggplot() + 
  geom_point(data=a_fft_78, aes(y=`EA-1`, x=-1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`EA-2`, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`EA-3`, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`EA-4`, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_point(data=a_fft_78, aes(y=`EA-5`, x=3), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`EA-1`,na.rm=TRUE), yend=mean(a_fft_78$`EA-1`,na.rm=TRUE), x=-1.4, xend=-.6), linewidth=1, color ="black") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`EA-2`,na.rm=TRUE), yend=mean(a_fft_78$`EA-2`,na.rm=TRUE), x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`EA-3`,na.rm=TRUE), yend=mean(a_fft_78$`EA-3`,na.rm=TRUE), x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`EA-4`,na.rm=TRUE), yend=mean(a_fft_78$`EA-4`,na.rm=TRUE), x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`EA-5`,na.rm=TRUE), yend=mean(a_fft_78$`EA-5`,na.rm=TRUE), x=2.6, xend=3.4), linewidth=1, color ="#139E02") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="Autonomy Subscale", y="Autonomy Score", 
       title="SHP Emotional Autonomy Scores by Question") +
  scale_x_continuous(breaks = -1:3,
                     labels = c("EA-1","EA-2", "EA-3", "EA-4", "EA-5"))




ggplot() + 
  geom_point(data=a_fft_78, aes(y=`FA-1`, x=-1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`FA-2`, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`FA-3`, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`FA-4`, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_point(data=a_fft_78, aes(y=`FA-5`, x=3), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`FA-1`,na.rm=TRUE), yend=mean(a_fft_78$`FA-1`,na.rm=TRUE), x=-1.4, xend=-.6), linewidth=1, color ="black") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`FA-2`,na.rm=TRUE), yend=mean(a_fft_78$`FA-2`,na.rm=TRUE), x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`FA-3`,na.rm=TRUE), yend=mean(a_fft_78$`FA-3`,na.rm=TRUE), x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`FA-4`,na.rm=TRUE), yend=mean(a_fft_78$`FA-4`,na.rm=TRUE), x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`FA-5`,na.rm=TRUE), yend=mean(a_fft_78$`FA-5`,na.rm=TRUE), x=2.6, xend=3.4), linewidth=1, color ="#139E02") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="Autonomy Subscale", y="Autonomy Score", 
       title="SHP Functional Autonomy Scores by Question") +
  scale_x_continuous(breaks = -1:3,
                     labels = c("FA-1","FA-2", "FA-3", "FA-4", "FA-5"))

##############################################################
# Plot Autonomy Subgroup
##############################################################

a_fft_78 <- a_fft_78 %>% mutate(Gender=ifelse(Gender!="Boy" & Gender!="Girl","Another Identity",Gender))

mean_Gender <- a_fft_78 %>% group_by(Gender) %>% summarise(autonomy_mean=mean(avg_autonomy, na.rm=TRUE),
                                                           ea_mean=mean(EA_Sub, na.rm=TRUE),
                                                           aa_mean=mean(AA_Sub, na.rm=TRUE),
                                                           fa_mean=mean(FA_Sub, na.rm=TRUE),
                                                           aftrs_mean=mean(avg_aftrs,na.rm=TRUE),
                                                           future_pos_mean=mean(future_pos_sub,na.rm=TRUE),
                                                           maladaptive_mean=mean(maladaptive_sub,na.rm=TRUE),
                                                           vis_mean=mean(vis_sub,na.rm=TRUE))
 
a_fft_78 <- a_fft_78 %>% mutate(VDOE_Race=ifelse(VDOE_Race=="Black/African-American","Black",VDOE_Race))
a_fft_78 <- a_fft_78 %>% mutate(VDOE_Race=ifelse(VDOE_Race=="Hispanic/Latino","Hispanic",VDOE_Race))
a_fft_78 <- a_fft_78 %>% mutate(VDOE_Race=ifelse(VDOE_Race=="Latinx/Hispanic","Hispanic",VDOE_Race))
a_fft_78 <- a_fft_78 %>% mutate(VDOE_Race=ifelse(VDOE_Race=="white","White",VDOE_Race))
a_fft_78 <- a_fft_78 %>% mutate(VDOE_Race=ifelse(VDOE_Race=="White/Asian","Multiracial",VDOE_Race))

mean_Eth <- a_fft_78 %>% group_by(VDOE_Race) %>% summarise(autonomy_mean=mean(avg_autonomy, na.rm=TRUE),
                                                           ea_mean=mean(EA_Sub, na.rm=TRUE),
                                                           aa_mean=mean(AA_Sub, na.rm=TRUE),
                                                           fa_mean=mean(FA_Sub, na.rm=TRUE),
                                                           aftrs_mean=mean(avg_aftrs,na.rm=TRUE),
                                                           future_pos_mean=mean(future_pos_sub,na.rm=TRUE),
                                                           maladaptive_mean=mean(maladaptive_sub,na.rm=TRUE),
                                                           vis_mean=mean(vis_sub,na.rm=TRUE))

a_fft_78 <- a_fft_78 %>% select(!`Grade...10`)
names(a_fft_78)[names(a_fft_78) == 'Grade...4'] <- 'Grade'


mean_grade <- a_fft_78 %>% group_by(Grade) %>% summarise(autonomy_mean=mean(avg_autonomy, na.rm=TRUE),
                                                           ea_mean=mean(EA_Sub, na.rm=TRUE),
                                                           aa_mean=mean(AA_Sub, na.rm=TRUE),
                                                           fa_mean=mean(FA_Sub, na.rm=TRUE),
                                                           aftrs_mean=mean(avg_aftrs,na.rm=TRUE),
                                                           future_pos_mean=mean(future_pos_sub,na.rm=TRUE),
                                                           maladaptive_mean=mean(maladaptive_sub,na.rm=TRUE),
                                                           vis_mean=mean(vis_sub,na.rm=TRUE))

a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Buford Middle School","Buford",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Buford Middle School/Burly","Buford",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Burley Middle School","Burley",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Jakson P Bulley Middle School","Burley",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="burley Middle School","Burley",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Other","Burley",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Lakeside Middle School","Lakeside",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Journey Middle School","Journey",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="I dont no","Burley",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Walton Middle School","Walton",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="Wilson Middle School","Outside of ACPS/CCS",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(School=="St. Anne's-Belfield","Outside of ACPS/CCS",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(Campminder_ID=="17590029","Journey",School))
a_fft_78 <- a_fft_78 %>% mutate(School=ifelse(Campminder_ID=="18581431","Journey",School))

mean_school <- a_fft_78 %>% group_by(School) %>% summarise(autonomy_mean=mean(avg_autonomy, na.rm=TRUE),
                                                         ea_mean=mean(EA_Sub, na.rm=TRUE),
                                                         aa_mean=mean(AA_Sub, na.rm=TRUE),
                                                         fa_mean=mean(FA_Sub, na.rm=TRUE))

#Autonomy by Gender
ggplot() + 
    geom_point(data=a_fft_78, aes(y=`avg_autonomy`, x = -1, color=Gender), position=position_jitter(0.175), alpha=.5) +
    geom_hline(data= mean_Gender, aes(yintercept = autonomy_mean,col=Gender), linewidth=1)+
    facet_grid(~Gender)+
    scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
    theme_minimal()+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.x=element_blank()) +
    labs(x="Gender", y="Autonomy Score", 
         title="SHP Autonomy Scores by Gender")

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_autonomy`, x = -1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`AA_Sub`, x = 0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`EA_Sub`, x = 1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`FA_Sub`, x = 2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_hline(data= mean_Gender, aes(yintercept = autonomy_mean,col=Gender), linewidth=1, color="black")+
  geom_hline(data= mean_Gender, aes(yintercept = aa_mean,col=Gender), linewidth=1, color="#8D029E")+
  geom_hline(data= mean_Gender, aes(yintercept = ea_mean,col=Gender), linewidth=1, color="#D9470C")+
  geom_hline(data= mean_Gender, aes(yintercept = fa_mean,col=Gender), linewidth=1, color="#0C9ED9")+
  facet_grid(~`Gender`)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Gender", y="Autonomy Score", 
       title="SHP Autonomy Scores by Gender")

#Autonomy by Race
ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_autonomy`, x = -1, color=VDOE_Race), position=position_jitter(0.175), alpha=.5) +
  geom_hline(data= mean_Eth, aes(yintercept = autonomy_mean,col=VDOE_Race), linewidth=1)+
  facet_grid(~VDOE_Race)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Race", y="Autonomy Score", 
       title="SHP Autonomy Scores by Race")

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_autonomy`, x = -1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`AA_Sub`, x = 0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`EA_Sub`, x = 1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`FA_Sub`, x = 2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_hline(data= mean_Eth, aes(yintercept = autonomy_mean,col=VDOE_Race), linewidth=1, color="black")+
  geom_hline(data= mean_Eth, aes(yintercept = aa_mean,col=VDOE_Race), linewidth=1, color="#8D029E")+
  geom_hline(data= mean_Eth, aes(yintercept = ea_mean,col=VDOE_Race), linewidth=1, color="#D9470C")+
  geom_hline(data= mean_Eth, aes(yintercept = fa_mean,col=VDOE_Race), linewidth=1, color="#0C9ED9")+
  facet_grid(~`VDOE_Race`)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Race", y="Autonomy Score", 
       title="SHP Autonomy Scores by Race")

#Autonomy by Grade

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_autonomy`, x = -1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`AA_Sub`, x = 0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`EA_Sub`, x = 1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`FA_Sub`, x = 2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_hline(data= mean_grade, aes(yintercept = autonomy_mean,col=Grade), linewidth=1, color="black")+
  geom_hline(data= mean_grade, aes(yintercept = aa_mean,col=Grade), linewidth=1, color="#8D029E")+
  geom_hline(data= mean_grade, aes(yintercept = ea_mean,col=Grade), linewidth=1, color="#D9470C")+
  geom_hline(data= mean_grade, aes(yintercept = fa_mean,col=Grade), linewidth=1, color="#0C9ED9")+
  facet_grid(~`Grade`)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Grade", y="Autonomy Score", 
       title="SHP Autonomy Scores by Grade")

#Autonomy by School

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_autonomy`, x = -1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`AA_Sub`, x = 0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`EA_Sub`, x = 1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`FA_Sub`, x = 2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_hline(data= mean_school, aes(yintercept = autonomy_mean,col=School), linewidth=1, color="black")+
  geom_hline(data= mean_school, aes(yintercept = aa_mean,col=School), linewidth=1, color="#8D029E")+
  geom_hline(data= mean_school, aes(yintercept = ea_mean,col=School), linewidth=1, color="#D9470C")+
  geom_hline(data= mean_school, aes(yintercept = fa_mean,col=School), linewidth=1, color="#0C9ED9")+
  facet_grid(~`School`)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="School", y="Autonomy Score", 
       title="SHP Autonomy Scores by School")



##############################################################
# Plot FFT Subscales
##############################################################

ggplot() + 
  geom_point(data=a_fft_78, aes(y=avg_aftrs, x=-1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=future_pos_sub, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=maladaptive_sub, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=vis_sub, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= a_fft_78, aes(y=mean_fft_avg, yend=mean_fft_avg, x=-1.4, xend=-.6), linewidth=1, color ="black") +
  geom_segment(data= a_fft_78, aes(y=mean_future_pos, yend=mean_future_pos, x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= a_fft_78, aes(y=mean_mal, yend=mean_mal, x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= a_fft_78, aes(y=mean_vis, yend=mean_vis, x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="FFT Subscale", y="FFT Score", 
       title="SHP Average Future Forward Thinking Scores by Subscale") +
  scale_x_continuous(breaks = -1:2,
                     labels = c("Composite","Future Positivity", "Maladaptive Future Thinking", "Future Visualization"))

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`AFTRS-1`, x=-1), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-2`, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-4`, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-12`, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-1`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-1`,na.rm=TRUE), x=-1.4, xend=-.6), linewidth=1, color ="#139E02") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-2`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-2`,na.rm=TRUE), x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-4`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-4`,na.rm=TRUE), x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-12`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-12`,na.rm=TRUE), x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="FFT Subscale", y="FFT Score", 
       title="SHP Future Positivity Scores by Question") +
  scale_x_continuous(breaks = -1:2,
                     labels = c("AFTRS-1","AFTRS-2", "AFTRS-4", "AFTRS-12"))

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`AFTRS-7`, x=-1), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-9`, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-10`, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-11`, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-7`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-7`,na.rm=TRUE), x=-1.4, xend=-.6), linewidth=1, color ="#139E02") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-9`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-9`,na.rm=TRUE), x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-10`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-10`,na.rm=TRUE), x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-11`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-11`,na.rm=TRUE), x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="FFT Subscale", y="FFT Score", 
       title="SHP Future Visualization Scores by Question") +
  scale_x_continuous(breaks = -1:2,
                     labels = c("AFTRS-7","AFTRS-9", "AFTRS-10", "AFTRS-11"))

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`AFTRS-3`, x=-1), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-6`, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-8`, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-13`, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-14`, x=3), position=position_jitter(0.175), alpha=.5, color="#F8BE3D") +
  geom_point(data=a_fft_78, aes(y=`AFTRS-15`, x=4), position=position_jitter(0.175), alpha=.5, color="black") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-3`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-3`,na.rm=TRUE), x=-1.4, xend=-.6), linewidth=1, color ="#139E02") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-6`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-6`,na.rm=TRUE), x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-8`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-8`,na.rm=TRUE), x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-13`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-13`,na.rm=TRUE), x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-14`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-14`,na.rm=TRUE), x=2.6, xend=3.4), linewidth=1, color ="#F8BE3D") +
  geom_segment(data= a_fft_78, aes(y=mean(a_fft_78$`AFTRS-15`,na.rm=TRUE), yend=mean(a_fft_78$`AFTRS-15`,na.rm=TRUE), x=3.6, xend=4.4), linewidth=1, color ="black") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="FFT Subscale", y="FFT Score", 
       title="SHP Maladaptive Future Thinking Scores by Question") +
  scale_x_continuous(breaks = -1:4,
                     labels = c("AFTRS-3","AFTRS-6", "AFTRS-8", "AFTRS-13", "AFTRS-14", "AFTRS-15"))

##############################################################
# Plot FFT by Subgroup
##############################################################

# FFT by Gender

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_aftrs`, x = -1, color=Gender), position=position_jitter(0.175), alpha=.5) +
  geom_hline(data= mean_Gender, aes(yintercept = aftrs_mean,col=Gender), linewidth=1)+
  facet_grid(~Gender)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Gender", y="Future Forward Thinking Score", 
       title="SHP FFT Scores by Gender")

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_aftrs`, x = -1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`vis_sub`, x = 0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`maladaptive_sub`, x = 1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`future_pos_sub`, x = 2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_hline(data= mean_Gender, aes(yintercept =aftrs_mean,col=Gender), linewidth=1, color="black")+
  geom_hline(data= mean_Gender, aes(yintercept = vis_mean,col=Gender), linewidth=1, color="#8D029E")+
  geom_hline(data= mean_Gender, aes(yintercept = maladaptive_mean,col=Gender), linewidth=1, color="#D9470C")+
  geom_hline(data= mean_Gender, aes(yintercept = future_pos_mean,col=Gender), linewidth=1, color="#0C9ED9")+
  facet_grid(~`Gender`)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Gender", y="Autonomy Score", 
       title="SHP Autonomy Scores by Gender")

#FFT by Race

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_aftrs`, x = -1, color=VDOE_Race), position=position_jitter(0.175), alpha=.5) +
  geom_hline(data= mean_Eth, aes(yintercept =aftrs_mean,col=VDOE_Race), linewidth=1)+
  facet_grid(~VDOE_Race)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Race", y="Future Forward Thinking Score", 
       title="SHP FFT Scores by Race")

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_aftrs`, x = -1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`vis_sub`, x = 0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`maladaptive_sub`, x = 1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`future_pos_sub`, x = 2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_hline(data= mean_Eth, aes(yintercept =aftrs_mean,col=VDOE_Race), linewidth=1, color="black")+
  geom_hline(data= mean_Eth, aes(yintercept = vis_mean,col=VDOE_Race), linewidth=1, color="#8D029E")+
  geom_hline(data= mean_Eth, aes(yintercept = maladaptive_mean,col=VDOE_Race), linewidth=1, color="#D9470C")+
  geom_hline(data= mean_Eth, aes(yintercept = future_pos_mean,col=VDOE_Race), linewidth=1, color="#0C9ED9")+
  facet_grid(~`VDOE_Race`)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Race", y="Autonomy Score", 
       title="SHP Autonomy Scores by Race")

#FFT by Grade

ggplot() + 
  geom_point(data=a_fft_78, aes(y=`avg_aftrs`, x = -1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=a_fft_78, aes(y=`vis_sub`, x = 0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=a_fft_78, aes(y=`maladaptive_sub`, x = 1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=a_fft_78, aes(y=`future_pos_sub`, x = 2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_hline(data= mean_grade, aes(yintercept = aftrs_mean,col=Grade), linewidth=1, color="black")+
  geom_hline(data= mean_grade, aes(yintercept = vis_mean,col=Grade), linewidth=1, color="#8D029E")+
  geom_hline(data= mean_grade, aes(yintercept = maladaptive_mean,col=Grade), linewidth=1, color="#D9470C")+
  geom_hline(data= mean_grade, aes(yintercept = future_pos_mean,col=Grade), linewidth=1, color="#0C9ED9")+
  facet_grid(~`Grade`)+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_blank()) +
  labs(x="Grade", y="FFT Score", 
       title="SHP FFT Scores by Grade")

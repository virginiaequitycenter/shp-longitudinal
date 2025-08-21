##############################################################
# 9th grade Survey Scales- Resilience          
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-08-06
# Summary: A document that analyzes the data from the Resilience 
#          Surveys implemented on Starr Hill Pathways students 
#          during the summer of 2024
##############################################################

##############################################################
# Library Intros                               
##############################################################

library(tidyverse)
library(readxl)
library(dplyr)

##############################################################
# Pull in Data                    
##############################################################

resilience_9 <- read_excel("raw_data/2024/Resilience 9th Survey Responses_.xlsx")

##############################################################
# Remove N/As and non 9th Graders                
##############################################################

resilience_9 <- resilience_9 %>% subset(!is.na(`CYRM-1`))
resilience_9 <- resilience_9 %>% subset(Grade=="9th")

##############################################################
# Examine students who missed attentional question
##############################################################

attentional <- resilience_9 %>% subset(Attentional!=1)

# Drop students who have answered the same answer for 14 or more questions

resilience_9 <- resilience_9 %>% subset(Campminder_ID!="17590098")
resilience_9 <- resilience_9 %>% subset(Campminder_ID!="17590100")
resilience_9 <- resilience_9 %>% subset(Campminder_ID!="17590053")
resilience_9 <- resilience_9 %>% subset(Campminder_ID!="17590015")

##############################################################
# Create Subscale Averages/ Total Averages
##############################################################

resilience_9$personal_sub <- rowMeans(resilience_9[,c(12,13,17,20,21,23,24,25,27)], na.rm=TRUE)
resilience_9$caregiver_sub <- rowMeans(resilience_9[,c(15,16,18,22,26)], na.rm=TRUE)
resilience_9$avg_cyrm <- rowMeans(resilience_9[,c(12,13,17,20,21,23,24,25,27,15,16,18,22,26)], na.rm=TRUE)

mean_personal <- mean(resilience_9$personal_sub,na.rm=TRUE)
mean_caregiver <- mean(resilience_9$caregiver_sub,na.rm=TRUE)
mean_cyrm_avg <- mean(resilience_9$avg_cyrm,na.rm=TRUE)

write_xlsx(resilience_9, 'data/2024/resilience_clean.xlsx')

##############################################################
# Plots by Subscale
##############################################################

ggplot() + 
  geom_point(data=resilience_9, aes(y=avg_cyrm, x=-1), position=position_jitter(0.175), alpha=.5, color="black") +
  geom_point(data=resilience_9, aes(y=personal_sub, x=0), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_point(data=resilience_9, aes(y=caregiver_sub, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= resilience_9, aes(y=mean_cyrm_avg, yend=mean_cyrm_avg, x=-1.4, xend=-.6), linewidth=1, color ="black") +
  geom_segment(data= resilience_9, aes(y=mean_personal, yend=mean_personal, x=-.4, xend=.4), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= resilience_9, aes(y=mean_caregiver, yend=mean_caregiver, x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="Resilience Subscale", y="Resilience Score", 
       title="SHP Average Resilience Scores by Subscale") +
  scale_x_continuous(breaks = -1:1,
                     labels = c("Composite","Personal Resilience", "Caregiver Resilience"))

##############################################################
# Plots by Question
##############################################################


#Personal Resilience Questions
ggplot() + 
  geom_point(data=resilience_9, aes(y=`CYRM-1`, x=-1), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  geom_point(data=resilience_9, aes(y=`CYRM-3`, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=resilience_9, aes(y=`CYRM-7`, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=resilience_9, aes(y=`CYRM-9`, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_point(data=resilience_9, aes(y=`CYRM-10`, x=3), position=position_jitter(0.175), alpha=.5, color="#F8BE3D") +
  geom_point(data=resilience_9, aes(y=`CYRM-12`, x=4), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  geom_point(data=resilience_9, aes(y=`CYRM-13`, x=5), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=resilience_9, aes(y=`CYRM-14`, x=6), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=resilience_9, aes(y=`CYRM-16`, x=7), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-1`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-1`,na.rm=TRUE), x=-1.4, xend=-.6), linewidth=1, color ="#139E02") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-3`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-3`,na.rm=TRUE), x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-7`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-7`,na.rm=TRUE), x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-9`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-9`,na.rm=TRUE), x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-10`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-10`,na.rm=TRUE), x=2.6, xend=3.4), linewidth=1, color ="#F8BE3D") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-12`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-12`,na.rm=TRUE), x=3.6, xend=4.4), linewidth=1, color ="#139E02") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-13`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-13`,na.rm=TRUE), x=4.6, xend=5.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-14`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-14`,na.rm=TRUE), x=5.6, xend=6.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-16`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-16`,na.rm=TRUE), x=6.6, xend=7.4), linewidth=1, color ="#0C9ED9") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="Personal Resilience Subscale", y="Resilience Score", 
       title="SHP Personal Resilience Scores by Question") +
  scale_x_continuous(breaks = -1:7,
                     labels = c("CYRM-1","CYRM-3", "CYRM-7", "CYRM-9", "CYRM-10","CYRM-12","CYRM-13","CYRM-14","CYRM-16"))


#Caregiver Resilience Questions
ggplot() + 
  geom_point(data=resilience_9, aes(y=`CYRM-4`, x=-1), position=position_jitter(0.175), alpha=.5, color="#139E02") +
  geom_point(data=resilience_9, aes(y=`CYRM-5`, x=0), position=position_jitter(0.175), alpha=.5, color="#8D029E") +
  geom_point(data=resilience_9, aes(y=`CYRM-8`, x=1), position=position_jitter(0.175), alpha=.5, color="#D9470C") +
  geom_point(data=resilience_9, aes(y=`CYRM-11`, x=2), position=position_jitter(0.175), alpha=.5, color="#0C9ED9") +
  geom_point(data=resilience_9, aes(y=`CYRM-15`, x=3), position=position_jitter(0.175), alpha=.5, color="#F8BE3D") +
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,6,.25), breaks=seq(0,5,.5)) +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-4`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-4`,na.rm=TRUE), x=-1.4, xend=-.6), linewidth=1, color ="#139E02") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-5`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-5`,na.rm=TRUE), x=-.4, xend=.4), linewidth=1, color ="#8D029E") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-8`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-8`,na.rm=TRUE), x=.6, xend=1.4), linewidth=1, color ="#D9470C") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-11`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-11`,na.rm=TRUE), x=1.6, xend=2.4), linewidth=1, color ="#0C9ED9") +
  geom_segment(data= resilience_9, aes(y=mean(resilience_9$`CYRM-15`,na.rm=TRUE), yend=mean(resilience_9$`CYRM-15`,na.rm=TRUE), x=2.6, xend=3.4), linewidth=1, color ="#F8BE3D") +
  theme_minimal()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  labs(x="Caregiver Resilience Subscale", y="Resilience Score", 
       title="SHP Caregiver Resilience Scores by Question") +
  scale_x_continuous(breaks = -1:3,
                     labels = c("CYRM-4","CYRM-5", "CYRM-8", "CYRM-11", "CYRM-15"))

##############################################################
# Plot Average Scale by Race
##############################################################

mean_CYRM_Eth <- resilience_9 %>% group_by(`VDOE_Race`) %>% summarise(mean_score=mean(avg_cyrm, na.rm=TRUE),obs=n())
mean_personal_Eth <- resilience_9 %>% group_by(`VDOE_Race`) %>% summarise(mean_score=mean(personal_sub, na.rm=TRUE),obs=n())
mean_caregiver_Eth <- resilience_9 %>% group_by(`VDOE_Race`) %>% summarise(mean_score=mean(caregiver_sub, na.rm=TRUE),obs=n())

                                                    
ggplot(data=resilience_9, aes(y=avg_cyrm)) + 
  geom_point(aes(x=0, color=`VDOE_Race`), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_CYRM_Eth, aes(yintercept = mean_score,col=`VDOE_Race`), linewidth=1) +
  labs(x="Race/Ethnicity", y="Resilience Score", 
       title="SHP Resilience Scores by Ethnicity")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,5,.25), breaks=seq(0,5,.5))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~`VDOE_Race`)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggplot(data=resilience_9, aes(y=personal_sub)) + 
  geom_point(aes(x=0, color=`VDOE_Race`), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_personal_Eth, aes(yintercept = mean_score,col=`VDOE_Race`), linewidth=1) +
  labs(x="Race/Ethnicity", y="Personal Resilience Subscore", 
       title="SHP Personal Resilience Subscores by Ethnicity")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,5,.25), breaks=seq(0,5,.5))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~`VDOE_Race`)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggplot(data=resilience_9, aes(y=caregiver_sub)) + 
  geom_point(aes(x=0, color=`VDOE_Race`), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_caregiver_Eth, aes(yintercept = mean_score,col=`VDOE_Race`), linewidth=1) +
  labs(x="Race/Ethnicity", y="Caregiver Resilience Subscore", 
       title="SHP Caregiver Resilience Subscores by Ethnicity")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(0,5.25), minor_breaks=seq(0,5,.25), breaks=seq(0,5,.5))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9", "#139E02", "#F8BE3D"))+
  facet_grid(~`VDOE_Race`)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

##############################################################
# Plot Average Scale by Gender
##############################################################

mean_CYRM_Gender <- resilience_9 %>% group_by(`Gender`) %>% summarise(mean_score=mean(avg_cyrm, na.rm=TRUE),obs=n())
mean_personal_Gender <- resilience_9 %>% group_by(`Gender`) %>% summarise(mean_score=mean(personal_sub, na.rm=TRUE),obs=n())
mean_caregiver_Gender <- resilience_9 %>% group_by(`Gender`) %>% summarise(mean_score=mean(caregiver_sub, na.rm=TRUE),obs=n())

ggplot(data=resilience_9, aes(y=avg_cyrm)) + 
  geom_point(aes(x=0, color=`Gender`), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_CYRM_Gender, aes(yintercept = mean_score,col=`Gender`), linewidth=1) +
  labs(x="Race/Ethnicity", y="Resilience Score", 
       title="SHP Resilience Scores by Gender")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,5,.25), breaks=seq(0,5,.5))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9"))+
  facet_grid(~`Gender`)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")


ggplot(data=resilience_9, aes(y=personal_sub)) + 
  geom_point(aes(x=0, color=`Gender`), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_personal_Gender, aes(yintercept = mean_score,col=`Gender`), linewidth=1) +
  labs(x="Race/Ethnicity", y="Personal Resilience Score", 
       title="SHP Personal Resilience Scores by Gender")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,5,.25), breaks=seq(0,5,.5))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9"))+
  facet_grid(~`Gender`)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")

ggplot(data=resilience_9, aes(y=caregiver_sub)) + 
  geom_point(aes(x=0, color=`Gender`), position=position_jitter(0.1), alpha=.7) +
  geom_hline(data= mean_caregiver_Gender, aes(yintercept = mean_score,col=`Gender`), linewidth=1) +
  labs(x="Race/Ethnicity", y="Caregiver Resilience Score", 
       title="SHP Caregiver Resilience Scores by Gender")+
  scale_x_continuous(limits=c(-.3,.3))+
  scale_y_continuous(limits=c(0,5), minor_breaks=seq(0,5,.25), breaks=seq(0,5,.5))+
  scale_color_manual(values=c("#8D029E", "#D9470C", "#0C9ED9"))+
  facet_grid(~`Gender`)+
  theme_minimal()+
  theme(axis.text.x=element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none")
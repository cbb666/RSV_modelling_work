# Library----
rm(list=ls())
# library(tidyverse)
library(readxl)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggsci)
library(forcats)
library(reshape)
library(patchwork)
library(ggpubr) 
library(metafor)
library(readr)
library(rgdal)
setwd("C:/系统文件 之外的/科研文件/RSV modelling/RSV_modelling_work/02. 数据代码")
# 0. data cleaned----
hospitalisation_MIQR <- read_excel("all data  BBcleaned_forboxplot.xlsx",sheet="hospitalisation",na = "NA") 
list1 <- hospitalisation_MIQR[c("ID","COUNTRY","income")]
mortality_MIQR <- read_excel("all data  BBcleaned_forboxplot.xlsx",sheet="mortality",na = "NA") 
list2 <- mortality_MIQR[c("ID","Country","income")]
names(list2)[2] <- "COUNTRY"
list <- rbind(list1,list2); list <- list %>% group_by(ID) %>% filter (! duplicated(ID) & !is.na(ID)) 
Ref.income <- write.csv(list,file="Ref.income.csv",row.names=F)
raw.data.descript <- read_excel("all data  BBcleaned_forboxplot.xlsx",sheet="basic_characteristics",na = "NA")

# 1. boxplots_median----
# 1.1 hospitalisation_ boxplots-----
# high-income
hospitalisation_MIQR <- read_excel("all data  BBcleaned_forboxplot.xlsx",sheet="hospitalisation",na = "NA") 
hospitalisation_MIQR <- hospitalisation_MIQR[complete.cases(hospitalisation_MIQR$ID),] 
hospitalisation_MIQR <- hospitalisation_MIQR[which(hospitalisation_MIQR$income=="High income"),]
hospitalisation_MIQR <- left_join(hospitalisation_MIQR,raw.data.descript,by="ID")
unique(hospitalisation_MIQR$ID)
unique(hospitalisation_MIQR[hospitalisation_MIQR$QA<4,]$ID)
# Add new variables
attach(hospitalisation_MIQR) # load
hospitalisation_MIQR$agegroup[AgeStart=="0" & AgeEnd=="11"]="<1"
hospitalisation_MIQR$agegroup[AgeStart=="1" & AgeEnd=="4"]="1-4"
hospitalisation_MIQR$agegroup[AgeStart=="5" & AgeEnd=="17"]="5-17"
hospitalisation_MIQR$agegroup[AgeStart=="5" & AgeEnd=="49"]="5-49"
hospitalisation_MIQR$agegroup[AgeStart=="18" & AgeEnd=="49"]="18-49"
hospitalisation_MIQR$agegroup[AgeStart=="50" & AgeEnd=="64"]="50-64"
hospitalisation_MIQR$agegroup[AgeStart=="65" & AgeEnd=="74"]="65-74"
hospitalisation_MIQR$agegroup[AgeStart=="75" & AgeEnd=="125"]="≥75"
hospitalisation_MIQR$agegroup[AgeStart=="65" & AgeEnd=="125"]="≥65"
hospitalisation_MIQR$agegroup[AgeStart=="0" & AgeEnd=="125"]="All"
# str(hospitalisation_MIQR)
hospitalisation_MIQR$Mean=as.numeric(hospitalisation_MIQR$Mean)
hospitalisation_MIQR$agegroup=as.factor(hospitalisation_MIQR$agegroup)
detach(hospitalisation_MIQR)
hospitalisation_MIQR <- dplyr::filter(hospitalisation_MIQR, !grepl("All|5-49",agegroup)) # not reported
# hospitalisation_MIQR$Study_period <- paste(hospitalisation_MIQR$StartDate, hospitalisation_MIQR$EndDate, sep = "-")
# hospitalisation_MIQR<- hospitalisation_MIQR[order(hospitalisation_MIQR$Study, decreasing = FALSE),]
hospitalisation_MIQR <- left_join(hospitalisation_MIQR, 
                                  hospitalisation_MIQR %>% group_by(Casedefinition,agegroup) %>% dplyr::summarize(num=n()))
# cyls = c('ARI hospitalisation','ALRI hospitalisation','P&I hospitalisation',"CRD hospitalisation", "C&R hospitalisation")
# names(cyls) = c('ARI hospitalisation','ALRI hospitalisation','P&I hospitalisation',"CRD hospitalisation", "C&R hospitalisation")
# for figures
hosp_boxplot <- hospitalisation_MIQR %>%
  mutate(agegroup = fct_relevel(agegroup, 
                                "<1", "1-4", "5-17", "18-49","50-64", "65-74", "≥75","≥65")) %>%
  mutate(Casedefinition = factor(Casedefinition, 
               levels =c('ARI hospitalisation','ALRI hospitalisation','P&I hospitalisation',"CRD hospitalisation", "C&R hospitalisation"))) %>% 
  ggplot( aes(x=agegroup,y=Mean)) + 
  geom_boxplot(width=0.25,alpha=0.35,aes(colour=income,fill=income)) +  #, width=0.5,alpha=0.2 , color="black",size=0.3
  scale_y_continuous(name="Rate per 100,000",trans = "log",
                     breaks = c(0.1,1,10, 100,1000, 10000),  # c(1,5,25, 125,625, 625*5,625*25)
                     limits = c(min(hospitalisation_MIQR$Mean)/3,
                                max(hospitalisation_MIQR$Mean)*1.4), labels = scales::comma) + # 0.33 #因为要进行log转换，变量中不能包含负数和零,可以连续性校正
  scale_x_discrete(name = "Age group") +
  geom_vline(xintercept="≥65",lty="dashed",col="#8F8A8E",lwd=0.6)  +
  geom_text(y =-1.4, aes(label =num),size=2,colour="blue4") +  #pow(10,-0.5) pow(10,-2)
  scale_colour_lancet()+
  scale_fill_lancet() +
  theme_bw()+
  theme(axis.title=element_text(size=12),plot.title = element_text(face="bold",size=14,hjust = 0.5),legend.position = "none") +
  facet_wrap(~Casedefinition,nrow=5)# , labeller = as_labeller(cyls)
ggsave(
  plot= hosp_boxplot,
  file="Figures/RSV-associated hospitalisation boxplot revised_y轴.pdf", width = 10, height = 12) 
# for tables
hospitalisation_MIQR_fortab <- hospitalisation_MIQR %>% group_by(income,agegroup,Casedefinition) %>% 
  dplyr::summarise(hosp_med=median(Mean),hosp_q1=quantile(Mean,0.25),hosp_q3=quantile(Mean,0.75),
                   hosp_max=max(Mean),hosp_min=min(Mean),no=n(),source="main")
write.csv(hospitalisation_MIQR_fortab,file = "hospitalisation_MIQR_fortab.csv",  row.names = FALSE)

# despription
hospitalisation_MIQR[hospitalisation_MIQR$]



# 1.1.1 QA----
hospitalisation_MIQR_QA_fortab <- hospitalisation_MIQR[hospitalisation_MIQR$QA>3,] %>% group_by(income,agegroup,Casedefinition) %>% 
  dplyr::summarise(hosp_med=median(Mean),hosp_q1=quantile(Mean,0.25),hosp_q3=quantile(Mean,0.75),
                   hosp_max=max(Mean),hosp_min=min(Mean),no=n(),source="QA")
hospitalisation_MIQR_all_fortab <- rbind(hospitalisation_MIQR_fortab,hospitalisation_MIQR_QA_fortab)
hospitalisation_MIQR_all_fortab$Range <- with(hospitalisation_MIQR_all_fortab, paste("[",
                                                                          format(round(hosp_min,1), nsmall=1),
                                                                          "-",
                                                                          format(round(hosp_max,1), nsmall=1),
                                                                          "]", sep = ""))
hospitalisation_MIQR_all_fortab$p25top75 <- with(hospitalisation_MIQR_all_fortab, paste("[",
                                                                                     format(round(hosp_q1,1), nsmall=1),
                                                                                     "-",
                                                                                     format(round(hosp_q3,1), nsmall=1),
                                                                                     "]", sep = ""))
hospitalisation_MIQR_all_fortab <- hospitalisation_MIQR_all_fortab[c("source","income", "agegroup", "Casedefinition","hosp_med",
                                                                     "Range", "p25top75","no")]
# unique(hospitalisation_MIQR_all_fortab$agegroup)
hospitalisation_MIQR_all_fortab$Casedefinition <- factor(hospitalisation_MIQR_all_fortab$Casedefinition,
                     c("ALRI hospitalisation","P&I hospitalisation","C&R hospitalisation","ARI hospitalisation", "CRD hospitalisation"))
hospitalisation_MIQR_all_fortab$agegroup <- factor(hospitalisation_MIQR_all_fortab$agegroup,
                     c("<1","1-4","5-17","18-49", "50-64","65-74","≥75","≥65" ))
hospitalisation_MIQR_all_fortab <- hospitalisation_MIQR_all_fortab [with(hospitalisation_MIQR_all_fortab,
                                                                         order(source,Casedefinition,agegroup)),]
# hospitalisation_MIQR_all_fortab$agegroup <- as.character(hospitalisation_MIQR_all_fortab$agegroup)
hospitalisation_MIQR_all_fortab <- ( hospitalisation_MIQR_all_fortab %>%  
                                   pivot_wider(names_from = c(Casedefinition), values_from = c(hosp_med,Range,p25top75,no)) )  
  # pivot_longer(!c(AGEGR,Group), names_to = c("reporting", ".value"), # "reporting", ".value","Year"   names_to表示以_分隔的列 前者变成 一列 后者变成对应的数值
  #              names_pattern = "(.*)_(.*)") 
hosp_MIQR_ALRI_fortab <- hospitalisation_MIQR_all_fortab %>% select(matches("ALRI") | c(source))
hosp_MIQR_PI_fortab <- hospitalisation_MIQR_all_fortab %>% select(matches("P&I") | c(source))
hosp_MIQR_CR_fortab <- hospitalisation_MIQR_all_fortab %>% select(matches("C&R") | c(source))
hosp_MIQR_ARI_fortab <- hospitalisation_MIQR_all_fortab %>% select(matches("ARI") | c(source))
hospitalisation_MIQR_all_fortab <- left_join(hosp_MIQR_ALRI_fortab,left_join(hosp_MIQR_PI_fortab,left_join(hosp_MIQR_CR_fortab,hosp_MIQR_ARI_fortab)))
# write.csv(hospitalisation_MIQR_all_fortab,file = "hospitalisation_MIQR_all_fortab.csv",  row.names = FALSE)  
write.table(hospitalisation_MIQR_all_fortab,"hospitalisation_MIQR_all_fortab.csv",row.names=FALSE,col.names=TRUE,sep=",",fileEncoding = "UTF-8")

  
# 1.2 mortality boxplots---------
# high income + upper-middle income
mortality_MIQR <- read_excel("all data  BBcleaned_forboxplot.xlsx",sheet="mortality",na = "NA") 
mortality_MIQR  <- mortality_MIQR[complete.cases(mortality_MIQR$ID),]
mortality_MIQR <- left_join(mortality_MIQR,raw.data.descript,by="ID")
unique(mortality_MIQR$ID)
unique(mortality_MIQR[mortality_MIQR$QA<4,]$ID)
# Add new variables
attach(mortality_MIQR)
mortality_MIQR$agegroup[AgeStart=="0" & AgeEnd=="11"]="<1"
mortality_MIQR$agegroup[AgeStart=="1" & AgeEnd=="4"]="1-4"
mortality_MIQR$agegroup[AgeStart=="5" & AgeEnd=="17"]="5-17"
mortality_MIQR$agegroup[AgeStart=="5" & AgeEnd=="49"]="5-49" 
mortality_MIQR$agegroup[AgeStart=="0" & AgeEnd=="4"]="<5"
mortality_MIQR$agegroup[AgeStart=="5" & AgeEnd=="125"]="≥5"  
mortality_MIQR$agegroup[AgeStart=="0" & AgeEnd=="64"]="<65"  
mortality_MIQR$agegroup[AgeStart=="18" & AgeEnd=="49"]="18-49"
mortality_MIQR$agegroup[AgeStart=="50" & AgeEnd=="64"]="50-64"
mortality_MIQR$agegroup[AgeStart=="65" & AgeEnd=="74"]="65-74"
mortality_MIQR$agegroup[AgeStart=="75" & AgeEnd=="125"]="≥75"
mortality_MIQR$agegroup[AgeStart=="65" & AgeEnd=="125"]="≥65"
mortality_MIQR$agegroup[AgeStart=="0" & AgeEnd=="125"]="All" 
mortality_MIQR $Mean=as.numeric(mortality_MIQR $Mean)
mortality_MIQR $agegroup=as.factor(mortality_MIQR $agegroup)
mortality_MIQR $income=as.factor(mortality_MIQR $income)
detach(mortality_MIQR )
mortality_MIQR <- dplyr::filter(mortality_MIQR, !grepl("All|5-49|≥5|<65", agegroup)) #  not reported
mortality_MIQR  <- left_join(mortality_MIQR,
                             mortality_MIQR  %>% group_by(Casedefinition,agegroup,income) %>% dplyr::summarize(num=n()))
# cyls = c('All-cause mortality','ARI mortality','P&I mortality',"C&R mortality")
# names(cyls) = c('All-cause mortality','ARI mortality','P&I mortality',"C&R mortality")
# for figs
mort_boxplot<- mortality_MIQR  %>%
  mutate(agegroup = fct_relevel(agegroup, 
                                "<1", "1-4", "5-17", "18-49", "50-64", "65-74","≥75", "<5","≥65")) %>%
  mutate(Casedefinition = factor(Casedefinition, 
                                 levels =c('All-cause mortality','ARI mortality','P&I mortality',"C&R mortality"))) %>% 
  ggplot( aes(x=agegroup,y=Mean)) + 
  geom_boxplot(width=0.4,alpha=0.35,aes(colour=income, fill=income)) +  #, alpha=0.2  width=0.15, color="black",size=0.3
  geom_text(y =-5.3, aes(label = num),size=2,colour="blue4") +  #pow(10,-0.5) pow(10,-2),position = position_dodge2(width = 2)
  scale_y_continuous(name="Rate per 100,000",trans = "log",
                     breaks = c(0.01,0.1,1,10,100,1000),  #0.015,0.06,0.25,1,4,16,64,256,256*4
                     limits = c(min(mortality_MIQR $Mean)/3,
                                max(mortality_MIQR $Mean)*1.4), labels = scales::comma) + # labels = comma 即以1,10,100这样表示 0.33 #因为要进行log转换，变量中不能包含负数和零,可以连续性校正
  scale_x_discrete(name = "Age group") +
  geom_vline(xintercept="<5",lty="dashed",col="#8F8A8E",lwd=0.6)  +
  scale_colour_lancet()+
  scale_fill_lancet() +
  theme_bw()+ guides(colour="none") +
  theme(axis.title=element_text(size=12),plot.title = element_text(face="bold",size=14,hjust = 0.5)) +
  labs(fill = "Income") +
  facet_wrap(~Casedefinition,nrow=4) + # , labeller = as_labeller(cyls)
  theme(legend.position="bottom",legend.box = "horizontal")
ggsave(
  plot= mort_boxplot,
  file="Figures/RSV-associated motrality by income_boxplot revised2_y轴.pdf", width = 10, height = 9.6
)
# for tables
mor_MIQR_fortab <- mortality_MIQR %>% group_by(income,agegroup,Casedefinition) %>% 
  dplyr::summarise(mor_med=median(Mean),mor_q1=quantile(Mean,0.25),mor_q3=quantile(Mean,0.75),
                   mor_max=max(Mean),mor_min=min(Mean),no=n(),source="main")
write.csv(mor_MIQR_fortab,file = "mor_MIQR_fortab.csv",  row.names = FALSE)

# 1.2.1 QA----
mor_MIQR_QA_fortab <- mortality_MIQR[mortality_MIQR$QA>3,] %>% group_by(income,agegroup,Casedefinition) %>% 
  dplyr::summarise(mor_med=median(Mean),mor_q1=quantile(Mean,0.25),mor_q3=quantile(Mean,0.75),
                   mor_max=max(Mean),mor_min=min(Mean),no=n(),source="QA")
mor_MIQR_all_fortab <- rbind(mor_MIQR_fortab,mor_MIQR_QA_fortab)
mor_MIQR_all_fortab$Range <- with(mor_MIQR_all_fortab, paste("[",
                                                                                     format(round(mor_min,1), nsmall=1),
                                                                                     "-",
                                                                                     format(round(mor_max,1), nsmall=1),
                                                                                     "]", sep = ""))
mor_MIQR_all_fortab$p25top75 <- with(mor_MIQR_all_fortab, paste("[",
                                                                                        format(round(mor_q1,1), nsmall=1),
                                                                                        "-",
                                                                                        format(round(mor_q3,1), nsmall=1),
                                                                                        "]", sep = ""))
mor_MIQR_all_fortab <- mor_MIQR_all_fortab[c("source","income", "agegroup", "Casedefinition","mor_med",
                                                                     "Range", "p25top75","no")]
# unique(mor_MIQR_all_fortab$agegroup)
mor_MIQR_all_fortab$Casedefinition <- factor(mor_MIQR_all_fortab$Casedefinition,
                                                         c("All-cause mortality","P&I mortality","C&R mortality","ARI mortality"))
mor_MIQR_all_fortab$agegroup <- factor(mor_MIQR_all_fortab$agegroup,
                                                   c("<1","1-4","5-17","18-49", "50-64","65-74","≥75","<5","≥5","≥65"))
mor_MIQR_all_fortab <- mor_MIQR_all_fortab [with(mor_MIQR_all_fortab,
                                                                         order(source,income,Casedefinition,agegroup)),]
# mor_MIQR_all_fortab$agegroup <- as.character(mor_MIQR_all_fortab$agegroup)
mor_MIQR_all_fortab <- ( mor_MIQR_all_fortab %>%  
                                       pivot_wider(names_from = c(Casedefinition), values_from = c(mor_med,Range,p25top75,no)) )  
# pivot_longer(!c(AGEGR,Group), names_to = c("reporting", ".value"), # "reporting", ".value","Year"   names_to表示以_分隔的列 前者变成 一列 后者变成对应的数值
#              names_pattern = "(.*)_(.*)") 
mor_MIQR_allc_fortab <- mor_MIQR_all_fortab %>% select(matches("All-cause") | c(source))
mor_MIQR_PI_fortab <- mor_MIQR_all_fortab %>% select(matches("P&I") | c(source))
mor_MIQR_CR_fortab <- mor_MIQR_all_fortab %>% select(matches("C&R") | c(source))
mor_MIQR_ARI_fortab <- mor_MIQR_all_fortab %>% select(matches("ARI") | c(source))
mor_MIQR_all_fortab <- left_join(mor_MIQR_allc_fortab,left_join(mor_MIQR_PI_fortab,left_join(mor_MIQR_CR_fortab,mor_MIQR_ARI_fortab)))
# write.csv(mor_MIQR_all_fortab,file = "mor_MIQR_all_fortab.csv",  row.names = FALSE)  
write.table(mor_MIQR_all_fortab,"mor_MIQR_all_fortab.csv",row.names=FALSE,col.names=TRUE,sep=",",fileEncoding = "UTF-8")


# 1.3 Comparison of modelled and recorded estimates----
#by age group
mode_obse_MIQR <- read_excel("all data  BBcleaned_forboxplot.xlsx",sheet="obse+mode analysis",na = "NA") 
mode_obse_MIQR <- mode_obse_MIQR[complete.cases(mode_obse_MIQR$ID),] 
mode_obse_MIQR <- left_join(mode_obse_MIQR,raw.data.descript,by="ID")
attach(mode_obse_MIQR)
mode_obse_MIQR$agegroup[AgeStart=="0" & AgeEnd=="11"]="<1"
mode_obse_MIQR$agegroup[AgeStart=="1" & AgeEnd=="4"]="1-4"
mode_obse_MIQR$agegroup[AgeStart=="5" & AgeEnd=="17"]="5-17"
mode_obse_MIQR$agegroup[AgeStart=="18" & AgeEnd=="49"]="18-49"
mode_obse_MIQR$agegroup[AgeStart=="50" & AgeEnd=="64"]="50-64"
mode_obse_MIQR$agegroup[AgeStart=="65" & AgeEnd=="125"]="≥65"
mode_obse_MIQR$agegroup[AgeStart=="0" & AgeEnd=="125"]="All"
mode_obse_MIQR$agegroup <- as.factor(mode_obse_MIQR$agegroup)
detach(mode_obse_MIQR)
mode_obse_MIQR <- dplyr::filter(mode_obse_MIQR, !grepl("All",agegroup)) 
mode_obse_MIQR <- left_join(mode_obse_MIQR,
                            mode_obse_MIQR %>% group_by(agegroup) %>% dplyr::summarize(num=n()))

mode_obse_boxplot <- mode_obse_MIQR %>%
  mutate(agegroup = fct_relevel(agegroup, 
                                "<1", "1-4", "5-17","18-49","50-64","≥65")) %>%
  ggplot( aes(x=agegroup,y=fold)) + 
  geom_boxplot(width=0.25,alpha=0.35,aes(colour="blue4",fill="blue4")) +  #, ,size=0.4"#ff6961"alpha=0.2 width=0.15, color="black",size=0.3,  aes(fill=income
  geom_text(y =-5.4, aes(label = num),size=3,colour="blue4") +  #pow(10,-0.5) pow(10,-2),position = position_dodge2(width = 2)
  # geom_hline(yintercept=1,lty="dashed",col="#8F8A8E",lwd=0.6)  +
  # geom_hline(yintercept=0.1,lty="dashed",col="#8F8A8E",lwd=0.6)  +
  # geom_hline(yintercept=0.7,lty="dashed",col="#8F8A8E",lwd=0.6)  +
  scale_y_continuous(name="Recorded/Modelled",trans = "log",
                     breaks = c(0.01,0.05,0.1,0.25,0.7,1,6.25),
                     limits = c(0.005,
                                max(mode_obse_MIQR$fold)*1.1)) + 
  scale_x_discrete(name = "Age group") +
  scale_fill_lancet()+
  scale_color_lancet()+
  theme_bw()+
  theme(axis.title=element_text(size=12)) +
  theme(legend.position ="none")  
ggsave(
  plot= mode_obse_boxplot,
  file="Figures/mode_obse boxplot by agegroup revised2.pdf", width = 10, height = 4
)
# 1.3.1 QA----
mode_obse_MIQR_QA <- mode_obse_MIQR[mode_obse_MIQR$QA>3,] # 全是中高质量的证据
# mode_obse_boxplot <- mode_obse_MIQR_QA %>%
#   mutate(agegroup = fct_relevel(agegroup, 
#                                 "<1", "1-4", "5-17","18-49","50-64","≥65")) %>%
#   ggplot( aes(x=agegroup,y=fold)) + 
#   geom_boxplot(width=0.25,alpha=0.35,aes(colour="blue4",fill="blue4")) +  #, ,size=0.4"#ff6961"alpha=0.2 width=0.15, color="black",size=0.3,  aes(fill=income
#   geom_text(y =-5.4, aes(label = num),size=3,colour="blue4") +  #pow(10,-0.5) pow(10,-2),position = position_dodge2(width = 2)
#   geom_hline(yintercept=1,lty="dashed",col="#8F8A8E",lwd=0.6)  +
#   geom_hline(yintercept=0.1,lty="dashed",col="#8F8A8E",lwd=0.6)  +
#   geom_hline(yintercept=0.7,lty="dashed",col="#8F8A8E",lwd=0.6)  +
#   scale_y_continuous(name="Recorded/Modelled",trans = "log",
#                      breaks = c(0.01,0.05,0.1,0.25,0.7,1,6.25),
#                      limits = c(0.005,
#                                 max(mode_obse_MIQR_QA$fold)*1.1)) + 
#   scale_x_discrete(name = "Age group") +
#   scale_fill_lancet()+
#   scale_color_lancet()+
#   theme_bw()+
#   theme(axis.title=element_text(size=12)) +
#   theme(legend.position ="none")  
# ggsave(
#   plot= mode_obse_boxplot,
#   file="Figures/mode_obse boxplot by agegroup_QA.pdf", width = 10, height = 4
# )

# 2. meta analysis----
# 2.1 hospitalisation----
# 2.1.1 ARI hospitalisation----
ARI_hospitalisation_meta <- read_excel("all data  BBcleaned_formeta.xlsx",sheet="ARI hospitalisation",na = "NA")
ref.income <- read.csv("Ref.income.csv",na = "NA")
ARI_hospitalisation_meta <- left_join(ARI_hospitalisation_meta,ref.income,by="ID")
ARI_hospitalisation_meta <- ARI_hospitalisation_meta[complete.cases(ARI_hospitalisation_meta$ID),]
ARI_hospitalisation_meta <- ARI_hospitalisation_meta[ARI_hospitalisation_meta$lower_CI_mean>0,]
ARI_hospitalisation_meta$Mean <- as.numeric(ARI_hospitalisation_meta$Mean)
ARI_hospitalisation_meta$yi <- log(ARI_hospitalisation_meta$Mean)
ARI_hospitalisation_meta$sei <- (log(ARI_hospitalisation_meta$upper_CI_mean) - log(ARI_hospitalisation_meta$lower_CI_mean))/3.92
sapply(ARI_hospitalisation_meta,class) # check variable types
ARI_hospitalisation_meta$Study_period <- paste(ARI_hospitalisation_meta$StartDate, ARI_hospitalisation_meta$EndDate, sep = "-")
# ARI_hospitalisation_meta <- ARI_hospitalisation_meta[order(ARI_hospitalisation_meta$Study, decreasing = FALSE),] 
ARI_hospitalisation_meta$Agegr <- with(ARI_hospitalisation_meta,paste(AgeStart,AgeEnd,sep="-"))
# unique(ARI_hospitalisation_meta$income)
by(ARI_hospitalisation_meta[ARI_hospitalisation_meta$Agegr=="0-11",],
   ARI_hospitalisation_meta[ARI_hospitalisation_meta$Agegr=="0-11",c("Agegr","income")],
   genHospRate,choice="0-11");graphics.off()
# genHospRate(ARI_hospitalisation_meta[ARI_hospitalisation_meta$Agegr=="0-11",],choice="0-11");graphics.off()
by(ARI_hospitalisation_meta[ARI_hospitalisation_meta$Agegr!="0-11",],
  ARI_hospitalisation_meta[ARI_hospitalisation_meta$Agegr!="0-11",c("Agegr","income")],
  genHospRate,choice="others");graphics.off()

# 2.1.2 ALRI hospitalisation----
ALRI_hospitalisation_meta <- read_excel("all data  BBcleaned_formeta.xlsx",sheet="ALRI hospitalisation",na = "NA") 
ALRI_hospitalisation_meta <- left_join(ALRI_hospitalisation_meta,ref.income,by="ID")
ALRI_hospitalisation_meta <- ALRI_hospitalisation_meta[complete.cases(ALRI_hospitalisation_meta$ID),]
ALRI_hospitalisation_meta <- ALRI_hospitalisation_meta[ALRI_hospitalisation_meta$lower_CI_mean>0,]
ALRI_hospitalisation_meta$Mean <- as.numeric(ALRI_hospitalisation_meta$Mean)
ALRI_hospitalisation_meta$yi <- log(ALRI_hospitalisation_meta$Mean)
ALRI_hospitalisation_meta$sei <- (log(ALRI_hospitalisation_meta$upper_CI_mean) - log(ALRI_hospitalisation_meta$lower_CI_mean))/3.92
sapply(ALRI_hospitalisation_meta,class) # check variable types
ALRI_hospitalisation_meta$Study_period <- paste(ALRI_hospitalisation_meta$StartDate, ALRI_hospitalisation_meta$EndDate, sep = "-")
# ALRI_hospitalisation_meta <- ALRI_hospitalisation_meta[order(ALRI_hospitalisation_meta$Study, decreasing = FALSE),] 
ALRI_hospitalisation_meta$Agegr <- with(ALRI_hospitalisation_meta,paste(AgeStart,AgeEnd,sep="-"))

by(ALRI_hospitalisation_meta[ALRI_hospitalisation_meta$Agegr=="0-11",],
   ALRI_hospitalisation_meta[ALRI_hospitalisation_meta$Agegr=="0-11",c("Agegr","income")],
   genHospRate,choice="0-11");graphics.off()
# genHospRate(ALRI_hospitalisation_meta[ALRI_hospitalisation_meta$Agegr=="0-11",],choice="0-11");graphics.off()
by(ALRI_hospitalisation_meta[ALRI_hospitalisation_meta$Agegr!="0-11",],
   ALRI_hospitalisation_meta[ALRI_hospitalisation_meta$Agegr!="0-11",c("Agegr","income")],
   genHospRate,choice="others");graphics.off()

# 2.1.3 P&I hospitalisation----
PI_hospitalisation <- read_excel("all data  BBcleaned_formeta.xlsx",sheet="P&I hospitalisation",na = "NA") 
PI_hospitalisation <- left_join(PI_hospitalisation,ref.income,by="ID")
PI_hospitalisation <- PI_hospitalisation[complete.cases(PI_hospitalisation$ID),]
PI_hospitalisation <- PI_hospitalisation[PI_hospitalisation$lower_CI_mean>0,]
PI_hospitalisation$Mean <- as.numeric(PI_hospitalisation$Mean)
PI_hospitalisation$yi <- log(PI_hospitalisation$Mean)
PI_hospitalisation$sei <- (log(PI_hospitalisation$upper_CI_mean) - log(PI_hospitalisation$lower_CI_mean))/3.92
sapply(PI_hospitalisation,class) # check variable types
PI_hospitalisation$Study_period <- paste(PI_hospitalisation$StartDate, PI_hospitalisation$EndDate, sep = "-")
# PI_hospitalisation <- PI_hospitalisation[order(PI_hospitalisation$Study, decreasing = FALSE),] 
PI_hospitalisation$Agegr <- with(PI_hospitalisation,paste(AgeStart,AgeEnd,sep="-"))

by(PI_hospitalisation[PI_hospitalisation$Agegr=="0-11",],
   PI_hospitalisation[PI_hospitalisation$Agegr=="0-11",c("Agegr","income")],
   genHospRate,choice="others");graphics.off()
# genHospRate(PI_hospitalisation[PI_hospitalisation$Agegr=="0-11",],choice="0-11");graphics.off()
by(PI_hospitalisation[PI_hospitalisation$Agegr!="0-11",],
   PI_hospitalisation[PI_hospitalisation$Agegr!="0-11",c("Agegr","income")],
   genHospRate,choice="others");graphics.off()

# 2.1.4 C&R hospitalisation----
CR_hospitalisation <- read_excel("all data  BBcleaned_formeta.xlsx",sheet="C&R hospitalisation+CRD住院",na = "NA") 
CR_hospitalisation <- left_join(CR_hospitalisation,ref.income,by="ID")
CR_hospitalisation <- CR_hospitalisation[complete.cases(CR_hospitalisation$ID),]
CR_hospitalisation <- CR_hospitalisation[CR_hospitalisation$lower_CI_mean>0,]
CR_hospitalisation$Mean <- as.numeric(CR_hospitalisation$Mean)
CR_hospitalisation$yi <- log(CR_hospitalisation$Mean)
CR_hospitalisation$sei <- (log(CR_hospitalisation$upper_CI_mean) - log(CR_hospitalisation$lower_CI_mean))/3.92
sapply(CR_hospitalisation,class) # check variable types
CR_hospitalisation$Study_period <- paste(CR_hospitalisation$StartDate, CR_hospitalisation$EndDate, sep = "-")
# CR_hospitalisation <- CR_hospitalisation[order(CR_hospitalisation$Study, decreasing = FALSE),] 
CR_hospitalisation$Agegr <- with(CR_hospitalisation,paste(AgeStart,AgeEnd,sep="-"))

by(CR_hospitalisation[CR_hospitalisation$Agegr=="0-11",],
   CR_hospitalisation[CR_hospitalisation$Agegr=="0-11",c("Agegr","income")],
   genHospRate,choice="others");graphics.off()
# genHospRate(CR_hospitalisation[CR_hospitalisation$Agegr=="0-11",],choice="0-11");graphics.off()
by(CR_hospitalisation[CR_hospitalisation$Agegr!="0-11",],
   CR_hospitalisation[CR_hospitalisation$Agegr!="0-11",c("Agegr","income")],
   genHospRate,choice="others");graphics.off()

# 2.2 mortality----
# 2.2.1 all-cause mortality----
all_cause_mortality_meta <- read_excel("all data  BBcleaned_formeta.xlsx",sheet="All-cause mortality",na = "NA")
all_cause_mortality_meta <- left_join(all_cause_mortality_meta,ref.income,by="ID")
all_cause_mortality_meta <- all_cause_mortality_meta[complete.cases(all_cause_mortality_meta$ID),]
all_cause_mortality_meta <- all_cause_mortality_meta[all_cause_mortality_meta$lower_CI_mean>0,]
all_cause_mortality_meta$Mean <- as.numeric(all_cause_mortality_meta$Mean)
all_cause_mortality_meta$yi <- log(all_cause_mortality_meta$Mean)
all_cause_mortality_meta$sei <- (log(all_cause_mortality_meta$upper_CI_mean) - log(all_cause_mortality_meta$lower_CI_mean))/3.92
sapply(all_cause_mortality_meta,class) # check variable types
all_cause_mortality_meta$Study_period <- paste(all_cause_mortality_meta$StartDate, all_cause_mortality_meta$EndDate, sep = "-")
# all_cause_mortality_meta <- all_cause_mortality_meta[order(all_cause_mortality_meta$Study, decreasing = FALSE),] 
all_cause_mortality_meta$Agegr <- with(all_cause_mortality_meta,paste(AgeStart,AgeEnd,sep="-"))
# unique(all_cause_mortality_meta$Agegr)
by(all_cause_mortality_meta[all_cause_mortality_meta$Agegr %in% c("65-125","75-125"),],
   all_cause_mortality_meta[all_cause_mortality_meta$Agegr %in% c("65-125","75-125"),c("Agegr","income")],
   genMorRate,choice %in% c("65-125","75-125"));graphics.off()
# genHospRate(all_cause_mortality_meta[all_cause_mortality_meta$Agegr=="0-11",],choice="0-11");graphics.off()
by(all_cause_mortality_meta[!all_cause_mortality_meta$Agegr %in% c("65-125","75-125"),],
   all_cause_mortality_meta[!all_cause_mortality_meta$Agegr %in% c("65-125","75-125"),c("Agegr","income")],
   genMorRate,choice="others");graphics.off()

# 2.2.2 ARI mortality----
ARI_mortality_meta <- read_excel("all data  BBcleaned_formeta.xlsx",sheet="ARI mortality",na = "NA")
ARI_mortality_meta <- left_join(ARI_mortality_meta ,ref.income,by="ID")
ARI_mortality_meta <- ARI_mortality_meta [complete.cases(ARI_mortality_meta $ID),]
ARI_mortality_meta <- ARI_mortality_meta [ARI_mortality_meta $lower_CI_mean>0,]
ARI_mortality_meta $Mean <- as.numeric(ARI_mortality_meta $Mean)
ARI_mortality_meta $yi <- log(ARI_mortality_meta $Mean)
ARI_mortality_meta $sei <- (log(ARI_mortality_meta $upper_CI_mean) - log(ARI_mortality_meta $lower_CI_mean))/3.92
sapply(ARI_mortality_meta ,class) # check variable types
ARI_mortality_meta $Study_period <- paste(ARI_mortality_meta $StartDate, ARI_mortality_meta $EndDate, sep = "-")
# ARI_mortality_meta <- ARI_mortality_meta [order(ARI_mortality_meta $Study, decreasing = FALSE),] 
ARI_mortality_meta $Agegr <- with(ARI_mortality_meta ,paste(AgeStart,AgeEnd,sep="-"))
# unique(ARI_mortality_meta $Agegr)
by(ARI_mortality_meta [ARI_mortality_meta $Agegr %in% c("65-125","75-125"),],
   ARI_mortality_meta [ARI_mortality_meta $Agegr %in% c("65-125","75-125"),c("Agegr","income")],
   genMorRate,choice %in% c("65-125","75-125"));graphics.off()
# genHospRate(ARI_mortality_meta [ARI_mortality_meta $Agegr=="0-11",],choice="0-11");graphics.off()
by(ARI_mortality_meta [!ARI_mortality_meta $Agegr %in% c("65-125","75-125"),],
   ARI_mortality_meta [!ARI_mortality_meta $Agegr %in% c("65-125","75-125"),c("Agegr","income")],
   genMorRate,choice="others");graphics.off()

# 2.2.3 P&I mortality----
PI_mortality_meta <- read_excel("all data  BBcleaned_formeta.xlsx",sheet="P&I mortality",na = "NA")
PI_mortality_meta <- left_join(PI_mortality_meta ,ref.income,by="ID")
PI_mortality_meta <- PI_mortality_meta [complete.cases(PI_mortality_meta $ID),]
PI_mortality_meta <- PI_mortality_meta [PI_mortality_meta $lower_CI_mean>0,]
PI_mortality_meta $Mean <- as.numeric(PI_mortality_meta $Mean)
PI_mortality_meta $yi <- log(PI_mortality_meta $Mean)
PI_mortality_meta $sei <- (log(PI_mortality_meta $upper_CI_mean) - log(PI_mortality_meta $lower_CI_mean))/3.92
sapply(PI_mortality_meta ,class) # check variable types
PI_mortality_meta $Study_period <- paste(PI_mortality_meta $StartDate, PI_mortality_meta $EndDate, sep = "-")
# PI_mortality_meta <- PI_mortality_meta [order(PI_mortality_meta $Study, decreasing = FALSE),] 
PI_mortality_meta $Agegr <- with(PI_mortality_meta ,paste(AgeStart,AgeEnd,sep="-"))
# unique(PI_mortality_meta $Agegr)
by(PI_mortality_meta [PI_mortality_meta $Agegr %in% c("65-125","75-125"),],
   PI_mortality_meta [PI_mortality_meta $Agegr %in% c("65-125","75-125"),c("Agegr","income")],
   genMorRate,choice %in% c("65-125","75-125"));graphics.off()
# genHospRate(PI_mortality_meta [PI_mortality_meta $Agegr=="0-11",],choice="0-11");graphics.off()
by(PI_mortality_meta [!PI_mortality_meta $Agegr %in% c("65-125","75-125"),],
   PI_mortality_meta [!PI_mortality_meta $Agegr %in% c("65-125","75-125"),c("Agegr","income")],
   genMorRate,choice="others");graphics.off()

# 3.1 Comparison of meta and median----
meta_median <- read_excel("meta_median.xlsx",na = "NA") 
meta_median <- meta_median [complete.cases(meta_median$meta_estimate),] 
meta_median$meta_estimate_log <- log(meta_median$meta_estimate)
meta_median$median_estimate_log <- log(meta_median$median)
meta_median_hosp <- dplyr::filter(meta_median, grepl("ALRI hospitalisation|ARI hospitalisation|C&R hospitalisation|P&I hospitalisation|Chronic respiratory disease hospitalisation",Casedefinition)) 
meta_median_mort <- dplyr::filter(meta_median, !grepl("ALRI hospitalisation|ARI hospitalisation|C&R hospitalisation|P&I hospitalisation|Chronic respiratory disease hospitalisation",Casedefinition)) 
cor(meta_median_hosp$meta_estimate_log,meta_median_hosp$median_estimate_log,method = "pearson")
cor.test(meta_median_hosp$meta_estimate_log,meta_median_hosp$median_estimate_log,method = "pearson")
# r=0.95,p<0.001 corr after log
cor(meta_median_mort$median_estimate_log,meta_median_mort$meta_estimate_log,method = "pearson")
cor.test(meta_median_mort$median_estimate_log,meta_median_mort$meta_estimate_log,method = "pearson")
# r=0.81,p<0.001  corr after log

meta_median_plot <- ggplot(meta_median,aes(x=median_estimate_log, y=meta_estimate_log,colour=case_definition)) +
  geom_point( ) +
  geom_smooth(method="lm",se=FALSE) + 
  # geom_point(meta_median_mort , aes(x=median, y=meta_estimate),colour="red3") +
  # geom_smooth(se = FALSE, method = "gam", formula = y ~ I(x^(-2)), size = 2, col = "#b3cde3") +
  # facet_zoom(x = meta_estimate < 450,y=median<200)  +
  scale_y_continuous(name="Meta estimate") +
  scale_x_continuous(name = "Median",  breaks = c(0,2,4,6,8)) +
  geom_abline(slope = 1, intercept = 0, lty = 2)+
  scale_colour_discrete(#values=c("#CC0000", "#006600", "#669999",
    # "#00CCCC", "#660099"),或者color/fill/colour,discrete/hue,
    name="Outcome")+
  theme_bw()
ggsave(
  plot= meta_median_plot,
  file="Figures/meta_median fit revised_revised.pdf", width = 10, height = 6
) 

# 3.2  Comparison of case definitions----
# P&I VS ALL-CAUSE MORTALITY
CaseDefinition_PIallcause <- read_excel("evaluation of case definition_no income.xlsx",sheet="case_definition compared revise",na = "NA") 
CaseDefinition_PIallcause <- left_join(CaseDefinition_PIallcause,raw.data.descript,by="ID")
unique(CaseDefinition_PIallcause$QA) # 3
# ARI VS ALL-CAUSE MORTALITY
CaseDefinition_ARIallcause <- read_excel("evaluation of case definition_no income.xlsx",sheet="ARI VS ALL-CAUSE",na = "NA") 
CaseDefinition_ARIallcause <- left_join(CaseDefinition_ARIallcause,raw.data.descript,by="ID")
table(CaseDefinition_ARIallcause$QA,CaseDefinition_ARIallcause$ID)
# C&R VS ALL-CAUSE MORTALITY
CaseDefinition_CRallcause <- read_excel("evaluation of case definition_no income.xlsx",sheet="C&R VS ALL-CAUSE",na = "NA") 
CaseDefinition_CRallcause <- left_join(CaseDefinition_CRallcause,raw.data.descript,by="ID")
table(CaseDefinition_CRallcause$QA,CaseDefinition_CRallcause$ID)
# CaseDefinition_PIallcause <- CaseDefinition_alldata[
#   with(CaseDefinition_alldata, 
#        ID %in% intersect( ID[Casedefinition == "P&I mortality" ],
#                            ID[Casedefinition == "All-cause mortality" ])
#        & Casedefinition %in% c("P&I mortality","All-cause mortality" )), 
#   c("ID", "Study", "Country", "Income",
#     "AGEGR","DenoRaw","ALRI_total","ALRI_tested","ALRI_RSV",
#     "sALRI_total","sALRI_tested","sALRI_RSV","vsALRI_total","vsALRI_tested","vsALRI_RSV",
#     "ALRIDeaths_total","ALRIDeaths_tested","ALRIDeaths_RSV")] 

# 4. others----
# 4.1 formap----
# load mapdata
world_map <- readOGR(dsn = "C:/系统文件 之外的/科研文件/R软件及代码/R语言练习数据/Worldmap/World_Countries.shp", stringsAsFactors = TRUE)
x <- world_map@data 
xs <- data.frame(x, id = seq(1:252)-1) 
world_map1 <- fortify(world_map)
world_map_data <- join(world_map1, xs, type = "full")  # library(plyr)
world_map_data %>% filter(COUNTRY != "Antarctica") -> world_map_data  
# load indata
da1 <- read_excel("all data  BBcleaned_forboxplot.xlsx",sheet="formap",na = "NA"); da2 <- da1
da1[c(3,4)] <- NULL
worlddata <- join(world_map_data,da1,by="COUNTRY",type="full")
# plots
a <- 
  ggplot() +
  geom_polygon(data=worlddata,aes(x=long,y=lat,group=group,fill=number),colour="gray50",size=0.2) + #white
  # theme_classic() 
  scale_fill_gradient(low="lightblue",high="darkblue",na.value = "white",breaks=c(1,3,5,7,9,11))+
  # coord_map("polyconic")+
  geom_text(aes(x=lon,y=lat,label=COUNTRY),data=da2,colour="red",size=3.3) +
  # facet_zoom(ylim = c(45,60),xlim = c(-20,20),zoom.size = 0.5,shrink=T) +  
  labs(fill="Number of study") +
  theme(
    panel.grid=element_blank(),
    panel.background=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    # legend.direction = "horizontal",
    legend.position=c(0.1,0.5)
  )

ggsave(
  plot=a,
  file="Figures/map1.pdf", width = 12, height = 6
) #width = 10, height = 12

b <- 
  ggplot() +
  geom_polygon(data=worlddata,aes(x=long,y=lat,group=group,fill=number),colour="gray50",size=0.2) + #white
  # theme_classic() +
  scale_fill_gradient(low="lightblue",high="darkblue",na.value = "white",breaks=c(1,3,5,7,9,11))+
  # coord_map("polyconic")+
  geom_text(aes(x=lon,y=lat,label=COUNTRY),data=da1,colour="red",size=3.5) +
  # facet_zoom(ylim = c(45,60),xlim = c(-20,20),zoom.size = 0.5,shrink=T) + 
  # labs(fill="Number of study") +
  coord_sf(xlim = c(-10,20),ylim = c(45,60)) + 
  # ggthemes::theme_map(base_size = 15)
  theme(
    panel.grid=element_blank(),
    panel.background=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank(),
    axis.title=element_blank(),
    # legend.direction = "horizontal",
    legend.position="none"
  )

ggsave(
  plot=b,
  file="Figures/map2.pdf", width = 6, height = 3
) #width = 10, height = 12
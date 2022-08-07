#Likelihood Ratios and Frequencies Main Script

#!!!!!! To run the "Cond.Appearance.fn" you need to uncomment lines 142-144 in ChangeDataset.R!!!!
#!!!!!! To run the "Cond.Appearance.ext.fn" you need to comment lines 142-144 in ChangeDataset.R!!!!

setwd("/Users/nagakamd/Desktop/3R/Project_s Main Code /")

library(tidyverse)
library(purrr)
source("ReadingData.R")
source("ChangeDataset.R")
source("Appearance_Functions.R")
source("LikelihoodRatio.R")



#Read the data
MainData_file="Data/S3.txt"
PathologyData_file="Data/S4.txt"

Data=ReadingData(MainData_file,PathologyData_file)
rm(MainData_file,PathologyData_file)

# Change the Data
Data=ChangeData(Data)

#Specify species
animal="rodent"


# Appearance dataframe . For each high level effect and study of specific duration
# Values: NA-> the study not conducted
#         1: Study conducted and effect observed 
#         0: Study conducted and effect not observed



Appearance=map_dfr(.x =unique(Data$MainData$identifier),.f = Appear_Drug, Data)


#Group together short and middle study.Three versions of the same function exists depending on how high level you would like to go
# with "Cond.Appearance.fn" creating the most general high level effects and "Cond.Appearance.ext.fn the most detailed meaning the same as Appearance

Summarised.Appearance = Cond.Appearance.fn(Appearance)
#Summarised.Appearance = Cond.Appearance.ext.fn(Appearance)

#Table containing likelihood ratios
Likelihood.Ratio=Likelihood.Ratio.fn(Summarised.Appearance %>% filter(species== animal))
Likelihood.Ratio[,-1]=round(Likelihood.Ratio[,-1],2)

#Likelihood ratios with p.value<0.05
Likelihood.Ratio.imp=Likelihood.Ratio %>% filter(p_value<=0.05) %>% 
  arrange(desc(LR_pos), desc(iLR_neg)) %>% select(-c(Sensitivity,Specificity))

View(Likelihood.Ratio.imp)

#Write it
#write.csv(file ="LR_non_rodent.csv",Likelihood.Ratio.imp)


#Plot Frequencies of FP and FN


#Plot Frequencies of FP and FN
Plot_freq_nonrod=Appear_plot(dataf = Likelihood.Ratio.imp ,species = "non-rodent", legend_pos=c(0.90,0.90))

Plot_freq_rod=Appear_plot(dataf = Likelihood.Ratio.imp ,species = "rodent", legend_pos=c(0.90,0.90))

fig6 <- ggarrange(Plot_freq_rod,Plot_freq_nonrod, labels = c("A","B"), common.legend = TRUE, nrow = 2)


png(paste0("Fig6",".png"),
    width= 30, height = 40, units="cm",  res= 600, type = "cairo-png")
print(fig6)
dev.off()

####################################################################################################################################################################
#Same analysis but with the adverse only


#Keep only the adverse
Adverse.Data=AdverseData.fn(Data)

# Adverse Appearance dataframe
# It contains all the drugs but not necessarily all the possible findings. 
# If a finding does not appear as adverse in any drug: Then it is not contained
Adverse.Appearance=map_dfr(.x =unique(Adverse.Data$MainData$identifier),.f = Appear_Drug, Adverse.Data)


# #Group together short and middle study

Summarised.Adverse.Appearance=Cond.Appearance.fn(Adverse.Appearance)


#Summarised.Adverse.Appearance                                                                                                                                                                           e=Cond.Appearance.fn(Adverse.Appearance)

#Table containing likelihood ratios
Adverse.Likelihood.Ratio=Likelihood.Ratio.fn(Summarised.Adverse.Appearance %>% filter(species=="non-rodent"))
Adverse.Likelihood.Ratio[,-1]=round(Adverse.Likelihood.Ratio[,-1],2)

#Likelihood ratios with p.value<0.05
Adverse.Likelihood.Ratio.imp=Adverse.Likelihood.Ratio %>% filter(p_value<=0.05) %>% 
  arrange(desc(LR_pos), desc(iLR_neg)) %>%  select(-c(Sensitivity,Specificity))

View(Adverse.Likelihood.Ratio.imp)

#Plot Frequencies of FP and FN
Plot_freq_nonrod=Appear_plot(dataf = Adverse.Likelihood.Ratio.imp ,species = "non-rodent", legend_pos=c(0.90,0.90))
Plot_freq_nonrod


Plot_freq_rod=Appear_plot(dataf = Adverse.Likelihood.Ratio.imp ,species = "rodent", legend_pos=c(0.90,0.90))
Plot_freq_rod

fig5 <- ggarrange(Plot_freq_rod,Plot_freq_nonrod, labels = c("A","B"), common.legend = TRUE, nrow = 2)


png(paste0("Fig5",".png"),
    width= 30, height = 40, units="cm",  res= 600, type = "cairo-png")
print(fig5)
dev.off()

#Write it
#write.csv(file ="ALR_rodent.csv",Adverse.Likelihood.Ratio)
####################################################################################################################################################################

# Adverse Finding table
# For each of the adverse findings observed in the long-term studies : for how many molecules it was also seen as adverse in the short/middle study,
# observed as not adverse in the short/middle study (so it progresses)  or  first appeared in the long-study
#Adversity=Adverse_finding(Summarised.Adverse.Appearance,Summarised.Appearance, type="lm")

#Table showing whether any adverse effects were recorded in the long-term study and initial studies( short and middle together) for each molecule
Adversity.Summary=Adversity.Summary.fn(Summarised.Adverse.Appearance, type="sm")


adversity_sm <-Adversity.Summary.fn(Summarised.Adverse.Appearance, type="sm")
adversity_lm <- Adversity.Summary.fn(Summarised.Adverse.Appearance, type="lm")




#Keep only the adverse
Adverse.Data=AdverseData.fn(Data)

# Adverse Appearance dataframe
# It contains all the drugs but not necessarily all the possible findings. 
# If a finding does not appear as adverse in any drug: Then it is not contained
Adverse.Appearance=map_dfr(.x =unique(Adverse.Data$MainData$identifier),.f = Appear_Drug, Adverse.Data)


# #Group together short and middle study
Summarised.Adverse.Appearance=Cond.Appearance.fn(Adverse.Appearance)


# Adverse Finding table
# For each of the adverse findings observed in the long-term studies : for how many molecules it was also seen as adverse in the short/middle study,
# observed as not adverse in the short/middle study (so it progresses)  or  first appeared in the long-study
#Adversity=Adverse_finding(Summarised.Adverse.Appearance,Summarised.Appearance, type="lm")

#Table showing whether any adverse effects were recorded in the long-term study and initial studies( short and middle together) for each molecule

adversity_sm <-Adversity.Summary.fn(Summarised.Adverse.Appearance, type="sm")
adversity_lm <- Adversity.Summary.fn(Summarised.Adverse.Appearance, type="lm")






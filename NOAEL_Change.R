
`%!in%` <- Negate(`%in%`)

NOAEL_drug=function(drug, dataf, animal, group1, group2, group3, group4) {
  
  df=dataf[dataf$identifier==drug,]
  
  #You may have more than two studies in a specific species for a specific group or combination of groups. In that case 
  #you keep the smallest of the identified NOAEL and compare it with the smallest of the identified NOAEL in the other group or combination of groups.
  #This is how you resolve also the cases where you have different NOAEL in females/males
  
  #BUT: If the smallest NOAEL in the short/middle is zero, you take the next smallest in the hierarchy. 
  #Because, that may means that you increased a lot the doses in one of the studies and that resulted in not NOAEL or that you
  #need to decrease the doses. It's the worst case scenario.
  
  dose_short=df$adj_dose[df$group==group1 | df$group==group2]
  dose_short=dose_short[order(dose_short)]
  if (dose_short[1]!=0) {
    dose_short=dose_short[1]
  }else if (dose_short[1]==0 & length(dose_short)==1){
    dose_short=dose_short[1]
  }else {
    dose_short=dose_short[2]
  }
  
  #dose_short=min(df$adj_dose[df$group==group1 | df$group==group2])
  dose_long=min(df$adj_dose[df$group==group3 | df$group==group4])
  
  #Specify difference
  Dif=dose_long-dose_short
  
  if(Dif==0) { #NOAEL didn't change
      
    return(data.frame(identifier=drug,noael="Same",species=animal))
      
  }else if (Dif<0) { #NOAEL decreased
      
    return(data.frame(identifier=drug,noael="Decrease",species=animal))
      
  }else {
      
    return(data.frame(identifier=drug,noael="Increase",species=animal))
  }
    
}


NOAEL_change= function(animal, modality, dataf, group1, group2, group3, group4) {
  
  #Keep only the ones for the specific species and for specific modality
  dataf= dataf %>% filter(species==animal & type==modality)
  
  #keep only the rows that the NOAEL is defined
  dataf=dataf %>% filter(noael=="yes")
  
  #Keep only the drugs that for the specific animal have the comparable studies
  dataf= dataf %>% group_by(identifier) %>% 
    mutate(keep=(any(group==group1) | any(group==group2)) & (any(group==group3) | any(group==group4))) %>%
    filter(keep== TRUE) %>% ungroup() %>% select(-keep)
  
  result=map_dfr(.x = unique(dataf$identifier),.f = NOAEL_drug, dataf, animal, group1, group2, group3, group4)
  
  return(result)
}








# 
# 
# NOAEL_plotting= function(df,xtitle, tag, legend_pos) {
#   
#   #Change the levels of NOAEL
#   levels(df$noael)[levels(df$noael)=="Decrease"]= "decreased"
#   levels(df$noael)[levels(df$noael)=="Increase"]= "stayed the same/increased"
#   levels(df$noael)[levels(df$noael)=="Same"]= "stayed the same/increased"
#   
#   #Dataset for plotting
#   df= df %>% group_by(species,noael) %>% summarise(n=n()) %>% ungroup()
#   
#   
#   df= df %>% group_by(species) %>% mutate(total_drugs=sum(n)) %>% ungroup()
#   df=df %>% mutate(proportion=n/total_drugs*100)
# 
#   #Create the plot 
#   Nplot=ggplot(df, aes(x= species, y=proportion, fill=factor(noael,levels=c("decreased","stayed the same/increased")))) +  
#     geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.75),colour="black") +
#     geom_text(aes(label = paste("",n,"mol.")), vjust = -0.3, hjust=0.5, position=position_dodge(0.75))+
#     scale_fill_manual("NOAEL change in long-term vs \n short&middle-term studies",
#                       values = c( "decreased" ="skyblue3", "stayed the same/increased" = "tomato3"))
#   
#   
#   Nplot= Nplot+
#     theme_bw()+
#     labs(y="Percentage",x= "", title=paste("NOAEL change in",xtitle)) +
#     scale_y_continuous(limits = c(0,100),breaks = c(0,20,40,60,80,100))+
#     theme(aspect.ratio = 1,# plot.background = element_rect(fill= NA, colour = "black", size = 1),
#           plot.margin = margin(10,10,10,10),
#           plot.title = element_text(size=14, face="bold", hjust =0.5,vjust=0.4 ),
#           plot.subtitle = element_text( hjust =0.5,vjust=0.5),
#           panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
#           panel.grid.minor = element_blank(),
#           panel.border = element_blank(),
#           axis.ticks.y = element_line(colour = "black", size = 0.2),
#           axis.ticks.x = element_line(colour = "black", size = 0.2),
#           axis.text.x = element_text(size=11, colour="black"),
#           axis.text.y = element_text(size=11, colour="black"),
#           axis.title.y = element_text(size = 11,colour = "black", margin=margin(r=10)),
#           axis.title.x = element_text(size = 11,colour = "black",face="bold", margin=margin(t=10)),
#           legend.background = element_blank(),
#           legend.position = legend_pos,
#           legend.justification = c("left", "top"),
#           legend.title = element_text(colour = "black", size = 11),
#           legend.text = element_text(colour = "black", size = 10))
#   
#   
#     
#   #Nplot
#   return(Nplot)
#   
#   
# }

#####################################################################################################################################
#Main Program

library(tidyverse)
library(purrr)
library(gridExtra)


#Read the data
source("ReadingData.R")

MainData_file="Data/S3.txt"
PathologyData_file="Data/S4.txt"

Data=ReadingData(MainData_file,PathologyData_file)
rm(MainData_file,PathologyData_file)


#Keep only the studies that have defined NOAEL
#Because you have study_id that have multiple durations you need to also group_by group
#In that way, studies that do not have defined NOAEL are not used and the problematic ro5095932-1022449 is resolved

dataf=Data$MainData
dataf=dataf %>% group_by(study_id,duration) %>%
  mutate(keep=any(noael=="yes")) %>% filter(keep==TRUE) %>% select(-keep) %>% ungroup()  

dataf$species[dataf$species=="cynomolgus"]="non-rodent"
dataf$species[dataf$species=="minipig"]="non-rodent"
dataf$species[dataf$species=="marmoset"]="non-rodent"
dataf$species[dataf$species=="dog"]="non-rodent"
dataf$species[dataf$species=="rat"]="rodent"
dataf$species[dataf$species=="mouse"]="rodent"


#Goup1 to goup4 are for specifying the comparison groups- SM MOLECULES
result_sm= map_dfr(.x = unique(dataf$species),.f = NOAEL_change, modality="sm", dataf,
                     group1="short", group2="middle", group3="long", group4="long")
#Plot it
#NOAEL_plotSM=NOAEL_plotting(result_sm,xtitle= "small molecules", tag= "A", legend_pos=c(0.05,0.95))



#Goup1 to goup4 are for specifying the comparison groups-LM MOLECULES
result_lm= map_dfr(.x = unique(dataf$species),.f = NOAEL_change, modality="lm", dataf,
                   group1="short", group2="middle", group3="long", group4="long")


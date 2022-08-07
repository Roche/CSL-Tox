options(warn = -1)

#Exploratory Data Plots
library(tidyverse)
library(cowplot)
library(scales)
library(ggpubr)
library(ggsci)
library(gridExtra)
Therapeutic_Areas.fn= function(dataf) {
  
  #Number of molecules in each therapeutic area
  dataf= dataf %>% group_by(therapeutic_area,type) %>% summarise(n=length(unique(identifier))) %>% ungroup()
  
  #Plot it
  Nplot=ggplot(dataf, aes(x= reorder(therapeutic_area,-n),y=n , fill = type)) +  
    geom_bar(stat = "identity", color="black" , width = 0.75)+
    geom_text(aes(label = n),  position = position_stack(vjust = 0.5))+
    scale_fill_npg()
   
  Nplot=Nplot+
    theme_bw(base_size = 16)+
    coord_flip()+
   ggtitle("Therapeutic distribution of the dataset")+
    labs(subtitle = "Small molecules = 25 ,Large molecules = 18")+
    ylab('Number of molecules')+ 
    theme(aspect.ratio =1,
          plot.margin = margin(2,2,2,2, "cm"),
          plot.title = element_text(size=15, face="bold", hjust =0.5,vjust=0.5),
          panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(colour = "black", size = 0.5),
          axis.text.y = element_text(color = "black", size = 15),
          axis.text.x = element_text(color = "black", size = 15),
          plot.subtitle = element_text( hjust =0.5,vjust=0.5 ),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 15,colour = "black", margin=margin(t=5),hjust=0.5 ))
          
  return(Nplot)
}

##########################################################################################################################################

Different_Species.fn=function(dataf) {
  
  #Number of different species used in each drug
  dataf.no.species=dataf %>% group_by(identifier,type) %>% summarise(n=length(unique(species)))


  #How many SM molecules used each of the species
  dataf.SM=dataf %>% filter(type=="sm") %>% group_by(species) %>% 
    summarise(n=length(unique(identifier))) 
  dataf.SM=dataf.SM %>% mutate(type.species=c(rep("non-rodent",4), rep("rodent",2)))
  
  #How many LM molecules used each of the species
  dataf.LM=dataf %>% filter(type=="lm") %>% group_by(species) %>% 
    summarise(n=length(unique(identifier))) 
  dataf.LM=dataf.LM %>% mutate(type.species=c(rep("non-rodent",2),rep("rodent",2)))
  dataf.LM=rbind(dataf.LM,data.frame(species=c("minipig","marmoset"), n=c(0,0),type.species =c("non-rodent","non-rodent")))
  
  dataf.LM$Adj_n=ifelse(dataf.LM$n==0,dataf.LM$n+0.1,dataf.LM$n)
  
  
  #SM plot of species
  SM.species.plot=ggplot(dataf.SM, aes(factor(species, levels=c("rat", "cynomolgus","dog", "mouse", "marmoset", "minipig")),y=n,fill=species)) +  
    geom_bar(stat = "identity",color="black",width = 0.6) +  
    geom_text(aes(label = n), vjust = -0.2)+
  scale_fill_manual("species", values = c("cynomolgus" = "skyblue3" ,"dog" =  "tomato3", "mouse" ="darkorange1",
                                          "rat" = "darkseagreen", "minipig" ="peachpuff2", "marmoset" ="antiquewhite1"))
           
  
  SM.species.plot = SM.species.plot +
    theme_bw(base_size = 15)+
    labs(x= element_blank(),y="Number of molecules",title="Distribution of species",
         subtitle = "Small molecules", fill="Species")+
    theme(#plot.background = element_rect(fill= NA, colour = "black", size = 1),
          #plot.margin = margin(10,10,10,10),
          plot.title = element_text(size=15, face="bold", hjust =0.5,vjust=0.4 ),
          plot.subtitle = element_text( hjust =0.5,vjust=0.5 ),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(colour = "black", size = 0.5),
          axis.ticks.x = element_line(colour = "black", size = 0.5),
          axis.text.y = element_text(colour = "black", size = 15),
          axis.text.x = element_text(size=15,colour="black"),
          axis.title.y = element_text(size = 15,colour = "black", margin=margin(r=10),hjust=0.5 ),
          legend.background = element_rect(fill = NA, size = 4, colour = NA),
          legend.position = "none",
          legend.justification = c("right", "top"),
          legend.title = element_text(color = "black", size = 15),
          legend.text = element_text(color = "black", size = 15))
  
  #LM plot of species
  LM.species.plot=ggplot(dataf.LM, aes(factor(species, levels=c("rat", "cynomolgus","dog", "mouse", "marmoset", "minipig")),y=Adj_n,fill=species)) +  
    geom_bar(stat = "identity",color="black",width = 0.6) +
    geom_text(aes(label = n), vjust = -0.2)+
    scale_fill_manual("species", values = c("cynomolgus" = "skyblue3" ,"dog" =  "tomato3", "mouse" ="darkorange1",
                                            "rat" = "darkseagreen", "minipig" ="peachpuff2", "marmoset" ="antiquewhite1"))
    ylim=c(0,25)
  
  
  LM.species.plot = LM.species.plot +
    theme_bw(base_size = 15)+
    ylim(0,25)+
    labs(x= element_blank(),y="Number of molecules",title="Distribution of species",
         subtitle = "Large molecules", fill="Species")+
    theme(#plot.background = element_rect(fill= NA, colour = "black", size = 1),
          plot.margin = margin(10,10,10,10),
          plot.title = element_text(size=15, face="bold", hjust =0.5,vjust=0.4 ),
          plot.subtitle = element_text( hjust =0.5,vjust=0.5 ),
          panel.border = element_blank(),
          panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
          panel.grid.minor = element_blank(),
          axis.ticks.y = element_line(colour = "black", size = 0.5),
          axis.ticks.x = element_line(colour = "black", size = 0.5),
          axis.text.y = element_text(colour = "black", size = 15),
          axis.text.x = element_text(size=15,colour="black"),
          axis.title.y = element_text(size = 15,colour = "black", margin=margin(r=10),hjust=0.5 ),
          legend.background = element_rect(fill = NA, size = 4, colour = NA),
          legend.position = "none")
          
  #Create  the plot
  final.plot=ggdraw(xlim = c(0,3),ylim =c(0,2))+
   #draw_plot(SMpie.plot,x = 0, y = 1, width = 1, height = 1) +
    #draw_plot(LMpie.plot,x = 0, y = 0, width = 1, height = 1) +
    draw_plot(SM.species.plot,x = 1, y = 1, width = 2, height = 1) +
    draw_plot(LM.species.plot,x = 1, y = 0, width = 2, height = 1) 
  
  return(final.plot)
    
  
}

######################################################################################################################################
StudiesPerFindingCat=function(dataf) {
  
  
  dataf$species[dataf$species=="cynomolgus"]="non-rodent"
  dataf$species[dataf$species=="minipig"]="non-rodent"
  dataf$species[dataf$species=="marmoset"]="non-rodent"
  dataf$species[dataf$species=="dog"]="non-rodent"
  dataf$species[dataf$species=="rat"]="rodent"
  dataf$species[dataf$species=="mouse"]="rodent"
  
  #Keep only the columns you want
  dataf= dataf %>% select(-c(route_of_administration,pre_treatment,dose_interval,
                             dose_lethality,noael,noael_sex,adverse_findings,dose,duration,lab_measurements,death_diagnosis))
  
  #For each study you will have:
  #YES: if you have a finding at any dose
  #NO: if you do not have a finding in any dose
  dataf=dataf %>% group_by(study_id,species,type) %>% 
    summarise(body_change= any(c("yes", "ayes") %in% body_weight_gain) | any(c("yes", "ayes") %in% body_weight),
              neuro=any(c("yes", "ayes") %in% neurological_clinical_signs),
              git=any(c("yes", "ayes") %in% git_clinical_signs),
              other=any(c("yes", "ayes") %in% other_clinical_signs),
              macro=any(c("yes", "ayes") %in% macroscopic),
              pathol=any(c("yes", "ayes") %in% pathology),
              org_wei=any(c("yes", "ayes") %in% organ_weight),
              cardio=any(c("yes", "ayes") %in% cardiovascular_effects)) %>% ungroup()
  
  
  #Number of studies per category for each species and modality
  dataf=dataf %>% group_by(species,type) %>% 
    summarise(body_change= sum(body_change),
              neuro=sum(neuro),
              git=sum(git),
              other=sum(other),
              macro=sum(macro),
              pathol=sum(pathol),
              org_wei=sum(org_wei),
              cardio=sum(cardio)) %>% ungroup()
  
  #Transform the data into longer form for the plotting
  dataf=dataf %>% pivot_longer(!c(species,type), names_to = "Category", values_to = "No.Studies")
  
  #Create facet_grid names
  labeler_names=c('lm'="Large molecules",'sm'="Small molecules")
  
  #Change the group names
  dataf$Category=as.factor(dataf$Category)
  levels(dataf$Category)=
    list("body weight changes" = "body_change",
         "neurological clinical signs" = "neuro",
         "gastrointestinal clinical signs" = "git",
         "other clinical signs" = "other",
         "macroscopic pathology" ="macro",
         "microscopic pathology"= "pathol",
         "organ weights" = "org_wei",
         "cardiovascular effects"="cardio")
  
  #Add a small value to the zeros for the plotting
  dataf$Adj=ifelse(dataf$No.Studies==0,dataf$No.Studies+0.5,dataf$No.Studies)
  
  
  #Plot it
  find_plot=ggplot(dataf, aes(x=Category,y=Adj,fill=Category)) + 
    geom_bar(stat = "identity",color="black", width=0.6) +
    geom_text(aes(label = No.Studies), vjust = -0.2)+
    scale_fill_brewer(palette="Spectral")+  #scale_fill_manual("species", values = c("dog" = "black" ,...))
    facet_grid(species~type,labeller= labeller(type=as_labeller(labeler_names)))
    
    
  find_plot = find_plot +
    theme_bw(base_size = 15)+
    labs(y="Number of studies",x="",title="Distribution of effects")+
    ylim(0,60)+
    theme(#plot.background = element_rect(fill= NA, colour = "black", size = 1),
      #plot.margin = margin(10,10,10,10),
      plot.title = element_text(size=15, face="bold", hjust =0.5,vjust=0.4 ),
      #plot.subtitle = element_text( hjust =0.5,vjust=0.5 ),
      #panel.border = element_rect(colour="black", fill= NA),
      panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      axis.ticks.y = element_line(colour = "black", size = 0.5),
      axis.ticks.x = element_line(colour = "black", size = 0.5),
      axis.text.x = element_blank(),
      axis.title.y = element_text(size = 15,colour = "black", margin=margin(r=10),hjust=0.5 ),
      legend.background = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(colour = "black", size = 15),
      strip.text = element_text(size=15, face="bold.italic"),
      strip.background = element_blank())
    

  return(find_plot)
  
  
}
#######################################################################################################################################
Dist.St.fn=function(dataf) {
  
  dataf= dataf %>% group_by(type,group) %>% summarise(n=length(unique(study_id)))%>% ungroup()
  
  dataf$group=factor(dataf$group)
  levels(dataf$group) <- list("short-term"  = "short", "middle-term" = "middle", "long-term" = "long")

  
  #SM plot
  studies_plot.SM=ggplot(dataf %>% filter(type=="sm"), aes(x=factor(group, levels=c("short-term","middle-term","long-term")),y=n,fill=group)) + 
    geom_bar(stat = "identity",color="black", width=0.6) +
    geom_text(aes(label = n), vjust = -0.2)+
    scale_fill_manual("species", values = c("short-term" ="skyblue3","middle-term" = "tomato3", "long-term" = "darkseagreen"))
    
  
  studies_plot.SM = studies_plot.SM +
    theme_bw(base_size = 15)+
    labs(y="",x="Small molecules",title="Distribution of studies", 
         subtitle = paste("Total number of studies:",sum(dataf %>% filter(type=="sm") %>% select(n))))+ 
    ylim(0,60)+
    theme(aspect.ratio = 1,#plot.background = element_rect(fill= NA, colour = "black", size = 1),
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
          plot.title = element_text(size=14, face="bold", hjust =0.5,vjust=0.4 ),
          plot.subtitle = element_text( hjust =0.5,vjust=0.5),
          panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
          panel.grid.minor = element_blank(),
         # panel.border = element_blank(),
          axis.ticks.y = element_line(colour = "black", size = 0.5),
          axis.ticks.x = element_line(colour = "black", size = 0.5),
          axis.text.x = element_text(size=15, colour="black"),
          axis.text.y = element_text(size=15, colour="black"),
          axis.title.y = element_text(size = 15,colour = "black", margin=margin(r=10)),
          axis.title.x = element_text(size = 15,colour = "black",face="bold", margin=margin(t=10)),
          legend.position = "none")
  
  
  #LM plot
  studies_plot.LM=ggplot(dataf %>% filter(type=="lm"), 
                         aes(x=factor(group, levels=c("short-term","middle-term","long-term")),y=n,fill=group)) + 
    geom_bar(stat = "identity",color="black", width=0.6) +
    geom_text(aes(label = n), vjust = -0.2)+
    scale_fill_manual("Duration", values = c("short-term" ="skyblue3","middle-term" = "tomato3", "long-term" = "darkseagreen"))
  
  studies_plot.LM = studies_plot.LM +
    theme_bw(base_size = 15)+
    labs(y="Number of studies",x="Large molecules",title="Distribution of studies",
         subtitle = paste("Total number of studies:",sum(dataf %>% filter(type=="lm") %>% select(n))))+
    ylim(0,60)+
    theme(aspect.ratio = 1,#plot.background = element_rect(fill= NA, colour = "black", size = 1),
          plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
          plot.title = element_text(size=14, face="bold", hjust =0.5,vjust=0.4 ),
          plot.subtitle = element_text( hjust =0.5,vjust=0.5),
          panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
          panel.grid.minor = element_blank(),
          #panel.border = element_blank(),
          axis.ticks.y = element_line(colour = "black", size = 0.2),
          axis.ticks.x = element_line(colour = "black", size = 0.2),
          axis.text.x = element_text(size=15, colour="black"),
          axis.text.y = element_text(size=15, colour="black"),
          axis.title.y = element_text(size = 15,colour = "black", margin=margin(r=10)),
          axis.title.x = element_text(size = 15,colour = "black",face="bold", margin=margin(t=10)),
          legend.background = element_blank(),
          legend.position = "none",
          legend.justification = c("left", "top"),
          legend.title = element_text(colour = "black", size = 12),
          legend.text = element_text(colour = "black", size = 12))
  
  
  
  final_plot = ggarrange(studies_plot.LM,studies_plot.SM,ncol = 2)

  return(final_plot)
}



##################################################################################################################################################
###################################################################################################################################################
#Main Program


#Read the data
source("ReadingData.R")
library(tidyverse)
 
MainData_file="Data/S3.txt"
PathologyData_file="Data/S4.txt"

Data=ReadingData(MainData_file,PathologyData_file)
rm(MainData_file,PathologyData_file)

plota <- Therapeutic_Areas.fn(Data$MainData)
plotb <- Different_Species.fn(Data$MainData)
plotc <- Dist.St.fn(Data$MainData) 
plotd <- StudiesPerFindingCat(Data$MainData) 




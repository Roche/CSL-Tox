#Appearance Main

#Input: Data
#Ouput: A dataframe in which for each finding and RO and rodents/non-rodents: It will have 0 if the finding is not existing in the "specific" 
#duration study and 1 if it exists.

#Read the data
MainData_file="Data/S3.txt"
PathologyData_file="Data/S4.txt"

Data=ReadingData(MainData_file,PathologyData_file)
rm(MainData_file,PathologyData_file)

# Change the Data
Data=ChangeData(Data)

Appear_Species=function(animal, Dataf_find, MainData_drug,findingD) {
  
  # Check whether you have studies for the specific animal and study
  Existence_short=ifelse(any(MainData_drug$species==animal) & any(MainData_drug$group=="short"),TRUE,NA)
  Existence_middle=ifelse(any(MainData_drug$species==animal) & any(MainData_drug$group=="middle"),TRUE,NA)
  Existence_long=ifelse(any(MainData_drug$species==animal) & any(MainData_drug$group=="long"),TRUE,NA)
  
  # Dataset for the specific drug, finding and species
  Dataf_find=Dataf_find %>% filter(species==animal)
  
  #Specify whether the finding exists for the specific species and the specific drug in short, middle and long duration (if they have been conducted)
  row=data.frame(drug=unique(MainData_drug$identifier),
                 modality=unique(MainData_drug$type),
                 species=animal,
                 finding=findingD,
                 short=any(Dataf_find$group=="short")*Existence_short,
                 middle=any(Dataf_find$group=="middle")*Existence_middle,
                 long=any(Dataf_find$group=="long")*Existence_long, stringsAsFactors = FALSE)
  return(row)
}               



Appear_Find=function(findingD, Dataf_drug, MainData_drug) {
  
  # Iterate through the different species
  App_Find=map_dfr(.x =c("rodent","non-rodent"),.f = Appear_Species, Dataf_drug %>% filter(description==findingD), MainData_drug,findingD)
  return(App_Find)
}


Appear_Drug=function(Specdrug,Dataf) {
  
  App_Drug=data.frame()
  for (key in names(Dataf)[-1]) {
    
    #Iterate for each finding
    findings=unique(Dataf[[key]]$description)
    App_Drug=rbind(App_Drug,map_dfr(.x =findings,.f = Appear_Find, Dataf[[key]] %>% filter(identifier==Specdrug),
                                    Dataf$MainData %>% filter(identifier==Specdrug)))
    
  }
  return(App_Drug)
  
}


Cond.Appearance.fn=function(Appearance) {
  
  #Define Systems
  Systems=c("Weight changes", "Neurological clinical signs", "Gastrointestinal clinical signs", "Other clinical signs", 
            "In life cardiovascular effects","Whole body", 
            "liver", "Lymphoid Tissues", "Endocrine System", "Reproductive System",
            "Nervous System", "Urinary System", "Respiratory System","Exocrine System",
            "Cutaneous", "MuscularSkeletal System", "GI tract", "Eye/conjuctiva", "Cardiovascular System", "whole body" )
  
  #Search the category for a specific finding
  search.Categ= function(finding, Systems) {
    return(Systems[which(sapply(Systems,grepl,finding))])
  }
  
  Appearance =Appearance %>%  mutate(Category=sapply(finding,search.Categ,Systems))
  Appearance$finding=Appearance$Category
  Appearance=Appearance %>% select(-Category)
  
  
  #Transform it to TRUE/FALSE
  Cond.Appearance= Appearance %>% group_by(drug,modality,species,finding) %>%
    summarise(short.cond=ifelse(all(is.na(short)) , NA ,any(short==1,na.rm = TRUE)),
              middle.cond=ifelse(all(is.na(middle)) , NA ,any(middle==1,na.rm = TRUE)),
              long.cond=ifelse(all(is.na(long)) , NA ,any(long==1,na.rm = TRUE))) %>% ungroup()
  
  #Create the short_middle.cond_Column
  Cond.Appearance=Cond.Appearance %>% group_by(drug,modality,species,finding) %>% 
    mutate(short_middle.cond= ifelse((is.na(short.cond) & is.na(middle.cond)), NA , any(short.cond==TRUE | middle.cond==TRUE,na.rm = TRUE))) %>% ungroup()
  
  return(Cond.Appearance)
}


Cond.Appearance.ext.fn=function(Appearance) {
  
  #Transform it to TRUE/FALSE
  Cond.Appearance= Appearance %>% group_by(drug,modality,species,finding) %>%
    summarise(short.cond=ifelse(all(is.na(short)) , NA ,any(short==1,na.rm = TRUE)),
              middle.cond=ifelse(all(is.na(middle)) , NA ,any(middle==1,na.rm = TRUE)),
              long.cond=ifelse(all(is.na(long)) , NA ,any(long==1,na.rm = TRUE))) %>% ungroup()
  
  #Create the short_middle.cond_Column
  Cond.Appearance=Cond.Appearance %>% group_by(drug,modality,species,finding) %>%
    mutate(short_middle.cond= ifelse((is.na(short.cond) & is.na(middle.cond)), NA , any(short.cond==TRUE | middle.cond==TRUE,na.rm = TRUE))) %>% ungroup()
  
  
  return(Cond.Appearance)
}



#Distribution of  FP, FN
Appear_plot=function(dataf,species,legend_pos) {
  
  #Number of molecules
  No_molecules=dataf$TP[1]+dataf$TN[1]+dataf$FN[1]+dataf$FP[1]
  
  #Keep only FP and FN and create the frequencies and transform the dataset in long format
  dataf=dataf %>% select(c(finding, FN, FP)) %>% 
    mutate(FP= round((FP/No_molecules*100),2), FN=round((FN/No_molecules*100),2)) %>% 
    pivot_longer(!finding, names_to = "type",  values_to = "value")
  
  #Create the Categories
  in_life=c("Weight changes", "Neurological clinical signs", "Gastrointestinal clinical signs", "Other clinical signs", 
            "In life cardiovascular effects")
  
  
  dataf= dataf %>% mutate(Category=ifelse(finding %in% in_life,"in life observations", "necropsy observations"))
  
  # For better plotting
  dataf$finding[dataf$finding=="Gastrointestinal clinical signs"]="Gastrointestinal \nclinical signs"
  dataf$finding[dataf$finding=="In life cardiovascular effects"]="In life cardiovascular\n effects"
  
  
  #Create the plot 
  Nplot=ggplot(dataf, aes(x= finding, y=value, fill=factor(type))) +  
    geom_bar(stat = "identity", width = 0.7,position = position_dodge(width = 0.75),colour="black") +
    #geom_text(aes(label = value), vjust = -0.3, hjust=0.5, position=position_dodge(0.75),size=2.5)+
    facet_grid(. ~Category,switch = "x", scales = "free", space = "free")+
    scale_fill_manual("Contingency table \nvalues",
                      values = c("FP" = "skyblue3", "FN" = "tomato3"))
  
  
  Nplot= Nplot+
    theme_bw()+
    labs(y="Frequency (%)",x= "", title=paste("Frequency of FP and FN across the different Categories in",species)) +
    scale_y_continuous(limits = c(0,100),breaks = c(0,10,20,30,40,50,60,70,80,90,100))+
    theme(#plot.background = element_rect(fill= NA, colour = "black", size = 1),
         # plot.margin = margin(10,10,10,10),
          plot.title = element_text(size=14, face="bold", hjust =0.5,vjust=0.4 ),
          plot.subtitle = element_text( hjust =0.5,vjust=0.5),
          panel.grid.major = element_line(colour = "grey50", size = 0.2, linetype = "dotted"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.spacing = unit(2, "lines"),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text.x = element_text(size=12,color="black",face = "bold.italic"),
          axis.ticks.y = element_line(colour = "black", size = 0.2),
          axis.ticks.x = element_line(colour = "black", size = 0.2),
          axis.text.x = element_text(size=10, colour="black", angle = 55, hjust=1),
          axis.text.y = element_text(size=11, colour="black"),
        #  axis.title.y = element_text(size = 11,colour = "black", margin=margin(r=10)),
        #  axis.title.x = element_text(size = 11,colour = "black",face="bold", margin=margin(t=10)),
          legend.background = element_blank(),
          legend.position = legend_pos,
          legend.justification = c("left", "top"),
          legend.title = element_text(colour = "black", size = 11, hjust = 0.5),
          legend.text = element_text(colour = "black", size = 10, hjust = 0.5))
  return(Nplot)
  
}

Adverse_finding=function(adverse_dataf,dataf, type) {
  
  # Filter the studies for a specific modality and that have long and short/middle study
  dataf=dataf %>% filter(modality==type)
  dataf= dataf %>% filter(!is.na(long.cond) & !is.na(short_middle.cond))
  
  adverse_dataf=adverse_dataf %>% filter(modality==type)
  adverse_dataf= adverse_dataf %>% filter(!is.na(long.cond) & !is.na(short_middle.cond))
  
  
  # Some findings are not included in the adverse_dataf because they have never observed as adverse. You want to exclude those
  # from the dataf before the comparison
  findings_vector=unique(dataf$finding)
  Not_keep= findings_vector[which(!findings_vector %in% adverse_dataf$finding)]
  dataf=dataf %>% filter(!finding %in% Not_keep)
  
  #Create the two columns
  find_dataf=dataf[,c(1:4)]
  
  # Was existing as adverse in the short or middle study too
  ExistingAdverse=adverse_dataf$long.cond==TRUE & adverse_dataf$short_middle.cond==TRUE
  
  # Was existing  in the short or middle study too
  ExistingNot_Adverse=adverse_dataf$long.cond==TRUE & dataf$short_middle.cond==TRUE & adverse_dataf$short_middle.cond==FALSE
  
  # You see it in the long study firt time
  NotExisting=adverse_dataf$long.cond==TRUE & dataf$short_middle.cond==FALSE & adverse_dataf$short_middle.cond==FALSE
  
  # add them to the dataset
  find_dataf= find_dataf %>% mutate(ExistingAdverse,ExistingNot_Adverse,NotExisting)
  
  # Group for each finding
  find_dataf=find_dataf %>% group_by(finding) %>% summarise(ExistingAdverse=sum(ExistingAdverse),
                                                            ExistingNot_Adverse=sum(ExistingNot_Adverse),
                                                            NotExisting=sum(NotExisting))
  
  return(find_dataf)
}


Adversity.Summary.fn=function(adverse_dataf, type) {
  
  #Filter only the specific modality
  adverse_dataf=adverse_dataf %>% filter(modality==type)
  
  #Answer the question: Did you have any adversity seen in the long or short/middle duration for a drug?
  adverse_dataf= adverse_dataf %>% group_by(drug) %>%  
    summarise(short_middle=ifelse(all(is.na(short_middle.cond)) , NA ,any(short_middle.cond,na.rm = TRUE)),
              long=ifelse(all(is.na(long.cond)) , NA ,any(long.cond,na.rm = TRUE))) %>% filter(!is.na(long) & !is.na(short_middle))
  
  # Order it
  adverse_dataf = adverse_dataf %>% arrange(short_middle,long)
  
  return(adverse_dataf)
}






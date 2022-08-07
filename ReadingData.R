#Function for reading the data and put it in a form of list
options(warn=-1)
ReadingData<- function(MainData_file,PathologyData_file){
  
  source("Read_Entry_Function.R")
  source("Pathology_Read_Entry_Function.R")
  source("ChangeDosesToNum_Fun.R" )
  source("AdjDoseFun_int.R")
  library(tidyverse)
 
  
  #Read the dataset
  MainData=read.delim(MainData_file,sep = " ", dec = ".",header = TRUE, stringsAsFactors = FALSE)
  
  #Delete any whitespaces in the beginning and end of the entries
  MainData=apply(MainData,2,trimws)
  MainData=as.data.frame(MainData,stringsAsFactors = FALSE)
  
  #Create the names of the new extended datasets
  Extract_Columns=colnames(MainData)[c(15,16,18:20,22,25,27)]
  Dataset_Names=paste(Extract_Columns,rep("Dataset",length((Extract_Columns))),sep="_")
  rm(Extract_Columns)
  
  #Initialise the Datasets
  for (i in 1:length(Dataset_Names)) {
    
    if (!(Dataset_Names[i] %in% c("macroscopic_Dataset","organ_weight_Dataset"))) {  
      
      df=data.frame(identifier=character(),
                    study_id=character(),
                    species=character(),
                    type=character(),
                    duration=character(),
                    dose=character(),
                    dose_interval=character(),
                    severity=character(),
                    description=character(), 
                    adversity=character(),
                    reversibility=character(),
                    extra_info=character(),
                    stringsAsFactors = FALSE)
      assign(Dataset_Names[i],df)
      rm(df)
      
      
    } else {
      
      df=data.frame(identifier=character(),
                    study_id=character(),
                    species=character(),
                    type=character(),
                    duration=character(),
                    dose=character(),
                    dose_interval=character(),
                    severity=character(),
                    description=character(), 
                    target=character(),
                    adversity=character(),
                    reversibility=character(),
                    extra_info=character(),
                    stringsAsFactors = FALSE)
      assign(Dataset_Names[i],df)
      rm(df)
    }
    
  }
  rm(Dataset_Names)
  
  
  # Fill the Datasets
  for (i in 1:length(MainData$identifier)) {
    
    #Extra Info needed by the read.entry function
    RO=MainData$identifier[i]
    StudyId=MainData$study_id[i]
    Species=MainData$species[i]
    type=MainData$type[i]
    Duration=as.character(MainData$duration[i]) 
    Dose=as.character(MainData$dose[i])
    Dose_interv=MainData$dose_interval[i]
    
    
    #Append the body_weight_Dataset
    if (grepl("ayes",MainData$body_weight[i])) {
      
      body_weight_Dataset=rbind(body_weight_Dataset,
                                read.entry(MainData$body_weight[i], RO, StudyId, Species, type ,Duration, Dose, Dose_interv))
      MainData$body_weight[i]="ayes"
      
    }else{
      
      if (grepl("yes",MainData$body_weight[i])) {
        
        body_weight_Dataset=rbind(body_weight_Dataset,
                                  read.entry(MainData$body_weight[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
        MainData$body_weight[i]="yes"
      }
    }
    
    
    #Append the body_weight_gain_Dataset
    if (grepl("ayes",MainData$body_weight_gain[i])) {
      
      body_weight_gain_Dataset=rbind(body_weight_gain_Dataset,
                                     read.entry(MainData$body_weight_gain[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
      MainData$body_weight_gain[i]="ayes"
      
    }else{
      
      if (grepl("yes",MainData$body_weight_gain[i])) {
        
        body_weight_gain_Dataset=rbind(body_weight_gain_Dataset,
                                       read.entry(MainData$body_weight_gain[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
        MainData$body_weight_gain[i]="yes"
      }
    }
    
    
    
    #Append the git_clinical_signs_Dataset
    if (grepl("ayes",MainData$git_clinical_signs[i])) {
      
      git_clinical_signs_Dataset=rbind(git_clinical_signs_Dataset,
                                       read.entry(MainData$git_clinical_signs[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
      MainData$git_clinical_signs[i]="ayes"
      
    }else{
      
      if (grepl("yes",MainData$git_clinical_signs[i])) {
        
        git_clinical_signs_Dataset=rbind(git_clinical_signs_Dataset,
                                         read.entry(MainData$git_clinical_signs[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
        MainData$git_clinical_signs[i]="yes"
      }
    }
    
    
    #Append the neurological_clinical_signs_Dataset
    if (grepl("ayes",MainData$neurological_clinical_signs[i])) {
      
      neurological_clinical_signs_Dataset=rbind(neurological_clinical_signs_Dataset,
                                                read.entry(MainData$neurological_clinical_signs[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
      MainData$neurological_clinical_signs[i]="ayes"
      
    }else{
      
      if (grepl("yes",MainData$neurological_clinical_signs[i])) {
        
        neurological_clinical_signs_Dataset=rbind(neurological_clinical_signs_Dataset,
                                                  read.entry(MainData$neurological_clinical_signs[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
        MainData$neurological_clinical_signs[i]="yes"
      }
    }
    
    
    
    #Append the other_clinical_signs_Dataset
    if (grepl("ayes",MainData$other_clinical_signs[i])) {
      
      other_clinical_signs_Dataset=rbind(other_clinical_signs_Dataset,
                                         read.entry(MainData$other_clinical_signs[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
      MainData$other_clinical_signs[i]="ayes"
      
    }else{
      
      if (grepl("yes",MainData$other_clinical_signs[i])) {
        
        other_clinical_signs_Dataset=rbind(other_clinical_signs_Dataset,
                                           read.entry(MainData$other_clinical_signs[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
        MainData$other_clinical_signs[i]="yes"
      }
    }
    
    
    
    #Append the cardiovascular_effects dataset
    if (grepl("ayes",MainData$cardiovascular_effects[i])) {
      
      cardiovascular_effects_Dataset=rbind(cardiovascular_effects_Dataset,
                                           read.entry(MainData$cardiovascular_effects[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
      MainData$cardiovascular_effects[i]="ayes"
      
    }else{
      
      if (grepl("yes",MainData$cardiovascular_effects[i])) {
        
        cardiovascular_effects_Dataset=rbind(cardiovascular_effects_Dataset,
                                             read.entry(MainData$cardiovascular_effects[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv))
        MainData$cardiovascular_effects[i]="yes"
      }
    }
    
    
    #Append the macroscopic_Dataset
    #If there exists one finding that is adverse, in the main Dataset you will have "ayes"
    
    if (grepl("ayes",MainData$macroscopic[i])) {
      
      macroscopic_Dataset=rbind(macroscopic_Dataset,
                                read.entry(MainData$macroscopic[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv,target=MainData$macroscopic_target[i]))
      MainData$macroscopic[i]="ayes"
      
    }else{
      
      if (grepl("yes",MainData$macroscopic[i])) {
        
        macroscopic_Dataset=rbind(macroscopic_Dataset,
                                  read.entry(MainData$macroscopic[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv,target=MainData$macroscopic_target[i]))
        MainData$macroscopic[i]="yes"
      }
    }
    
    
    #Append the organ_weight_Dataset
    #If there exists one finding that is adverse, in the main Dataset you will have "ayes"
    
    if (grepl("ayes",MainData$organ_weight[i])) {
      
      organ_weight_Dataset=rbind(organ_weight_Dataset,
                                 read.entry(MainData$organ_weight[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv,target=MainData$organ_weight_target[i]))
      MainData$organ_weight[i]="ayes"
      
    }else{
      
      if (grepl("yes",MainData$organ_weight[i])) {
        
        organ_weight_Dataset=rbind(organ_weight_Dataset,
                                   read.entry(MainData$organ_weight[i], RO, StudyId, Species, type, Duration, Dose, Dose_interv,target=MainData$organ_weight_target[i]))
        MainData$organ_weight[i]="yes"
      }
    }
  }
  
  #Drop the targets from the main dataset
  drops=c("macroscopic_target","organ_weight_target")
  MainData=MainData[,!(names(MainData) %in% drops)]
  
  ##################################################################################################################################################
  # Read Pathology Dataset

  PathologyData=read.delim(PathologyData_file,sep = " ", dec = ".",header = TRUE, stringsAsFactors = FALSE)
  
  #Delete any whitespaces in the beginning and end of the entries
  PathologyData=apply(PathologyData,2,trimws)
  PathologyData=as.data.frame(PathologyData,stringsAsFactors = FALSE)
  
  #Adding type
  PathologyData=PathologyData %>% add_column(type=MainData$type[match(PathologyData$study_id,MainData$study_id)],.before="duration")
  
  #Replace NA in the recovery column with na
  PathologyData$reversible=replace_na(PathologyData$reversible,"na")
  

  
  #Initialise the dataset
  pathology_Dataset=data.frame(identifier=character(),
                               study_id=character(),
                               species=character(),
                              type=character(),
                               duration=character(),
                               dose=character(),
                               dose_interval=character(),
                               severity=character(),
                               description=character(), 
                               target=character(),
                               adversity=character(),
                               reversibility=character(),
                               extra_info=character(),
                               stringsAsFactors = FALSE)
  
  for (i in 1:length(PathologyData$study_id)) {
    
    pathology_Dataset=rbind(pathology_Dataset,
                            read.pathology.entry(PathologyData[i,]))
    
  }
  rm(PathologyData)
  
  #################################################################################################################################################
  #Define the data list
  Data=list(MainData,body_weight_Dataset,body_weight_gain_Dataset,neurological_clinical_signs_Dataset,
            git_clinical_signs_Dataset,other_clinical_signs_Dataset,macroscopic_Dataset,organ_weight_Dataset,cardiovascular_effects_Dataset, pathology_Dataset)
  names(Data)=c("MainData","body_weight_Dataset","body_weight_gain_Dataset","neurological_clinical_signs_Dataset",
                "git_clinical_signs_Dataset","other_clinical_signs_Dataset","macroscopic_Dataset","organ_weight_Dataset","cardiovascular_effects_Dataset","pathology_Dataset")
  
  
  
  
  ###################################################################################################################################################
  # Define the category that each study belongs to. Possible values are (short, middle, long)
  for (key in names(Data)) {
    
    Data[[key]]$duration = as.numeric(Data[[key]]$duration)
    Data[[key]] = Data[[key]]  %>% mutate(group=cut(duration, breaks=c(0,7,17,60), labels=c("short","middle","long")))
  }
  
  
  #Remove the studies of the RNA molecules
  for (key in names(Data)) {
    
    Data[[key]] = Data[[key]]  %>% filter(type!="rm")
    
  }
  
  #Create the Adjusted_Doses column:
   
  for (key in names(Data)) {
    
    #Make them numerical, solve complex doses and different doses for males/females
    Data[[key]]=ChangeDose_Fun(Data[[key]])
    
    
    #Adjust for dose interval, find the equivalent dose if the dose_interval= day
    Data[[key]]=AdjDose_intervalFun(Data[[key]])
  }
  

  ##################################################################################################################################################
  
  return(Data)
  
  
}



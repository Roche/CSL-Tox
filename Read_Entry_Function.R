# Function for reading one entry of the Main Data
# Input: One entry of the table + additional info for the study
# Output: DataFrame to be rbind in the main dataset
# If the entry is from the macroscopic column or the organ weight column,target is not NA

read.entry <- function(entry, RO, StudyId, Species,type, Duration, Dose, Dose_interv, target=NA) {
  
  # Define all possible options for every category
  Reversibility_Values=c("r", "tr", "nr", "ap_r", "na","r_tr")
  Severity_Values=c("minimal", "mild", "slight", "moderate","marked","severe")
  Second_level_info_Values=c("immunogenicity", "secondary", "stress")
  
  
  #Create the empty dataFrame
  if (any(is.na(target))) {
    
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
    
  }else {
    
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
  }
  
  
  #Split the enrty into the different findings and delete uneccesary gaps in the beginning and the end of each finding
  findings_vector=unlist(strsplit(entry, ","))
  findings_vector=as.character(sapply(findings_vector,trimws))
  
  # If target is not NA, split the targets and delete uneccesary gaps in the beginning and the end of each target organ
  if (!any(is.na(target))) {
    
    target_organs=unlist(strsplit(target,","))
    target_organs=as.character(sapply(target_organs,trimws))
    
  }
  
  #Define number of findings
  numb_find=length(findings_vector)
  
  
  #Start breaking each of the findings
  for (i in 1:numb_find) {
    
    #Define one specific finding
    finding=findings_vector[i]
    
    
    #Split the entries in the finding and delete uneccessary spaces
    finding=unlist(strsplit(finding,"[-,/]"))
    finding=as.character(sapply(finding,trimws))
    
    #Start the row-entry in the dataframe
    df_row=c(RO, StudyId, Species,type, Duration, Dose, Dose_interv)
    
    #Define Adversity
    index=1
    if (finding[index]=="ayes") {
      
      adversity="yes"
      index=index+1
      
    }else if (finding[index]=="yes") {
      
      adversity="no"
      index=index+1
      
    } else {
      
      print(paste("Error in adversity at study", StudyId,"in the dose", Dose ))
      
      
    }
    
    #Define Severity
    if (finding[index] %in% Severity_Values) {
      
      severity=finding[index]
      index=index+1
      
    }else {
      
      severity=NA
      
    }
    
    #Define Description
    description=finding[index]
    index=index+1
    
    #Define Reversibility
    if (finding[index] %in% Reversibility_Values){
      
      if (finding[index]=="r") {
        
        reversibility="Reversible"
        
      } else if (finding[index]=="nr") {
        
        reversibility="Not_Reversible"
        
      } else if (finding[index]=="ap_r") {
        
        reversibility="Appeared_in_Recovery"
        
      } else if (finding[index]=="tr") {
        
        reversibility="Trend_Recovery"
        
      } else if (finding[index]=="na") {
        
        reversibility=NA
        
      } else if (finding[index]=="r_tr") {
        
        reversibility="Trend_Recovery_or_Recovery"
      }
      
      index=index+1
      
    } else {
      
      print(paste("Error in reversibility at study", StudyId,"in the dose", Dose ))
      
    }
    
    
    #Define extra_info  
    if (index==length(finding)) {
      
      extra_info=finding[index]
      
    }else{
      
      extra_info=NA
    }
    
    # Define target if exist
    if (!any(is.na(target))) {
      
      finding_target=target_organs[i]
    
    }
    
    # Combine everything into a new row of the df
    
    if (any(is.na(target))) {
     
       df_row= append(df_row,c(severity,description,adversity,reversibility,extra_info))
      
    }else {
      
      df_row= append(df_row,c(severity,description,finding_target,adversity,reversibility,extra_info))
      
    }
    
    #Add it to the df dataframe
    df_row=data.frame(t(df_row), stringsAsFactors = FALSE)
    names(df_row)=names(df)
    df=rbind(df,df_row)
    rm(df_row)
    
  }
  
  return(df)
  
  
}
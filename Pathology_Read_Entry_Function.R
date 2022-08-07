# Function for reading one entry in the pathology dataset
# Input: One row of the pathology dataset
# Output: DataFrame to be rbind in the main dataset

read.pathology.entry <- function(pathology_row) {
  
  # Define all possible options for every category
  Reversibility_Values=c("r", "tr", "nr", "ap_r", "na","r_tr")
  Severity_Values=c("minimal", "mild", "slight", "moderate","marked","severe")
  Second_level_info_Values=c("immunogenicity", "secondary", "stress")
  
  #Create the empty dataFrame
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
  
  #Split the entry into the different findings and delete uneccesary gaps in the beginning and the end of each finding
  findings_vector=unlist(strsplit(pathology_row$description, ","))
  findings_vector=as.character(sapply(findings_vector,trimws))
  
  #Define number of findings
  numb_find=length(findings_vector)
  
  # Start breaking each of the findings
  for (i in 1:numb_find) {
    
    #Define one specific finding
    finding=findings_vector[i]
    
    #Split the entries in the finding and delete uneccessary spaces
    finding=unlist(strsplit(finding,"[-]"))
    finding=as.character(sapply(finding,trimws))
    
    #Start the row-entry in the dataframe
    df_row=c(pathology_row$identifier,pathology_row$study_id, pathology_row$species, pathology_row$type, pathology_row$duration, pathology_row$dose, pathology_row$dose_interval)
    
    #Define Severity
    index=1
    if (finding[index] %in% Severity_Values) {
      
      severity=finding[index]
      index=index+1
      
    }else {
      
      severity=NA
      
    }
    
    #Define Description
    description=finding[index]
    
    #Change Reversibility
    if (pathology_row$reversible=="r") {
      
      reversibility="Reversible"
      
    } else if (pathology_row$reversible=="nr") {
      
      reversibility="Not_Reversible"
      
    } else if (pathology_row$reversible=="ap_r") {
      
      reversibility="Appeared_in_Recovery"
      
    } else if (pathology_row$reversible=="tr") {
      
      reversibility="Trend_Recovery"
      
    } else if (pathology_row$reversible=="na") {
      
      reversibility=NA
      
    } else if (pathology_row$reversible=="r_tr") {
      
      reversibility="Trend_Recovery_or_Recovery"
    
    } else {
    
      print(paste("Error in reversibility at study", pathology_row$study_id,"in the dose", pathology_row$dose ))
    }
    
    
    #Combine everything into a new row of the df
    df_row= append(df_row,c(severity,description,pathology_row$target_organ,pathology_row$adverse,reversibility,pathology_row$extra_info))
      
    
    #Add it to the df dataframe
    df_row=data.frame(t(df_row), stringsAsFactors = FALSE)
    names(df_row)=names(df)
    df=rbind(df,df_row)
    rm(df_row)
    
  }
  return(df)
  
  
}
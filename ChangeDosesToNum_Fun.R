# Function to change the non-numeric doses to numeric
# resolving cases when the dose is changing or you have different doses for males/females
# When you have different doses for males and females: You always take the smallest dose as the adj_dose

ChangeDose_row=function(dose) {
  
  Not_numeric_flag=grepl("[-,/,&]",dose)
  Sex_flag=grepl("[Male,Female]",dose,ignore.case = TRUE)
  
  if (Not_numeric_flag==FALSE) {
    
    return(round(as.numeric(dose),digits = 2))
    
  } else if (Not_numeric_flag==TRUE & Sex_flag==FALSE) {
    
    dose=unlist(strsplit(dose,"[-,/]"))
    dose=round(as.numeric(dose),digits = 2)
    return(tail(dose,1))
  
  } else if (Not_numeric_flag==TRUE & Sex_flag==TRUE) {
    
    dose=unlist(strsplit(dose,"&"))
    dose=gsub("[male,female]","",dose, ignore.case = TRUE)
    dose=sapply(dose,trimws)
    for ( i in 1:length(dose)) {
      
      temp=unlist(strsplit(dose[i],"[-,/]"))
      dose[i]=tail(temp,1)
    }
    
    dose=round(as.numeric(dose),digits = 2)
    return(min(dose))
  }
  
}

ChangeDose_Fun=function(dataf) {
  
 Adj.dose=as.vector(mapply(ChangeDose_row,as.vector(dataf$dose)))
 dataf = dataf  %>% mutate(adj_dose=Adj.dose, .after=dose)
 
 return(dataf)

}
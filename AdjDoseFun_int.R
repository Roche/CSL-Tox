#Functions to re-adjust the Adj_dose column.

#You need to first call ChangeDosestoNumeric.R
#Each study has a different dose interval. In order to be able to compare doses of different studies, 
#the equivalent dose if the dose interval was "day" is computed.

#When the dosing interval changes during the duration of the study, 
#you keep the last dosing interval 

AdjDoseFun_row=function(dose, dose_interval) {
  
  if (dose_interval=="day") {
    
    return(dose)
    
  }else if (dose_interval=="week"){
    
    return(round(dose/7,digits = 2))
    
  }else if (dose_interval=="qod") {
    
    return(round(dose/2,digits = 2))
  
  }else if (dose_interval=="bi weekly") {
    
    return(round(dose/14,digits = 2))
    
  }else if (dose_interval=="every four days") {
    
    return(round(dose/4,digits = 2))
  
  }else if (dose_interval=="every four weeks") {
    
    return(round(dose/28,digits = 2))
  
  } else if (dose_interval=="twice weekly") {
    
    return(round(dose/4,digits = 2)) 
  
  }else if (dose_interval=="every five days") {
    
    return(round(dose/5,digits = 2))
    
  } else if (dose_interval== "bi weekly / every four weeks") {
    
    return(round(dose/28,digits = 2))
  }
}

AdjDose_intervalFun=function(dataf) {
  
  Adj.dose=mapply(AdjDoseFun_row,as.vector(dataf$adj_dose),as.vector(dataf$dose_interval))
  dataf$adj_dose=Adj.dose
  return(dataf)
  
}



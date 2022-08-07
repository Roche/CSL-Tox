#Input: The list of Dataframes (Data)

#Function: It changes species into "rodents/non-rodents", changes other clinical signs, neurological, GIT, Cardiovascular columns, group 
#the target organs into systems, change macroscopic/pathology/organ weight findings, merge the weight and weight gain

#Output: The list of the Dataframes after the transformation



ChangeData= function(Data) {
  
  #Change the dataset's species to rodent/non-rodent
  for (key in names(Data)) {
    
    Data[[key]]$species[Data[[key]]$species=="cynomolgus"]="non-rodent"
    Data[[key]]$species[Data[[key]]$species=="minipig"]="non-rodent"
    Data[[key]]$species[Data[[key]]$species=="marmoset"]="non-rodent"
    Data[[key]]$species[Data[[key]]$species=="dog"]="non-rodent"
    Data[[key]]$species[Data[[key]]$species=="rat"]="rodent"
    Data[[key]]$species[Data[[key]]$species=="mouse"]="rodent"
  }
  
  ######################################################################################################################
  #Change the neurological dataset according to controlled terminology
  Data$neurological_clinical_signs_Dataset$description[
    Data$neurological_clinical_signs_Dataset$description=="tremor" | 
    Data$neurological_clinical_signs_Dataset$description=="convulsions"]="tremor/convulsions"
  
  Data$neurological_clinical_signs_Dataset$description[
    Data$neurological_clinical_signs_Dataset$description=="hypoactivity" | 
    Data$neurological_clinical_signs_Dataset$description=="hyperactivity"]="hypoactivity/hyperactivity"
  
  Data$neurological_clinical_signs_Dataset$description[
    Data$neurological_clinical_signs_Dataset$description=="abnormal gait" | 
    Data$neurological_clinical_signs_Dataset$description=="abnormal posture"]="abnormal gait/posture"
  
  
  #Change the other_clinical signs dataset
  Data$other_clinical_signs_Dataset$description[
    Data$other_clinical_signs_Dataset$description=="skin thickening" | 
    Data$other_clinical_signs_Dataset$description=="skin lesions" | 
    Data$other_clinical_signs_Dataset$description=="skin discoloration" |
    Data$other_clinical_signs_Dataset$description=="skin infection"]="skin lesions"
  
  Data$other_clinical_signs_Dataset$description[
    Data$other_clinical_signs_Dataset$description=="partially closed or closed eyes"]="morbidity"
  
  Data$other_clinical_signs_Dataset$description[
    Data$other_clinical_signs_Dataset$description=="ERG changes" | 
    Data$other_clinical_signs_Dataset$description=="ocular examination changes" | 
    Data$other_clinical_signs_Dataset$description=="OCT changes"]="ocular effects"
  
  #Change GIT dataset
  Data$git_clinical_signs_Dataset$description=rep("Gastrointestinal clinical signs",
                                                  length(Data$git_clinical_signs_Dataset$description))
  
  #Change cardiovascular dataset
  Data$cardiovascular_effects_Dataset$description=rep("In life cardiovascular effects",
                                                      length(Data$cardiovascular_effects_Dataset$description))
  
  ##################################################################################################################################################
  #Group target organs and change the findings in organ weight/macroscopic and pathology Dataset
  
  #Define the Systems and put all the different organs seen in the dataset to one system
  Nervous_System=c("nerve,tibial", "nerve,trigeminal", "ganglion,dorsal root", "hypoglossal nerve", "glossopharyngeal nerve", "brain", "nerve,peroneal",
                   "nerve,sural", "spinal cord,cervical", "spinal cord,thoracic", "ganglion,cervical", "sciatic nerve", "spinal cord",
                   "central nervous system","peroneal nerve", "sural nerve","choroid plexus","hypothalamus", "nervous system")
  
  GI_tract=c("stomach", "gastrointestinal tract", "jejunum", "ileum", "esophagus", "colon", "tooth",
             "hard palate", "oral cavity", "tongue", "gland,brunner's", "gallbladder","abdomen","gall bladder","mesentery","large intestine",
             "small intestine", "cecum", "duodenum","rectum","abdomen","retroperitoneum", "gi tract")
  
  Reproductive_System=c("prostate gland", "testis", "vagina", "male reproductive system", "ovary", "uterus",
                        "epididymis","female reproductive system","sex organ", "preputial gland", "clitoral gland", "seminal vesicle","reproductive system")
  
  MuscularSkeletal_System=c("muscle,skeletal", "bone", "sternum", "shoulder", "axilla","tail","tarsus bone","axillary","flank",
                            "neck","skeletal muscle", "ankle", "knee","hindlimb", "femur joint","stifle joint",
                            "thigh muscle", "knee joint","femur", "muscularskeletal system")
  
  Endocrine_System=c("thyroid gland", "adrenal gland", "parathyroid gland", "pancreas", "pituitary gland","endocrine system")
  
  Exocrine_System=c("mammary gland", "salivary gland,sublingual", "gland,lacrimal", "parotid gland", "salivary gland",
                    "submandibular gland", "gland,harderian", "prostate gland", "exocrine system")
  
  Respiratory_system=c("lung", "nasal turbinate", "larynx","pleural cavities", "trachea","respiratory system")
  
  Cutaneous=c("skin","injection site", "adipose tissue, brown","mesenteric adipose tissue","most tissues","brown adipose tissue","cutaneous")
  
  Eye_conjuctiva=c("eye", "conjunctiva", "eye/conjuctiva")
  
  Urinary_system=c("bladder", "urinary system", "ureter", "kidney","urinary system")
  
  Lymphoid_Tissues=c("axillary lymph node", "tonsil", "submandibular lymph node", "lymphoid tissues", "lymph node", "mesenteric lymph node", "spleen",
                     "bone marrow", "thymus gland", "mandibular lymph node","inguinal lymph node","lymphoid tissues")
  
  Cardiovascular_System=c("heart","injection vein", "aorta","cardiovascular system")
  
  Whole_Body=c("terminal body","whole body")
  
 
  #Look only macroscopic pathology, microscopic pathology and organ weights 
  Datasets_To_look=names(Data)[c(7,8,10)]
  for (key in Datasets_To_look) {
    
    Data[[key]]$target[Data[[key]]$target %in% Nervous_System]="Nervous System"
    Data[[key]]$target[Data[[key]]$target %in% GI_tract]="GI tract"
    Data[[key]]$target[Data[[key]]$target %in% Reproductive_System]="Reproductive System"
    Data[[key]]$target[Data[[key]]$target %in% MuscularSkeletal_System]="MuscularSkeletal System"
    Data[[key]]$target[Data[[key]]$target %in% Endocrine_System]="Endocrine System"
    Data[[key]]$target[Data[[key]]$target %in% Exocrine_System]="Exocrine System"
    Data[[key]]$target[Data[[key]]$target %in% Respiratory_system]="Respiratory System"
    Data[[key]]$target[Data[[key]]$target %in% Cutaneous]="Cutaneous"
    Data[[key]]$target[Data[[key]]$target %in% Eye_conjuctiva]="Eye/conjuctiva"
    Data[[key]]$target[Data[[key]]$target %in% Urinary_system]="Urinary System"
    Data[[key]]$target[Data[[key]]$target %in% Lymphoid_Tissues]="Lymphoid Tissues"
    Data[[key]]$target[Data[[key]]$target %in% Cardiovascular_System]="Cardiovascular System"
    Data[[key]]$target[Data[[key]]$target %in% Whole_Body]="Whole body"
  }
  
  # Change the findings: Transform all the findings as organ "system/Category"
  Data$macroscopic_Dataset$description=paste(Data$macroscopic_Dataset$target,"macroscopic pathology",sep="/")
  Data$organ_weight_Dataset$description=paste(Data$organ_weight_Dataset$target,"organ weight change",sep="/")
  Data$pathology_Dataset$description=paste(Data$pathology_Dataset$target,"microscopic pathology",sep="/ ")
  ###############################################################################################################################################
  #Merge weight and weight gain
  #By the merging, you have some almost identical rows but since appearance of a finding is what you are looking at, 
  #it doesn't matter. For a different analysis you may have to solve these cases
  
  dataf=rbind(Data$body_weight_Dataset,Data$body_weight_gain_Dataset)
  
  #Remove identical rows because of the rbind of the two datasets
  dataf=distinct(dataf)
  
  #Update body_weight_Dataset
  Data$body_weight_Dataset=dataf
  Data$body_weight_Dataset$description=paste("weight", Data$body_weight_Dataset$description,sep=" ")
  
  #Remove body_weight_gain_Dataset
  Data[which(names(Data)=="body_weight_gain_Dataset")] <- NULL
  ##############################################################################################################################################
  #Further group the findings belonging to neurological and other clinical signs and the findings belonging to weight changes.
  #For the most general high level terms
  
  Data$neurological_clinical_signs_Dataset$description="Neurological clinical signs"
  Data$other_clinical_signs_Dataset$description="Other clinical signs"
  Data$body_weight_Dataset$description="Weight changes"
  
  return(Data)
}


#Keep in all datasets only the adverse findings
AdverseData.fn=function(Data) {
  
  for (key in names(Data)[-1]) { 
    
    Data[[key]]= Data[[key]] %>% filter(adversity=="yes")
  }
  return(Data)
}
  












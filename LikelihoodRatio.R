#Find the positive likelihood ratio LR+ and the inverse negative likelihood ratio -iLR for a specific finding.

#Positive likelihood ratios indicate a higher likelihood that the long-term finding will be observed if
#the short/middle finding is observed 

#The negative likelihood (LR-) refers to the likelihood that  long-term findings will not be observed
#if no short/middle term finding is observed. 

# ***
#!! You may get Nan in the likelihood ratios because of 0/0
#***

Likelihood.Ratio.fn<- function(dataf) {

  #Remove drugs that do not have long study or short/middle
  # For those studies, in all the findings you will have NA
  dataf= dataf %>% filter(!is.na(long.cond) & !is.na(short_middle.cond))

  
  #Create TP, TN, FP, FN columns
  Likel.Ratio= dataf %>% group_by(finding) %>% summarise(TP=sum(short_middle.cond==TRUE & long.cond==TRUE),
                                                      FP=sum(short_middle.cond==TRUE & long.cond==FALSE),
                                                      FN=sum(short_middle.cond==FALSE & long.cond==TRUE),
                                                      TN=sum(short_middle.cond==FALSE & long.cond==FALSE),
                                                      Sensitivity=TP/(TP+FN),
                                                      Specificity=TN/(TN+FP),
                                                      LR_pos=Sensitivity/(1-Specificity),
                                                      iLR_neg=1/((1-Sensitivity)/Specificity),
                                                      p_value=fisher.test(matrix(c(TP, FP, FN, TN), nrow = 2, ncol = 2),
                                                                          alternative = "greater",
                                                                          conf.int = TRUE, conf.level = 0.95)$p.value
                                                      #PPV_TPR=TP/(TP+FP),
                                                      #NPV=TN/(TN+FP),
                                                      #FNR=FN/(FN+TN)
                                                      )
  
  return(Likel.Ratio)
}






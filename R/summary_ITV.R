############################################################################################################################
#Function to calculate standart deviation, mean, median, interquartile range, coefficient of variation, and robust coefficient of variation
#for a manually selected column containing trait values and by manually selected aggregators (e.g. trait, species)
#############################################################################################################################

#Data reqirements:
#Dataframe containing trait data per species and trait in "long table" format

#Authors:
#Susanne Tautenhahn, Gabriel Walther

summary_ITV <- function(Data_ITV, aggregate_by = "StdValue", aggregators = c("TraitID", "TraitName", "AccSpeciesName_short")){
  
  aggregatelist <- list()
  for (i in 1:length(aggregators)){
    aggregatelist <- list.append(aggregatelist, Data_ITV[, names(Data_ITV) == aggregators[i]])
  }
  
  
  Data_ITV <- Data_ITV[!is.na(Data_ITV[, names(Data_ITV) == aggregate_by]), ]
  
  #standart deviation
  Finalsd <- aggregate(Data_ITV[, names(Data_ITV) == aggregate_by], by = aggregatelist,
                       FUN = sd)
  names(Finalsd) <- c(aggregators, "sd")
  
  #mean
  Finalmean <- aggregate(Data_ITV[, names(Data_ITV) == aggregate_by], by = aggregatelist,
                         FUN = mean)
  names(Finalmean) <- c(aggregators, "mean")
  
  #median
  Finalmedian <- aggregate(Data_ITV[, names(Data_ITV) == aggregate_by], by = aggregatelist,
                           FUN = median)
  names(Finalmedian) <- c(aggregators, "median")
  
  #IQR
  Finaliqr <- aggregate(Data_ITV[, names(Data_ITV) == aggregate_by], by = aggregatelist,
                        FUN = IQR)
  names(Finaliqr) <- c(aggregators, "iqr")
  
  # combine dataframes
  #-------------------
  Final_ges <- cbind(Finalsd, "mean" = Finalmean$mean, 
                     "median" = Finalmedian$median,
                     "iqr" = Finaliqr$iqr)
  Final_ges$cv <- Final_ges$sd / Final_ges$mean
  Final_ges$robustcv <- Final_ges$iqr / Final_ges$median
  Final_ges <- droplevels(Final_ges)
  
  return(Final_ges)
}

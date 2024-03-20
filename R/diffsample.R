#' Create a sample of multiple ages
#'
#' This function creates a sample of people using a count variable as the number to sample.
#' Sampling without replacement is used
#'
#' @export
#' @param people A data frame containing observations
#' @param pplage The column of the data frame that contains the ages.
#' @param sampledf A data frame containing the age and sample size values
#' @param smplage The column containing the ages in the sample information data frame
#' @param smplcounts The column containing the sample size in the sample information data frame
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

#' @return A data frame of people sampled according to the age sample sizes required.
#' 
#' 
diffsample <- function(people, pplage, sampledf, smplage, smplcounts, userseed = NULL) {
  
  # need to do the name change here
  
  peopleRenamed <- people %>%
    rename(AgeVar = !! pplage)
  
  sampleRenamed <- sampledf %>%
    rename(AgeVar = !! smplage,
           NumToSample = !! smplcounts)
  
  AgeColName <- sym(names(people[pplage]))
  
  if (!is.null(userseed)) {
    set.seed(userseed)
  }
  
  for (i in 1:nrow(sampleRenamed)) {
    
    currentAgeRow <- sampleRenamed[i,]
    
    currentAge <- as.numeric(currentAgeRow$AgeVar)
    currentSampleSize <- as.numeric(currentAgeRow$NumToSample)

    currentSample <- peopleRenamed %>%
      filter(AgeVar == currentAge)
    
    if(currentSampleSize > nrow(currentSample)) {
      
      currentSampleSize <- nrow(currentSample)
      
      # closes if(currentSampleSize > nrow(currentSample))
    }
    
    sampledAge <- currentSample %>%
      slice_sample(n = currentSampleSize, replace = FALSE)
    

    if(exists("FinalDF")) {
      
      FinalDF <- bind_rows(FinalDF, sampledAge)
      
      # closes if(exists(FinalDataset))
    } else {
      
      FinalDF <- sampledAge
      
      # closes else to  if(exists(FinalDataset)) 
    }
    
    # closes for (i in 1:nrow(sampleRenamed))
  }
  
  # create output dataframe and output
  OutputDataFrame <- FinalDF %>%
    rename(!!quo_name(pplage) := AgeVar)
  

  return(OutputDataFrame)
  # closes function
}
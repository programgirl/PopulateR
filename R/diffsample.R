#' Sample from groups, when the sample size for each group is different.
#'
#' This function produces samples by group, enabling different sample sizes to be specified for each group. Sampling without replacement is used. While the function example is based on sampling by age, in practice sampling can be performed using any variable of choice. Only one grouping variable is used.
#' 
#'
#' @export
#' @param people A data frame containing individual people.
#' @param pplage The variable containing the ages, in the people data frame. 
#' @param sampledf A data frame containing ages and sample size counts.
#' @param smplage The variable containing the ages, in the sampledf data frame.
#' @param smplcounts The variable containing the sample size counts, in the sampledf data frame.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.

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
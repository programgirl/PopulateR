#' Construct pairs of people into couples with a household identifier.
#' This function creates a data frame of couples, based on a distribution of age differences. The function uses a log-normal distribution.
#' Two data frames are required. One person from each data frame will be matched, based on the age difference distribution specified. If the data frames are different sizes, the smalldf data frame must be the smaller of the two.In this situation, a random subsample of the largedf data frame will be used.
#' Both data frames must be restricted to only those people that will have a couples match performed.
#' @export
#' @param smalldf A data frame containing one set of observations to be paired.
#' @param smlidcol The ID variable in the smalldf data frame.
#' @param smlagecol The age variable in the smalldf data frame.
#' @param largedf A data frame containing the second set of observations to be paired.
#' @param lrgidcol The ID variable in the largedf data frame.
#' @param lrgagecol The age variable in the largedf data frame.
#' @param meanvalue The log mean value to use. If this value is negative, the smaller data frame holds the oldest ages. If this value is positive, the smaller data frame holds the youngest ages.
#' @param sdvalue the log standard deviation to use.
#' @param IDStartValue The starting number for generating the household identifier value that identifies a couple. Must be numeric.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param ptostop The critical p-value stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param numiters The maximum number of iterations used to construct the coupled data frame. The default value is 10, and is the stopping rule if that number of attempts at matching has failed to create a match.

#' @return A list of two data frames $Matched contains the data frame of pairs. $Unmatched contains the unmatched observations from largedf. If there are no unmatched people, $Unmatched will be an empty data frame.
#'
#' @examples
#' library(dplyr)
#'
#' set.seed(1)
#' PartneredFemales <- Township %>%
#'   filter(Sex == "Female", Relationship == "Partnered")
#' PartneredMales <- Township %>%
#'   filter(Sex == "Male", Relationship == "Partnered")
#'
#' # partners females and males, using a normal distribution, with the females
#' being younger by a mean of -2 and a standard deviation of 3
#' OppSexCouples <- couples(PartneredFemales, smlidcol=3, smlagecol=4,
#'                          PartneredMales, lrgidcol=3, lrgagecol=4, meanvalue = -2,
#'                          sdvalue = 3, IDStartValue = 100, HouseholdNumVariable="HouseholdID",
#'                          userseed = 4, ptostop=.01,  numiters=1000000)


pairlns <- function(smalldf, smlid, smlage, largedf, lrgid, lrgage, meanvalue=NULL, sdvalue=NULL, 
                   IDStartValue, HouseholdNumVariable, userseed=NULL, ptostop=NULL, numiters=10) {
  
  
  
  
  
  #####################################
  # check for missing input information
  #####################################
  
  # tests for first data frame
  if (!smlid %in% names(smalldf)) {
    stop("The ID variable in the first data frame does not exist.")
  }
  
  if (!smlage %in% names(smalldf)) {
    stop("The age variable in the first data frame does not exist.")
  }
  
  
  # tests for second data frame
  if (!lrgid %in% names(largedf)) {
    stop("The ID variable in the second data frame does not exist.")
  }
  
  if (!lrgage %in% names(largedf)) {
    stop("The age variable in the second data frame does not exist.")
  }
  
  
  if(is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
  }
  
  if(is.null(sdvalue) | sdvalue < 0) {
    stop("The mean age difference must be greater than zero.")
  }
  
  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################
  
  # smalldf ID variable
  smlidcolName <- sym(names(smalldf[smlid]))
  # smalldf age variable
  smlagecolName <- sym(names(smalldf[smlage]))
  
  # largedf ID variable
  lrgidcolName <- sym(names(largedf[lrgid]))
  # largedf age variable
  lrgagecolName <- sym(names(largedf[lrgage]))
  
  #####################################
  #####################################
  # end column names
  #####################################
  #####################################
  
  # more testing
  
  if (!any(duplicated(smalldf[[smlidcolName]])) == FALSE) {
    stop("The ID variable in the first data frame has duplicated values.")
  }
  
  if (!any(duplicated(largedf[[lrgidcolName]])) == FALSE) {
    stop("The ID variable in the second data frame has duplicated values.")
  }
  
  if (!is.numeric(smalldf[[smlagecolName]])) {
    stop("The age variable in the first data frame is not numeric.")
  }
  
  if (!is.numeric(largedf[[lrgagecolName]])) {
    stop("The age variable in the first second frame is not numeric.")
  }
  

  # create internal data frames
  smlRenamed <- smalldf %>%
    rename(smallAge = !! smlagecolName,
           smallID = !! smlidcolName)
  
  lrgRenamed <- largedf %>%
    rename(largeAge = !! lrgagecolName,
           largeID = !! lrgidcolName)
  
  
 
  #####################################
  #####################################
  # set up pre-data information for matching
  #####################################
  #####################################

  # make sure the function is working with a positive meanlog
  if(meanvalue < 0) {
    posMeanValue <- abs(meanvalue)
  } else {
    posMeanValue <- meanvalue
    
    # closes  if(meanvalue < 0) {
  }
  
  # cycle through smalldf
  # if meanvalue is <0 then smalldf has the smallest ages
  # check for seed
  if (!is.null(userseed)) {
    set.seed(userseed)
  }
  
  currentHHID <- IDStartValue
  
  #####################################
  #####################################
  # perform the matching
  #####################################
  #####################################
  
  for(i in 1:nrow(smlRenamed)) {
    
    counter <- 0
   
    # get current smalldf age
    currentSml <- smlRenamed[i,]
    currentAge <- currentSml$smallAge
    
    # try to match
    while(!(counter == numiters)) {
      
      # create an age difference based on the distribution
      drawResult <- round(rlnorm(1, meanlog = posMeanValue, sdlog = sdvalue),0)
      
      # required age of older person
      if(meanvalue < 0) {
       reqAge <- currentAge - drawResult
      } else {
        reqAge <- currentAge + drawResult
        # closes if(meanvalue < 0) {
      }
      
      # generate random sample of people that age from the largedf
      lrgSubset <- lrgRenamed %>%
        filter(largeAge == reqAge)
      
      if(nrow(lrgSubset) > 0) {
        lrgChosen <- lrgSubset %>%
          slice_sample(n = 1)
        
        counter <- numiters
        
        isMatched <- "Y"
        
      } else {
        
        counter <- counter + 1
        
        isMatched <- "N"
        
        # closes if(nrow(lrgSubset) > 0) {
      }
      
      # closes  while(counter <= numiters) {
    }
    
    if(isMatched == "Y") {
      
      matchedSml <- currentSml %>%
        select(smallID) %>%
        mutate(internalHHID = currentHHID)
      
      matchedLrg <- lrgChosen %>%
        select(largeID) 
      
      matchedSmFull <- left_join(matchedSml, smlRenamed, by = ("smallID"))  %>%
        filter(smallID == matchedSml$smallID) 
      
      matchedLrgFull <- lrgRenamed %>%
        filter(largeID == matchedLrg$largeID) %>%
        mutate(internalHHID = currentHHID)
      
      
      # keep putting the matches into a single data frame
      # only need the one test, if the smaller data frame is matched, so is the larger one
      if(exists("FullMatchedSml") == TRUE) {
        
        FullMatchedSml <- bind_rows(FullMatchedSml, matchedSmFull)
        FullMatchedLrg <- bind_rows(FullMatchedLrg, matchedLrgFull)
        
      } else {
        
        FullMatchedSml <- matchedSmFull
        FullMatchedLrg <- matchedLrgFull
        
        # closes if(exists("FullMatchedSml") == TRUE) {
      }
      
      
      # removed matched person from being reselected from largerdf
      lrgRenamed <- lrgRenamed %>%
        filter(!(largeID == matchedLrg$largeID))
      
      
      currentHHID <- currentHHID + 1
      
      
      # closes if(isMatched == "Y") {
    }
    
    
    # closes  for(i in 1:nrow(youngRenamed)) {
  }
  

  
  #####################################
  #####################################
  # iteration for matching pair ages ends here
  #####################################
  #####################################
  
  # prep the two data frames for output
  # need to do the renames separately
  
  FullMatchedSml <- FullMatchedSml %>%
    rename(!! smlidcolName := smallID,
           !! smlagecolName := smallAge,
           {{HouseholdNumVariable}} := internalHHID)
  
  FullMatchedLrg <- FullMatchedLrg %>%
    rename(!! lrgidcolName := largeID,
           !! lrgagecolName := largeAge,
           {{HouseholdNumVariable}} := internalHHID)
  
  
  OutputDataframe <- rbind(FullMatchedSml, FullMatchedLrg)

  cat("The individual dataframes are $Matched, $Smaller, and $Larger", "\n")
  cat("$Smaller contains unmatched observations from the smaller data frame", "\n")
  cat("$Larger contains unmatched observations from the larger data frame", "\n")

MatchedIDs <- OutputDataframe %>%
  pull({{lrgidcolName}})

noSmalls <- largedf %>%
  filter(!({{lrgidcolName}} %in% MatchedIDs))

noLarge <- smalldf %>%
  filter(!({{smlidcolName}} %in% MatchedIDs))


MergedList <- list()

MergedList$Matched <- OutputDataframe
MergedList$Smaller <- noLarge
MergedList$Larger <- noSmalls

return(MergedList)

}

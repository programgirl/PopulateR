#' Construct pairs of people, when one data frame contains a household identifier.
#' This function creates a data frame of pairs, based on a distribution of age differences. The function will use either a skew normal or normal distribution, depending on whether a skew ("locationP") parameter is provided. The default value for the skew is 0, and using the default will cause a normal distribution to be used.
#' Two data frames are required. One person from each data frame will be matched, based on the age difference distribution specified. If the data frames are different sizes, the smalldf data frame must be the smaller of the two. In this situation, a random subsample of the largedf data frame will be used. 
#' The household identifier variable can exist in either data frame. The function will apply the relevant household identifier once each pair is constructed.
#' Both data frames must be restricted to only those people that are successfully paired.
#' @export
#' @param smalldf A data frame containing one set of observations to be paired.
#' @param smlid The column number for the ID variable in the smalldf data frame.
#' @param smlage The column number for the age variable in the smalldf data frame.
#' @param largedf A data frame containing the second set of observations to be paired.
#' @param lrgid The column number for the ID variable in the largedf data frame.
#' @param lrgage The column number for the age variable in the largedf data frame.
#' @param shapeA This is the first shape parameter of the four-parameter beta distribution If this value is negative, the smaller data frame holds the oldest ages. If this value is positive, the smaller data frame holds the youngest ages.
#' @param shapeB This is the second shape parameter of the four-parameter beta distribution This value must be positive.
#' @param locationP The location parameter of the four-parameter beta distribution
#' @param scaleP The scale parameter of the four-parameter beta distribution
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param attempts The maximum number of times the large data frame will be sampled to draw an age match from the correct distribution, for each observation in the small data frame. The default number of attempts is 10.

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
#' OppSexCouples <- couples(PartneredFemales, smlid=3, smlage=4,
#'                          PartneredMales, lrgid=3, lrgage=4, shapeA = -2,
#'                          directomega = 3, IDStartValue = 100, HouseholdNumVariable="Househlrgid",
#'                          userseed = 4, ptostop=.01,  attempts=1000000)


pair4PbetaNum <- function(smalldf, smlid, smlage, largedf, lrgid, lrgage, shapeA=NULL, shapeB=NULL, locationP=NULL, 
                       scaleP = NULL, HouseholdNumVariable, userseed=NULL, attempts=10) {
  
  
  
  
  
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
  
  if(shapeB < 0) {
    stop("shapeB must be greater than zero.")
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
  
  smlRenamed <- smalldf %>%
    rename(smallAge = !! smlagecolName,
           smallID = !! smlidcolName)
  
  lrgRenamed <- largedf %>%
    rename(largeAge = !! lrgagecolName,
           largeID = !! lrgidcolName)
  
  
  
  #####################################
  #####################################
  # which data frame contains the household identifier?
  # and then add the identifier as an variable to that data frame
  #####################################
  #####################################

   # print(HouseholdNumVariable %in% names(smalldf))
   # print(HouseholdNumVariable %in% names(largedf))
  
  if(HouseholdNumVariable %in% names(smalldf)) {
    
    HHIDcolName <- sym(names(smalldf[HouseholdNumVariable]))

    smlRenamed <- smlRenamed %>%
      rename(smallHHID = !! HHIDcolName)

    WhereHHID <- "Small"

    # cat("Household ID is in smalldf \n")

  } else {
    
    HHIDcolName <- sym(names(largedf[HouseholdNumVariable]))

    lrgRenamed <- lrgRenamed %>%
      rename(largeHHID = !! HHIDcolName)

    WhereHHID <- "Large"
    
    # cat("Household ID is in largedf \n")

  }

   
   
  
  #####################################
  #####################################
  # set up pre-data information for matching
  #####################################
  #####################################
  
  # make sure the function is working with a positive meanlog
  if(shapeA < 0) {
    posShapeA <- abs(shapeA)
  } else {
    posShapeA <- shapeA
    
    # closes  if(meanvalue < 0) {
  }
  
  # cycle through smalldf
  # if meanvalue is <0 then smalldf has the smallest ages
  # check for seed
  if (!is.null(userseed)) {
    set.seed(userseed)
  }
  

  #####################################
  #####################################
  # perform the matching
  #####################################
  #####################################
  
  # NumAttempts <- 0
  
  for(i in 1:nrow(smlRenamed)) {
    
    counter <- 0
    
    # get current young person age
    currentSml <- smlRenamed[i,]
    currentAge <- currentSml$smallAge
    
    # try to match
    
    while(!(counter == attempts)) {
      
      # create an age difference based on the distribution
      drawResult <- round(PearsonDS::rpearsonI(1, a=posShapeA, b=shapeB, location=locationP, scale=scaleP),0)
      
      # NumAttempts <- NumAttempts + 1
      
      # required age of older person
      if(shapeA < 0) {
        reqAge <- currentAge - drawResult
      } else {
        reqAge <- currentAge + drawResult
        # closes if(meanvalue < 0) {
      }
      
      
      # generate random sample of people that age from the smalldf
      lrgSubset <- lrgRenamed %>%
        filter(largeAge == reqAge)
      
      if(nrow(lrgSubset) > 0) {
        lrgChosen <- lrgSubset %>%
          slice_sample(n = 1)
        
        counter <- attempts
        
        isMatched <- "Y"
        
      } else {
        
        counter <- counter + 1
        
        isMatched <- "N"
        
        # closes if(nrow(lrgSubset) > 0) {
      }
      
      # closes  while(counter <= attempts) {
    } 
    
    if(isMatched == "Y") {
      
      # cat("Is matched \n")

      if(WhereHHID == "Small") {
        
        # cat("HHID in small df \n")
        
        currentHHID <- currentSml$smallHHID
        
        matchedSml <- currentSml %>%
          select(smallID)
        
        matchedLrg <- lrgChosen %>%
          select(largeID) 
        
        matchedSmFull <- left_join(matchedSml, smlRenamed, by = ("smallID"))  %>%
          filter(smallID == matchedSml$smallID) %>%
          rename(internalHHID = smallHHID)
        
        matchedLrgFull <- lrgRenamed %>%
          filter(largeID == matchedLrg$largeID) %>%
          mutate(internalHHID = currentHHID)
        
        # closes if(WhereHHID == "Small") {
      } else {
        
        # cat("HHID in large df \n")
        
        matchedSml <- currentSml %>%
          select(smallID)
        
        currentHHID <- lrgChosen$largeHHID
        
        matchedLrg <- lrgChosen %>%
          select(largeID) 
        
        matchedSmFull <- left_join(matchedSml, smlRenamed, by = ("smallID"))  %>%
          filter(smallID == matchedSml$smallID) %>%
          mutate(internalHHID = currentHHID)
        
        matchedLrgFull <- lrgRenamed %>%
          filter(largeID == matchedLrg$largeID) %>%
          rename(internalHHID = largeHHID)
        
        # closes else to if(WhereHHID == "Small") {
      }
      
      
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
      

      #######################################################################################
      # Start of first set of chi-squared output addition
      #######################################################################################
      
      if(exists("CurrentAgeMatch")) {
        
        smlSummaryData <- matchedSmFull %>%
          select(smallID, smallAge)
        lrgSummaryData <- matchedLrgFull %>%
          select(largeID, largeAge)
        interimAgeMatch <- bind_cols(smlSummaryData, lrgSummaryData)
        CurrentAgeMatch <- bind_rows(CurrentAgeMatch, interimAgeMatch)
        
        
      } else {
        
        smlSummaryData <- matchedSmFull %>%
          select(smallID, smallAge)
        lrgSummaryData <- matchedLrgFull %>%
          select(largeID, largeAge)
        CurrentAgeMatch <- bind_cols(smlSummaryData, lrgSummaryData)
        
        }
      
      #######################################################################################
      # End of first set of chi-squared output addition
      #######################################################################################
      
      
      # removed matched person from being reselected from largerdf
      lrgRenamed <- lrgRenamed %>%
        filter(!(largeID == matchedLrg$largeID))

      
      # closes if(isMatched == "Y") {
    }
    
    
    # closes  for(i in 1:nrow(youngRenamed)) {
  }

  
  
  #####################################
  #####################################
  # iteration for matching pair ages ends here
  #####################################
  #####################################
  
  #####################################
  #####################################
  # Only do the chi-squared output if at least 30 people
  # can't have too many 0 cells or it fails
  #####################################
  #####################################
  
  #######################################################################################
  # Start of second set of chi-squared output addition
  #######################################################################################
  
  if(nrow(CurrentAgeMatch) >= 30) {
  
  min_bin <- round(PearsonDS::qpearsonI(1/100000, a=posShapeA, b=shapeB, location=locationP, scale=scaleP))-0.5
  max_bin <- round(PearsonDS::qpearsonI(1-(1/100000), a=posShapeA, b=shapeB, location=locationP, scale=scaleP))+0.5
  
  # cat("Error when trying to bin", "\n")
  
  bins <- c(min_bin:max_bin)
  # cat("The bins are", "\n")
  # print(bins)
  
  # cat("Error after making the bins", "\n")
  
  # construct the probabilities for each bin, gives n(bins)-1
  Probabilities <- PearsonDS::ppearsonI(bins[-1], a=posShapeA, b=shapeB, location=locationP, scale=scaleP) -
    PearsonDS::ppearsonI(bins[-length(bins)], a=posShapeA, b=shapeB, location=locationP, scale=scaleP)
  # cat("The probabilities are", "\n")
  # print(Probabilities)
  
  logProb <- c(log(Probabilities))
  logBins <- c(min_bin:max_bin)
  # cat("The logProbs are", "\n")
  # print(logProb)
  # cat("The logBins are", "\n")
  # print(logBins)
  
  ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
  logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))
  # cat("The ExpectedAgeProbs are", "\n")
  # print(ExpectedAgeProbs)
  # cat("The logEAgeProbs are", "\n")
  # print(logEAgeProbs)
  
  # construct starting set of observed age difference values for iteration
  if(shapeA < 0) {
    
    ObservedAgeDifferences <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,4], breaks = bins, plot=FALSE)$counts
    log0ObservedAges <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,4], breaks = logBins, plot=FALSE)$counts
    
  } else {
    
    ObservedAgeDifferences <- hist(CurrentAgeMatch[,4] - CurrentAgeMatch[,2], breaks = bins, plot=FALSE)$counts
    log0ObservedAges <- hist(CurrentAgeMatch[,4] - CurrentAgeMatch[,2], breaks = logBins, plot=FALSE)$counts
    
  }
  
  # cat("The ObservedAgeDifferences are", length(ObservedAgeDifferences), "\n")
  # print(ObservedAgeDifferences)
  # cat("The log0ObservedAges are", length(log0ObservedAges), "\n")
  # print(log0ObservedAges)
  
  logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs, log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
  log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))
  
  
  Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
  
  cat("Current chi-squared value is", round(log_chisq,3), "and critical chi-squared value is", 
      round(Critical_log_chisq,3), "for p = 0.01", "\n")
  
  # closes if(nrow(CurrentAgeMatch) >= 30)
  }
  
  #######################################################################################
  # End of second set of chi-squared output addition
  #######################################################################################
  
  #######################################################################################
  #######################################################################################
  # prep the two data frames for output
  # need to do the renames separately
  # rename depends on whether the small or large df contained the HH ID variable
  #######################################################################################
  #######################################################################################
  

  FullMatchedSml <- FullMatchedSml %>%
    rename(!! smlidcolName := smallID,
           !! smlagecolName := smallAge,
           !! HHIDcolName := internalHHID)
  
  FullMatchedLrg <- FullMatchedLrg %>%
    rename(!! lrgidcolName := largeID,
           !! lrgagecolName := largeAge,
           !! HHIDcolName := internalHHID)

  #######################################################################################
  # produce output
  #######################################################################################
  
  OutputDataframe <- rbind(FullMatchedSml, FullMatchedLrg)
  
  # print(NumAttempts)
  
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

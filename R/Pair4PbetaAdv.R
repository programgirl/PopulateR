#' Pairs people from smalldf with people from largedf, creating couples in households.
#' This function creates a data frame of paired people, based on a distribution of age differences. The function uses a four-parameter beta distribution to create the pairs.
#' Two data frames are required. One person from each data frame will be matched, based on the age difference distribution specified. If the data frames are different sizes, the smalldf data frame must be the smaller of the two. In this situation, a random subsample of the largedf data frame will be used.
#' Both data frames must be restricted to only those people that will be paired.
#' @export
#' @param smalldf The data frame containing one set of people to be paired. If the two data frames contain different numbers of people, this must be the data frame containing the smallest number.
#' @param smlid The variable containing the unique ID for each person, in the smalldf data frame.
#' @param smlage The age variable, in the smalldf data frame.
#' @param largedf A data frame containing the second set of people to be paired. If the two data frames contain different numbers of people, this must be the data frame containing the largest number.
#' @param lrgid The variable containing the unique ID for each person, in the largedf data frame.
#' @param lrgage The age variable, in the largedf data frame.
#' @param shapeA This is the first shape parameter of the four-parameter beta distribution If this value is negative, smalldf has the oldest ages. If this value is positive, smalldf has the youngest ages.
#' @param shapeB This is the second shape parameter of the four-parameter beta distribution This value must be positive.
#' @param locationP The location parameter of the four-parameter beta distribution.
#' @param scaleP The scale parameter of the four-parameter beta distribution.
#' @param HHStartNum The starting value for HHNumVar. Must be numeric.
#' @param HHNumVar The column name for the household variable.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#' @param ptostop The critical p-value stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param attempts The maximum number of times largedf will be sampled to draw an age match from the correct distribution, for each observation in the smalldf. The default number of attempts is 10.
#' @param numiters The maximum number of iterations used to construct the output data frame ($Matched) containing the pairs. The default value is 1000000, and is the stopping rule if the algorithm does not converge.

#' @return A list of three data frames. $Matched contains the data frame of pairs. $Smaller contains the unmatched observations from smalldf. $Larger contains the unmatched observations from largedf.
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
#'                          directomega = 3, HHStartNum = 100, HHNumVar="Househlrgid",
#'                          userseed = 4, ptostop=.01,  numiters=1000000)


pair4PbetaAdv <- function(smalldf, smlid, smlage, largedf, lrgid, lrgage, shapeA=NULL, shapeB=NULL, locationP=NULL, 
                        scaleP = NULL, HHStartNum, HHNumVar, userseed=NULL, ptostop=NULL, attempts=10,
                        numiters=1000000) {
  
  
  
  
  
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
  
  
  if(is.null(HHNumVar)) {
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
    stop("The ID variable in the small data frame has duplicated values.")
  }
  
  if (!any(duplicated(largedf[[lrgidcolName]])) == FALSE) {
    stop("The ID variable in the large data frame has duplicated values.")
  }
  
  if (!is.numeric(smalldf[[smlagecolName]])) {
    stop("The age variable in the small data frame is not numeric.")
  }
  
  if (!is.numeric(largedf[[lrgagecolName]])) {
    stop("The age variable in the large data frame is not numeric.")
  }
  
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
  
  # perform the matching
  
  currentHHID <- HHStartNum
  
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
      drawResult <- round(PearsonDS::rpearsonI(1, a=shapeA, b=shapeB, location=locationP, scale=scaleP),0)
      
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
      
      matchedSml <- currentSml %>%
        select(smallID) 
      
      matchedLrg <- lrgChosen %>%
        select(largeID) 
      
      matchedSmFull <- left_join(matchedSml, smlRenamed, by = ("smallID"))  %>%
        filter(smallID == matchedSml$smallID) 
      
      matchedLrgFull <- lrgRenamed %>%
        filter(largeID == matchedLrg$largeID) 
      

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
        
        # closes else to  if(exists("CurrentAgeMatch"))
      }

      # remove matched person from being reselected from largerdf
      lrgRenamed <- lrgRenamed %>%
        filter(!(largeID == matchedLrg$largeID))

      
      # closes if(isMatched == "Y") {
    } else {
      # just draw a sample that is between the correct ages.
      
      minAgeNeeded <- currentAge + 
        round(PearsonDS::qpearsonI(1/100000, a=posShapeA, b=shapeB, location=locationP, scale=scaleP))
      
      maxAgeNeeded <- currentAge + 
        round(PearsonDS::qpearsonI(1-(1/100000), a=posShapeA, b=shapeB, location=locationP, scale=scaleP))
        
      lrgSubset <- lrgRenamed %>%
        filter(between(largeAge, minAgeNeeded, maxAgeNeeded))
      
      if(nrow(lrgSubset) > 0) {
        lrgChosen <- lrgSubset %>%
          slice_sample(n = 1)
        
        # do the extraction work here only if there is a matching person drawn
        matchedSml <- currentSml %>%
          select(smallID) 
        
        matchedLrg <- lrgChosen %>%
          select(largeID) 
        
        matchedSmFull <- left_join(matchedSml, smlRenamed, by = ("smallID"))  %>%
          filter(smallID == matchedSml$smallID) 
        
        matchedLrgFull <- lrgRenamed %>%
          filter(largeID == matchedLrg$largeID) 
        
        
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
          
          # closes else to  if(exists("CurrentAgeMatch"))
        }
        
        # remove matched person from being reselected from largerdf
        lrgRenamed <- lrgRenamed %>%
          filter(!(largeID == matchedLrg$largeID))
        
        # closes if(nrow(lrgSubset) > 0) {
      }
      
      # closes else to if(isMatched == "Y") {
    } 
    
    # closes  for(i in 1:nrow(youngRenamed)) {
  }
  

  #######################################################################################
  # calculation of key values for chi-square testing
  #######################################################################################

  min_bin <- round(PearsonDS::qpearsonI(1/100000, a=posShapeA, b=shapeB, location=locationP, scale=scaleP))-0.5
  max_bin <- round(PearsonDS::qpearsonI(1-(1/100000), a=posShapeA, b=shapeB, location=locationP, scale=scaleP))+0.5
  
  # cat("Error when trying to bin", "\n")
  
  bins <- c(min_bin:max_bin)
  # cat("The bins are", "\n")
  # print(bins)

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
  
  if (is.null(ptostop)) {
    
    Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
    
  } else {
    
    Critical_log_chisq <- log(qchisq(ptostop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
    
  }
  
  cat("Current chi-squared value is", round(log_chisq,3), "and critical chi-squared value is", 
      round(Critical_log_chisq,3), "\n")
  
  #####################################
  #####################################
  #####################################
  #####################################
  # pairing swap subfunction
  
 
  swap_largedf <- function(pair1, pair2) {
    swap <- pair1
    swap$largeID <- pair2$largeID
    swap$largeAge <- pair2$largeAge
    return(swap)
  }
  
  # #####################################
  # # chi-squared check subfunction
  #####################################
  
  compare_logK <- function(prop, curr) {
    # what we want to do is know if sum(exp(prop)) > sum(exp(curr))
    # but we can't work out exp(prop) or exp(curr) during the process..
    
    # to do this, we first eliminate those that don't matter
    w = prop != curr
    if (sum(w) == 0) {
      return(0) # no change
    }
    prop = prop[w]
    curr = curr[w]
    
        # next we find which is the dominant exponent, as changes these are all that will matter
        # i.e. we write exp(a) + exp(b) = exp(a)[1 + exp(b-a)] where a > b, so that the additional terms are less than 1
        # and we can exponentiate them safely. We then ignore the base (it's common) and just use extras
    base <- max(prop, curr)
    prop = prop - base
    curr = curr - base
    sum(exp(prop)) - sum(exp(curr))
  }
  
  #####################################
  #####################################
  # sub functions end
  #####################################
  #####################################
  
  #####################################
  #####################################
  # swapping
  #####################################
  #####################################
  
  if(log_chisq > Critical_log_chisq) {

  
  for (i in 1:numiters) {
    
    # print(i)
    
    # randomly choose two pairs
    Pick1 <- sample(nrow(CurrentAgeMatch), 1)
    Pick2 <- sample(nrow(CurrentAgeMatch), 1)
    Current1 <- CurrentAgeMatch[Pick1,]
    Current2 <- CurrentAgeMatch[Pick2,]
    
    # # proposed pairing after a swap
    PropPair1 <- swap_largedf(Current1, Current2)
    PropPair2 <- swap_largedf(Current2, Current1)
    

    # check for swap that creates an age difference out of bounds
    if(shapeA < 0) {
      
      if((PropPair1$smallAge - PropPair1$largeAge < min_bin) | 
         (PropPair2$smallAge - PropPair2$largeAge < min_bin) |
         (PropPair1$smallAge - PropPair1$largeAge > max_bin) | 
         (PropPair2$smallAge - PropPair2$largeAge > max_bin)) {
        
        next
      }
      
    } else {
      
      if((PropPair1$largeAge - PropPair1$smallAge < min_bin) | 
         (PropPair2$largeAge - PropPair2$smallAge < min_bin) |
         (PropPair1$largeAge - PropPair1$smallAge > max_bin) | 
         (PropPair2$largeAge - PropPair2$smallAge > max_bin)) {
        
        next
      }
      
      # closes if(shapeA < 0) {
    }
    
    
    # compute change in Chi-squared value from current pairing to proposed pairing

    PropAgeMatch <- CurrentAgeMatch %>%
      # filter(!(smalldf[[smlidcolName]] %in% c(PropPair1[,1], PropPair2[,1]))) %>%
      filter(!(smallID %in% c(PropPair1[,1], PropPair2[,1]))) %>%
      bind_rows(., PropPair1,PropPair2)
    
    # cat("PropAgeMatch done", "\n")
    
    # do chi-squared
    if(shapeA < 0) {
      
      Proplog0 <- hist(PropAgeMatch[,2] - PropAgeMatch[,4], breaks = logBins, plot=FALSE)$counts
      
    } else {
      
      Proplog0 <- hist(PropAgeMatch[,4] - PropAgeMatch[,2], breaks = logBins, plot=FALSE)$counts
      
      # closes if(shapeA < 0) {
    }

    
    ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs
    
    prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))
    
    if (compare_logK(ProplogK, logKObservedAges) < 0) { # we cancel out the bits that haven't changed first.
      
      CurrentAgeMatch[Pick1,] <- PropPair1
      CurrentAgeMatch[Pick2,] <- PropPair2
      
      
      log0ObservedAges <- Proplog0
      logKObservedAges <- ProplogK
      log_chisq <- prop_log_chisq
      
      # print(prop_log_chisq)
      
    }
    
    if (log_chisq <= Critical_log_chisq) {
      break
      
    }
    
    # closes for (i in 1:numiters)
  }
   
    cat(i, "iterations were used, and the final chi-squared value was", round(log_chisq,3), "\n")
    # closes if(log_chisq > Critical_log_chisq) {
  }
  

  #####################################
  #####################################
  # iteration for matching pair ages ends here
  #####################################
  #####################################
  
  # add the household ID
  MaxHHStartNum <- (nrow(CurrentAgeMatch)-1) + HHStartNum
  
  CurrentAgeMatch <- CurrentAgeMatch %>%
    mutate(internalHHID = seq(HHStartNum, MaxHHStartNum))


  # prep the two data frames for output
  # need to do the renames separately
  
  FullMatchedSml <- CurrentAgeMatch %>%
    select(smallID, smallAge, internalHHID) %>%
    rename(!! smlidcolName := smallID,
           !! smlagecolName := smallAge,
           {{HHNumVar}} := internalHHID) %>%
    left_join(smalldf)
  
  FullMatchedLrg <- CurrentAgeMatch %>%
    select(largeID, largeAge, internalHHID) %>%
    rename(!! lrgidcolName := largeID,
           !! lrgagecolName := largeAge,
           {{HHNumVar}} := internalHHID) %>%
    left_join(largedf)

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

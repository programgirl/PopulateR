#' Construct pairs of people into couples with a household identifier.
#' This function creates a data frame of couples, based on a distribution of age differences. The function will use either a skew normal or normal distribution, depending on whether a skew ("locationP") parameter is provided. The default value for the skew is 0, and using the default will cause a normal distribution to be used.
#' Two data frames are required. One person from each data frame will be matched, based on the age difference distribution specified. If the data frames are different sizes, the OlderDF data frame must be the smaller of the two.In this situation, a random subsample of the YoungDF data frame will be used.
#' Both data frames must be restricted to only those people that will have a couples match performed.
#' @export
#' @param OlderDF A data frame containing one set of observations to be paired.
#' @param oldid The column number for the ID variable in the OlderDF data frame.
#' @param oldpage The column number for the age variable in the OlderDF data frame.
#' @param YoungDF A data frame containing the second set of observations to be paired.
#' @param yngid The column number for the ID variable in the YoungDF data frame.
#' @param yngage The column number for the age variable in the YoungDF data frame.
#' @param shapeA If a skew-normal distribution is used, this is the location value. If the default locationP value of 0 is used, this defaults to the mean value for the normal distribution.
#' @param shapeB If a skew-normal distribution is used, this is the scale value. If the default locationP value of 0 is used, this defaults to the standard deviation value for the normal distribution.
#' @param locationP The skew. If a normal distribution is to be used, this can be omitted as the default value is 0 (no skew).
#' @param IDStartValue The starting number for generating the household identifier value that identifies a couple. Must be numeric.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#' @param ptostop The critical p-value stopping rule for the function. If this value is not set, the critical p-value of .01 is used.
#' @param numiters The maximum number of iterations used to construct the coupled data frame. The default value is 1000000, and is the stopping rule if the algorithm does not converge.

#' @return A list of two data frames $Matched contains the data frame of pairs. $Unmatched contains the unmatched observations from YoungDF. If there are no unmatched people, $Unmatched will be an empty data frame.
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
#' OppSexCouples <- couples(PartneredFemales, oldid=3, oldage=4,
#'                          PartneredMales, yngid=3, yngage=4, shapeA = -2,
#'                          directomega = 3, IDStartValue = 100, HouseholdNumVariable="Househyngid",
#'                          userseed = 4, ptostop=.01,  numiters=1000000)


pair4PbetaSwaps <- function(OlderDF, oldid, oldage, YoungDF, yngid, yngage, shapeA=NULL, shapeB=NULL, locationP=NULL, 
                       scaleP = NULL, IDStartValue, HouseholdNumVariable, userseed=NULL, ptostop=NULL, 
                       numiters=1000000) {
  
  
  
  
  
  #####################################
  # check for missing input information
  #####################################
  
  # tests for first data frame
  if (!oldid %in% names(OlderDF)) {
    stop("The ID variable in the first data frame does not exist.")
  }
  
  if (!oldage %in% names(OlderDF)) {
    stop("The age variable in the first data frame does not exist.")
  }
  
  
  # tests for second data frame
  if (!yngid %in% names(YoungDF)) {
    stop("The ID variable in the second data frame does not exist.")
  }
  
  if (!yngage %in% names(YoungDF)) {
    stop("The age variable in the second data frame does not exist.")
  }
  
  
  if(is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
  }
  
  if(shapeA < 0) {
    stop("shapeA must be greater than zero.")
  }
  
  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################
  
  # OlderDF ID variable
  oldidcolName <- sym(names(OlderDF[oldid]))
  # OlderDF age variable
  oldagecolName <- sym(names(OlderDF[oldage]))
  
  # YoungDF ID variable
  yngidcolName <- sym(names(YoungDF[yngid]))
  # YoungDF age variable
  yngagecolName <- sym(names(YoungDF[yngage]))
  
  #####################################
  #####################################
  # end column names
  #####################################
  #####################################
  
  # more testing
  
  if (!any(duplicated(OlderDF[[oldidcolName]])) == FALSE) {
    stop("The ID variable in the first data frame has duplicated values.")
  }
  
  if (!any(duplicated(YoungDF[[yngidcolName]])) == FALSE) {
    stop("The ID variable in the second data frame has duplicated values.")
  }
  
  if (!is.numeric(OlderDF[[oldagecolName]])) {
    stop("The age variable in the first data frame is not numeric.")
  }
  
  if (!is.numeric(YoungDF[[yngagecolName]])) {
    stop("The age variable in the first second frame is not numeric.")
  }
  
  #####################################
  #####################################

  
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
    
    #     # next we find which is the dominant exponent, as changes these are all that will matter
    #     # i.e. we write exp(a) + exp(b) = exp(a)[1 + exp(b-a)] where a > b, so that the additional terms are less than 1
    #     # and we can exponentiate them safely. We then ignore the base (it's common) and just use extras
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
  # set up pre-data information for matching
  #####################################
  #####################################
  

  # set up bins for iterations
  # enable at least some extreme age differences to be assigned to the Inf categories
  # otherwise the bins will be wrong

    # MaxAgeDifference <-  (max(YoungDF[[yngagecolName]]) -
    #                         min(OlderDF[[oldagecolName]]))-5
  
  MaxAgeDifference <-  (max(OlderDF[[oldagecolName]]) -
                          min(YoungDF[[yngagecolName]]))-5
  
  NegMaxAgeDiff <- -MaxAgeDifference
  
  # cat("NegMaxAgeDiff is", NegMaxAgeDiff, "\n")

   # cat("Starting the bins", "\n")
  
  # estimate expected minimum and maximum ages from the distribution, and bin these

    min_bin <- round(PearsonDS::qpearsonI(0.000001, a=shapeA, b=shapeB, location=locationP, scale=scaleP))-0.5
    max_bin <- round(PearsonDS::qpearsonI(0.999999, a=shapeA, b=shapeB, location=locationP, scale=scaleP))+0.5
    
         # cat("Error when trying to bin", "\n")
    # if(NegMaxAgeDiff < min_bin) {
    # 
    #   min_bin <- NegMaxAgeDiff - 0.5
    # }
    
    
    bins <- c(-9999, min_bin:max_bin, 9999)
    

    # cat("Error after making the bins", "\n")
    
    # construct the probabilities for each bin, gives n(bins)
    Probabilities <- PearsonDS::ppearsonI(bins[-1], a=shapeA, b=shapeB, location=locationP, scale=scaleP) -
      PearsonDS::ppearsonI(bins[-length(bins)], a=shapeA, b=shapeB, location=locationP, scale=scaleP)


    # cat("Error after making the probabilities", "\n")
    
    # assign realistic expected probabilities in the bins outside the bins constructed earlier
    # use minAge and maxAge for this, only need range for included ages
    # Uses midpoint rule.
    
    # TODO: choose sensible values for -MaxAgeDifference and MaxAgeDifference that cover
    # the range of the age (differences?!?) that are going to be possible here.
    smallest_age_difference <- min(min_bin-0.5, -MaxAgeDifference)
    largest_age_difference  <-  max(max_bin-0.5, MaxAgeDifference)
    
    replacement_inf_logprob <- min(log(Probabilities[Probabilities > 0]))*10
    
    
    if (smallest_age_difference < min_bin - 0.5) {
      logProbLow <- PearsonDS::dpearsonI(smallest_age_difference:(min_bin-0.5), a=shapeA, b=shapeB, location=locationP, 
                                         scale=scaleP, log=TRUE)
      
      # cat("Original logProbLow is", "\n")
      # print(logProbLow)
      
      realLogProbLowValues <- logProbLow[!is.infinite(logProbLow)]
      
      if (length(realLogProbLowValues) > 0) {
      # logProbLow[is.infinite(logProbLow)] <- replacement_inf_logprob
      logProbLow <- c(rev(seq_along(logProbLow[is.infinite(logProbLow)])*10*replacement_inf_logprob),
                      realLogProbLowValues) 
      
      } else {
        logProbLow <- rev(seq_along(logProbLow[is.infinite(logProbLow)])*10*replacement_inf_logprob) 
        
        # closes if (length(realLogProbLowValues) > 0) {
      }
      
    } else {
      logProbLow = c()
      
      # closes if (smallest_age_difference < min_bin - 0.5) 
    }
    
   

    if (largest_age_difference > max_bin + 0.5) {
      logProbHigh <- PearsonDS::dpearsonI((max_bin+0.5):largest_age_difference, a=shapeA, b=shapeB, location=locationP, 
                                          scale=scaleP, log=TRUE)
      
      # cat("Original logProbHigh is", "\n")
      # print(logProbHigh)
      # logProbHigh[is.infinite(logProbHigh)] <- replacement_inf_logprob
      realLogProbHighValues <- logProbHigh[!is.infinite(logProbHigh)]

      if(length(realLogProbHighValues) > 0) {
      
      logProbHigh <-c(realLogProbHighValues, seq_along(logProbHigh[is.infinite(logProbHigh)])*10*replacement_inf_logprob)
      
      } else {
        
        logProbHigh <-seq_along(logProbHigh[is.infinite(logProbHigh)])*10*replacement_inf_logprob
        
        # closes if(length(realLogProbHighValues) > 0) {
      }
      
    } else {
      logProbHigh = c()
      
      # closes if (largest_age_difference > max_bin + 0.5) {
    }

    # cat("logProbLow is", "\n")
    # print(logProbLow)
    # cat("Probabilities are", "\n")
    # print(Probabilities)
    # cat("logProbHigh is", "\n")
    # print(logProbHigh)
    
    logProb <- c(logProbLow, log(Probabilities[-c(1, length(Probabilities))]), logProbHigh)
    
    # print(logProb)

    logBins <- c(-Inf, (smallest_age_difference + 0.5):(largest_age_difference-.5), Inf)
    
    # print(logBins)
    
    if (length(logBins) != length(logProb) + 1) {
      stop("logBins is not one longer than logProb, stuff will break...")
    }

    # cat("Error after making the logProb and logBins", "\n")
    

  
  #####################################
  #####################################
  # end set up
  #####################################
  #####################################
  
  
  #####################################
  #####################################
  # create initial age matches
  #####################################
  #####################################
  # this is a random sample so age differences will not follow desired distribution
  # however, if the YoungDF data frame is larger than the OlderDF data frame
  # this ensures that a random selection of YoungDFs has the correct count
  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  # set up the sampling from the larger df
    
    # cat("YoungDF data frame is", nrow(YoungDF), "long, and OlderDF dataframe has", nrow(OlderDF), "rows", "\n")
    
   

      #TODO: the sampling occurs on the other dataset so the columns have different names
      # the dataset order is set by which dataset is younger versus older
      # thus, the order of the datasets cannot change, so either can be larger
      # need to account for the younger dataset being the one that is sampled.
      
  #     # get counts for each single age from the YoungDF data frame
  #     YoungDFCounts <- YoungDF %>%
  #       group_by(across(!!yngagecolName)) %>%
  #       summarise(AgeCount=n())
  #     
  #     YoungDFAges <- pull(YoungDFCounts[1])
  #     YoungDFAgeCounts <- pull(YoungDFCounts[2])
  #   
  # CurrentAgeMatch <- OlderDF %>%
  #   select(!!oldidcolName, !!oldagecolName) %>%
  #   rename(OlderDFAge = !! oldagecolName,
  #          OlderDFID = !! oldidcolName) %>%
  #   mutate(YoungDFAge = sample(rep(YoungDFAges, YoungDFAgeCounts),
  #                              size=nrow(OlderDF),
  #                              replace = FALSE))
  # 
  # IDType <- "Older"
  
  # NEED TO DUPLICATE EVERYTHING THAT GOES IN BELOW

  # closes if (nrow(YoungDF) > nrow(OlderDF))
    # } else {
      
      OlderDFCounts <- OlderDF %>%
        group_by(across(!!oldagecolName)) %>%
        summarise(AgeCount=n())


      OlderDFAges <- pull(OlderDFCounts[1])
      OlderDFAgeCounts <- pull(OlderDFCounts[2])
      

      CurrentAgeMatch <- YoungDF %>%
        select(!!yngidcolName, !!yngagecolName) %>%
        rename(YoungDFAge = !! yngagecolName,
               YoungDFID = !! yngidcolName) %>%
        mutate(OlderDFAge = sample(rep(OlderDFAges, OlderDFAgeCounts),
                                 size=nrow(YoungDF),
                                 replace = FALSE))
      
      #####################################
      #####################################
      # pairing swap subfunction
      
      swap_the_ages <- function(pair1, pair2) {
        swap <- pair1
        swap$YoungDFID <- pair2$YoungDFID
        swap$YoungDFID <- pair2$YoungDFID
        return(swap)
      }

      # set up for chi-squared test
      ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)
      
      logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))
      
     
      # construct starting set of observed age difference values for iteration
      ObservedAgeDifferences <- hist(CurrentAgeMatch[,3] - CurrentAgeMatch[,2], breaks = bins, plot=FALSE)$counts

      # set up for chi-squared
      log0ObservedAges <- hist(CurrentAgeMatch[,3] - CurrentAgeMatch[,2], breaks = logBins, plot=FALSE)$counts

      
      # cat("MaxAgeDifference is", MaxAgeDifference, "min_bin is",min_bin, "max_bin is", max_bin, "bins is", length(bins), "\n")
      # cat("Probabilities is", length(Probabilities), "logProbLow is", length(logProbLow), "logProbHigh is", length(logProbHigh), "logProb is", length(logProb), "logBins is", length(logBins), "\n")
      # cat("CurrentAgeMatch is", length(CurrentAgeMatch), "ExpectedAgeProbs is", length(ExpectedAgeProbs), 
      #     "logEAgeProbs is", length(logEAgeProbs), "log0ObservedAges is", length(log0ObservedAges), "\n")
      # cat("bins are", "\n")
      # print(bins)
      # cat("LogProbLow is", "\n")
      # print(logProbLow)
      # cat("Probabilities are", "\n")
      # print(Probabilities)
      # cat("logProbHigh is", "\n")
      # print(logProbHigh)
      # cat("logProb is", "\n")
      # print(logProb)
      # cat("logBins are", "\n")
      # print(logBins)

      logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs, log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
      

     log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))
      
      if (is.null(ptostop)) {
        
        Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
        
      } else {
        
        Critical_log_chisq <- log(qchisq(ptostop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
        
      }

      
      #####################################
      #####################################
      # iteration for matching children ages starts here
      #####################################
      #####################################
      
      for (i in 1:numiters) {

        
        # randomly choose two pairs
        Pick1 <- sample(nrow(CurrentAgeMatch), 1)
        Pick2 <- sample(nrow(CurrentAgeMatch), 1)
        Current1 <- CurrentAgeMatch[Pick1,]
        Current2 <- CurrentAgeMatch[Pick2,]
        
        # proposed pairing after a swap
        PropPair1 <- swap_the_ages(Current1, Current2)
        PropPair2 <- swap_the_ages(Current2, Current1)

        # compute change in Chi-squared value from current pairing to proposed pairing
        PropAgeMatch <- CurrentAgeMatch %>%
          filter(!(YoungDF[[yngid]] %in% c(PropPair1[,1], PropPair2[,1]))) %>%
          bind_rows(., PropPair1,PropPair2)
        
        # do chi-squared
        Proplog0 <- hist(PropAgeMatch[,3] - PropAgeMatch[,2], breaks = logBins, plot=FALSE)$counts
        

        ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs
        
        prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))
        
        if (compare_logK(ProplogK, logKObservedAges) < 0) { # we cancel out the bits that haven't changed first.
          
          CurrentAgeMatch[Pick1,] <- PropPair1
          CurrentAgeMatch[Pick2,] <- PropPair2
          
          
          log0ObservedAges <- Proplog0
          logKObservedAges <- ProplogK
          log_chisq <- prop_log_chisq
          
        }
        
        if (log_chisq <= Critical_log_chisq) {
          break
          
        }
        
        print(log_chisq)
        # closes for (i in 1:numiters)
      }
      
      #####################################
      #####################################
      # iteration for matching children ages ends here
      #####################################
      #####################################
      
      
      #####################################
      #####################################
      # pairing the actual children starts here
      #####################################
      #####################################
      # return full YoungDF and OlderDF rows as matched household pairs

     # OlderDF work
     # extract ages counts for matching the OlderDF
     MatchedOlderDFAges <- CurrentAgeMatch %>%
       dplyr::select(OlderDFAge) %>%
       group_by(OlderDFAge) %>%
       mutate(OlderDFAgeCount = row_number()) %>%
       ungroup()
     
         #   # generate same AgeCount second ID variable for the OlderDF data
     #   # the AgeCount is used to ensure that the first OlderDF with a specific age is matched first
     #   # the second OlderDF with a specific age is matched second
     #   # and so forth
     OlderDFToMatch <- OlderDF %>%
       group_by(!!oldagecolName) %>%
       mutate(OlderDFAgeCount = row_number()) %>%
       ungroup()


     # do the rename first and then join
     OlderDFToMatch <- OlderDFToMatch %>%
      rename(OlderDFAge = {{oldagecolName}}) %>%
      mutate(InsideOlderDFAge = OlderDFAge)
     
     OlderDFMatched <- left_join(MatchedOlderDFAges, OlderDFToMatch, by = c("OlderDFAge", "OlderDFAgeCount")) %>%
       mutate(!!oldagecolName := OlderDFAge)
     
     # construct same file for the YoungDF
     # need both YoungDF age and YoungDF age count so that the join between the YoungDF and the OlderDF works
     # do not need YoungDF age as this will be a duplicate column on the merge
     
     YoungDFMatchPrep <- CurrentAgeMatch %>%
       group_by(OlderDFAge) %>%
       mutate(OlderDFAgeCount = row_number()) %>%
       rename(!!yngidcolName := YoungDFID) %>%
       dplyr::select(-c(2))
     
     
    YoungDFReadyToMatch <- suppressMessages(left_join(YoungDF, YoungDFMatchPrep)) %>%
      mutate(InsideYoungDFAge = {{yngagecolName}})
    

      MaxIDStartValue <- (nrow(YoungDFReadyToMatch)-1) + IDStartValue
      
      
      
      FullMatchedDataFrame <- left_join(OlderDFMatched, YoungDFReadyToMatch, by=c("OlderDFAge", "OlderDFAgeCount")) %>%
        dplyr::select(-OlderDFAge, -OlderDFAgeCount) %>%
        ungroup() %>%
        mutate(AgeDifference = InsideOlderDFAge - InsideYoungDFAge) %>%
        filter(between(AgeDifference, min_bin, max_bin))  #%>%
     #   mutate({{HouseholdNumVariable}} := seq(IDStartValue, MaxIDStartValue))
        
        return(FullMatchedDataFrame)

      # convert from wide to long, use .x and .y to do the split
      
      FirstDataframeSplit <- FullMatchedDataFrame %>%
        dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
        rename_all(list(~gsub("\\.x$", "", .)))
      
      SecondDataframeSplit <- FullMatchedDataFrame %>%
        dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
        rename_all(list(~gsub("\\.y$", "", .)))
      
      OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)

      # #####################################
      # #####################################
      # # pairing the actual couples ends here
      # #####################################
      # #####################################
      
      # use for checking number of iterations used, the p-value to stop, and the p-value reached
      #
      cat("The number of iterations used was", i, "\n")
      # print(Critical_log_chisq)
      # print(log_chisq)
      
      # if(nrow(YoungDF) > nrow(OlderDF)) {
      #
      #   cat("The second dataframe contained more observations than the first dataframe.", "\n")
      #   cat("A merged dataframe has been returned as a list.", "\n")
      cat("The individual dataframes are $Matched and $Unmatched.", "\n")
      
      MatchedIDs <- OutputDataframe %>%
        pull({{oldidcolName}})
      
      UnmatchedDataframe <- OlderDF %>%
        filter(!({{oldidcolName}} %in% MatchedIDs)) #%>%
      # mutate({{HouseholdNumVariable}} = NA)
      
      MergedList <- list()
      
      MergedList$Matched <- OutputDataframe
      MergedList$Unmatched <- UnmatchedDataframe
      
      return(MergedList)

      # closes else to if (nrow(YoungDF) > nrow(OlderDF))
    # }
    

    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  # set up for chi-squared test
  ExpectedAgeProbs <- Probabilities * nrow(CurrentAgeMatch)

  logEAgeProbs <- logProb + log(nrow(CurrentAgeMatch))

  # construct starting set of observed age difference values for iteration
  ObservedAgeDifferences <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = bins, plot=FALSE)$counts
  
  # set up for chi-squared
  log0ObservedAges <- hist(CurrentAgeMatch[,2] - CurrentAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
  logKObservedAges = ifelse(log0ObservedAges == 0, 2*logEAgeProbs, log((log0ObservedAges - exp(logEAgeProbs))^2)) - logEAgeProbs
  log_chisq = max(logKObservedAges) + log(sum(exp(logKObservedAges - max(logKObservedAges))))
  
  if (is.null(ptostop)) {
    
    Critical_log_chisq <- log(qchisq(0.01, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
    
  } else {
    
    Critical_log_chisq <- log(qchisq(ptostop, df=(length(logEAgeProbs-1)), lower.tail = TRUE))
    
  }
  
  #####################################
  #####################################
  # iteration for matching couple ages starts here
  #####################################
  #####################################
  
  for (i in 1:numiters) {
    
    # print(i)
    
    # randomly choose two pairs
    Pick1 <- sample(nrow(CurrentAgeMatch), 1)
    Pick2 <- sample(nrow(CurrentAgeMatch), 1)
    Current1 <- CurrentAgeMatch[Pick1,]
    Current2 <- CurrentAgeMatch[Pick2,]
    
    # proposed pairing after a swap
    PropPair1 <- swap_YoungDF(Current1, Current2)
    PropPair2 <- swap_YoungDF(Current2, Current1)
    
    # compute change in Chi-squared value from current pairing to proposed pairing
    PropAgeMatch <- CurrentAgeMatch %>%
      filter(!(smalldf[[smlidcolName]] %in% c(PropPair1[,1], PropPair2[,1]))) %>%
      bind_rows(., PropPair1,PropPair2)
    
    # do chi-squared
    Proplog0 <- hist(PropAgeMatch[,2] - PropAgeMatch[,3], breaks = logBins, plot=FALSE)$counts
    ProplogK = ifelse(Proplog0 == 0, 2*logEAgeProbs, log((Proplog0 - exp(logEAgeProbs))^2)) - logEAgeProbs

    
    prop_log_chisq = max(ProplogK) + log(sum(exp(ProplogK - max(ProplogK))))
    
    if (compare_logK(ProplogK, logKObservedAges) < 0) { # we cancel out the bits that haven't changed first.
      
      CurrentAgeMatch[Pick1,] <- PropPair1
      CurrentAgeMatch[Pick2,] <- PropPair2
      
      
      log0ObservedAges <- Proplog0
      logKObservedAges <- ProplogK
      log_chisq <- prop_log_chisq
      
    }
    
    if (log_chisq <= Critical_log_chisq) {
      break
      
    }
    
  }
  
  #####################################
  #####################################
  # iteration for matching couple ages ends here
  #####################################
  #####################################
  
  
  #####################################
  #####################################
  # pairing the actual couples starts here
  #####################################
  #####################################
  # return full YoungDF and OlderDF rows as matched household pairs
  # extract ages counts for matching the YoungDFs
  MatchedYoungDFids <- CurrentAgeMatch %>%
    dplyr::select(YoungDFid) %>%
    group_by(YoungDFid) %>%
    mutate(YoungDFidCount = row_number()) %>%
    ungroup()
  
  print(706)
  return(MatchedYoungDFids)
  
  #   # generate same AgeCount second ID variable for the YoungDF data
  #   # the AgeCount is used to ensure that the first YoungDF with a specific age is matched first
  #   # the second YoungDF with a specific age is matched second
  #   # and so forth
  YoungDFsToMatch <- YoungDF %>%
    group_by(!!yngagecolName) %>%
    mutate(YoungDFidCount = row_number()) %>%
    ungroup()
  
  
  # reduce pool of potentially partnered YoungDFs to only those matched to OlderDFs
  # YoungDFsMatched <- left_join(MatchedYoungDFids,
  #                              rename_at(YoungDFsToMatch, {{yngagecolName}}, ~ names(MatchedYoungDFids)[1]),
  #                              by = c(names(MatchedYoungDFids)[1], "YoungDFidCount")) %>%
  #   mutate(!!yngagecolName := YoungDFid)
  
  # rename_at no longer working
  # do the rename first and then join
  print(727)
  
  YoungDFsToMatch <- YoungDFsToMatch %>%
    rename(YoungDFid = {{yngagecolName}})
  
  YoungDFsMatched <- left_join(MatchedYoungDFids, YoungDFsToMatch, by = c("YoungDFid", "YoungDFidCount")) %>%
    mutate(!!yngagecolName := YoungDFid)
  
  
  
  
  # construct same file for the OlderDFs
  # need both YoungDF age and YoungDF age count so that the join between the OlderDFs and the YoungDFs works
  # do not need OlderDF age as this will be a duplicate column on the merge
  
  OlderDFsMatchPrep <- CurrentAgeMatch %>%
    group_by(YoungDFid) %>%
    mutate(YoungDFidCount = row_number()) %>%
    dplyr::select(-c(2))
  
  
  OlderDFsReadyToMatch <- suppressMessages(left_join(OlderDF, OlderDFsMatchPrep))
  
  
  # now merge the full data of the subset YoungDFs to the OlderDFs
  # by YoungDF age and YoungDF age count
  # OlderDF data frame is the one to which observations must be joined
  # also add the household numbers at this point
  
  
  MaxIDStartValue <- (nrow(OlderDFsReadyToMatch)-1) + IDStartValue
  
  
  
  FullMatchedDataFrame <- left_join(OlderDFsReadyToMatch, YoungDFsMatched, by=c("YoungDFid", "YoungDFidCount")) %>%
    dplyr::select(-YoungDFid, -YoungDFidCount) %>%
    ungroup() %>%
    mutate({{HouseholdNumVariable}} := seq(IDStartValue, MaxIDStartValue))
  
  # convert from wide to long, use .x and .y to do the split
  
  FirstDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
    rename_all(list(~gsub("\\.x$", "", .)))
  
  SecondDataframeSplit <- FullMatchedDataFrame %>%
    dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
    rename_all(list(~gsub("\\.y$", "", .)))
  
  OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)
  
  
  # #####################################
  # #####################################
  # # pairing the actual couples ends here
  # #####################################
  # #####################################
  
  # use for checking number of iterations used, the p-value to stop, and the p-value reached
  #
  cat("The number of iterations used was", i, "\n")
  # print(Critical_log_chisq)
  # print(log_chisq)
  
  # if(nrow(YoungDF) > nrow(OlderDF)) {
  #
  #   cat("The second dataframe contained more observations than the first dataframe.", "\n")
  #   cat("A merged dataframe has been returned as a list.", "\n")
  cat("The individual dataframes are $Matched and $Unmatched.", "\n")
  
  MatchedIDs <- OutputDataframe %>%
    pull({{yngidcolName}})
  
  UnmatchedDataframe <- YoungDF %>%
    filter(!({{yngidcolName}} %in% MatchedIDs)) #%>%
  # mutate({{HouseholdNumVariable}} = NA)
  
  MergedList <- list()
  
  MergedList$Matched <- OutputDataframe
  MergedList$Unmatched <- UnmatchedDataframe
  
  return(MergedList)
  
}

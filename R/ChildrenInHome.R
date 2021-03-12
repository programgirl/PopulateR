#' Create a subset of observations of children who need to be placed with a parent or guardian
#'
#' Children in a family household can be any age. This function constructs a data frame of children to be placed with a parent, sampled from all possible child ages.
#'
#' @export
#'
#' @param Possibles A data frame containing the people from which children-in-a-family-home will be selected.
#' @param PossiblesAgeCol The column number of the Possibles data frame that contains the ages.
#' @param PossiblesWeightCol The column number for the probability of selection for any observation, within age. If no column is supplied, the weight is assumed to be 1 for all observations.
#' @param SummaryCounts A data frame containing information on the counts required for each age of child.
#' @param SummaryAgeCol The column number of the SummaryCounts data frame that contains the ages that must be obtained
#' @param SummaryProbCol The column number of the SummaryCounts data frame that contains the probabilty of selection of children that age. A probability of 1 means that all children that age will be sampled.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples. If IDStartValue is specified, household allocation will be performed.
#'
#' @examples
#'
ChildrenInHome <- function(Possibles, PossiblesAgeCol = NULL, PossiblesWeightCol = NULL, SummaryCounts,
                           SummaryAgeCol = NULL, SummaryProbCol = NULL, UserSeed = NULL) {



   if(is.null(SummaryAgeCol)) {

      stop("The column for the ages to sample must be specified.")
   }

   if(is.null(SummaryProbCol)) {

      stop("The column for the counts required for sampling must be specified.")
   }

   PossiblesAgeColName <- sym(names(Possibles[PossiblesAgeCol]))

   # cat("Error place 1  for missing all_of", "\n")

   if(is.null(PossiblesWeightCol)) {

      PossiblesRenamed <- Possibles %>%
         rename(PossiblesAge = !! PossiblesAgeCol) %>%
         mutate(TempWeightAtEndID = 1)

   } else {

      # cat("Error place 2 for missing all_of", "\n")

      TempWeightAtEndID <- sym(names(Possibles[PossiblesWeightCol]))

      PossiblesRenamed <- Possibles %>%
         rename(PossiblesAge = !! PossiblesAgeCol, TempWeightAtEndID = !! PossiblesWeightCol)


   }

   if (!is.null(UserSeed)) {
      set.seed(UserSeed)
   }


  # do the sampling
   for(i in 1:nrow(SummaryCounts)) {

     AgeToSample = as.numeric(SummaryCounts[i, SummaryAgeCol])

     CountNeeded = as.numeric(SummaryCounts[i, SummaryProbCol])

     SubsetForThatAge <- PossiblesRenamed %>%
        filter(PossiblesAge == AgeToSample)

     SampledForThatAge <- SubsetForThatAge %>%
       slice_sample(weight_by = TempWeightAtEndID, n = nrow(SubsetForThatAge)*CountNeeded)

   #  cat("The age is", AgeToSample, "the count needed is", nrow(SampledForThatAge), "\n")


     if(exists("SampleResults")) {

        SampleResults <- bind_rows(SampleResults, SampledForThatAge)

     } else {


     SampleResults <- SampledForThatAge

     }


   }


   SampleResults <- SampleResults %>%
      rename(!!PossiblesAgeColName := PossiblesAge)

    # cat("Error place 3 for missing all_of", "\n")

   if(!(is.null(PossiblesWeightCol))) {

    # if(exists("TempWeightAtEndID")) {

      # cat("Error place 4 for missing all_of", "\n")

      SampleResults <- SampleResults %>%
         rename(!!TempWeightAtEndID := PossiblesWeightCol)

   }  else {

      SampleResults <- SampleResults %>%
         select(-TempWeightAtEndID)
   }



 return(SampleResults)
}

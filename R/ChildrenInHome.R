#' Create a subset of observations of children who need to be placed with a parent or guardian
#'
#' Children in a family household can be any age. This function constructs a data frame of children to be placed with a parent, sampled from all possible child ages.
#'
#' @export
#' @param Possibles A data frame containing the people from which children-in-a-family-home will be selected.
#' @param PossiblesAgeCol The column number of the Possibles data frame that contains the ages.
#' @param PossiblesWeightCol The column number for the probability of selection for any observation, within age. If no column is supplied, the weight is assumed to be 1 for all observations.
#' @param SummaryCounts A data frame containing information on the counts required for each age of child.
#' @param SummaryAgeCol The column number of the Missings data frame that contains the ages that must be obtained
#' @param SummaryCountCol The column number of the Missings data frame that contains the counts that are required for each age.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples. If IDStartValue is specified, household allocation will be performed.
#'
#' @examples
#'
ChildrenInHome <- function(Possibles, PossiblesAgeCol = NULL, PossiblesWeightCol = NULL, SummaryCounts,
                           SummaryAgeCol = NULL, SummaryCountCol = NULL, UserSeed = NULL) {



   if(is.null(SummaryAgeCol)) {

      stop("The column for the ages to sample must be specified.")
   }

   if(is.null(SummaryCountCol)) {

      stop("The column for the counts required for sampling must be specified.")
   }

   if(is.null(PossiblesWeightCol)) {

      PossiblesRenamed <- Possibles %>%
         rename(PossiblesAge = !! PossiblesAgeCol) %>%
         mutate(TempWeightAtEndID = 1)

   } else {

   PossiblesRenamed <- Possibles %>%
      rename(PossiblesAge = !! PossiblesAgeCol, TempWeightAtEndID = PossiblesWeightCol)

}


  # do the sampling
   for(i in 1:nrow(SummaryCounts)) {

     AgeToSample = as.numeric(SummaryCounts[i, SummaryAgeCol])

     CountNeeded = as.numeric(SummaryCounts[i, SummaryCountCol])

     cat("The age is", AgeToSample, "the count needed is", CountNeeded, "\n")

     SampledForThatAge <- PossiblesRenamed %>%
        filter(PossiblesAge == AgeToSample) %>%
        slice_sample(n = CountNeeded)


   }






#
#   PropToUpWeight <- dataframe %>%
#     filter(dataframe[,AgeCol] >= UpWeightLowerAge & dataframe[,AgeCol] <= UpWeightUpperAge) %>%
#     summarise(Value=n()) %>%
#     mutate(PropResult = Value/nrow(dataframe)) %>%
#     pull(PropResult)
#
#
#   UpWeightObs <- dataframe %>%
#     filter(dataframe[,AgeCol] >= UpWeightLowerAge & dataframe[,AgeCol] <= UpWeightUpperAge)
#
#   # seed must come before  sample is cut
#   if (!is.null(UserSeed)) {
#     set.seed(UserSeed)
#   }
#
#   # check against actual proportion
#   # only adjust proportion if this differs to expected
#
#   if (PropToUpWeight != UpWeightProp) {
#
#     # upweight fix
#     # create upweighted subset
#     # create downweighted subset
#     # merge to form output dataset
#
#     UpWeightCount <- plyr::round_any(as.numeric((ProbSameSex*(UpWeightProp/PropToUpWeight)*nrow(UpWeightObs))), 2)
#
#     UpWeightObsSample <- UpWeightObs[sample(1:as.numeric(nrow(UpWeightObs)), UpWeightCount, replace=FALSE),]
#
#     DownWeightObs <- dataframe %>%
#       filter(dataframe[,AgeCol] < UpWeightLowerAge | dataframe[,AgeCol] > UpWeightUpperAge)
#
#     DownWeightObsSample <- DownWeightObs[sample(1:as.numeric(nrow(DownWeightObs)), (NumberRequired - UpWeightCount), replace=FALSE),]
#
#     SameSexCouples <- rbind(UpWeightObsSample, DownWeightObsSample)
#
#
#   } else {
#
#     # the expected and actual proportions are the same so just output a random sample
#
#     SameSexCouples <- dataframe[sample(1:CountPartneredCouples, NumberRequired, replace=FALSE),]
#
#   }
#
#   }
#
#   # create households if a start household number is provided
#   if (is.numeric(IDStartValue)) {
#
#     if(is.null(AgeCol)) {
#       stop("The column number for the age variable must be supplied.")
#     }
#
#     if(is.null(HouseholdNumVariable)) {
#       stop("A name for the household count variable must be supplied.")
#     }
#
#     MaxIDStartValue <- (nrow(SameSexCouples)/2)-1
#     SameSexCouples <- SameSexCouples %>%
#       arrange(SameSexCouples[,AgeCol]) %>%
#       mutate({{HouseholdNumVariable}} := rep((IDStartValue):(IDStartValue+MaxIDStartValue),
#                                              each=2))
#
#     }
#
#
#
#
 return(PossiblesRenamed)
}

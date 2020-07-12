#'
#' min parent and max parent ages must be supplied




AddChildLnLoop <- function(Children, ChildIDVariable, ChildAgeVariable, Parents, ParentIDVariable, ParentAgeVariable,
  meanlogUsed, sdlogUsed,  MinParentAge = NULL, MaxParentAge = NULL, MinPropRemain = 0, DyadIDValue = NULL,
  HouseholdNumVariable= NULL, UserSeed=NULL, pValueToStop = .01, NumIterations = 1000000)

{

  options(dplyr.summarise.inform=F)

  # content check
  if (!any(duplicated(Children[ChildIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the child data frame must be supplied.")
  }

  if (!is.numeric(ChildAgeVariable)) {
    stop("Both the child ID and the child age column numbers must be supplied.")
  }

  if (!any(duplicated(Parents[ParentIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the parent data frame must be supplied.")
  }

  if (is.null(MinParentAge)) {
    stop("The minimum parent age must be supplied.")
  }

  if (is.null(MaxParentAge)) {
    stop("The maximum parent age must be supplied.")
  }

  if (is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
  }

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # Child ID variable
  ChildIDColName <- sym(names(Children[ChildIDVariable]))

  # Child age variable
  ChildAgeColName <- sym(names(Children[ChildAgeVariable]))

  # Parent age variable
  ParentAgeColName <- sym(names(Parents[ParentAgeVariable]))

  minChildAge <- min(Children[ChildAgeVariable])

  maxChildAge <- max(Children[ChildAgeVariable])

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################


  #####################################
  #####################################
  # set up pre-data information for matching
  #####################################
  #####################################

  # restrict parent ages to those allowed by the user specifications

  if (!(is.null(MinParentAge))) {

    Parents <- Parents %>%
      filter(({{ParentAgeColName}} - minChildAge) >= MinParentAge)
  }

  if (!(is.null(MaxParentAge))) {

    Parents <- Parents %>%
      filter(({{ParentAgeColName}} - maxChildAge) <= MaxParentAge)
  }

  # create minimum and maximum Parent ages off the data if none are supplied.



  # get counts for each single age from the parent data frame
  # ensure that any requirements to not use a particular number of counts per age is incorporated
  # ensure all parent ages are represented in the data frame of counts
  # use the minimum and maximum values to create an age sequence from MinParentAge to MaxParentAge

  ParentCounts <- Parents %>%
    group_by_at(ParentAgeVariable) %>%
    summarise(AgeCount=n()) %>%
    mutate(AgeCount = floor(AgeCount*(1-MinPropRemain))) %>%
    complete({{ParentAgeColName}}:=seq(min({{ParentAgeColName}}), max({{ParentAgeColName}})),
             fill = list(AgeCount = 0))

  minIndexAge <- as.integer(ParentCounts[1,1])
  maxIndexAge <- as.integer(ParentCounts[nrow(ParentCounts),1])



  ParentAgeCountVector <- ParentCounts$AgeCount


  #####################################
  #####################################
  # end set up
  #####################################
  #####################################


  #####################################
  #####################################
  # age matching
  #####################################
  #####################################

  # as the minimum and maximum Parent ages are known and the child age is known,
  # a check is made is to to see if the parent age draw is within range
  # if so, a check is made to ensure that there is an available parent of that age
  # if not, parent match is rejected, to be redone later
  # if still no match, third pass gives a drunkard's walk assigned to one of the
  # still-available parent ages that will not cause the age at childbirth to be out-of-bounds

  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  for (j in 1:nrow(Children)) {

    AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
    Children$MatchedAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
    age_index <- Children$MatchedAge[j]-(minIndexAge -1)

    if (Children$MatchedAge[j] >= MinParentAge & Children$MatchedAge[j] <=  MaxParentAge & ParentAgeCountVector[age_index] > 0) {

      Children$AgeDifference[j] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

      } else {

        Children$MatchedAge[j] <- NA
        Children$AgeDifference[j] <- NA

        }

  }


  # there may still be a relatively large number of children not matched, so repeat the matching process a second time

  # extract remove matched children from Children data frame
  # the matched ones are removed from the Children data frame as doing it the other way around is a nightmare for referring
  # to the age column in a newly constructed data frame

  MatchedChildren <- Children %>%
    filter(!(is.na(MatchedAge)))

  Children <- Children %>%
    filter(is.na(MatchedAge))

  for (j in 1:nrow(Children)) {

    AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
    Children$MatchedAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
    age_index <- Children$MatchedAge[j]-(minIndexAge -1)

    if (Children$MatchedAge[j] >= MinParentAge & Children$MatchedAge[j] <=  MaxParentAge & ParentAgeCountVector[age_index] > 0) {

      Children$AgeDifference[j] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    } else {

      Children$MatchedAge[j] <- NA
      Children$AgeDifference[j] <- NA

    }

  }

  MatchedSecondGo <- Children %>%
    filter(!(is.na(MatchedAge)))

  MatchedChildren <- rbind(MatchedChildren, MatchedSecondGo)

  RemainingChildren <- Children %>%
    filter(is.na(MatchedAge))


  # force last lot of children to be matched on the basis of first parent age after minimum
  # need to work from minimum child age
  # find first current parent age that is still available
  # will stuff up distribution entered, but if the function has hit this point, the distribution did not fit

 # for (j in 1:nrow(Children)) {

  for (j in 1:nrow(Children)) {

    # ensure initial age selection is within min and max parent ages

    AgeDifference <- round(runif(1, MinParentAge, MaxParentAge))
    Children$AgeDifference[j] <- AgeDifference
    Children$MatchedAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeDifference[j]
    #
    age_index <- Children$AgeDifference[j]-(minIndexAge -1)
    # Children$AgeIndex[j] <- age_index

    # Children$YesFirstTime[j] <- "Yes"

    if (ParentAgeCountVector[age_index] > 0)  {

      ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1

    }

    # replace if there are 0 parent counts available for the age at childbirth allocated
    # use a random walk based on starting values
    # reset age index if the random walk exceeds the min and max permitted parent ages

    if (ParentAgeCountVector[age_index] == 0) {
  #
      Children$MatchedAge[j] <- NA
      age_index <- which.max(ParentAgeCountVector)
      Children$AgeDifference[j] <- age_index


      while ((ParentAgeCountVector[age_index] == 0 & Children$AgeDifference[j] < MinParentAge & Children$AgeDifference[j] > MaxParentAge )) {

        age_index <- age_index + round(runif(1, -1, 1), 0)

         if (age_index > maxIndexAge) {

           age_index <- maxIndexAge - 3

         }

        if (age_index < minIndexAge) {

          age_index <- minIndexAge + 3

        }



        #       if (Children$AgeDifference[j] < MinParentAge) {
        #         age_index <-  minIndexAge + round(runif(1, 1.1*maxIndexAge, 1.2*maxIndexAge))
        #       }
        #


              Children$AgeIndex[j] <- age_index
              Children$AgeDifference[j] <- Children$AgeIndex[j] + (minIndexAge -1)
              Children$MatchedAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeDifference[j]


       #      print(Children$AgeDifference[j])



      }


      ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1


  #       if (Children$AgeDifference[j] > MaxParentAge) {
  #         age_index <- round(age_index/2, 0)
  #       }
  #
  #       if (Children$AgeDifference[j] < MinParentAge) {
  #         age_index <- round(age_index*2, 0)
  #       }
  #
  #       age_index <- age_index + round(runif(1, -1, 1), 0)
  #
  #       if(age_index == 0) {
  #         age_index <-Children[[ChildAgeVariable]][j] + (minIndexAge -1)
  #       }
  #
  #       Children$AgeIndex[j] <- age_index
  #       Children$AgeDifference[j] <- Children$AgeIndex[j] + (minIndexAge -1)
  #       Children$MatchedAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeDifference[j]
  #
  #       i <- i + 1
  #
  #      print(Children$AgeDifference[j])
  #
      }
  #
  #      # may need to leave this bit until tomorrow
  #
  #      if(i == 100) {
  #
  #        Children$AgeDifference[j] <- minIndexAge
  #        Children$MatchedAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeDifference[j]
  #        age_index <- Children$MatchedAge[j] - (minIndexAge -1)
  #        Children$AgeIndex <- AgeIndex
  #        Children$YesFirstTime[j] <- "Third time's a charm"
  #
  #        while (ParentAgeCountVector[age_index] == 0 ) {
  #
  #          age_index <- age_index + 1
  #
  #        }
  #
  #
  #
  #      }
  #
  #
  #     # if(Children$AgeDifference[j]  > MaxParentAge) {
  #     #   IndexReset <- round(.5 * (MaxParentAge - MinParentAge), 0)
  #     # #   age_index <- IndexReset - minIndexAge - 1
  #     # #
  #     #   print(IndexReset)
  #     # }
  #   #
  #     }
  #   #
  #   #   Children$AgeIndex[j] <- age_index
  #   #   Children$MatchedAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeIndex[j] + minIndexAge - 1
  #   #   Children$AgeDifference[j] <- Children$MatchedAge[j] - Children[[ChildAgeVariable]][j]
  #   #   ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
  #   #
  #   # #
  #    #else {
  #   # #
  #   # #
    # ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
  #   #
  #   # }
  #
  }


 return(ParentAgeCountVector)


}


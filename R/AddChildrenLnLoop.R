#' Create a subset of observations containing only children matched to parents/guardians
#' This function creates a data frame of child-parent/guardian pairs, based on a population distribution of age differences. The distribution used in this function is the log normal. However, the matching is affected by the age structure of the children and parent data frames. The distribution provides a framework upon which to base the matching. The final distribution of age differences, however, may not follow a lognormal distribution.
#' Two data frames are required. The Children data frame contains the age data, to which the Parent (Guardian) data will be applied.
#' The minimum and maximum ages of parents must be specified. This ensures that there are no parents who were too young (e.g. 11 years) or too old (e.g. 70 years) at the time the child was born. The presence of too young and too old parents is tested throughout this function. Thus, pre-cleaning the Parent data frame is not required.
#'
#'
#'
#' NEED TO FIX THE INFORMATION BELOW, DEPENDING ON HOW THE FUNCTION ENDS UP BEING WRITTEN.
#'
#' Some children and/or parents may not be matched. This situation will occur for two reasons. First, if the number of parents relative to the number of children is relatively small, there are fewer matches available at each parent age. Second, if the age structure of the children has a poor alignment with the age structure of the parents, given the lognormal distribution used, some parent ages will be upsampled relative to their frequency. .
#' The number of observations output is dependent on the ratio of children: available parents. If the parent data frame is relatively small compared to the children data frame it is possible that unmatched children will occur. The function only outputs the children and parents that have been matched.
#'
#'
#'
#'
#'
#' The function performs a reasonableness check for child ID, child age, parent ID variable, and household number.
#'
#' @export
#' @param Children A data frame containing observations limited to the children to be matched An age column is required. All children in this data frame will be matched to a parent/guardian.
#' @param ChildIDVariable The column number for the ID variable in the Children data frame.
#' @param ChildAgeVariable The column number for the Age variable in the Children data frame.
#' @param meanlogUsed The mean of the natural log for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#'  @param sdlogUsed The standard deviation of the natural log for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#' @param Parents A data frame containing observations limited to parents. An age column is required. This can contain the entire set of people who can be parents, as the assignment is made on age at becoming a parent, not current age. This file can contain the people who can be guardians, as well as parents. This data frame should contain more observations than the Children data frame. The relative sizes of the data frames are compared. If the Parens data frame is not sufficiently larger than the Children data frame, the latter is randomly sampled to construct a smaller data frame.
#' @param ParentIDVariable The column number for the ID variable in the Parent data frame.
#' @param ParentAgeVariable The column number for the Age variable in the Parent data frame.
#' @param MinParentAge The youngest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param MaxParentAge The oldest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param MinPropRemain The minimum proportion of people, at each age, who are not parents. The default is zero, which may result in all people at a specific age being allocated as parents. This will leave age gaps for any future work, and may not be desirable. If nrow(Children) == nrow(Parents), assigning any value other than 0 will result in an error.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied, and in quotes.
#' @param DyadIDValue The starting number for generating a variable that identifies the observations in a child-parent dyad. Must be numeric. If no value is provided, the Dyad ID starts at 1.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.




AddChildrenLnLoop <- function(Children, ChildIDVariable, ChildAgeVariable, NumChildren = 2, TwinRate = 0, Parents, ParentIDVariable, ParentAgeVariable,
                           meanlogUsed, sdlogUsed,MinParentAge = NULL, MaxParentAge = NULL, HouseholdNumVariable= NULL,
                           DyadIDValue = 1, UserSeed=NULL)

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

  ChildrenRenamed <- Children %>%
    rename(ChildID = !! ChildIDVariable, ChildAge = !! ChildAgeVariable)

  # Child ID variable
  ChildIDColName <- sym(names(Children[ChildIDVariable]))

  # Child age variable
  ChildAgeColName <- sym(names(Children[ChildAgeVariable]))

  ParentsRenamed <- Parents %>%
    rename(ParentID = !! ParentIDVariable, ParentAge = !! ParentAgeVariable)

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

    ParentsRenamed <- ParentsRenamed %>%
      filter((ParentAge - minChildAge) >= MinParentAge)
  }

  if (!(is.null(MaxParentAge))) {

    ParentsRenamed <- ParentsRenamed %>%
      filter((ParentAge - maxChildAge) <= MaxParentAge)
  }

  # get counts for each single age from the parent data frame
  # ensure that any requirements to not use a particular number of counts per age is incorporated
  # ensure all parent ages are represented in the data frame of counts
  # use the minimum and maximum values to create an age sequence from MinParentAge to MaxParentAge

  ParentCounts <- ParentsRenamed %>%
    group_by(ParentAge) %>%
    summarise(AgeCount=n()) %>%
    tidyr::complete(ParentAge = seq(min(ParentAge), max(ParentAge)),
                    fill = list(AgeCount = 0))

  minIndexAge <- as.integer(ParentCounts[1,1])
  maxIndexAge <- as.integer(ParentCounts[nrow(ParentCounts),1])

  ParentAgeCountVector <- ParentCounts$AgeCount

  if (nrow(ParentsRenamed)*2.05 < nrow(Children)) {

    ChildrenRenamed <- ChildrenRenamed %>%
      slice_sample(n = round(nrow(ParentsRenamed) * 1.9), 0)
  }

  # TODO MAKE SURE THAT NROW(CHILDREN) %MODULO% NUMCHILDREN == O, OTHERWISE SAMPLE SO THIS IS THE STATE
  # NOT SURE ABOUT THIS TEST DUE TO THE WAY THE CHILDREN/PARENT ARE BEING CONNECTED, COME BACK TO THIS



  # #####################################
  # #####################################
  # # end set up
  # #####################################
  # #####################################
  #
  # #####################################
  # #####################################
  # # Split into twins and non-twins
  # #####################################
  # #####################################
  #
  #
  if (TwinRate > 0) {

    TwinsDataFrame <- ChildrenRenamed %>%
      slice_sample(prop = TwinRate/2)

    NoTwinsDataFrame <- ChildrenRenamed %>%
     filter(!(ChildID %in%  TwinsDataFrame$ChildID))

    TwinsMatched <- left_join(TwinsDataFrame %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                              NoTwinsDataFrame %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                              by = c("ChildAge", "Counter"))

    # add parent

    if (!is.null(UserSeed)) {
      set.seed(UserSeed)
    }

    for (c in 1:nrow(TwinsMatched)) {

      AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
      TwinsMatched$AgeDifference[c] <- AgeDifference
      TwinsMatched$ParentAge[c] <- TwinsMatched$ChildAge[c] + AgeDifference
      age_index <- TwinsMatched$ParentAge[c]-(minIndexAge -1)
      TwinsMatched$age_index[c] <- age_index



      while (!(TwinsMatched$AgeDifference[c] >= MinParentAge && TwinsMatched$AgeDifference[c] <= MaxParentAge &&
             ParentAgeCountVector[age_index] > 0 && TwinsMatched$ParentAge[c] >= minIndexAge &&
             TwinsMatched$ParentAge[c] <= maxIndexAge)) {

        print(c)

          AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
          TwinsMatched$AgeDifference[c] <- AgeDifference
          TwinsMatched$ParentAge[c] <- TwinsMatched$ChildAge[c] + AgeDifference
          age_index <- TwinsMatched$ParentAge[c]-(minIndexAge -1)

      }

      TwinsMatched$AgeDifference[c] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    }

    TwinsMatched <- TwinsMatched %>%
      select(-c(AgeDifference, age_index))

    # remove matched twin ids from the avaiable children in the NoTwinsDataFrame

    NoTwinsDataFrame <- NoTwinsDataFrame %>%
      filter(!(ChildID %in%  TwinsMatched$ChildID.y))

    #  add in the extra children to the twins, where there are more than 2 children in the household

    # create counts of children remaining so that the non-twins can be matched to the twins

    ChildrenCounts <- NoTwinsDataFrame %>%
      group_by(ChildAge) %>%
      summarise(AgeCount=n()) %>%
      tidyr::complete(ChildAge = seq(min(ChildAge), max(ChildAge)),
                      fill = list(AgeCount = 0))

    minChildIndexAge <- as.integer(ChildrenCounts[1,1])
    maxChildIndexAge <- as.integer(ChildrenCounts[nrow(ChildrenCounts),1])

    ChildrenAgeCountVector <- ChildrenCounts$AgeCount

    if (NumChildren > 2) {

      #create the column names
   for (x in 3:NumChildren) {

      TwinsMatched <- TwinsMatched %>%
         tibble::add_column(!! paste0("ChildAge", x) := 1000)

      # closes column name loop
   }

      # it being a tibble seemed to be the problem for the looping below.

      TwinsMatched <- as.data.frame(TwinsMatched)

      # ncolStart <- ncol(TwinsMatched) - (NumChildren - 3)
      # cat("ncol(TwinsMatched) " = ncol(TwinsMatched), "NumChildren " = NumChildren -3, "ncolStart "= ncolStart, "\n")


  # now iterate through the children
  # nested loop must be columns within rows

 for (x in 1:nrow(TwinsMatched)) {

  #   for (y in (ncol(TwinsMatched)-NumChildren+1):ncol(TwinsMatched)) {

   for (y in (ncol(TwinsMatched) - (NumChildren - 3)):ncol(TwinsMatched)) {

       AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
          TwinsMatched[x,y] <- TwinsMatched$ParentAge[x] - AgeDifference
          age_index <- TwinsMatched[x,y]


          # cat("TwinsMatched$ParentAge[x] = ", TwinsMatched$ParentAge[x], "TwinsMatched[x,y] = ", TwinsMatched[x,y], "age_index  =",
          #     age_index, "\n")

          while (!(age_index < minChildAge && age_index > maxChildAge)) {

            cat("Entered while loop", "age_index = ", age_index)
 # # #               # AgeDifference %in% UsedAgesVector[x] &&
 # #
 # #                age_index < 1 && age_index > length(ChildrenAgeCountVector))) {
 # #
            AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
            TwinsMatched[x,y] <- AgeDifference
            age_index <- TwinsMatched[x,y]
 # #
 # # #
 #        # close while test
          }

          cat("age_index = ", age_index)
 # #
 # #      # ChildrenAgeCountVector[age_index] = ChildrenAgeCountVector[age_index] - 1
 # #      # UsedAgesVector <- cbind(UsedAgesVector, AgeDifference)
 # #
 # #      # closes for column loop
 #
      }
 # #
 # #      # closes for numchildren loop
    }
 # #
 # #      #closes if numchildren test
      }


    #closes twin set of functions
    }
  #
  #
  # # TODO MATCH CHILDREN, NO TWINS, FOR OTHER KIDS
  #
  # #####################################
  # #####################################
  # # age matching
  # #####################################
  # #####################################
  #
  # # as the minimum and maximum Parent ages are known and the child age is known,
  # # a check is made is to to see if the parent age draw is within range
  # # if so, a check is made to ensure that there is an available parent of that age
  # # if not, parent match is rejected, to be redone later
  # # if still no match, third pass gives a drunkard's walk assigned to one of the
  # # still-available parent ages that will not cause the age at childbirth to be out-of-bounds
  #
  # # if (!is.null(UserSeed)) {
  # #   set.seed(UserSeed)
  # # }
  # #
  # # for (j in 1:nrow(Children)) {
  # #
  # #   AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
  # #   Children$AgeDifference[j] <- AgeDifference
  # #   Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
  # #   age_index <- Children$ParentAge[j]-(minIndexAge -1)
  # #
  # #
  # #   if (Children$AgeDifference[j] >= MinParentAge && Children$AgeDifference[j] <= MaxParentAge && ParentAgeCountVector[age_index] > 0 &&
  # #       Children$ParentAge[j] >= minIndexAge && Children$ParentAge[j] <= maxIndexAge) {
  # #
  # #     Children$AgeDifference[j] <- AgeDifference
  # #     ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1
  # #
  # #   } else {
  # #
  # #     Children$ParentAge[j] <- NA
  # #     Children$AgeDifference[j] <- NA
  # #
  # #   }
  # #
  # # }
  # #
  # #
  # # # there may still be a relatively large number of children not matched, so repeat the matching process a second time
  # #
  # # # extract remove matched children from Children data frame
  # # # the matched ones are removed from the Children data frame as doing it the other way around is a nightmare for referring
  # # # to the age column in a newly constructed data frame
  # #
  # # MatchedChildren <- Children %>%
  # #   filter(!(is.na(ParentAge)))
  # #
  # # Children <- Children %>%
  # #   filter(is.na(ParentAge))
  # #
  # # for (j in 1:nrow(Children)) {
  # #
  # #
  # #   AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
  # #   Children$AgeDifference[j] <- AgeDifference
  # #   Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
  # #   age_index <- Children$ParentAge[j]-(minIndexAge -1)
  # #
  # #
  # #   if (Children$AgeDifference[j] >= MinParentAge && Children$AgeDifference[j] <= MaxParentAge && ParentAgeCountVector[age_index] > 0 &&
  # #       Children$ParentAge[j] >= minIndexAge && Children$ParentAge[j] <= maxIndexAge) {
  # #
  # #     Children$AgeDifference[j] <- AgeDifference
  # #     ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1
  # #
  # #   } else {
  # #
  # #     Children$ParentAge[j] <- NA
  # #     Children$AgeDifference[j] <- NA
  # #
  # #   }
  # #
  # #
  # # }
  # #
  # # MatchedSecondGo <- Children %>%
  # #   filter(!(is.na(ParentAge)))
  # #
  # # MatchedChildren <- rbind(MatchedChildren, MatchedSecondGo)
  # #
  # # Children <- Children %>%
  # #   filter(is.na(ParentAge))
  # #
  # # print(nrow(MatchedChildren))
  # #
  # #
  # # # force last lot of children to be matched on the basis of first parent age after minimum
  # # # need to work from minimum child age
  # # # find first current parent age that is still available
  # # # will stuff up distribution entered, but if the function has hit this point, the distribution did not fit
  # #
  # # for (j in 1:nrow(Children)) {
  # #
  # #   # ensure initial age selection is within min and max parent ages
  # #
  # #   AgeDifference <- round(runif(1, MinParentAge, MaxParentAge))
  # #   Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
  # #   age_index <- Children$ParentAge[j]-(minIndexAge -1)
  # #
  # #   if (ParentAgeCountVector[age_index] > 0)  {
  # #
  # #     ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
  # #     Children$AgeDifference[j] <- AgeDifference
  # #
  # #   } else {
  # #
  # #     Children$AgeDifference[j] <- NA
  # #     Children$ParentAge[j] <- NA
  # #
  # #     age_index <- which.max(ParentAgeCountVector)
  # #     Children$ParentAge[j] <- age_index + (minIndexAge -1)
  # #     Children$AgeDifference[j] <- Children$ParentAge[j] - Children[[ChildAgeVariable]][j]
  # #
  # #     while (!(ParentAgeCountVector[age_index] > 0 && Children$AgeDifference[j] >= MinParentAge && Children$AgeDifference[j] <= MaxParentAge)) {
  # #
  # #       age_index <- age_index + round(runif(1,-2,2),0)
  # #
  # #       if(age_index < 1) {
  # #         age_index <- round(length(ParentAgeCountVector)*.2, 0)
  # #       }
  # #
  # #       if(age_index > length(ParentAgeCountVector)) {
  # #         age_index <- round(length(ParentAgeCountVector)*.8, 0)
  # #       }
  # #
  # #       Children$ParentAge[j] <- age_index + (minIndexAge -1)
  # #       Children$AgeDifference[j] <- Children$ParentAge[j] - Children[[ChildAgeVariable]][j]
  # #     }
  # #
  # #     ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
  # #
  # #   }
  # #
  # #   print(j)
  # #
  # # }
  # #
  # # #   Combine the three Children Dataframes
  # #
  # # Children <- rbind(MatchedChildren, Children)
  # #
  # # # #####################################
  # # #####################################
  # # # pairing the actual parent-child dyads starts here
  # # #####################################
  # # #####################################
  # # # return full donor and recipient rows as matched household pairs
  # # # extract ages counts for matching the donors
  # # MatchedParentAges <- Children %>%
  # #   dplyr::select(ParentAge) %>%
  # #   group_by(ParentAge) %>%
  # #   mutate(ParentAgeCount = row_number()) %>%
  # #   ungroup()
  # #
  # #
  # # # generate same AgeCount second ID variable for the parent data
  # # # the AgeCount is used to ensure that the first parent with a specific age is matched first
  # # # the second parent with a specific age is matched second and so forth
  # # ParentsToMatch <- Parents %>%
  # #   group_by({{ParentAgeColName}}) %>%
  # #   mutate(ParentAgeCount = row_number()) %>%
  # #   ungroup()
  # #
  # # # reduce pool of potentially partnered donors to only those matched to recipients
  # # ParentsMatched <- left_join(MatchedParentAges,
  # #                             rename_at(ParentsToMatch, ParentAgeVariable, ~ names(MatchedParentAges)[1],
  # #                                       ParentsToMatch, ParentAgeVariable, ~ names(MatchedParentAges)[2]),
  # #                             by = c(names(MatchedParentAges)[1], "ParentAgeCount")) %>%
  # #   mutate(!!ParentAgeColName := ParentAge)
  # #
  # #
  # # # construct same file for the children
  # # # need both parent age and parent age count so that the join between the children and the parents works
  # # # do not need child age as this will be a duplicate column on the merge
  # # ChildrenMatchPrep <- Children %>%
  # #   group_by(ParentAge) %>%
  # #   mutate(ParentAgeCount = row_number()) %>%
  # #   dplyr::select(-c(2)) %>%
  # #   ungroup()
  # #
  # # # join the matched parents to the children
  # # # by parent age and parent age count
  # # # children data frame is the one to which observations must be joined
  # # # also add the household numbers at this point
  # #
  # # MaxDyadIDValue <- (nrow(ChildrenMatchPrep)-1) + DyadIDValue
  # #
  # # FullMatchedDataFrame <- left_join(ChildrenMatchPrep, ParentsMatched, by=c("ParentAge", "ParentAgeCount")) %>%
  # #   dplyr::select(-ParentAge, -ParentAgeCount) %>%
  # #   ungroup() %>%
  # #   mutate({{HouseholdNumVariable}} := seq(DyadIDValue, MaxDyadIDValue))
  # #
  # # # convert from wide to long, use .x and .y to do the split
  # #
  # # FirstDataframeSplit <- FullMatchedDataFrame %>%
  # #   dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
  # #   rename_all(list(~gsub("\\.x$", "", .)))
  # #
  # # SecondDataframeSplit <- FullMatchedDataFrame %>%
  # #   dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
  # #   rename_all(list(~gsub("\\.y$", "", .)))
  # #
  # #
  # # OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)
  # #
  # # #####################################
  # # #####################################
  # # # pairing the parents to children ends here
  # # #####################################
  # # #####################################
  # #
  # #
  # # return(OutputDataframe)

  return(TwinsMatched)

#closes function
}

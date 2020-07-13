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
    Children$AgeDifference[j] <- AgeDifference
    Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
    age_index <- Children$ParentAge[j]-(minIndexAge -1)


    if (Children$AgeDifference[j] >= MinParentAge && Children$AgeDifference[j] <= MaxParentAge && ParentAgeCountVector[age_index] > 0 &&
        Children$ParentAge[j] >= minIndexAge && Children$ParentAge[j] <= maxIndexAge) {

      Children$AgeDifference[j] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

      } else {

        Children$ParentAge[j] <- NA
        Children$AgeDifference[j] <- NA

        }

  }


  # there may still be a relatively large number of children not matched, so repeat the matching process a second time

  # extract remove matched children from Children data frame
  # the matched ones are removed from the Children data frame as doing it the other way around is a nightmare for referring
  # to the age column in a newly constructed data frame

  MatchedChildren <- Children %>%
    filter(!(is.na(ParentAge)))

  Children <- Children %>%
    filter(is.na(ParentAge))

  for (j in 1:nrow(Children)) {

    AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
    Children$AgeDifference[j] <- AgeDifference
    Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
    age_index <- Children$ParentAge[j]-(minIndexAge -1)


    if (Children$AgeDifference[j] >= MinParentAge && Children$AgeDifference[j] <= MaxParentAge && ParentAgeCountVector[age_index] > 0 &&
        Children$ParentAge[j] >= minIndexAge && Children$ParentAge[j] <= maxIndexAge) {

      Children$AgeDifference[j] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    } else {

      Children$ParentAge[j] <- NA
      Children$AgeDifference[j] <- NA

    }

  }

  MatchedSecondGo <- Children %>%
    filter(!(is.na(ParentAge)))

  MatchedChildren <- rbind(MatchedChildren, MatchedSecondGo)

  Children <- Children %>%
    filter(is.na(ParentAge))


  # force last lot of children to be matched on the basis of first parent age after minimum
  # need to work from minimum child age
  # find first current parent age that is still available
  # will stuff up distribution entered, but if the function has hit this point, the distribution did not fit

  for (j in 1:nrow(Children)) {

    # ensure initial age selection is within min and max parent ages

    AgeDifference <- round(runif(1, MinParentAge, MaxParentAge))
 #   Children$AgeDifference[j] <- AgeDifference
    Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
    age_index <- Children$ParentAge[j]-(minIndexAge -1)
 #   Children$AgeIndex[j] <- age_index

   if (ParentAgeCountVector[age_index] > 0)  {

     ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
     Children$AgeDifference[j] <- AgeDifference
     Children$AgeIndex[j] <- NA

     # works fine up to this point

      } else {

        Children$AgeDifference[j] <- NA
        Children$ParentAge[j] <- NA

        age_index <- which.max(ParentAgeCountVector)
        # Children$AgeDifference[j] <- age_index + (minIndexAge -1)
        # Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeDifference[j]
        # Children$AgeIndex[j] <- age_index

        Children$ParentAge[j] <- age_index + (minIndexAge -1)
        Children$AgeDifference[j] <- Children$ParentAge[j] - Children[[ChildAgeVariable]][j]

    #   while (ParentAgeCountVector[age_index] == 0 || Children$AgeDifference[j] < MinParentAge || Children$AgeDifference[j] > MaxParentAge) {

        while (!(ParentAgeCountVector[age_index] > 0 && Children$AgeDifference[j] >= MinParentAge && Children$AgeDifference[j] <= MaxParentAge)) {

          age_index <- age_index + round(runif(1,-2,2),0)

          if(age_index < 1) {
            age_index <- round(length(ParentAgeCountVector)*.2, 0)
          }

          if(age_index > length(ParentAgeCountVector)) {
            age_index <- round(length(ParentAgeCountVector)*.8, 0)
            }

          # Children$AgeDifference[j] <- age_index + (minIndexAge -1)
          # Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeDifference[j]

          Children$ParentAge[j] <- age_index + (minIndexAge -1)
          Children$AgeDifference[j] <- Children$ParentAge[j] - Children[[ChildAgeVariable]][j]
          Children$AgeIndex[j] <- age_index
        }

        ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1

      }

  }

  # Combine the three Children Dataframes

 # Children <- rbind(MatchedChildren, Children)

  # # #####################################
  # #####################################
  # # pairing the actual parent-child dyads starts here
  # #####################################
  # #####################################
  # # return full donor and recipient rows as matched household pairs
  # # extract ages counts for matching the donors
  # MatchedParentAges <- Children %>%
  #   dplyr::select(ParentAge) %>%
  #   group_by(ParentAge) %>%
  #   mutate(ParentAgeCount = row_number()) %>%
  #   ungroup()
  #
  #
  # # generate same AgeCount second ID variable for the parent data
  # # the AgeCount is used to ensure that the first parent with a specific age is matched first
  # # the second parent with a specific age is matched second and so forth
  # ParentsToMatch <- Parents %>%
  #   group_by({{ParentAgeColName}}) %>%
  #   mutate(ParentAgeCount = row_number()) %>%
  #   ungroup()
  #
  # # reduce pool of potentially partnered donors to only those matched to recipients
  # ParentsMatched <- left_join(MatchedParentAges,
  #                             rename_at(ParentsToMatch, ParentAgeVariable, ~ names(MatchedParentAges)[1],
  #                                       ParentsToMatch, ParentAgeVariable, ~ names(MatchedParentAges)[2]),
  #                             by = c(names(MatchedParentAges)[1], "ParentAgeCount")) %>%
  #   mutate(!!ParentAgeColName := ParentAge)
  #
  # #
  # # # construct same file for the children
  # # # need both parent age and parent age count so that the join between the children and the parents works
  # # # do not need child age as this will be a duplicate column on the merge
  # # ChildrenMatchPrep <- CurrentAgeMatch %>%
  # #   group_by(ParentAge) %>%
  # #   mutate(ParentAgeCount = row_number()) %>%
  # #   dplyr::select(-c(2)) %>%
  # #   ungroup()
  # #
  # # ChildrenReadyToMatch <- left_join(Children, ChildrenMatchPrep, by = c(names(Children[ChildIDVariable])))
  # #
  # #
  # #
  # # # now merge the full data of the subset donors to the recipients
  # # # by parent age and parent age count
  # # # children data frame is the one to which observations must be joined
  # # # also add the household numbers at this point
  # # MaxDyadIDValue <- (nrow(ChildrenReadyToMatch)-1) + DyadIDValue
  # #
  # # FullMatchedDataFrame <- left_join(ChildrenReadyToMatch, ParentsMatched, by=c("ParentAge", "ParentAgeCount")) %>%
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


 return(ParentAgeCountVector)


}


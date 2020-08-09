#' Create a subset of observations containing only children matched to parents/guardians
#' This function creates a data frame of child-parent/guardian pairs, based on a population distribution of age differences. The distribution used in this function is the log normal. However, the matching is affected by the age structure of the children and parent data frames. The distribution provides a framework upon which to base the matching. The final distribution of age differences, however, may not follow a lognormal distribution.
#' Two data frames are required. The Children data frame contains the age data, to which the Parent (Guardian) data will be applied.
#' The minimum and maximum ages of parents must be specified. This ensures that there are no parents who were too young (e.g. 11 years) or too old (e.g. 70 years) at the time the child was born. The presence of too young and too old parents is tested throughout this function. Thus, pre-cleaning the Parent data frame is not required..
#' The minimum proportion prevents the outcome where most/all people of a particular age, eg. the entire set of 25-year-olds, are parents. The default value is NULL, which assumes that all people of any age can be parents. The defalt value is 0, enabling a pre-cleaned data frame of parents to be used.
#' An even number of observations is output, which is one child-parent pair.
#'
#' The function performs a reasonableness check for child ID, child age, parent ID variable, and household number.
#'
#' @export
#' @param Children A data frame containing observations limited to the children to be matched An age column is required. All children in this data frame will be matched to a parent/guardian.
#' @param ChildIDVariable The column number for the ID variable in the Children data frame.
#' @param ChildAgeVariable The column number for the Age variable in the Children data frame.
#' @param meanlogUsed The mean of the natural log for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#'  @param sdlogUsed The standard deviation of the natural log for the distribution of parent ages at the time the child is born. For women, this will commonly be the age at childbirth.
#' @param Parents A data frame containing observations limited to parents. An age column is required. This can contain the entire set of people who can be parents, as the assignment is made on age at becoming a parent, not current age. This file can contain the people who can be guardians, as well as parents. This data frame must contain at least the same number of observations as the Children data frame.
#' @param ParentIDVariable The column number for the ID variable in the Parent data frame.
#' @param ParentAgeVariable The column number for the Age variable in the Parent data frame.
#' @param MinParentAge The youngest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param MaxParentAge The oldest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param MinPropRemain The minimum proportion of people, at each age, who are not parents. The default is zero, which may result in all people at a specific age being allocated as parents. This will leave age gaps for any future work, and may not be desirable. If nrow(Children) == nrow(Parents), assigning any value other than 0 will result in an error.
#' @param HouseholdIDVariable The column number for the household variable in the Parents data frame. This must be provided.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


AddChild <- function(Children, ChildIDVariable, ChildAgeVariable, Parents, ParentIDVariable, ParentAgeVariable, meanlogUsed, sdlogUsed,
                           MinParentAge = NULL, MaxParentAge = NULL, MinPropRemain = 0, HouseholdIDVariable= NULL, UserSeed=NULL)

{

  options(dplyr.summarise.inform=F)

  # content check
  # content check
  if (!any(duplicated(Children[ChildIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the child data frame must be supplied, and the ID must be unique to each child.")
  }

  if (!is.numeric(ChildAgeVariable)) {
    stop("Both the child ID and the child age column numbers must be supplied.")
  }

  if (!any(duplicated(Parents[ParentIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the parent data frame must be supplied, and the ID must be unique to each parent.")
  }

  if (is.null(MinParentAge)) {
    stop("The minimum parent age must be supplied.")
  }

  if (is.null(MaxParentAge)) {
    stop("The maximum parent age must be supplied.")
  }

  if (!any(duplicated(Parents[HouseholdIDVariable])) == FALSE) {
    stop("The column number for the household ID variable in the parent data frame must be supplied, and the household number must be unique to each parent.")
  }

  if(ncol(Children) != ncol(Parents)-1) {
    stop("The number of columns requirement is not met.")
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
    rename(ParentID = !! ParentIDVariable, ParentAge = !! ParentAgeVariable,
           HouseholdID = !! HouseholdIDVariable)

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

  # get the number of columns in the children data frames
  # this is important when splitting out the child and matched parent

  NumberColsChildren <- as.numeric(ncol(ChildrenRenamed))


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

  for (j in 1:nrow(ChildrenRenamed)) {

    AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
    ChildrenRenamed$AgeDifference[j] <- AgeDifference
    ChildrenRenamed$ParentAge[j] <- ChildrenRenamed$ChildAge[j] + AgeDifference
    age_index <- ChildrenRenamed$ParentAge[j]-(minIndexAge -1)


    if (ChildrenRenamed$AgeDifference[j] >= MinParentAge && ChildrenRenamed$AgeDifference[j] <= MaxParentAge && ParentAgeCountVector[age_index] > 0 &&
        ChildrenRenamed$ParentAge[j] >= minIndexAge && ChildrenRenamed$ParentAge[j] <= maxIndexAge) {

      ChildrenRenamed$AgeDifference[j] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    } else {

      ChildrenRenamed$ParentAge[j] <- NA
      ChildrenRenamed$AgeDifference[j] <- NA

    }

  }


  # there may still be a relatively large number of children not matched, so repeat the matching process a second time

  # extract remove matched children from Children data frame
  # the matched ones are removed from the Children data frame as doing it the other way around is a nightmare for referring
  # to the age column in a newly constructed data frame

  MatchedChildren <- ChildrenRenamed %>%
    filter(!(is.na(ParentAge)))

  ChildrenRenamed <- ChildrenRenamed %>%
    filter(is.na(ParentAge))

  for (j in 1:nrow(Children)) {

    AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
    ChildrenRenamed$AgeDifference[j] <- AgeDifference
    ChildrenRenamed$ParentAge[j] <- ChildrenRenamed$ChildAge[j] + AgeDifference
    age_index <- ChildrenRenamed$ParentAge[j]-(minIndexAge -1)


    if (ChildrenRenamed$AgeDifference[j] >= MinParentAge && ChildrenRenamed$AgeDifference[j] <= MaxParentAge && ParentAgeCountVector[age_index] > 0 &&
        ChildrenRenamed$ParentAge[j] >= minIndexAge && ChildrenRenamed$ParentAge[j] <= maxIndexAge) {

      ChildrenRenamed$AgeDifference[j] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    } else {

      ChildrenRenamed$ParentAge[j] <- NA
      ChildrenRenamed$AgeDifference[j] <- NA

    }

  }

  MatchedSecondGo <- ChildrenRenamed %>%
    filter(!(is.na(ParentAge)))

  MatchedChildren <- rbind(MatchedChildren, MatchedSecondGo)

  ChildrenRenamed <- ChildrenRenamed %>%
    filter(is.na(ParentAge))


  # force last lot of children to be matched on the basis of first parent age after minimum
  # need to work from minimum child age
  # find first current parent age that is still available
  # will stuff up distribution entered, but if the function has hit this point, the distribution did not fit

  for (j in 1:nrow(ChildrenRenamed)) {

    # ensure initial age selection is within min and max parent ages

    AgeDifference <- round(runif(1, MinParentAge, MaxParentAge))
    ChildrenRenamed$ParentAge[j] <- ChildrenRenamed$ChildAge[j] + AgeDifference
    age_index <- ChildrenRenamed$ParentAge[j]-(minIndexAge -1)

    if (ParentAgeCountVector[age_index] > 0)  {

      ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
      ChildrenRenamed$AgeDifference[j] <- AgeDifference

    } else {

      ChildrenRenamed$AgeDifference[j] <- NA
      ChildrenRenamed$ParentAge[j] <- NA

      age_index <- which.max(ParentAgeCountVector)
      ChildrenRenamed$ParentAge[j] <- age_index + (minIndexAge -1)
      ChildrenRenamed$AgeDifference[j] <- ChildrenRenamed$ParentAge[j] - ChildrenRenamed$ChildAge[j]

      #   while (ParentAgeCountVector[age_index] == 0 || Children$AgeDifference[j] < MinParentAge || Children$AgeDifference[j] > MaxParentAge) {

      while (!(ParentAgeCountVector[age_index] > 0 && ChildrenRenamed$AgeDifference[j] >= MinParentAge && ChildrenRenamed$AgeDifference[j] <= MaxParentAge)) {

        age_index <- age_index + round(runif(1,-2,2),0)

        if(age_index < 1) {
          age_index <- round(length(ParentAgeCountVector)*.2, 0)
        }

        if(age_index > length(ParentAgeCountVector)) {
          age_index <- round(length(ParentAgeCountVector)*.8, 0)
        }

        # Children$AgeDifference[j] <- age_index + (minIndexAge -1)
        # Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeDifference[j]

        ChildrenRenamed$ParentAge[j] <- age_index + (minIndexAge -1)
        ChildrenRenamed$AgeDifference[j] <- ChildrenRenamed$ParentAge[j] - ChildrenRenamed$ChildAge[j]
      }

      ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1

    }

  }

  #   Combine the three Children Dataframes

  ChildrenRenamed <- rbind(MatchedChildren, ChildrenRenamed)

  # #####################################
  #####################################
  # pairing the actual parent-child dyads starts here
  #####################################
  #####################################
  # return full donor and recipient rows as matched household pairs
  # extract ages counts for matching the donors
  MatchedParentAges <- ChildrenRenamed %>%
    dplyr::select(ParentAge) %>%
    group_by(ParentAge) %>%
    mutate(ParentAgeCount = row_number()) %>%
    ungroup()


  # generate same AgeCount second ID variable for the parent data
  # the AgeCount is used to ensure that the first parent with a specific age is matched first
  # the second parent with a specific age is matched second and so forth
  ParentsToMatch <- ParentsRenamed %>%
    group_by(ParentAge) %>%
    mutate(ParentAgeCount = row_number()) %>%
    ungroup()

  # reduce pool of potentially partnered donors to only those matched to recipients
  ParentsMatched <- left_join(MatchedParentAges,
                              by = c("ParentAge", "ParentAgeCount"))


  # construct same file for the children
  # need both parent age and parent age count so that the join between the children and the parents works
  # do not need child age as this will be a duplicate column on the merge
  ChildrenMatchPrep <- ChildrenRenamed %>%
    group_by(ParentAge) %>%
    mutate(ParentAgeCount = row_number()) %>%
    dplyr::select(-c(2)) %>%
    ungroup()

  # join the matched parents to the children
  # by parent age and parent age count
  # children data frame is the one to which observations must be joined
  # also add the household numbers at this point

   FullMatchedDataFrame <- left_join(ChildrenMatchPrep, ParentsMatched, by=c("ParentAge", "ParentAgeCount")) %>%
    dplyr::select(-ParentAge, -ParentAgeCount) %>%
    ungroup()

  # separate child and parent in data frames , use .x and .y to do the split

  # FirstDataframeSplit <- FullMatchedDataFrame %>%
  #   dplyr::select(ends_with(".x"), {{HouseholdNumVariable}}) %>%
  #   rename_all(list(~gsub("\\.x$", "", .)))
  #
  # SecondDataframeSplit <- FullMatchedDataFrame %>%
  #   dplyr::select(ends_with(".y"), {{HouseholdNumVariable}}) %>%
  #   rename_all(list(~gsub("\\.y$", "", .)))
  #
  #
  # OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)

  #####################################
  #####################################
  # pairing the parents to children ends here
  #####################################
  #####################################


  return(FullMatchedDataFrame)


}


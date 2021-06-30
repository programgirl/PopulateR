#' Create child- parent/guardian pairs with an added household identifier
#' This function creates a data frame of child-parent/guardian pairs, based on a population distribution of age differences. The distribution used in this function is the skew normal. However, the matching is affected by the age structure of the children and parent data frames. The distribution provides a framework upon which to base the matching. The final distribution of age differences, however, may not follow a skew normal distribution. The function requires the direct parameters for xi, omega, and alpha. These can be obtained using the extractSECdistr() function in the sn package. The function logs the xi and omega values.
#' Two data frames are required. The children data frame contains the age data, to which the Parent (Guardian) data will be applied.
#' The minimum and maximum ages of parents must be specified. This ensures that there are no parents who were too young (e.g. 11 years) or too old (e.g. 70 years) at the time the child was born. The presence of too young and too old parents is tested throughout this function. Thus, pre-cleaning the Parent data frame is not required..
#' The minimum proportion prevents the outcome where most/all people of a particular age, eg. the entire set of 25-year-olds, are parents. The default value is NULL, which assumes that all people of any age can be parents. The defalt value is 0, enabling a pre-cleaned data frame of parents to be used.
#' An even number of observations is output, which is one child-parent pair.
#'
#' The function performs a reasonableness check for child ID, child age, parent ID variable, and household number.
#'
#' @export
#' @param children A data frame containing the children to be paired with a parent/guardian.
#' @param chlidcol The column number for the ID variable in the children data frame.
#' @param chlagecol The column number for the age variable in the children data frame.
#' @param parents A data frame containing the potential parents. This data frame must contain at least the same number of observations as the children data frame.
#' @param paridcol The column number for the ID variable in the parents data frame.
#' @param paragecol The column number for the age variable in the parent data frame.
#' @param directxi If a skew-normal distribution is used, this is the location value. If the default alphaused value of 0 is used, this defaults to the mean value for the normal distribution.
#' @param directomega If a skew-normal distribution is used, this is the scale value. If the default alphaused value of 0 is used, this defaults to the standard deviation value for the normal distribution.
#' @param alphaused The skew. If a normal distribution is to be used, this can be omitted as the default value is 0 (no skew).
#' @param hhidstart The starting number for generating the household identifier value that identifies a couple. Must be numeric.
#' @param hhidvar The column name for the household variable. This must be supplied in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.


childno <- function(children, chlidcol, chlagecol, parents, paridcol, paragecol, directxi, directomega,
                    alphaused=0, minparage = NULL, maxparage = NULL, hhidstart = NULL, hhidvar= NULL,
                    UserSeed=NULL)

{

  options(dplyr.summarise.inform=F)

  # content check
  # content check
  if (!any(duplicated(children[chlidcol])) == FALSE) {
    stop("The column number for the ID variable in the child data frame must be supplied, and the ID must be unique to each child.")
  }

  if (!is.numeric(chlagecol)) {
    stop("Both the child ID and the child age column numbers must be supplied.")
  }

  if (!any(duplicated(parents[paridcol])) == FALSE) {
    stop("The column number for the ID variable in the parent data frame must be supplied, and the ID must be unique to each parent.")
  }

  if (is.null(minparage)) {
    stop("The minimum parent age must be supplied.")
  }

  if (is.null(maxparage)) {
    stop("The maximum parent age must be supplied.")
  }

  if(ncol(children) != ncol(parents)) {
    stop("The number of columns does not match between the children and parents' data frames.")
  }

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # Child variable names
  chlidcolName <- sym(names(children[chlidcol]))

  chlagecolName <- sym(names(children[chlagecol]))

  # Parent variable names
  parentsIDColName <- sym(names(parents[paridcol]))

  parentsAgeColName <- sym(names(parents[paragecol]))


  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  childrenRenamed <- children %>%
    rename(ChildID = !! chlidcol, ChildAge = !! chlagecol)


  parentsRenamed <- parents %>%
    rename(ParentID = !! paridcol, ParentAge = !! paragecol)


  minChildAge <- min(childrenRenamed$ChildAge)

  maxChildAge <- max(childrenRenamed$ChildAge)

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

  if (!(is.null(minparage))) {

    parentsRenamed <- parentsRenamed %>%
      filter((ParentAge - minChildAge) >= minparage)
  }

  if (!(is.null(maxparage))) {

    parentsRenamed <- parentsRenamed %>%
      filter((ParentAge - maxChildAge) <= maxparage)
  }

  # get counts for each single age from the parent data frame
  # ensure that any requirements to not use a particular number of counts per age is incorporated
  # ensure all parent ages are represented in the data frame of counts
  # use the minimum and maximum values to create an age sequence from minparage to maxparage

  ParentCounts <- parentsRenamed %>%
    group_by(ParentAge) %>%
    summarise(AgeCount=n()) %>%
    tidyr::complete(ParentAge = seq(min(ParentAge), max(ParentAge)),
                    fill = list(AgeCount = 0))


  minIndexAge <- as.integer(ParentCounts[1,1])
  maxIndexAge <- as.integer(ParentCounts[nrow(ParentCounts),1])

  ParentAgeCountVector <- ParentCounts$AgeCount

  # get the number of columns in the children data frames
  # this is important when splitting out the child and matched parent

  NumberColschildren <- as.numeric(ncol(childrenRenamed))

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

  for (j in 1:nrow(childrenRenamed)) {

    AgeDifference <- round(sn::rsn(1, xi = directxi, omega = directomega, alpha = alphaused, tau = 0),0)
    childrenRenamed$AgeDifference[j] <- AgeDifference
    childrenRenamed$ParentAge[j] <- childrenRenamed$ChildAge[j] + AgeDifference
    age_index <- childrenRenamed$ParentAge[j]-(minIndexAge -1)


    if (childrenRenamed$AgeDifference[j] >= minparage && childrenRenamed$AgeDifference[j] <= maxparage && ParentAgeCountVector[age_index] > 0 &&
        childrenRenamed$ParentAge[j] >= minIndexAge && childrenRenamed$ParentAge[j] <= maxIndexAge) {

      childrenRenamed$AgeDifference[j] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    } else {

      childrenRenamed$ParentAge[j] <- NA
      childrenRenamed$AgeDifference[j] <- NA

    }

  }


  # there may still be a relatively large number of children not matched, so repeat the matching process a second time
  # extract remove matched children from children data frame
  # the matched ones are removed from the children data frame as doing it the other way around is a nightmare for
  # referring to the age column in a newly constructed data frame

  Matchedchildren <- childrenRenamed %>%
    filter(!(is.na(ParentAge)))

  childrenRenamed <- childrenRenamed %>%
    filter(is.na(ParentAge))

  for (j in 1:nrow(childrenRenamed)) {

    AgeDifference <- round(sn::rsn(1, xi = directxi, omega = directomega, alpha = alphaused, tau = 0), 0)
    childrenRenamed$AgeDifference[j] <- AgeDifference
    childrenRenamed$ParentAge[j] <- childrenRenamed$ChildAge[j] + AgeDifference
    age_index <- childrenRenamed$ParentAge[j]-(minIndexAge -1)


    if (childrenRenamed$AgeDifference[j] >= minparage && childrenRenamed$AgeDifference[j] <= maxparage
        && ParentAgeCountVector[age_index] > 0 &&
        childrenRenamed$ParentAge[j] >= minIndexAge && childrenRenamed$ParentAge[j] <= maxIndexAge) {

      childrenRenamed$AgeDifference[j] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    } else {

      childrenRenamed$ParentAge[j] <- NA
      childrenRenamed$AgeDifference[j] <- NA

    }

  }

  MatchedSecondGo <- childrenRenamed %>%
    filter(!(is.na(ParentAge)))

  Matchedchildren <- rbind(Matchedchildren, MatchedSecondGo)

 #  cat("First rbind here", "\n")

  childrenRenamed <- childrenRenamed %>%
    filter(is.na(ParentAge))

 # cat("The number of children that need to be matched in future is", nrow(childrenRenamed), "\n")

  # force last lot of children to be matched on the basis of first parent age after minimum
  # need to work from minimum child age
  # find first current parent age that is still available
  # will stuff up distribution entered, but if the function has hit this point, the distribution did not fit

  # there may be no children that remain to be matched, test

  if(nrow(childrenRenamed) > 0) {


  for (j in 1:nrow(childrenRenamed)) {

    # estimate permissible parent age range

    Currentchild <- childrenRenamed[j,]

    Currentminparentage <- Currentchild$ChildAge + minparage
    Currentmaxparentage <- Currentchild$ChildAge + maxparage

    if(Currentmaxparentage > maxIndexAge) {

      Currentmaxparentage <- maxIndexAge
    }

    cat("Min parent age is", Currentminparentage, "Max parent age is", Currentmaxparentage)

   # parentrange <-

    return(ParentAgeCountVector)


    # ensure initial age selection is within min and max parent ages
#
#     AgeDifference <- round(runif(1, minparage, maxparage))
#     childrenRenamed$ParentAge[j] <- childrenRenamed$ChildAge[j] + AgeDifference
#     age_index <- childrenRenamed$ParentAge[j]-(minIndexAge -1)
#
     # if (ParentAgeCountVector[age_index] > 0)  {
     #
     #  ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
     #  childrenRenamed$AgeDifference[j] <- AgeDifference
#
    # } else {
    #
    #   childrenRenamed$AgeDifference[j] <- NA
    #   childrenRenamed$ParentAge[j] <- NA
    #
    #   age_index <- which.max(ParentAgeCountVector)
    #   childrenRenamed$ParentAge[j] <- age_index + (minIndexAge -1)
    #   childrenRenamed$AgeDifference[j] <- childrenRenamed$ParentAge[j] - childrenRenamed$ChildAge[j]
    #
    #   while (!(ParentAgeCountVector[age_index] > 0 && childrenRenamed$AgeDifference[j] >= minparage &&
    #            childrenRenamed$AgeDifference[j] <= maxparage)) {
    #
    #     age_index <- age_index + round(runif(1,-2,2),0)
    #
    #     if(age_index < 1) {
    #       age_index <- round(length(ParentAgeCountVector)*.2, 0)
    #     }
    #
    #     if(age_index > length(ParentAgeCountVector)) {
    #       age_index <- round(length(ParentAgeCountVector)*.8, 0)
    #     }
    #
    #     childrenRenamed$ParentAge[j] <- age_index + (minIndexAge -1)
    #     childrenRenamed$AgeDifference[j] <- childrenRenamed$ParentAge[j] - childrenRenamed$ChildAge[j]
    #
    #     #closes while index or age numbers are wrong
    #   }
    #
    #   ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
    #
    #   #closes if the age vector is greater than 0
    # }

    #closes the loop through children renamed
  }

  #   Combine the three children Dataframes

  childrenRenamed <- rbind(Matchedchildren, childrenRenamed)

  } else {

    childrenRenamed <- Matchedchildren
  }

 #  cat("Second rbind here", "\n")

  # #####################################
  #####################################
  # pairing the actual parent-child dyads starts here
  #####################################
  #####################################
  # return full donor and recipient rows as matched household pairs
  # extract ages counts for matching the donors
  MatchedParentAges <- childrenRenamed %>%
    dplyr::select(ParentAge) %>%
    group_by(ParentAge) %>%
    mutate(ParentAgeCount = row_number()) %>%
    ungroup()


  # generate same AgeCount second ID variable for the parent data
  # the AgeCount is used to ensure that the first parent with a specific age is matched first
  # the second parent with a specific age is matched second and so forth
  parentsToMatch <- parentsRenamed %>%
    group_by(ParentAge) %>%
    mutate(ParentAgeCount = row_number()) %>%
    ungroup()

  # reduce pool of potentially partnered donors to only those matched to recipients
  parentsMatched <- left_join(parentsToMatch, MatchedParentAges,
                              by = c("ParentAge", "ParentAgeCount"))


  # construct same file for the children
  # need both parent age and parent age count so that the join between the children and the parents works
  # do not need child age as this will be a duplicate column on the merge
  childrenMatchPrep <- childrenRenamed %>%
    group_by(ParentAge) %>%
    mutate(ParentAgeCount = row_number()) %>%
    ungroup()

  # join the matched parents to the children
  # by parent age and parent age count
  # children data frame is the one to which observations must be joined
  # also add the household numbers at this point

  FullMatchedDataFrame <- left_join(childrenMatchPrep, parentsMatched, by=c("ParentAge", "ParentAgeCount"))


  # separate child and parent in data frames
  Maxhhidstart <- (nrow(FullMatchedDataFrame)-1) + hhidstart

#  cat("Dyad value constructed", "\n")

  childrenFinal <- FullMatchedDataFrame %>%
    ungroup() %>%
    dplyr::select(all_of(1:NumberColschildren)) %>%
    rename_all(list(~gsub("\\.x$", "", .))) %>%
    mutate({{hhidvar}} := seq(hhidstart, Maxhhidstart))

#   cat("childrenFinal data frame constructed", "\n")

  parentsFinal <- FullMatchedDataFrame %>%
    ungroup() %>%
    dplyr::select(all_of((NumberColschildren+1): ncol(.))) %>%
    rename_all(list(~gsub("\\.y$", "", .))) %>%
    mutate({{hhidvar}} := seq(hhidstart, Maxhhidstart))

#   cat("parentsFinal data frame constructed", "\n")

  childrenFinal <- childrenFinal %>%
    rename(!!chlidcolName := ChildID, !!chlagecolName := ChildAge)

  parentsFinal <- parentsFinal %>%
    rename(!!parentsIDColName := ParentID, !!parentsAgeColName := ParentAge) %>%
    dplyr::select(-c(AgeDifference, ParentAgeCount))

  OutputDataframe <- rbind(parentsFinal, childrenFinal)

#  cat("Third rbind here")
#
#   #####################################
#   #####################################
#   # pairing the parents to children ends here
#   #####################################
#   #####################################


  return(OutputDataframe)


}


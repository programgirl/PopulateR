#' Create child- parent/guardian pairs as many-to-one  using an existing household identifier
#' This function creates a data frame of child-parent/guardian pairs, based on a distribution of age differences. Multiple children will be matched to the same parent.
#' Two data frames are required: one for children and one for potential parents.
#' The minimum and maximum ages of parents must be specified. This ensures that there are no parents who were too young (e.g. 11 years) or too old (e.g. 70 years) at the time the child was born. The presence of too young and too old parents is tested throughout this function. Thus, pre-cleaning the parents data frame is not required.
#'
#' @export
#' @param children A data frame containing the children to be paired with a parent/guardian.
#' @param chlidcol The column number for the ID variable in the children data frame.
#' @param chlagecol The column number for the age variable in the children data frame.
#' @param numchild The number of children that are required in each household.
#' @param twinrate The proportion of the child population who are twins.
#' @param parents A data frame containing the potential parents. This data frame must contain at least the same number of observations as the children data frame.
#' @param paridcol The column number for the ID variable in the parents data frame.
#' @param paragecol The column number for the age variable in the parent data frame.
#' @param minparage The youngest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param maxparage The oldest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param hhidcol The column number for the household identifier variable in the parent data frame
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A list of three  data frames. $Matched contains the data frame of child-parent matches. $Adults contains any unmatched observations from the parents data frame. $Children contains any unmatched observations from the children data frame. $Adults and/or $Children may be empty data frames.
#'
#' @examples
# library(dplyr)
# library("dplyr")
# set.seed(1)
# Parents <- Township %>%
#   filter(Relationship == "Partnered", Age > 18) %>%
#   slice_sample(n = 500) %>%
#   mutate(HouseholdID = row_number()+500)
#
# Children <- Township %>%
#   filter(Relationship == "NonPartnered", Age < 20) %>%
#   slice_sample(n = 400)
#
# # example with assigning two children to a parent
# # the same number of children is assigned to all parents
# # adding two children to each parent
#
# ChildrenMatchedID <- childrenyes(Children, chlidcol = 3, chlagecol = 4, numchild = 5,
#                      twinrate = .2, Parents, paridcol = 3, paragecol = 4,
#                      minparage = 18, maxparage = 54, hhidcol = 6,
#                      UserSeed = 4)



childrenno <- function(children, chlidcol, chlagecol, numchild = 2, twinrate = 0, parents, paridcol, paragecol,
                        minparage = NULL, maxparage = NULL, hhidstart = NULL, hhidvar= NULL, UserSeed=NULL)

{

  options(dplyr.summarise.inform=F)

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


  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  childrenRenamed <- children %>%
    rename(ChildID = !! chlidcol, ChildAge = !! chlagecol)

  parentsRenamed <- parents %>%
    rename(ParentID = !! paridcol, ParentAge = !! paragecol) %>%
    mutate(IntHHVar = 1:n())

  # Child variable names
  chlidcolName <- sym(names(children[chlidcol]))

  chlagecolName <- sym(names(children[chlagecol]))

  # Parent variable names
  parentsIDColName <- sym(names(parents[paridcol]))

  parentsAgeColName <- sym(names(parents[paragecol]))

  HouseholdColName <- sym(hhidvar)

  #####################################
  #####################################
  # end column names
  #####################################
  #####################################

  # Parent age variable

  minChildAge <- min(childrenRenamed$ChildAge)

  maxChildAge <- max(childrenRenamed$ChildAge)




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


  # create cut-down version (columns) for parent matching
  # the parent data can be linked to this later
  # have to restrict parent data frame to only those parent ages where there are at least min parent age + numkids
  # otherwise get into the problem of not enough parents for numkids of different ages

  # parentsRenamed <- parentsRenamed %>%
  #   filter(ParentAge >= minparage + (numchild*2) ,
  #          ParentAge <= maxparage + maxChildAge - (numchild*2)
  #   )

  parentsSubset <- parentsRenamed %>%
    select(ParentAge, ParentID, IntHHVar)

  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # cat("Number of rows of parents is", nrow(parentsRenamed), "\n", "Number of rows of children is", nrow(children), "\n")

  # remove any children that can't be allocated to parents if there are not enough parents

  if ((nrow(parentsRenamed)*numchild) < (nrow(children))) {

    childrenRenamed <- childrenRenamed %>%
      slice_sample(n = nrow(parentsRenamed)*numchild)

  }


  if (nrow(childrenRenamed) %% numchild != 0) {

    childrenRenamed <- childrenRenamed[-sample(1:nrow(childrenRenamed), nrow(childrenRenamed) %% numchild), ]

  }

  cat("Final size of child data frame is ", nrow(childrenRenamed), "\n")

  # get counts for each single age from the parent data frame
  # ensure that any requirements to not use a particular number of counts per age is incorporated
  # ensure all parent ages are represented in the data frame of counts
  # use the minimum and maximum values to create an age sequence from minparage to maxparage

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

  cat("Minimum parent age is ", minIndexAge, ", and maximum parent age is", maxIndexAge, "\n")

  ParentAgeCountVector <- ParentCounts$AgeCount

  # get the number of columns in the children data frames
  # this is important when splitting out the children matched data frames later

  NumberColschildren <- as.numeric(ncol(childrenRenamed))

  #####################################
  #####################################
  # end set up
  #####################################
  #####################################


  #####################################
  # GENERAL APPROACH
  # PICK ONE CHILD
  # MATCH THEM TO A MOTHER
  # MATCH OTHER children TO GET A WIDE FILE
  # TRANSFORM INTO LONG FILE
  #####################################

  #####################################
  #####################################
  # Functions for twins and their siblings
  #####################################
  #####################################


  #####################################
  #####################################
  # Split into twins and non-twins
  #####################################
  #####################################

  if (twinrate > 0) {

    TwinsDataFrame <- childrenRenamed %>%
      slice_sample(prop = twinrate/2)

    NoTwinsDataFrame <- childrenRenamed %>%
      filter(!(ChildID %in%  TwinsDataFrame$ChildID))

    TwinsMatched <- left_join(TwinsDataFrame %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                              NoTwinsDataFrame %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                              by = c("ChildAge", "Counter"))

    TwinsMatched <- as.data.frame(TwinsMatched)

    # add parent

    for (c in 1:nrow(TwinsMatched)) {

      CurrentAge <- sample(minIndexAge:maxIndexAge, 1, replace = FALSE, prob = c(ParentAgeCountVector))
      TwinsMatched$ParentAge[c] <- CurrentAge
      TwinsMatched$AgeDifference[c] <- CurrentAge - TwinsMatched$ChildAge[c]
      age_index <- TwinsMatched$ParentAge[c]-(minIndexAge -1)
      TwinsMatched$age_index[c] <- age_index

      # cat("1 age_index = ", age_index, "Age diff is", TwinsMatched$AgeDifference[c], "current age is", CurrentAge, "\n")

      while (ParentAgeCountVector[age_index] == 0 || TwinsMatched$AgeDifference[c] < minparage || TwinsMatched$AgeDifference[c] > maxparage) {

        CurrentAge <- sample(minIndexAge:maxIndexAge, 1, replace = FALSE, prob = c(ParentAgeCountVector))
        TwinsMatched$ParentAge[c] <- CurrentAge
        TwinsMatched$AgeDifference[c] <- CurrentAge - TwinsMatched$ChildAge[c]
        age_index <- TwinsMatched$ParentAge[c]-(minIndexAge -1)
        TwinsMatched$age_index[c] <- age_index

        # closes while loop
      }

      # cat("Current row is ",  c, "Age difference is ", TwinsMatched$AgeDifference[c], "age index is ", age_index,
      #     "Parent age count vector index is ", ParentAgeCountVector[age_index], "\n")

      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

      # closes parent match loop
    }

    TwinsMatched <- TwinsMatched %>%
      select(-c(AgeDifference, age_index))

    # remove matched twin ids from the available children in the NoTwinsDataFrame

    NoTwinsDataFrame <- NoTwinsDataFrame %>%
      filter(!(ChildID %in% c(TwinsMatched$ChildID.y)))

    #NoTwinsDF2

    #  add in the extra children to the twins, where there are more than 2 children in the household

    # create counts of children remaining so that the non-twins can be matched to the twins
    childrenCounts <- NoTwinsDataFrame %>%
      group_by(ChildAge) %>%
      summarise(AgeCount=n()) %>%
      tidyr::complete(ChildAge = seq(min(ChildAge), max(ChildAge)),
                      fill = list(AgeCount = 0))

    minChildIndexAge <- as.integer(childrenCounts[1,1])
    maxChildIndexAge <- as.integer(childrenCounts[nrow(childrenCounts),1])

    # cat("minChildIndexAge = ", minChildIndexAge, "maxChildIndexAge = ", maxChildIndexAge)

    childrenAgeCountVector <- childrenCounts$AgeCount

    # cat("childrenAgeCountVector=\n")
    # print(childrenAgeCountVector)
    # cat("\n")

    #####################################
    # Non-twins loop
    # only entered if there are more children required in addition to the twins
    #####################################

    if (numchild > 2) {

      #create the column names
      for (x in 3:numchild) {

        TwinsMatched <- TwinsMatched %>%
          tibble::add_column(!! paste0("ChildAge", x) := 1000)

        # closes column name loop
      }

      # it being a tibble seemed to be the problem for the looping below.

      TwinsMatched <- as.data.frame(TwinsMatched)

      # now iterate through the non-twins children
      # nested loop must be columns within rows

      for (x in 1:nrow(TwinsMatched)) {

        AgesUsed <- as.numeric(TwinsMatched$ChildAge[x])

        for (y in (ncol(TwinsMatched) - (numchild - 3)):ncol(TwinsMatched)) {

          NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(childrenAgeCountVector))
          TwinsMatched[x,y] <- NewChildAge
          AgeDifference <- TwinsMatched$ParentAge[x]- TwinsMatched[x,y]
          age_index <- (NewChildAge + 1) - minChildAge

          # cat("2 New child age is", NewChildAge, "age_index =", age_index, "Age diff is", AgeDifference, "\n")

          while (TwinsMatched[x,y] %in% (AgesUsed) || AgeDifference < minparage || AgeDifference > maxparage || childrenAgeCountVector[age_index] == 0) {

            # cat("Entered loop", "Current age is ",TwinsMatched[x,y], "Ages used are ", AgesUsed, "Parent age at childbirth is", AgeDifference, "\n")

            NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(childrenAgeCountVector))
            TwinsMatched[x,y] <- NewChildAge
            AgeDifference <- TwinsMatched$ParentAge[x]- TwinsMatched[x,y]
            age_index <- (NewChildAge + 1) - minChildAge

            # cat("3 New child age is", NewChildAge, "age_index =", age_index,  "Age diff is", AgeDifference, "\n")

            # close while test
          }

          childrenAgeCountVector[age_index] = childrenAgeCountVector[age_index] - 1
          AgesUsed <- cbind(AgesUsed, TwinsMatched[x,y])
          # print(AgesUsed)
          # #
          # #      # closes for column loop
          #
        }
        # #
        # #      # closes for numchild loop
      }
      # #
      # #      #closes if numchild test
    }

    # join in a parent to the parent age
    # but filter so that only parent ID and Household ID are joined
    # these will be the last 2 columns in the data frame
    # this means that the number of rbind-ed columns is known
    # so that the ChildAge[x] column indices can be used
    # as the parents data frame can contain any number of rows from 3 upwards
    # (Parent Age, Parent ID, Household ID)
    # this adds an additional two columns - Parent ID and Household ID

    TwinsMatched <- left_join(TwinsMatched %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                              parentsSubset %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                              by = c("ParentAge", "Counter"))

    # remove all parent information brought in via join, apart from Parent ID and Household ID
    # need to do this so that the joins work okay
    # as is based on column index

    FirstTwinMatched <- TwinsMatched %>%
      ungroup() %>%
      select(all_of(1:NumberColschildren), ncol(.)) %>%
      rename_all(list(~gsub("\\.x$", "", .)))

    SecondTwinMatched <- TwinsMatched %>%
      ungroup() %>%
      select(all_of((NumberColschildren+2):(NumberColschildren*2)), ChildAge,  ncol(.)) %>%
      rename_all(list(~gsub("\\.y$", "", .)))

    # remove SecondTwinMatched Child IDs from working children data frame
    # looks like it was done earlier, test
    # NoTwinsDataFrame <- NoTwinsDataFrame %>%
    #   filter(!(ChildID %in%  SecondTwinMatched$ChildID))

    TwinsFinal <- rbind(FirstTwinMatched, SecondTwinMatched)

    ParentOfTwins <- TwinsMatched %>%
      ungroup() %>%
      select((ncol(.)-1):ncol(.))

    ParentOfTwins <- left_join(ParentOfTwins, parentsRenamed, by = c("ParentID", "IntHHVar"))

    # extract remaining children and rbind these to each other
    # will eventually be rbind'ed to the twins and parent data

    # only use loop if number of children exceeds 2

    if(numchild > 2) {

      for (z in 3:numchild) {

        # cat("Number of children is", numchild, "and z is", z, "\n")

        OtherKids <- TwinsMatched %>%
          ungroup() %>%
          select(all_of((NumberColschildren*2)+z-1), ncol(.)) %>%
          rename(ChildAge = paste0("ChildAge", z))

        OtherKids <- left_join(OtherKids %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                               NoTwinsDataFrame %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                               by = c("ChildAge", "Counter")) %>%
          select(-Counter)

        TwinsFinal <- bind_rows(TwinsFinal, OtherKids)

        # cat("1 The number of rows of no twins is", nrow(NoTwinsDataFrame), "\n")

        NoTwinsDataFrame <- NoTwinsDataFrame %>%
          filter(!(ChildID %in% c(OtherKids$ChildID)))

        #closes extra child addition loop
      }

      # closes test to see if the number of children is > 2 and additional children should be added
    }

    # no twins data frame is updated with no allocated children
    # continues to be the same name

    ChildrenRenamed <- NoTwinsDataFrame

    # cat("2 The number of rows of no twins is", nrow(ChildrenRenamed), "\n")

    # update parent counts
    # remove parents already joined
    parentsRenamed <- parentsRenamed %>%
      filter(!(ParentID %in%  ParentOfTwins$ParentID))

    parentsSubset <- parentsRenamed %>%
      select(ParentAge, ParentID, IntHHVar)

    ParentCounts <- parentsRenamed %>%
      group_by(ParentAge) %>%
      summarise(AgeCount=n()) %>%
      tidyr::complete(ParentAge = seq(min(ParentAge), max(ParentAge)),
                      fill = list(AgeCount = 0))


    ParentAgeCountVector <- ParentCounts$AgeCount

    #closes twin set of functions
  }

  #####################################
  #####################################
  # non-twin age matching
  # entered for all households but all matches must be non-twins
  # replicates the non-twin matching above
  # with a different start because first match is a non-twin
  # and subsequent matches are all non-twins as well
  #####################################
  #####################################

  # create data frame that contains the children that form the base to which the parents and other kids will be joined

  BaseDataFrame <- ChildrenRenamed %>%
    slice_sample(n = nrow(.)/numchild)

  BaseDataFrame <- as.data.frame(BaseDataFrame)

  # match parent
  # cat("number of rows in base dataframe is", nrow(BaseDataFrame), "\n")

  for (c in 1:nrow(BaseDataFrame)) {

    Currentchild <- BaseDataFrame[c,]

    Currentmin <- (minparage - minIndexAge) + Currentchild$ChildAge + 1
    Currentmax <- Currentchild$ChildAge + maxparage - (minIndexAge -1)

    if(Currentmin < 1) {
      Currentmin <- 1
    }

    if(Currentmax > (maxIndexAge-(minIndexAge-1))) {

      Currentmax <- maxIndexAge-(minIndexAge-1)
    }

    parentprobs <-ParentAgeCountVector[Currentmin:Currentmax]

    parentindex <- seq(Currentmin, Currentmax, by=1)


    if(sum(parentprobs > 0)) {

      # cat("sum of parent probs is", sum(parentprobs), "\n")

      age_index <- sample(parentindex, 1, prob=c(parentprobs))
      #
      #       cat("parent index is", parentindex, "\n")
      #       cat("parent probs are", parentprobs, "\n")
      #       cat("and age_index is",  age_index, "\n")

      Currentchild$ParentAge = age_index + minIndexAge - 1
      Currentchild$AgeDifference = Currentchild$ParentAge - Currentchild$ChildAge

      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

      if(exists("InterimDataFrame")) {
        InterimDataFrame <- bind_rows(InterimDataFrame, Currentchild)
      } else {
        InterimDataFrame <- Currentchild
        # closes build of data frame of children matched to parents
      }

      # closes if(sum(parentprobs > 0))
    }

    # closes for (c in 1:nrow(BaseDataFrame))
  }

  BaseDataFrame <- InterimDataFrame

  BaseDataFrame <- left_join(BaseDataFrame %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                             parentsSubset %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                             by = c("ParentAge", "Counter"))

  # construct the child ages remaining into a vector

  BaseDataFrame <- BaseDataFrame %>%
    select(-c(AgeDifference, Counter))

  # remove the matched children ids from the available children in ChildrenRenamed

  ChildrenRenamed <- ChildrenRenamed %>%
    filter(!(ChildID %in%  c(BaseDataFrame$ChildID)))

  #  add in the extra children to the twins, where there are more than 2 children in the household

  # create counts of children remaining so that the rest of the children are brought into the dataframe
  # also, the ChildAge variable has to start from 2 and not 3, as only one child has been matched in the BaseDataFrame

  ChildrenCounts <- ChildrenRenamed %>%
    group_by(ChildAge) %>%
    summarise(AgeCount=n()) %>%
    tidyr::complete(ChildAge = seq(min(ChildAge), max(ChildAge)),
                    fill = list(AgeCount = 0))

  minChildIndexAge <- as.integer(ChildrenCounts[1,1])
  maxChildIndexAge <- as.integer(ChildrenCounts[nrow(ChildrenCounts),1])

  ChildrenAgeCountVector <- ChildrenCounts$AgeCount

  # cat("ChildrenAgeCountVector=", ChildrenAgeCountVector, "\n")

  # match the remaining children

  # create the column names

  for (x in 2:numchild) {

    BaseDataFrame <- BaseDataFrame %>%
      tibble::add_column(!! paste0("ChildAge", x) := 1000)

    # closes column name loop
  }

  # it being a tibble seemed to be the problem for the looping below.

  BaseDataFrame <- as.data.frame(BaseDataFrame)

  # there are problems with the younger parents not having children available
  # BaseDataFrame <- BaseDataFrame %>%
  #  arrange(ParentAge)

  # now iterate through the other children
  # nested loop must be columns within rows

  for (x in 1:nrow(BaseDataFrame)) {

    AgesUsed <- as.numeric(BaseDataFrame$ChildAge[x])

    for (y in (NumberColschildren + 4):ncol(BaseDataFrame)) {

      # cat("Numbercols is", NumberColschildren, "x is", x, "and y is", y, "\n")

      Counter <- 0

      NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
      BaseDataFrame[x,y] <- NewChildAge
      AgeDifference <- BaseDataFrame$ParentAge[x]- BaseDataFrame[x,y]
      age_index <- (NewChildAge + 1) - minChildAge

      # cat("4 New child age is", NewChildAge, "age_index =", age_index,  "Age diff is", AgeDifference, "\n")

      while (BaseDataFrame[x,y] %in% (AgesUsed) || AgeDifference < minparage || AgeDifference > maxparage || ChildrenAgeCountVector[age_index] == 0) {


        NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
        BaseDataFrame[x,y] <- NewChildAge
        AgeDifference <- BaseDataFrame$ParentAge[x]- BaseDataFrame[x,y]
        age_index <- (NewChildAge + 1) - minChildAge

        # cat("5 New child age is", NewChildAge, "age_index =", age_index,  "Age diff is", AgeDifference, "\n")

        Counter <- Counter + 1

        if (Counter == 1000 ) {

          # if (ChildrenAgeCountVector[age_index] == 0) {
          #   cat("No children were available at the ages tested", "The problem household is", BaseDataFrame$IntHHVar[x],"\n")
          # }

          if (AgeDifference < minparage || AgeDifference > maxparage) {

            # cat("No credible available parent ages were located", "The problem household is", BaseDataFrame$IntHHVar[x],"\n")

            if (exists("WrongParentAge")) {
              WrongParentAge <- c(WrongParentAge, BaseDataFrame$IntHHVar[x])

            } else {
              WrongParentAge <- as.vector(BaseDataFrame$IntHHVar[x])
            }
          }

          if (BaseDataFrame[x,y] %in% (AgesUsed)) {
            # cat("Twins were constructed even though the twin families were previously allocated", "The problem household is", BaseDataFrame$IntHHVar[x],"\n")


            if (exists("ShouldNotBeTwins")) {
              ShouldNotBeTwins <- c(ShouldNotBeTwins, BaseDataFrame$IntHHVar[x])

            } else {
              ShouldNotBeTwins <- as.vector(BaseDataFrame$IntHHVar[x])
            }

          }

          break
        }

        # closes while test
      }

      # cat("Row is ", x, "Current age is ", BaseDataFrame[x,y], "Ages used are ", AgesUsed, "Parent age is ", BaseDataFrame$ParentAge[x], "\n")

      ChildrenAgeCountVector[age_index] = ChildrenAgeCountVector[age_index] - 1
      AgesUsed <- cbind(AgesUsed, BaseDataFrame[x,y])

      # cat("4 New child age is", NewChildAge, "age_index =", age_index,"\n")
      # print(ChildrenAgeCountVector)
      # cat("\n")

      # closes for column loop
    }

    # closes for numchild loop
  }

  # base data frame contains the original child and numchild matched - 1
  # so a three-person household has two additional children matched

  NotTwins <- BaseDataFrame %>%
    ungroup() %>%
    select(all_of(1:NumberColschildren), NumberColschildren+3)

  NoTwinsDataFrame <- NoTwinsDataFrame %>%
    filter(!(ChildID %in% c(NotTwins$ChildID)))

  ParentOfNotTwins <- BaseDataFrame %>%
    ungroup() %>%
    select(all_of((NumberColschildren+2) : (NumberColschildren+3)))

  ParentOfNotTwins <- left_join(ParentOfNotTwins, parentsRenamed, by = c("ParentID", "IntHHVar"))

  # extract remaining children and rbind these to each other
  # will eventually be rbind'ed to the twins and parent data


  # extract the base child
  BaseNonTwin <- BaseDataFrame %>%
    select(all_of(1:c(NumberColschildren)), NumberColschildren+3)

  # return other children
  # need HH ID and child age

  for(z in 2:numchild) {

    CurrentChildIteration <- BaseDataFrame %>%
      select(NumberColschildren+3, NumberColschildren+(3+z-1)) %>%
      rename_with(.cols = 2, ~"ChildAge")

    if(exists("ChildrenToAdd")) {

      ChildrenToAdd <- bind_rows(ChildrenToAdd, CurrentChildIteration)

    } else {

      ChildrenToAdd <- bind_rows(CurrentChildIteration)

    }

  }

  # merge the long ChildrenToAdd file with the remaining children
  # who generated the ChildrenAgeCountVector
  # this is ChildrenRenamed data
  # match on age, but use a count for each age
  # e.g. 1st row with 10 gets count of 1, 2nd row with 10 gets count of 2
  # this creates a unique combination using two columns

  ChildrenRenamed <- ChildrenRenamed %>%
    group_by(ChildAge) %>%
    mutate(AgeCounter = row_number()) %>%
    ungroup()

  ChildrenToAdd <- ChildrenToAdd %>%
    group_by(ChildAge) %>%
    mutate(AgeCounter = row_number()) %>%
    ungroup()

  ChildrenRenamed <- right_join(ChildrenRenamed, ChildrenToAdd, by = c("ChildAge", "AgeCounter")) %>%
    #  ChildrenRenamed <- left_join(ChildrenRenamed, ChildrenToAdd, by = c("ChildAge", "AgeCounter")) %>%
    select(-AgeCounter)

  # bind the children in the base dataframe

  NoTwinsDataFrame <- bind_rows(BaseNonTwin, ChildrenRenamed)



  #####################################
  #####################################
  # join all the data frames together
  #####################################
  #####################################


  if (exists("TwinsFinal")) {
    # ChildrenFinal <- rbind(TwinsFinal, NoTwinsDataFrame)
    ChildrenFinal <- NoTwinsDataFrame


  } else {
    ChildrenFinal <- NotTwins
  }

  parentsFinal <- ParentOfNotTwins


  ChildrenFinal <- ChildrenFinal

  parentsFinal <- parentsFinal

  #####################################
  #####################################
  # fix the out-of-bounds ages and households with twins who shouldn't have
  #####################################
  #####################################
  if(exists("WrongParentAge")) {

    # # convert to unique households
    WrongParentAge <- unique(WrongParentAge)

    WrongParentAgeHouseholds <- parentsFinal %>%
      filter(IntHHVar %in% c(WrongParentAge))

    AmendedparentsFinal <- parentsFinal %>%
      filter(!IntHHVar %in% c(WrongParentAgeHouseholds$IntHHVar))

    AmendedChildrenFinal <- ChildrenFinal %>%
      filter(!IntHHVar %in% c(WrongParentAgeHouseholds$IntHHVar))

    #extract out parent age
    for(a in 1:length(WrongParentAge)) {
      CurrentHouseholdID <- WrongParentAgeHouseholds$IntHHVar[a]

      IncorrectParentAge <- WrongParentAgeHouseholds %>%
        filter(IntHHVar == WrongParentAgeHouseholds$IntHHVar[a]) %>%
        rename(IncorrectParentAge = ParentAge) %>%
        pull(IncorrectParentAge)

      # print(IncorrectParentAge)

      if(IncorrectParentAge <= maxparage) {
        PermittedChildAgeMin <- 0
      } else {
        PermittedChildAgeMin <- IncorrectParentAge - maxparage

        if(PermittedChildAgeMin < 0) {
          PermittedChildAgeMin <- 0
        }
      }

      PermittedChildAgeMax <- IncorrectParentAge - minparage

      # get the problem children
      ChildProblemAges <- ChildrenFinal %>%
        filter(IntHHVar == WrongParentAgeHouseholds$IntHHVar[a] &
                 !(between(ChildAge, PermittedChildAgeMin, PermittedChildAgeMax)))

      # move through this data frame doing swaps with children in the Children Final data frame
      for (b in 1:nrow(ChildProblemAges)) {

        SwapLoopCount <- 1

        Swap <- "No"

        AgeToSwap <- ChildProblemAges$ChildAge[b]

        # extract random child age
        # draw a random number for the data frame check to start
        Startrow <- round(runif(1, min= 1, max = nrow(AmendedparentsFinal)))

        # print(Startrow)

        while (SwapLoopCount < nrow(AmendedparentsFinal) & Swap == "No") {

          PossibleMatch <- AmendedChildrenFinal[Startrow,]

          # cat("Current household ID is", CurrentHouseholdID, "and matched household ID is",
          #     PossibleMatch$IntHHVar, "\n")

          # need to check:
          # 1. will the swap to the problem household recreate the problem with the
          #    problem household parent being too young or too old?
          # 2. will the swap to the donor create a problem with the donor being too young or too old?
          # 3. will the swap create a twin for the recipient
          # 4. will the swap create a twin for the donor

          # test 1
          MatchedAge <- PossibleMatch$ChildAge

          Test1 <- IncorrectParentAge - MatchedAge

          MatchedHousehold <- PossibleMatch$IntHHVar

          Test2 <- AmendedparentsFinal %>%
            filter(IntHHVar == MatchedHousehold) %>%
            select(ParentAge) %>%
            mutate(DonorAgeDiff = ParentAge - AgeToSwap) %>%
            pull(DonorAgeDiff)

          Test3 <- ChildrenFinal %>%
            filter(IntHHVar == WrongParentAgeHouseholds$IntHHVar[a] &
                     !(ChildID == ChildProblemAges$ChildID[b])) %>%
            rename(OtherAges = ChildAge) %>%
            select(OtherAges) %>%
            pull(OtherAges)

          Test4 <- ChildrenFinal %>%
            filter(IntHHVar == PossibleMatch$IntHHVar,
                   !(ChildID == PossibleMatch$ChildID)) %>%
            rename(OtherAges = ChildAge) %>%
            select(OtherAges) %>%
            pull(OtherAges)

          if(between(Test1, minparage, maxparage) == TRUE &
             between(Test2, minparage, maxparage) == TRUE &
             (MatchedAge %in% c(Test3)) == FALSE  &
             (AgeToSwap %in% c(Test4)) == FALSE) {

            # cat("Swap is okay", "\n")

            Swap <- "Yes"

            SwapChildRowIndex <- as.numeric(which(ChildrenFinal$ChildID==PossibleMatch$ChildID))
            ProblemChildRowIndex <- as.numeric(which(ChildrenFinal$ChildID==ChildProblemAges$ChildID[b]))

            # cat("The donor row index is", SwapChildRowIndex, "and the problem child row index is",
            #     ProblemChildRowIndex, "\n")

            # do the swapping
            # note: this is directly to the file used, so there is no interim file
            ChildrenFinal[SwapChildRowIndex, "IntHHVar"] <- ChildProblemAges$IntHHVar[b]
            ChildrenFinal[ProblemChildRowIndex, "IntHHVar"] <- PossibleMatch$IntHHVar

            # closes if loop for check if the swap parameters are in range

          }

          SwapLoopCount <- SwapLoopCount + 1

          Startrow <- Startrow + 1

          if(Startrow > nrow(AmendedparentsFinal)) {

            Startrow <- 1

          }

          # closes while (SwapLoopCount < nrow(AmendedparentsFinal) & Swap == "No")
        }


        if(Swap == "No" & Startrow == nrow(AmendedparentsFinal)) {

          # cat("Reached max", "\n")


          ChildrenFinal <- ChildrenFinal %>%
            filter(!(IntHHVar == CurrentHouseholdID))

          parentsFinal <- parentsFinal %>%
            filter(!(IntHHVar == CurrentHouseholdID))
        }

        # closes for (b in 1:nrow(ChildProblemAges))
      }

      # closes for(a in 1:length(WrongParentAge))
    }

    # closes if(exists("WrongParentAge"))
  }


  if (exists("ShouldNotBeTwins")) {

    ShouldNotBeTwins <- unique(ShouldNotBeTwins)

    # cat("There is a problem with too many twins in the data, with", length(ShouldNotBeTwins), "households affected", "\n")

    WrongTwinHouseholds <- parentsFinal %>%
      filter(IntHHVar %in% c(ShouldNotBeTwins))

    AmendedparentsFinal <- parentsFinal %>%
      filter(!IntHHVar %in% c(WrongTwinHouseholds$IntHHVar))

    for(a in 1:nrow(WrongTwinHouseholds)) {
      CurrentHouseholdID <- WrongTwinHouseholds$IntHHVar[a]

      # cat("Current wrong twin household is", CurrentHouseholdID, "\n")

      IncorrectParentAge <- WrongTwinHouseholds %>%
        filter(IntHHVar == WrongTwinHouseholds$IntHHVar[a]) %>%
        rename(IncorrectParentAge = ParentAge) %>%
        pull(IncorrectParentAge)

      if(IncorrectParentAge <= maxparage) {
        PermittedChildAgeMin <- 0
      } else {
        PermittedChildAgeMin <- IncorrectParentAge - maxparage

        if(PermittedChildAgeMin < 0) {
          PermittedChildAgeMin <- 0
        }

        PermittedChildAgeMax <- IncorrectParentAge - minparage

        # closes if(IncorrectParentAge <= maxparage)
      }

      # cat("Parent age is", IncorrectParentAge, "and child minimum age is", PermittedChildAgeMin,
      #     "and household ID is", CurrentHouseholdID, "\n")

      # randomly select a duplicate child
      # the function assumes only one child in the household needs to be swapped
      # in a incorrect triplet household, this would mean one triplet swapped out so twins remained
      # ignoring that at the moment

      # identify the duplicate age/s

      DuplicateAges <- ChildrenFinal %>%
        filter(IntHHVar == CurrentHouseholdID) %>%
        group_by(ChildAge) %>%
        filter(n()>1) %>%
        summarise(AgesToFix = n()) %>%
        pull(ChildAge)

      if (!(is.na(DuplicateAges)) == TRUE) {

        for(b in 1:length(DuplicateAges)) {

          Swap <- "No"

          SwapLoopCount <- 1

          AgeToSwap <- DuplicateAges[b]

          # extract random child age
          # draw a random number for the data frame check to start
          Startrow <- round(runif(1, min= 1, max = nrow(AmendedparentsFinal)))

          print(Startrow)

          # sample that age from the household
          SampledIncorrectTwin <- ChildrenFinal %>%
            filter(IntHHVar == CurrentHouseholdID,
                   ChildAge == AgeToSwap) %>%
            slice_sample(n = 1)

          while (SwapLoopCount < nrow(AmendedparentsFinal) & Swap == "No") {

            PossibleMatch <- AmendedChildrenFinal[Startrow,]

            # cat("Current household ID is", CurrentHouseholdID, "and matched household ID is", PossibleMatch$IntHHVar, "\n")

            #         # need to check:
            #         # 1. will the swap to the problem household recreate the problem with the
            #         #    problem household parent being too young or too old?
            #         # 2. will the swap to the donor create a problem with the donor being too young or too old?
            #         # 3. will the swap create a twin for the recipient
            #         # 4. will the swap create a twin for the donor
            #
            #         # test 1
            MatchedAge <- PossibleMatch$ChildAge

            Test1 <- IncorrectParentAge - MatchedAge

            MatchedHousehold <- PossibleMatch$IntHHVar

            Test2 <- AmendedparentsFinal %>%
              filter(IntHHVar == MatchedHousehold) %>%
              select(ParentAge) %>%
              mutate(DonorAgeDiff = ParentAge - AgeToSwap) %>%
              pull(DonorAgeDiff)

            Test3 <- ChildrenFinal %>%
              filter(IntHHVar == WrongTwinHouseholds$IntHHVar[a]) %>%
              rename(OtherAges = ChildAge) %>%
              select(OtherAges) %>%
              pull(OtherAges)

            Test4 <- ChildrenFinal %>%
              filter(IntHHVar == PossibleMatch$IntHHVar &
                       !(ChildID == PossibleMatch$ChildID)) %>%
              rename(OtherAges = ChildAge) %>%
              select(OtherAges) %>%
              pull(OtherAges)

            if(between(Test1, minparage, maxparage) == TRUE &
               between(Test2, minparage, maxparage) == TRUE &
               (MatchedAge %in% c(Test3)) == FALSE  &
               (AgeToSwap %in% c(Test4)) == FALSE) {


              # cat("ChildID is", SampledIncorrectTwin$ChildID, "\n")
              # cat("Swap is okay", "\n")

              SwapChildRowIndex <- as.numeric(which(ChildrenFinal$ChildID==PossibleMatch$ChildID))
              ProblemChildRowIndex <- as.numeric(which(ChildrenFinal$ChildID==SampledIncorrectTwin$ChildID))

              Swap <- "Yes"

              # cat("The donor row index is", SwapChildRowIndex, "and the problem child row index is",
              #     ProblemChildRowIndex, "\n")
              #
              # cat("The original household IDs are", SampledIncorrectTwin$IntHHVar, "for the problem child, and",
              #     PossibleMatch$IntHHVar, "for the child that will be swapped in", "\n")

              # do the swapping
              # note: this is directly to the file used, so there is no interim file
              ChildrenFinal[SwapChildRowIndex, "IntHHVar"] <- SampledIncorrectTwin$IntHHVar
              ChildrenFinal[ProblemChildRowIndex, "IntHHVar"] <- PossibleMatch$IntHHVar

              # closes if loop for check if the swap parameters are in range
            }

            SwapLoopCount <- SwapLoopCount + 1

            Startrow <- Startrow + 1

            if(Startrow > nrow(AmendedparentsFinal)) {

              Startrow <- 1

              # closes loop for swapping
            }

            if(Swap == "No") {

              ChildrenFinal <- ChildrenFinal %>%
                filter(!(IntHHVar == CurrentHouseholdID))

              parentsFinal <- parentsFinal %>%
                filter(!(IntHHVar == CurrentHouseholdID))

              # closes if(Swap == "No")
            }

            # close while (SwapLoopCount < nrow(AmendedparentsFinal))
          }

          # closes for(b in 1:length(DuplicateAges))
        }

        # closes if (!(is.na(DuplicateAges)) == TRUE)
      }

      # closes  for(a in 1:nrow(WrongTwinHouseholds))
    }

    # closes if (exists("ShouldNotBeTwins"))
  }

  # reconstruct interim data frame so corrected with children
  if (exists("TwinsFinal")) {
    ChildrenFinal <- bind_rows(ChildrenFinal, TwinsFinal)
  }

  # remove any children who are not in the right household size
  KidsWrongSize <- ChildrenFinal %>%
    group_by(IntHHVar) %>%
    summarise(KidsInHH = n()) %>%
    filter(KidsInHH < numchild)

  if(exists("KidsWrongSize")) {

    ChildrenFinal <- ChildrenFinal %>%
      filter(!(IntHHVar %in% c(KidsWrongSize$IntHHVar)))

    parentsFinal <- parentsFinal %>%
      filter(!(IntHHVar %in% c(KidsWrongSize$IntHHVar)))

    # closes if(exists("KidsWrongSize"))
  }


  if (exists("ParentOfTwins")) {
    parentsFinal <- rbind(parentsFinal, ParentOfTwins)
  }

  hhidend <- hhidstart+(nrow(parentsFinal)-1)

  parentsFinal <- parentsFinal %>%
    mutate(NewHHVar = hhidstart:hhidend)

  JoinHHValues <- parentsFinal %>%
    select(IntHHVar, NewHHVar)

  ChildrenFinal <- left_join(ChildrenFinal, JoinHHValues, by = "IntHHVar")

  ChildrenFinal <- ChildrenFinal %>%
    select(-IntHHVar) %>%
    rename(!!chlidcolName := ChildID, !!chlagecolName := ChildAge,
           {{hhidvar}} := NewHHVar)

  parentsFinal <- parentsFinal %>%
    select(-IntHHVar) %>%
    rename(!!parentsIDColName := ParentID, !!parentsAgeColName := ParentAge,
           {{hhidvar}} := NewHHVar)

  OutputDataframe <- bind_rows(ChildrenFinal, parentsFinal)

  MatchedIDs <- OutputDataframe %>%
    pull({{parentsIDColName}})

  Nokids <- parents %>%
    filter(!({{parentsIDColName}} %in% MatchedIDs))

  Noparents <- children %>%
    filter(!({{parentsIDColName}} %in% MatchedIDs))

  MergedList <- list()

  MergedList$Matched <- OutputDataframe
  MergedList$Adults <- Nokids
  MergedList$Children <- Noparents

  return(MergedList)

  return(OutputDataframe)

  # closes function
}

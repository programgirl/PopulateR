#' Create a subset of observations containing only children matched to parents/guardians
#' This function creates a data frame of child-parent/guardian pairs, based on a population distribution of age differences. The distribution used in this function is the log normal. However, the matching is affected by the age structure of the children and parent data frames. The distribution provides a framework upon which to base the matching. The final distribution of age differences, however, may not follow a lognormal distribution.
#' Two data frames are required. The Children data frame contains the age data, to which the Parent (Guardian) data will be applied.
#' The minimum and maximum ages of parents must be specified. This ensures that there are no parents who were too young (e.g. 11 years) or too old (e.g. 70 years) at the time the child was born. The presence of too young and too old parents is tested throughout this function. Thus, pre-cleaning the Parent data frame is not required.
#' It is possible that some children may not be matched, or will be matched incorrectly. While the code attempts to prevent this, incorrect matching may occur. First, twins may be allocated to a household not selected for the presence of twins. Second, there may be no suitable parent ages for the remaining child ages. If either of these problems occur, a message will be printed to the console, identifying the problem household. Testing has shown that use of a different set.seed() may remove the problem. Alternatively, a manual fix can be performed after using this function.
#' The function only outputs the children and parents that have been matched. As the output combines the children and parents into one data frame. The number of columns in the parent data frame must be one larger than the number of columns in the children data frame, as the parents data frame is the only one that contains the household ID variable. In the case where a problem in matching has occurred, a combined parent/children data frame is still output.
#'
#' The function performs a reasonableness check for child ID, child age, parent ID variable, and household number.
#'
#' @export
#' @param Children A data frame containing observations limited to the children to be matched An age column is required. All children in this data frame will be matched to a parent/guardian.
#' @param ChildIDCol The column number for the ID variable in the Children data frame.
#' @param ChildAgeCol The column number for the Age variable in the Children data frame.
#' @param NumChildren The number of children that are required in each household.
#' @param TwinRate The proportion of the child population who are twins.
#' @param Parents A data frame containing observations limited to parents. An age column is required. This can contain the entire set of people who can be parents, as the assignment is made on age at becoming a parent, not current age. This file can contain the people who can be guardians, as well as parents. This data frame should contain more observations than the Children data frame. The relative sizes of the data frames are compared. If the Parens data frame is not sufficiently larger than the Children data frame, the latter is randomly sampled to construct a smaller data frame.
#' @param ParentIDCol The column number for the ID variable in the Parent data frame.
#' @param ParentAgeCol The column number for the Age variable in the Parent data frame.
#' @param MinParentAge The youngest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param MaxParentAge The oldest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param MinPropRemain The minimum proportion of people, at each age, who are not parents. The default is zero, which may result in all people at a specific age being allocated as parents. This will leave age gaps for any future work, and may not be desirable. If nrow(Children) == nrow(Parents), assigning any value other than 0 will result in an error.
#' @param HouseholdIDCol The column number for the household variable in the Parents data frame. This must be provided.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

AddChildren <- function(Children, ChildIDCol, ChildAgeCol, NumChildren = 2, TwinRate = 0, Parents, ParentIDCol, ParentAgeCol,
                        MinParentAge = NULL, MaxParentAge = NULL, HouseholdIDCol= NULL, UserSeed=NULL)

{

  options(dplyr.summarise.inform=F)

  # content check
  if (!any(duplicated(Children[ChildIDCol])) == FALSE) {
    stop("The column number for the ID variable in the child data frame must be supplied, and the ID must be unique to each child.")
  }

  if (!is.numeric(ChildAgeCol)) {
    stop("Both the child ID and the child age column numbers must be supplied.")
  }

  if (!any(duplicated(Parents[ParentIDCol])) == FALSE) {
    stop("The column number for the ID variable in the parent data frame must be supplied, and the ID must be unique to each parent.")
  }

  if (is.null(MinParentAge)) {
    stop("The minimum parent age must be supplied.")
  }

  if (is.null(MaxParentAge)) {
    stop("The maximum parent age must be supplied.")
  }

  if (!any(duplicated(Parents[HouseholdIDCol])) == FALSE) {
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
    rename(ChildID = !! ChildIDCol, ChildAge = !! ChildAgeCol)

  ParentsRenamed <- Parents %>%
    rename(ParentID = !! ParentIDCol, ParentAge = !! ParentAgeCol,
           HouseholdID = !! HouseholdIDCol)

  # Parent age variable

  minChildAge <- min(ChildrenRenamed$ChildAge)

  maxChildAge <- max(ChildrenRenamed$ChildAge)

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


  # create cut-down version (columns) for parent matching
  # the parent data can be linked to this later
  # have to restrict parent data frame to only those parent ages where there are at least min parent age + numkids
  # otherwise get into the problem of not enough parents for numkids of different ages

  ParentsRenamed <- ParentsRenamed %>%
    filter(ParentAge >= MinParentAge + (NumChildren*2) ,
           ParentAge <= MaxParentAge + maxChildAge - (NumChildren*2)
    )

  ParentsSubset <- ParentsRenamed %>%
    select(ParentAge, ParentID, HouseholdID)


  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  cat("Number of rows of parents is", nrow(ParentsRenamed), "\n", "Number of rows of children is", nrow(Children), "\n")

  if ((nrow(ParentsRenamed)*NumChildren) < (nrow(Children)*1.1)) {

    ChildrenRenamed <- ChildrenRenamed %>%
      slice_sample(n = round(nrow(ParentsRenamed) *.8, 0)*NumChildren)
  }

  if (nrow(ChildrenRenamed) %% NumChildren != 0) {

    ChildrenRenamed <- ChildrenRenamed[-sample(1:nrow(ChildrenRenamed), nrow(ChildrenRenamed) %% NumChildren), ]
  }

  cat("Final size of child data frame is ", nrow(ChildrenRenamed), "\n")

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
  # this is important when splitting out the children matched data frames later

  NumberColsChildren <- as.numeric(ncol(ChildrenRenamed))

  #####################################
  #####################################
  # end set up
  #####################################
  #####################################


  #####################################
  # GENERAL APPROACH
  # PICK ONE CHILD
  # MATCH THEM TO A MOTHER
  # MATCH OTHER CHILDREN TO GET A WIDE FILE
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

  if (TwinRate > 0) {

    TwinsDataFrame <- ChildrenRenamed %>%
      slice_sample(prop = TwinRate/2)

     NoTwinsDataFrame <- ChildrenRenamed %>%
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

      while (ParentAgeCountVector[age_index] == 0 || TwinsMatched$AgeDifference[c] < MinParentAge || TwinsMatched$AgeDifference[c] > MaxParentAge) {

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
    ChildrenCounts <- NoTwinsDataFrame %>%
      group_by(ChildAge) %>%
      summarise(AgeCount=n()) %>%
      tidyr::complete(ChildAge = seq(min(ChildAge), max(ChildAge)),
                      fill = list(AgeCount = 0))

    minChildIndexAge <- as.integer(ChildrenCounts[1,1])
    maxChildIndexAge <- as.integer(ChildrenCounts[nrow(ChildrenCounts),1])

    # cat("minChildIndexAge = ", minChildIndexAge, "maxChildIndexAge = ", maxChildIndexAge)

    ChildrenAgeCountVector <- ChildrenCounts$AgeCount

    # cat("ChildrenAgeCountVector=\n")
    # print(ChildrenAgeCountVector)
    # cat("\n")

    #####################################
    # Non-twins loop
    # only entered if there are more children required in addition to the twins
    #####################################

    if (NumChildren > 2) {

      #create the column names
      for (x in 3:NumChildren) {

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

        for (y in (ncol(TwinsMatched) - (NumChildren - 3)):ncol(TwinsMatched)) {

          NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
          TwinsMatched[x,y] <- NewChildAge
          AgeDifference <- TwinsMatched$ParentAge[x]- TwinsMatched[x,y]
          age_index <- (NewChildAge + 1) - minChildAge

          # cat("2 New child age is", NewChildAge, "age_index =", age_index, "Age diff is", AgeDifference, "\n")

          while (TwinsMatched[x,y] %in% (AgesUsed) || AgeDifference < MinParentAge || AgeDifference > MaxParentAge || ChildrenAgeCountVector[age_index] == 0) {

            # cat("Entered loop", "Current age is ",TwinsMatched[x,y], "Ages used are ", AgesUsed, "Parent age at childbirth is", AgeDifference, "\n")

            NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
            TwinsMatched[x,y] <- NewChildAge
            AgeDifference <- TwinsMatched$ParentAge[x]- TwinsMatched[x,y]
            age_index <- (NewChildAge + 1) - minChildAge

            # cat("3 New child age is", NewChildAge, "age_index =", age_index,  "Age diff is", AgeDifference, "\n")

            # close while test
          }

          ChildrenAgeCountVector[age_index] = ChildrenAgeCountVector[age_index] - 1
          AgesUsed <- cbind(AgesUsed, TwinsMatched[x,y])
          # print(AgesUsed)
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

    # join in a parent to the parent age
    # but filter so that only parent ID and Household ID are joined
    # these will be the last 2 columns in the data frame
    # this means that the number of rbind-ed columns is known
    # so that the ChildAge[x] column indices can be used
    # as the Parents data frame can contain any number of rows from 3 upwards
    # (Parent Age, Parent ID, Household ID)
    # this adds an additional two columns - Parent ID and Household ID

    TwinsMatched <- left_join(TwinsMatched %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                              ParentsSubset %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                              by = c("ParentAge", "Counter"))

    # remove all parent information brought in via join, apart from Parent ID and Household ID
    # need to do this so that the joins work okay
    # as is based on column index

    FirstTwinMatched <- TwinsMatched %>%
      ungroup() %>%
      select(all_of(1:NumberColsChildren), ncol(.)) %>%
      rename_all(list(~gsub("\\.x$", "", .)))

    SecondTwinMatched <- TwinsMatched %>%
      ungroup() %>%
      select(all_of((NumberColsChildren+2):(NumberColsChildren*2)), ChildAge,  ncol(.)) %>%
      rename_all(list(~gsub("\\.y$", "", .)))

    # remove SecondTwinMatched Child IDs from working Children data frame
    # looks like it was done earlier, test
    # NoTwinsDataFrame <- NoTwinsDataFrame %>%
    #   filter(!(ChildID %in%  SecondTwinMatched$ChildID))

    TwinsFinal <- rbind(FirstTwinMatched, SecondTwinMatched)

    ParentOfTwins <- TwinsMatched %>%
      ungroup() %>%
      select((ncol(.)-1):ncol(.))

    ParentOfTwins <- left_join(ParentOfTwins, ParentsRenamed, by = c("ParentID", "HouseholdID"))

    # extract remaining children and rbind these to each other
    # will eventually be rbind'ed to the twins and parent data

    # only use loop if number of children exceeds 2

    if(NumChildren > 2) {

    for (z in 3:NumChildren) {

      # cat("Number of children is", NumChildren, "and z is", z, "\n")

      OtherKids <- TwinsMatched %>%
        ungroup() %>%
        select(all_of((NumberColsChildren*2)+z-1), ncol(.)) %>%
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
    ParentsRenamed <- ParentsRenamed %>%
      filter(!(ParentID %in%  ParentOfTwins$ParentID))

    ParentsSubset <- ParentsRenamed %>%
      select(ParentAge, ParentID, HouseholdID)

    ParentCounts <- ParentsRenamed %>%
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
    slice_sample(n = nrow(.)/NumChildren)

  BaseDataFrame <- as.data.frame(BaseDataFrame)

  # match parent

  for (c in 1:nrow(BaseDataFrame)) {

    CurrentAge <- sample(minIndexAge:maxIndexAge, 1, replace = FALSE, prob = c(ParentAgeCountVector))
    BaseDataFrame$ParentAge[c] <- CurrentAge
    BaseDataFrame$AgeDifference[c] <- CurrentAge - BaseDataFrame$ChildAge[c]
    age_index <- BaseDataFrame$ParentAge[c]-(minIndexAge -1)
    BaseDataFrame$age_index[c] <- age_index

    while (ParentAgeCountVector[age_index] == 0 || BaseDataFrame$AgeDifference[c] < MinParentAge || BaseDataFrame$AgeDifference[c] > MaxParentAge) {

      CurrentAge <- sample(minIndexAge:maxIndexAge, 1, replace = FALSE, prob = c(ParentAgeCountVector))
      BaseDataFrame$ParentAge[c] <- CurrentAge
      BaseDataFrame$AgeDifference[c] <- CurrentAge - BaseDataFrame$ChildAge[c]
      age_index <- BaseDataFrame$ParentAge[c]-(minIndexAge -1)
      BaseDataFrame$age_index[c] <- age_index

      # closes while loop
    }

    # cat("Current row is ",  c, "Age difference is ", TwinsMatched$AgeDifference[c], "age index is ", age_index,
    #     "Parent age count vector index is ", ParentAgeCountVector[age_index], "\n")

    ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    # closes parent match loop
  }


  BaseDataFrame <- left_join(BaseDataFrame %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                             ParentsSubset %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                             by = c("ParentAge", "Counter"))

  # construct the child ages remaining into a vector

  BaseDataFrame <- BaseDataFrame %>%
    select(-c(AgeDifference, age_index, Counter))

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
  # cat("ChildrenAgeCountVector=\n")
  # print(ChildrenAgeCountVector)
  # cat("\n")

  # match the remaining children

  # create the column names
  for (x in 2:NumChildren) {

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

    for (y in (NumberColsChildren + 4):ncol(BaseDataFrame)) {

      Counter <- 0

      NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
      BaseDataFrame[x,y] <- NewChildAge
      AgeDifference <- BaseDataFrame$ParentAge[x]- BaseDataFrame[x,y]
      age_index <- (NewChildAge + 1) - minChildAge

    #   cat("4 New child age is", NewChildAge, "age_index =", age_index,  "Age diff is", AgeDifference, "\n")

      while (BaseDataFrame[x,y] %in% (AgesUsed) || AgeDifference < MinParentAge || AgeDifference > MaxParentAge || ChildrenAgeCountVector[age_index] == 0) {


        NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
        BaseDataFrame[x,y] <- NewChildAge
        AgeDifference <- BaseDataFrame$ParentAge[x]- BaseDataFrame[x,y]
        age_index <- (NewChildAge + 1) - minChildAge

        # cat("5 New child age is", NewChildAge, "age_index =", age_index,  "Age diff is", AgeDifference, "\n")

        Counter <- Counter + 1

        if (Counter == 1000 ) {

          if (ChildrenAgeCountVector[age_index] == 0) {
            cat("No children were available at the ages tested", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")
          }

          if (AgeDifference < MinParentAge || AgeDifference > MaxParentAge) {

            # cat("No credible available parent ages were located", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")

            if (exists("WrongParentAge")) {
              WrongParentAge <- c(WrongParentAge, BaseDataFrame$HouseholdID[x])

            } else {
              WrongParentAge <- as.vector(BaseDataFrame$HouseholdID[x])
            }
          }

          if (BaseDataFrame[x,y] %in% (AgesUsed)) {
            # cat("Twins were constructed even though the twin families were previously allocated", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")


          if (exists("ShouldNotBeTwins")) {
            ShouldNotBeTwins <- c(ShouldNotBeTwins, BaseDataFrame$HouseholdID[x])

          } else {
            ShouldNotBeTwins <- as.vector(BaseDataFrame$HouseholdID[x])
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

    # closes for numchildren loop
  }

  # base data frame contains the original child and numchildren matched - 1
  # so a three-person household has two additional children matched

  NotTwins <- BaseDataFrame %>%
    ungroup() %>%
    select(all_of(1:NumberColsChildren), NumberColsChildren+3)

  NoTwinsDataFrame <- NoTwinsDataFrame %>%
    filter(!(ChildID %in% c(NotTwins$ChildID)))

  ParentOfNotTwins <- BaseDataFrame %>%
    ungroup() %>%
    select(all_of((NumberColsChildren+2) : (NumberColsChildren+3)))

  ParentOfNotTwins <- left_join(ParentOfNotTwins, ParentsRenamed, by = c("ParentID", "HouseholdID"))

  # extract remaining children and rbind these to each other
  # will eventually be rbind'ed to the twins and parent data

  # extract the base child
  BaseNonTwin <- BaseDataFrame %>%
    select(all_of(1:c(NumberColsChildren)), NumberColsChildren+3)

   # return other children
  # need HH ID and child age

  for(z in 2:NumChildren) {

    CurrentChildIteration <- BaseDataFrame %>%
      select(NumberColsChildren+3, NumberColsChildren+(3+z-1)) %>%
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

  ChildrenRenamed <- left_join(ChildrenRenamed, ChildrenToAdd, by = c("ChildAge", "AgeCounter")) %>%
    select(-AgeCounter)

  # bind the children in the base dataframe

  NoTwinsDataFrame <- bind_rows(BaseNonTwin, ChildrenRenamed)

  #####################################
  #####################################
  # join all the data frames together
  #####################################
  #####################################


  if (exists("TwinsFinal")) {
    ChildrenFinal <- rbind(TwinsFinal, NoTwinsDataFrame)

  } else {
    ChildrenFinal <- NotTwins
  }

  if (exists("ParentOfTwins")) {
    ParentsFinal <- rbind(ParentOfTwins, ParentOfNotTwins)

  } else {
    ParentsFinal <- ParentOfNotTwins
  }

  ChildrenFinal <- ChildrenFinal #%>%
    #rename(PersonID = ChildID, Age = ChildAge)

  ParentsFinal <- ParentsFinal #%>%
    #rename(PersonID = ParentID, Age = ParentAge)

   #  InterimDataframe <- rbind(ParentsFinal, ChildrenFinal)

  #####################################
  #####################################
  # fix the out-of-bounds ages and households with twins who shouldn't have
  #####################################
  #####################################

# deal with too young parents first
  if(exists("WrongParentAge")) {

    # # convert to unique households
    WrongParentAge <- unique(WrongParentAge)

    WrongParentAgeHouseholds <- ParentsFinal %>%
      filter(HouseholdID %in% c(WrongParentAge))

    AmendedParentsFinal <- ParentsFinal %>%
      filter(!HouseholdID %in% c(WrongParentAgeHouseholds$HouseholdID))

    #extract out parent age
    for(a in 1:length(WrongParentAge)) {
      CurrentHouseholdID <- WrongParentAgeHouseholds$HouseholdID[a]

      IncorrectParentAge <- WrongParentAgeHouseholds %>%
        filter(HouseholdID == WrongParentAgeHouseholds$HouseholdID[a]) %>%
        rename(IncorrectParentAge = ParentAge) %>%
        pull(IncorrectParentAge)

        if(IncorrectParentAge < MaxParentAge + 1) {
          PermittedChildAgeMin <- 0
        } else {
          PermittedChildAgeMin <- IncorrectParentAge - MaxParentAge
        }

        PermittedChildAgeMax <- IncorrectParentAge - MinParentAge

      # get the problem children
      ChildProblemAges <- ChildrenFinal %>%
        filter(HouseholdID == WrongParentAgeHouseholds$HouseholdID[a] &
               !(between(ChildAge, PermittedChildAgeMin, PermittedChildAgeMax)))

      # move through this data frame doing swaps with children in the Children Final data frame
      Swap <- 0
      SwapLoopCount <- 1

      for (b in 1:nrow(ChildProblemAges)) {

        AgeToSwap <- ChildProblemAges$ChildAge[b]

        while (Swap == 0 & SwapLoopCount < 200) {

        # extract random child age
        PossibleMatch <- ChildrenFinal %>%
          filter(HouseholdID %in% c(AmendedParentsFinal$HouseholdID)) %>%
          slice_sample(n= 1)

        # cat("Current household ID is", CurrentHouseholdID, "and matched household ID is", PossibleMatch$HouseholdID, "\n")

        # need to check:
        # 1. will the swap to the problem household recreate the problem with the
        #    problem household parent being too young or too old?
        # 2. will the swap to the donor create a problem with the donor being too young or too old?
        # 3. will the swap create a twin for the recipient
        # 4. will the swap create a twin for the donor

        # test 1
        MatchedAge <- PossibleMatch$ChildAge
        Test1 <- IncorrectParentAge - MatchedAge

        MatchedHousehold <- PossibleMatch$HouseholdID

        Test2 <- AmendedParentsFinal %>%
          filter(HouseholdID == MatchedHousehold) %>%
          select(ParentAge) %>%
         mutate(DonorAgeDiff = ParentAge - AgeToSwap) %>%
         pull(DonorAgeDiff)

        Test3 <- ChildrenFinal %>%
          filter(HouseholdID == WrongParentAgeHouseholds$HouseholdID[a] &
                 !(ChildID == ChildProblemAges$ChildID[b])) %>%
          rename(OtherAges = ChildAge) %>%
          select(OtherAges) %>%
          pull(OtherAges)

        Test4 <- ChildrenFinal %>%
          filter(HouseholdID == PossibleMatch$HouseholdID &
                 !(ChildID == PossibleMatch$ChildID)) %>%
          rename(OtherAges = ChildAge) %>%
          select(OtherAges) %>%
          pull(OtherAges)

        # cat("Problem child age is", AgeToSwap, "Matched child age is", MatchedAge, "Test 1 is", Test1,
        #     "Test 2 is", Test2, "Test 3 data are", Test3, "Test 4 data are", Test4,
        #     "matched parent household is", PossibleMatch$HouseholdID, "\n")


        if(between(Test1, PermittedChildAgeMin, PermittedChildAgeMax) == TRUE &
        between(Test2, MinParentAge, MaxParentAge) == TRUE &
        !(MatchedAge %in% c(Test4)) == FALSE  &
        !(AgeToSwap %in% c(Test3)) == FALSE) {

          cat("Swap is okay", "\n")

          SwapChildRowIndex <- as.numeric(which(ChildrenFinal$ChildID==PossibleMatch$ChildID))
          ProblemChildRowIndex <- as.numeric(which(ChildrenFinal$ChildID==ChildProblemAges$ChildAge[b]))

          # cat("The donor row index is", SwapChildRowIndex, "and the problem child row index is",
          #     ProblemChildRowIndex, "\n")

              # do the swapping
              # note: this is directly to the file used, so there is no interim file
              ChildrenFinal[SwapChildRowIndex, HouseholdID] <- ProblemChildHouseholdID
              ChildrenFinal[ProblemChildRowIndex, HouseholdID] <- SwapChildHouseholdID

       # closes if loop for check if the swap parameters are in range

          Swap <- 1
        }

        SwapLoopCount <- SwapLoopCount + 1
          # closes while swap loop for no swap
        }

        if(SwapLoopCount == 200) {

          # cat("No match", "\n")

          # get parents of correct age, who are not in the final parents data frame into a new data frame
          # ensure that they can take all the child ages for the children in the household
          # as this is an entire household swap, not just for one child.

          # get all child ages in problem household
          AllAgesInProblemHousehold <- ChildrenFinal %>%
            filter(HouseholdID == WrongParentAgeHouseholds$HouseholdID[a]) %>%
            pull(ChildAge)

          MinProblemChildAge <- min(AllAgesInProblemHousehold)
          MaxProblemChildAge <- max(AllAgesInProblemHousehold)

          # cat("Minimum child age is", MinProblemChildAge, "Maximum child age is", MaxProblemChildAge, "\n")

          MinNewParentAge <- MinParentAge + MaxProblemChildAge
          MaxNewParentAge <- MaxParentAge + MinProblemChildAge

          # cat("Minimum parent age is", MinNewParentAge, "Maximum parent age is", MaxNewParentAge, "\n")

          NewSampleParent <- ParentsRenamed %>%
            filter(!(HouseholdID %in% c(ParentsFinal$HouseholdID)),
                   between(ParentAge, MinNewParentAge, MaxNewParentAge)) %>%
            slice_sample(n = 1)

          if(nrow(NewSampleParent) == 0) {

            # the problem is not fixable, so stop the function

            stop("There are an insufficient number of suitably aged parents in the Parent data frame")
          }

          # swap in the new parent for the old
          # remove the old parent from the Parents data frame
          AmendedParentsFinal <- AmendedParentsFinal %>%
            bind_rows(NewSampleParent)

          ParentsRenamed <- ParentsRenamed %>%
            filter(!(ParentID == NewSampleParent$ParentID))

          ChildrenReassignedHouseholdID <- ChildrenFinal %>%
            filter(HouseholdID == WrongParentAgeHouseholds$HouseholdID[a])

          ChildrenReassignedHouseholdID$HouseholdID <- NewSampleParent$HouseholdID

          ChildrenFinal <- ChildrenFinal %>%
            filter(!(ChildID %in% c(ChildrenReassignedHouseholdID$ChildID))) %>%
            bind_rows(ChildrenReassignedHouseholdID)



          # reassign the household ID for all the children in the problem household


          # closes fix if there for no swap in  data frames used
        }

        # closes loop through the problem child data frame
      }

      # closes loop through problem ages
    }

      # closes loop to check if there is a problem ages data frame
  }


  #  InterimDataframe <- rbind(ParentsFinal, ChildrenFinal)


  if (exists("ShouldNotBeTwins")) {

    ShouldNotBeTwins <- unique(ShouldNotBeTwins)

    cat("There is a problem with too many twins in the data, with", length(ShouldNotBeTwins), "households affected", "\n")

    WrongTwinHouseholds <- ParentsFinal %>%
      filter(HouseholdID %in% c(ShouldNotBeTwins))

     AmendedParentsFinal <- ParentsFinal %>%
      filter(!HouseholdID %in% c(WrongTwinHouseholds$HouseholdID))

    for(a in 1:length(WrongTwinHouseholds)) {
      CurrentHouseholdID <- WrongTwinHouseholds$HouseholdID[a]

      IncorrectParentAge <- WrongTwinHouseholds %>%
        filter(HouseholdID == WrongTwinHouseholds$HouseholdID[a]) %>%
        rename(IncorrectParentAge = ParentAge) %>%
        pull(IncorrectParentAge)

      if(IncorrectParentAge < MaxParentAge + 1) {
        PermittedChildAgeMin <- 0
      } else {
        PermittedChildAgeMin <- IncorrectParentAge - MaxParentAge
      }

      # cat("Parent age is", IncorrectParentAge, "and child minimum age is", PermittedChildAgeMin,
      #     "and household ID is", CurrentHouseholdID, "\n")

      # randomly select a duplicate child
      # the function assumes only one child in the household needs to be swapped
      # in a incorrect triplet household, this would mean one triplet swapped out so twins remained
      # ignoring that at the moment

      # identify the duplicate age/s

      DuplicateAges <- ChildrenFinal %>%
        filter(HouseholdID == CurrentHouseholdID) %>%
        group_by(ChildAge) %>%
        filter(n()>1) %>%
        summarise(AgesToFix = n()) %>%
        pull(ChildAge)

      for(b in 1:length(DuplicateAges)) {

        AgeToSwap <- DuplicateAges[b]

    #    print(AgeToSwap)

        # sample that age from the household
        SampledIncorrectTwin <- ChildrenFinal %>%
          filter(HouseholdID == CurrentHouseholdID,
                 ChildAge == AgeToSwap) %>%
          slice_sample(n = 1)

        return(SampledIncorrectTwin)

        # close for loop moving through the duplicate ages vector
      }

      # closes loop through fixing the households that incorrectly contain twins
    }

    # closes the if loop for if there are too many households with twins
    }


  #   ParentTooYoung <- unique(ParentTooYoung)
  #
  #   cat("The number of parents too young are", length(ParentTooYoung), "\n")
  #
  #   if(length(ParentTooYoung) > 1) {
  #
  # for (a in 1:length(ParentTooYoung)) {
  #
  #  print(ParentTooYoung[a])
  #
  #   # extract problem household
  #   ProblemHousehold <- InterimDataframe %>%
  #     filter(HouseholdID == ParentTooYoung[a])
  #
  #   # identify the ages out of range
  #   # extract parent
  #   ProblemHouseholdParent <- ProblemHousehold %>%
  #     slice_head(n = 1)
  #
  #   PermittedChildAgeMin <- max(minChildAge, (ProblemHouseholdParent$Age - MaxParentAge))
  #   PermittedChildAgeMax <- ProblemHouseholdParent$Age - MinParentAge
  #
  #   # test parent permitted age conditions
  #   # cat("Minimum child age is", PermittedChildAgeMin, "maximum child age is", PermittedChildAgeMax, "for parent age of", ProblemHouseholdParent$Age, "\n")
  #
  #   ProblemHouseholdChildren <- ProblemHousehold %>%
  #     slice(- 1) %>%
  #     filter(!(between(Age, PermittedChildAgeMin, PermittedChildAgeMax)))
  #
  #   OkayAges <- ProblemHousehold %>%
  #     filter(!(PersonID %in% ProblemHouseholdParent$PersonID) &
  #              !(PersonID %in% ProblemHouseholdChildren$PersonID)) %>%
  #     select(Age) %>%
  #     pull(Age)
  #
  #   for (b in 1:nrow(ProblemHouseholdChildren)) {
  #
  #     # print(ProblemHouseholdChildren$PersonID[b])
  #
  #     PossibleSwapHouseholds <- ParentsFinal %>%
  #       filter(!(HouseholdID %in% ProblemHousehold$HouseholdID),
  #              !(PersonID %in% ParentOfTwins$ParentID),
  #              between(Age, ProblemHouseholdChildren$Age[b] + 18, ProblemHouseholdChildren$Age[b] + MaxParentAge))
  #
  #     # test that the swap is correct, i.e. child ages are within bounds for both the children in the swap
  #     # cat("Minimum parent age is", ProblemHouseholdChildren$Age[b] + 18, "and maximum parent age is", ProblemHouseholdChildren$Age[b] + MaxParentAge,
  #     # "for household", ProblemHousehold$HouseholdID[b], "\n")
  #
  #     PossibleSwapChildren <- ChildrenFinal %>%
  #       filter(HouseholdID %in% PossibleSwapHouseholds$HouseholdID)
  #
  #     WouldOtherwiseHaveTwins <- PossibleSwapChildren %>%
  #       filter(Age %in% OkayAges)
  #
  #     print(nrow(WouldOtherwiseHaveTwins))
  #
  #     # delete these households from the possible children list
  #     PossibleSwapChildren <- PossibleSwapChildren %>%
  #       filter(!(HouseholdID %in% WouldOtherwiseHaveTwins$HouseholdID),
  #              between(Age, max(ProblemHouseholdParent$Age - 54, 0), ProblemHouseholdParent$Age - 18))
  #
  #     print(nrow(PossibleSwapChildren))
  #
  #     # only do the code below if the number of rows in PossibleSwapChildren is 2 or more
  #
  #
  #     # randomly select a child to swap, what will actually swap is the household ID
  #     ChildToSwap <- PossibleSwapChildren %>%
  #       slice_sample(n = 1)
  #
  #     SwapChildHouseholdID <- ChildToSwap$HouseholdID
  #     ProblemChildHouseholdID <- ProblemHouseholdChildren$HouseholdID[b]
  #
  #     cat("Child", ChildToSwap$PersonID, "in household ID", ChildToSwap$HouseholdID, "will donate household ID to", ProblemHouseholdChildren$PersonID[b], "in", ProblemHouseholdChildren$HouseholdID[b], "\n")
  #
  #     # perform the swapping, only household ID to be swapped
  #
  #     SwapChildRowIndex <- as.numeric(which(ChildrenFinal$PersonID==ChildToSwap$PersonID))
  #     ProblemChildRowIndex <- as.numeric(which(ChildrenFinal$PersonID==ProblemHouseholdChildren$PersonID[b]))
  #
  #     # cat("The donor row index is", SwapChildRowIndex, "and the problem child row index is", ProblemChildRowIndex, "\n")
  #
  #     # do the swapping
  #     # note: this is directly to the file used, so there is no interim file
  #     ChildrenFinal[SwapChildRowIndex, HouseholdIDCol] <- ProblemChildHouseholdID
  #     ChildrenFinal[ProblemChildRowIndex, HouseholdIDCol] <- SwapChildHouseholdID
  #
  #     SwapChildAge <- ChildToSwap %>%
  #       pull(Age)
  #
  #    OkayAges <- c(OkayAges, SwapChildAge)
  #
  #    # cat("Household ID is", ProblemHousehold$HouseholdID, "\n")
  #     # return(OkayAges)
  #   }
  #
  #   # close fix for the households with children who are too old
  # }
  #
  #   # close if loop for ensuring there is an even number of households to swap
  #
  # }
#
#     # reconstruct interim data frame so corrected with children
#     InterimDataframe <- rbind(ParentsFinal, ChildrenFinal)
#
    # close test for whether problem households exist
  # }

  # InterimDataframe <- InterimDataframe

 #  #####################################
 #  #####################################
 #  # fix the households which incorrectly contain twins
 #  #####################################
 #  #####################################
 #
 #  if(exists("ShouldNotBeTwins")) {
 #
 #    ShouldNotBeTwins <- unique(ShouldNotBeTwins)
 #
 #    cat("The number of parents who should not have twins is", length(ShouldNotBeTwins), "\n")
 #
 # for (c in 1:length(ShouldNotBeTwins)) {
 #
 #    #   print(ParentTooYoung[a])
 #
 #    # extract problem household
 #    ProblemHousehold <- InterimDataframe %>%
 #      filter(HouseholdID == ShouldNotBeTwins[c])
 #
 #    ProblemHouseholdParent <- ProblemHousehold %>%
 #      slice_head(n = 1)
 #
 #    PermittedChildAgeMin <- max(minChildAge, (ProblemHouseholdParent$Age - MaxParentAge))
 #    PermittedChildAgeMax <- ProblemHouseholdParent$Age - MinParentAge
 #
 #    # identify the ages out of range
 #    # extract parent
 #    ProblemHouseholdChildren <- ProblemHousehold %>%
 #      slice(-1)
 #
 #    # duplicates may have been fixed by the swaps in the earlier section
 #    # see if age duplicates exist and, if, so the age of the duplicates
 #
 #    DuplicatedAge <- ProblemHouseholdChildren %>%
 #      group_by(Age) %>%
 #      summarise(DuplicateAge = n()) %>%
 #      filter(DuplicateAge > 1) %>%
 #      mutate(NumberToReplace = DuplicateAge - 1)
 #
 #    # there may no longer be any incorrect duplicates after fixing the incorrectly aged children
 #    # so put this bit in a wrapper to test whether the DuplicatedAge dataframe has any content
 #
 #    if(!(is.na(DuplicatedAge$Age[1])) == TRUE) {
 #
 #    # deduct 1 from each duplicate age as only need to match one of them
 #    # but also need to grab out the okay ages so these aren't selected by random
 #    # which would create twins, rather than removing them
 #    # NOTE: the age of the shouldn-be-twins is fine, because one twin is NOT being replaced
 #
 #    OkayAges <- ProblemHouseholdChildren %>%
 #      select(Age) %>%
 #      distinct(Age) %>%
 #      pull(Age)
 #
 #    # this needs to loop just in case there is more than one duplicated age
 #    for (d in 1:nrow(DuplicatedAge)) {
 #
 #      # get the number of children that age to be replaced
 #      CountToReplace <- as.numeric(DuplicatedAge[d,3])
 #
 #      AgeToReplace <- as.numeric(DuplicatedAge[d,1])
 #
 #      # cat("Age to replace is", AgeToReplace, "and number to replace is", CountToReplace, "in household", ProblemHouseholdChildren$HouseholdID[1] , "\n")
 #
 #      # cat("The number of children aged", AgeToReplace, "to be replaced is", CountToReplace, "\n" )
 #
 #      # use the NumToReplace to loop through the children that age, who are duplicates
 #      # selecting one child that age each time, so the replacement count is updated each time in the loop
 #      # and therefore no requirement to affect d
 #
 #      for (e in 1:CountToReplace) {
 #
 #        # select child to replace
 #        ProblemChild <- ProblemHouseholdChildren %>%
 #          filter(Age == AgeToReplace) %>%
 #          slice_sample(n = 1)
 #
 #          PossibleSwapHouseholds <- ParentsFinal %>%
 #            filter(!(HouseholdID %in% ProblemHousehold$HouseholdID),
 #                   !(PersonID %in% ParentOfTwins$ParentID),
 #                   between(Age, ProblemChild$Age + 18, ProblemChild$Age + MaxParentAge))
 #
 #          # cat("The number of rows in the possible swap households is", nrow(PossibleSwapHouseholds), "\n")
 #
 #          if(nrow(PossibleSwapHouseholds) > 0) {
 #
 #          # test that the swap is correct, i.e. child ages are within bounds for both the children in the swap
 #          # cat("Minimum parent age is", ProblemChild$Age + 18, "and maximum parent age is", ProblemChild$Age + MaxParentAge,
 #          # "for household", ProblemChild$HouseholdID, "\n")
 #
 #          PossibleSwapChildren <- ChildrenFinal %>%
 #            filter(HouseholdID %in% PossibleSwapHouseholds$HouseholdID)
 #
 #          WouldOtherwiseHaveTwins <- PossibleSwapChildren %>%
 #            filter(Age %in% OkayAges)
 #
 #          # delete these households from the possible children list
 #          PossibleSwapChildren <- PossibleSwapChildren %>%
 #            filter(!(HouseholdID %in% WouldOtherwiseHaveTwins$HouseholdID),
 #                   between(Age, max(ProblemHouseholdParent$Age - 54, 0), ProblemHouseholdParent$Age - 18))
 #
 #          if (nrow(PossibleSwapChildren)==0) {
 #
 #            cat("There are no children available to swap for", ProblemChild$PersonID, "in household ID", ProblemChild$HouseholdID, "\n")
 #
 #          }
 #
 #
 #          if(!(is.na(PossibleSwapChildren$Age[1]) == TRUE)) {
 #
 #
 #          # randomly select a child to swap, what will actually swap is the household ID
 #          ChildToSwap <- PossibleSwapChildren %>%
 #            slice_sample(n = 1)
 #
 #          SwapChildHouseholdID <- ChildToSwap$HouseholdID
 #          ProblemChildHouseholdID <- ProblemChild$HouseholdID
 #
 #          # cat("Child", ChildToSwap$PersonID, "in household ID", ChildToSwap$HouseholdID, "will donate household ID to", ProblemChild$PersonID,
 #          #     "in", ProblemChild$HouseholdID, "\n")
 #
 #          # perform the swapping, only household ID to be swapped
 #
 #          SwapChildRowIndex <- as.numeric(which(ChildrenFinal$PersonID==ChildToSwap$PersonID))
 #          ProblemChildRowIndex <- as.numeric(which(ChildrenFinal$PersonID==ProblemChild$PersonID))
 #
 #          # cat("The donor row index is", SwapChildRowIndex, "and the problem child row index is", ProblemChildRowIndex, "\n")
 #
 #          # do the swapping
 #          # note: this is directly to the file used, so there is no interim file
 #          ChildrenFinal[SwapChildRowIndex, HouseholdIDCol] <- ProblemChildHouseholdID
 #          ChildrenFinal[ProblemChildRowIndex, HouseholdIDCol] <- SwapChildHouseholdID
 #
 #          SwapChildAge <- ChildToSwap %>%
 #            pull(Age)
 #
 #          OkayAges <- c(OkayAges, SwapChildAge)
 #
 #          #closes test to make sure that a swap occurs only if a child is available to be swapped
 #          }
 #
 #          #closes test to make sure that a swap household is available
 #          }
 #
 #          # print(as.numeric(ProblemChild$PersonID))
 #          ProblemHouseholdChildren <- ProblemHouseholdChildren %>%
 #            filter(PersonID != ProblemChild$PersonID)
 #
 #
 #
 #        # closes loop through each child that needs to be replaced (i.e. swap of household ID)
 #      }
 #
 #      # closes loop for all should-not-be twins in the same household
 #    }
 #
 #    # closes loop while test for whether there is a DuplicatedAges dataframe
 #    }
 #
 #    # closes loop through all households that should not contain twins, but do
 #    }
 #
 #    # reconstruct interim data frame so corrected with children
  ChildrenFinal <- ChildrenFinal %>%
  rename(PersonID = ChildID, Age = ChildAge)

  ParentsFinal <- ParentsFinal %>%
  rename(PersonID = ParentID, Age = ParentAge)

    InterimDataframe <- rbind(ParentsFinal, ChildrenFinal)
 #    # closes test for whether households with incorrect duplicate child ages exist
 #  }

  # OutputDataframe <- InterimDataframe

 # return(ChildrenFinal)

  # closes function
}

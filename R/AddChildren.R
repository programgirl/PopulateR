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
#' @param ChildIDVariable The column number for the ID variable in the Children data frame.
#' @param ChildAgeVariable The column number for the Age variable in the Children data frame.
#' @param NumChildren The number of children that are required in each household.
#' @param TwinRate The proportion of the child population who are twins.
#' @param Parents A data frame containing observations limited to parents. An age column is required. This can contain the entire set of people who can be parents, as the assignment is made on age at becoming a parent, not current age. This file can contain the people who can be guardians, as well as parents. This data frame should contain more observations than the Children data frame. The relative sizes of the data frames are compared. If the Parens data frame is not sufficiently larger than the Children data frame, the latter is randomly sampled to construct a smaller data frame.
#' @param ParentIDVariable The column number for the ID variable in the Parent data frame.
#' @param ParentAgeVariable The column number for the Age variable in the Parent data frame.
#' @param MinParentAge The youngest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param MaxParentAge The oldest age at which a person becomes a parent. The default value is NULL, which will cause the function to stop.
#' @param MinPropRemain The minimum proportion of people, at each age, who are not parents. The default is zero, which may result in all people at a specific age being allocated as parents. This will leave age gaps for any future work, and may not be desirable. If nrow(Children) == nrow(Parents), assigning any value other than 0 will result in an error.
#' @param HouseholdIDVariable The column number for the household variable in the Parents data frame. This must be provided.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

AddChildren <- function(Children, ChildIDVariable, ChildAgeVariable, NumChildren = 2, TwinRate = 0, Parents, ParentIDVariable, ParentAgeVariable,
                        MinParentAge = NULL, MaxParentAge = NULL, HouseholdIDVariable= NULL, UserSeed=NULL)

{

  options(dplyr.summarise.inform=F)

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

  ParentsRenamed <- Parents %>%
    rename(ParentID = !! ParentIDVariable, ParentAge = !! ParentAgeVariable,
           HouseholdID = !! HouseholdIDVariable)

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

    # add parent

    for (c in 1:nrow(TwinsMatched)) {

      CurrentAge <- sample(minIndexAge:maxIndexAge, 1, replace = FALSE, prob = c(ParentAgeCountVector))
      TwinsMatched$ParentAge[c] <- CurrentAge
      TwinsMatched$AgeDifference[c] <- CurrentAge - TwinsMatched$ChildAge[c]
      age_index <- TwinsMatched$ParentAge[c]-(minIndexAge -1)
      TwinsMatched$age_index[c] <- age_index

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

    # cat("minChildIndexAge = ", minChildIndexAge, "maxChildIndexAge = ", maxChildIndexAge)

    ChildrenAgeCountVector <- ChildrenCounts$AgeCount

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
          age_index <- NewChildAge + 1

          # cat("age_index = ", age_index, "length of ChildrenAgeCountVector[age_index] = ", length(ChildrenAgeCountVector[age_index]), "\n")

          while (TwinsMatched[x,y] %in% (AgesUsed) || AgeDifference < MinParentAge || AgeDifference > MaxParentAge || ChildrenAgeCountVector[age_index] == 0) {

            # cat("Entered loop", "Current age is ",TwinsMatched[x,y], "Ages used are ", AgesUsed, "Parent age at childbirth is", AgeDifference, "\n")

            # cat("ChildrenAgeCountVector = ", ChildrenAgeCountVector, "Entered while loop", "age_index = ", age_index, "\n")
            # AgeDifference %in% UsedAgesVector[x] &&

            NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
            TwinsMatched[x,y] <- NewChildAge
            AgeDifference <- TwinsMatched$ParentAge[x]- TwinsMatched[x,y]
            age_index <- NewChildAge + 1

            # cat("Child Age is ", TwinsMatched[x,y], "and Index is ", age_index, "\n")

            # close while test
          }

          # cat("Current age is ",TwinsMatched[x,y], "Ages used are ", AgesUsed, "Parent age at childbirth is", AgeDifference, "\n")

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

    for (z in 3:NumChildren) {

      OtherKids <- TwinsMatched %>%
        ungroup() %>%
        select(all_of((NumberColsChildren*2)+z-1), ncol(.)) %>%
        rename(ChildAge = paste0("ChildAge", z))

      OtherKids <- left_join(OtherKids %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                             NoTwinsDataFrame %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                             by = c("ChildAge", "Counter")) %>%
        select(-Counter)

      TwinsFinal <- bind_rows(TwinsFinal, OtherKids)

      NoTwinsDataFrame <- NoTwinsDataFrame %>%
        filter(!(ChildID %in%  OtherKids$ChildID))

      #closes extra child addition loop
    }

    # no twins data frame is updated with no allocated children
    # continues to be the same name

    ChildrenRenamed <- NoTwinsDataFrame

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
    filter(!(ChildID %in%  BaseDataFrame$ChildID))

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

    # for (x in 1:152) {

    AgesUsed <- as.numeric(BaseDataFrame$ChildAge[x])

    for (y in (NumberColsChildren + 4):ncol(BaseDataFrame)) {

      Counter <- 0

      NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
      BaseDataFrame[x,y] <- NewChildAge
      AgeDifference <- BaseDataFrame$ParentAge[x]- BaseDataFrame[x,y]
      age_index <- NewChildAge + 1

      # cat("age_index = ", age_index, "length of ChildrenAgeCountVector[age_index] = ", length(ChildrenAgeCountVector[age_index]), "\n")

      while (BaseDataFrame[x,y] %in% (AgesUsed) || AgeDifference < MinParentAge || AgeDifference > MaxParentAge || ChildrenAgeCountVector[age_index] == 0) {

        # cat("Entered loop", "age_index = ", age_index, "length of ChildrenAgeCountVector[age_index] = ", length(ChildrenAgeCountVector[age_index]), "\n")

        NewChildAge <- sample(minChildAge:maxChildAge, 1, replace = FALSE, prob = c(ChildrenAgeCountVector))
        BaseDataFrame[x,y] <- NewChildAge
        AgeDifference <- BaseDataFrame$ParentAge[x]- BaseDataFrame[x,y]
        age_index <- NewChildAge + 1

        Counter <- Counter + 1

        if (Counter == 1000 ) {

          if (ChildrenAgeCountVector[age_index] == 0) {
            cat("No children were available at the ages tested", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")
          }

          if (AgeDifference < MinParentAge || AgeDifference > MaxParentAge) {

            # cat("No credible available parent ages were located", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")

            if (exists("ParentTooYoung")) {
              ParentTooYoung <- c(ParentTooYoung, BaseDataFrame$HouseholdID[x])

            } else {
              ParentTooYoung <- as.vector(BaseDataFrame$HouseholdID[x])
            }
          }

          if (BaseDataFrame[x,y] %in% (AgesUsed)) {
            cat("Twins were constructed even though the twin families were previously allocated", "The problem household is", BaseDataFrame$HouseholdID[x],"\n")


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

      # closes for column loop
    }

    # closes for numchildren loop
  }


  NotTwins <- BaseDataFrame %>%
    ungroup() %>%
    select(all_of(1:NumberColsChildren), NumberColsChildren+3)

  NoTwinsDataFrame <- NoTwinsDataFrame %>%
    filter(!(ChildID %in%  NotTwins$ChildID))

  ParentOfNotTwins <- BaseDataFrame %>%
    ungroup() %>%
    select(all_of((NumberColsChildren+2) : (NumberColsChildren+3)))

  ParentOfNotTwins <- left_join(ParentOfNotTwins, ParentsRenamed, by = c("ParentID", "HouseholdID"))

  # extract remaining children and rbind these to each other
  # will eventually be rbind'ed to the twins and parent data

  for (z in 2:NumChildren) {


    #   cat("z is ", z, " and child age column is", BaseDataFrame[(NumberColsChildren + z + 1),])

    OtherNotTwins <- BaseDataFrame %>%
      ungroup() %>%
      select(all_of(c((NumberColsChildren+3), (NumberColsChildren + z + 2)))) %>%
      rename(ChildAge = paste0("ChildAge", z))

    OtherNotTwins <- left_join(OtherNotTwins %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                               NoTwinsDataFrame %>% group_by(ChildAge) %>% mutate(Counter = row_number()),
                               by = c("ChildAge", "Counter")) %>%
      select(-Counter)

    NotTwins <- bind_rows(NotTwins, OtherNotTwins)

    NoTwinsDataFrame <- NoTwinsDataFrame %>%
      filter(!(ChildID %in%  OtherNotTwins$ChildID))

    #closes extra child addition loop
  }

  #####################################
  #####################################
  # join all the data frames together
  #####################################
  #####################################


  if (exists("TwinsFinal")) {
    ChildrenFinal <- rbind(TwinsFinal, NotTwins)

  } else {
    ChildrenFinal <- NotTwins
  }

  if (exists("ParentOfTwins")) {
    ParentsFinal <- rbind(ParentOfTwins, ParentOfNotTwins)

  } else {
    ParentsFinal <- ParentOfNotTwins
  }

  ParentTooYoung <- unique(ParentTooYoung)

  ChildrenFinal <- ChildrenFinal %>%
    rename(PersonID = ChildID, Age = ChildAge)

  ParentsFinal <- ParentsFinal %>%
    rename(PersonID = ParentID, Age = ParentAge)

  InterimDataframe <- rbind(ParentsFinal, ChildrenFinal)

  # # test that the column number is still available to the function
  # cat("Household column number is", HouseholdIDVariable, "\n")

  # fix the out-of-bounds ages

  for (a in 1:length(ParentTooYoung)) {

 #   print(ParentTooYoung[a])

    # extract problem household
    ProblemHousehold <- InterimDataframe %>%
      filter(HouseholdID == ParentTooYoung[a])

    # identify the ages out of range
    # extract parent
    ProblemHouseholdParent <- ProblemHousehold %>%
      slice_head(1)

    PermittedChildAgeMin <- max(minChildAge, (ProblemHouseholdParent$Age - MaxParentAge))
    PermittedChildAgeMax <- ProblemHouseholdParent$Age - MinParentAge

    # test parent permitted age conditions
    # cat("Minimum child age is", PermittedChildAgeMin, "maximum child age is", PermittedChildAgeMax, "for parent age of", ProblemHouseholdParent$Age, "\n")

    ProblemHouseholdChildren <- ProblemHousehold %>%
      slice(- 1) %>%
      filter(!(between(Age, PermittedChildAgeMin, PermittedChildAgeMax)))

    OkayAges <- ProblemHousehold %>%
      filter(!(PersonID %in% ProblemHouseholdParent$PersonID) &
               !(PersonID %in% ProblemHouseholdChildren$PersonID)) %>%
      select(Age)

    for (b in 1:nrow(ProblemHouseholdChildren)) {

      # print(ProblemHouseholdChildren$PersonID[b])

      PossibleSwapHouseholds <- ParentsFinal %>%
        filter(!(HouseholdID %in% ProblemHousehold$HouseholdID),
               !(PersonID %in% ParentOfTwins$ParentID),
               between(Age, ProblemHouseholdChildren$Age[b] + 18, ProblemHouseholdChildren$Age[b] + MaxParentAge))

      # test that the swap is correct, i.e. child ages are within bounds for both the children in the swap
      # cat("Minimum parent age is", ProblemHouseholdChildren$Age[b] + 18, "and maximum parent age is", ProblemHouseholdChildren$Age[b] + MaxParentAge,
      # "for household", ProblemHousehold$HouseholdID[b], "\n")

      PossibleSwapChildren <- ChildrenFinal %>%
        filter(HouseholdID %in% PossibleSwapHouseholds$HouseholdID)

      WouldOtherwiseHaveTwins <- PossibleSwapChildren %>%
        filter(Age %in% OkayAges)

      # delete these households from the possible children list
      PossibleSwapChildren <- PossibleSwapChildren %>%
        filter(!(HouseholdID %in% WouldOtherwiseHaveTwins$HouseholdID),
               between(Age, max(ProblemHouseholdParent$Age - 54, 0), ProblemHouseholdParent$Age - 18))

      # randomly select a child to swap, what will actually swap is the household ID
      ChildToSwap <- PossibleSwapChildren %>%
        slice_sample(n = 1)

      SwapChildHouseholdID <- ChildToSwap$HouseholdID
      ProblemChildHouseholdID <- ProblemHouseholdChildren$HouseholdID[b]

      # cat("Child", ChildToSwap$PersonID, "in household ID", ChildToSwap$HouseholdID, "will donate household ID to", ProblemHouseholdChildren$PersonID[b],
      #     "in", ProblemHouseholdChildren$HouseholdID[b], "\n")

      # perform the swapping, only household ID to be swapped

      SwapChildRowIndex <- as.numeric(which(ChildrenFinal$PersonID==ChildToSwap$PersonID))
      ProblemChildRowIndex <- as.numeric(which(ChildrenFinal$PersonID==ProblemHouseholdChildren$PersonID[b]))

      # cat("The donor row index is", SwapChildRowIndex, "and the problem child row index is", ProblemChildRowIndex, "\n")

      # do the swapping
      ChildrenFinal[SwapChildRowIndex, HouseholdIDVariable] <- ProblemChildHouseholdID
      ChildrenFinal[ProblemChildRowIndex, HouseholdIDVariable] <- SwapChildHouseholdID


    }

    # close fix for the households with children who are too old
  }




 # return(OutputDataframe)
  return(ShouldNotBeTwins)


  # closes function
}

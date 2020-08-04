#' Create a subset of observations containing only children matched to parents/guardians
#' This function creates a data frame of child-parent/guardian pairs, based on a population distribution of age differences. The distribution used in this function is the log normal. However, the matching is affected by the age structure of the children and parent data frames. The distribution provides a framework upon which to base the matching. The final distribution of age differences, however, may not follow a lognormal distribution.
#' Two data frames are required. The Children data frame contains the age data, to which the Parent (Guardian) data will be applied.
#' The minimum and maximum ages of parents must be specified. This ensures that there are no parents who were too young (e.g. 11 years) or too old (e.g. 70 years) at the time the child was born. The presence of too young and too old parents is tested throughout this function. Thus, pre-cleaning the Parent data frame is not required.
#' If the parent data frame is not proportionately larger than the smaller data frame, some children may not be matched. There will be fewer matches available at each parent age, potentially leading to the situation when all suitable parent age matches have missing available parents. As well, if the age structure of the children has a poor alignment with the age structure of the parents, given the lognormal distribution used, some parent ages will be upsampled relative to their frequency. Again, this can cause gaps in the range of suitable parent ages given the age of the child.
#' The function only outputs the children and parents that have been matched. As the output combines the children and parents into one data frame. The number of columns in the parent data frame must be one larger than the number of columns in the children data frame, as the parents data frame is the only one that contains the household ID variable.
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
#' @param HouseholdIDVariable The column name for the household variable in the Parents data frame. This must be supplied, and in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.




AddChildrenLnLoop <- function(Children, ChildIDVariable, ChildAgeVariable, NumChildren = 2, TwinRate = 0, Parents, ParentIDVariable, ParentAgeVariable,
                           meanlogUsed, sdlogUsed,MinParentAge = NULL, MaxParentAge = NULL, HouseholdIDVariable= NULL, UserSeed=NULL)

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

  # create cut-down version (columns) for parent matching
  # the parent data can be linked to this later

  ParentsSubset <- ParentsRenamed %>%
    select(ParentAge, ParentID, HouseholdID)


  # seed must come before first sample is cut
  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  if (nrow(ParentsRenamed)*2.05 < nrow(Children)) {

    ChildrenRenamed <- ChildrenRenamed %>%
      slice_sample(n = round(nrow(ParentsRenamed) * 1.9), 0)
  }

  # TODO MAKE SURE THAT NROW(CHILDREN) %MODULO% NUMCHILDREN == O, OTHERWISE SAMPLE SO THIS IS THE STATE
  # NOT SURE ABOUT THIS TEST DUE TO THE WAY THE CHILDREN/PARENT ARE BEING CONNECTED, COME BACK TO THIS

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


          # closes while loop
          }

      TwinsMatched$AgeDifference[c] <- AgeDifference
      ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

      # closes parent match loop
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

     AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
     TwinsMatched[x,y] <- TwinsMatched$ParentAge[x] - AgeDifference

     age_index <- TwinsMatched[x,y] + 1

          # cat("TwinsMatched$ParentAge[x] = ", TwinsMatched$ParentAge[x], "TwinsMatched[x,y] = ", TwinsMatched[x,y], "age_index  =",
          #     age_index, "\n")

          # cat("age_index = ", age_index, "length of ChildrenAgeCountVector[age_index] = ", length(ChildrenAgeCountVector[age_index]), "\n")

          while (age_index < 1 || age_index > length(ChildrenAgeCountVector) || (ChildrenAgeCountVector[age_index]) < 1 ||
                 length(ChildrenAgeCountVector[age_index] == 0)==0 ||  TwinsMatched[x,y] %in% (AgesUsed)) {

            # cat("Entered loop", "\n")

            # cat("ChildrenAgeCountVector = ", ChildrenAgeCountVector, "Entered while loop", "age_index = ", age_index, "\n")
               # AgeDifference %in% UsedAgesVector[x] &&

 #                age_index < 1 && age_index > length(ChildrenAgeCountVector))) {

            AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
            TwinsMatched[x,y] <- TwinsMatched$ParentAge[x] - AgeDifference
            age_index <- TwinsMatched[x,y] + 1

            # cat("Child Age is ", TwinsMatched[x,y], "and Index is ", age_index, "\n")

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

    # join in a parent
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
    # for (z in 3:3) {

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

    ParentCounts <- ParentsRenamed %>%
      group_by(ParentAge) %>%
      summarise(AgeCount=n()) %>%
      tidyr::complete(ParentAge = seq(min(ParentAge), max(ParentAge)),
                      fill = list(AgeCount = 0))


    ParentAgeCountVector <- ParentCounts$AgeCount

    # create cut-down version (columns) for parent matching
    # the parent data can be linked to this later

    ParentsSubset <- ParentsRenamed %>%
      select(ParentAge, ParentID, HouseholdID)

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

    AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
    BaseDataFrame$AgeDifference[c] <- AgeDifference
    BaseDataFrame$ParentAge[c] <- BaseDataFrame$ChildAge[c] + AgeDifference
    age_index <- BaseDataFrame$ParentAge[c]-(minIndexAge -1)
    BaseDataFrame$age_index[c] <- age_index



    while (!(BaseDataFrame$AgeDifference[c] >= MinParentAge && BaseDataFrame$AgeDifference[c] <= MaxParentAge &&
             ParentAgeCountVector[age_index] > 0 && BaseDataFrame$ParentAge[c] >= minIndexAge &&
             BaseDataFrame$ParentAge[c] <= maxIndexAge)) {

      # print(c)

      AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
      BaseDataFrame$AgeDifference[c] <- AgeDifference
      BaseDataFrame$ParentAge[c] <- BaseDataFrame$ChildAge[c] + AgeDifference
      age_index <- BaseDataFrame$ParentAge[c]-(minIndexAge -1)


      # closes while loop
    }

    BaseDataFrame$AgeDifference[c] <- AgeDifference
    ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1

    # closes parent match loop
  }

  BaseDataFrame <- left_join(BaseDataFrame %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                            ParentsSubset %>% group_by(ParentAge) %>% mutate(Counter = row_number()),
                            by = c("ParentAge", "Counter"))

  # no longer need parents as they are all matched at this point
  # don't use the distribution any more as will create problems for later matching
  # just do random draws of ages between the minimum and maximum child age
  # and stay within permitted parent age boundaries

  # construct the child ages remaining into a vector

  BaseDataFrame <- BaseDataFrame %>%
    select(-c(AgeDifference, age_index, Counter))

  # remove the matched children ids from the avaiable children in ChildrenRenamed

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

  cat("minChildIndexAge = ", minChildIndexAge, "maxChildIndexAge = ", maxChildIndexAge)

  ChildrenAgeCountVector <- ChildrenCounts$AgeCount


  # match the remaining children
  # not using the distribution otherwise may cause problems for the last rows in the BaseDataFrame

  #create the column names
  for (x in 2:NumChildren) {

    BaseDataFrame <- BaseDataFrame %>%
      tibble::add_column(!! paste0("ChildAge", x) := 1000)

    # closes column name loop
  }

  # it being a tibble seemed to be the problem for the looping below.

  BaseDataFrame <- as.data.frame(BaseDataFrame)

  # now iterate through the other children
  # nested loop must be columns within rows

  for (x in 1:nrow(BaseDataFrame)) {

    AgesUsed <- as.numeric(BaseDataFrame$ChildAge[x])

    for (y in (NumberColsChildren + 4):ncol(BaseDataFrame)) {

      # NOT WEIGHTED otherwise it would select the same values for the same parent, as it is row by column
      # and we don't want twins

      BaseDataFrame[x,y] <- sample(minChildIndexAge:maxChildIndexAge, 1)
      age_index <- BaseDataFrame[x,y] + 1

      # cat("TwinsMatched$ParentAge[x] = ", TwinsMatched$ParentAge[x], "TwinsMatched[x,y] = ", TwinsMatched[x,y], "age_index  =",
      #     age_index, "\n")

      # cat("age_index = ", age_index, "length of ChildrenAgeCountVector[age_index] = ", length(ChildrenAgeCountVector[age_index]), "\n")

      while (BaseDataFrame[x,y] %in% c(AgesUsed) || BaseDataFrame$ParentAge[x] - BaseDataFrame[x,y] < minIndexAge ||
             BaseDataFrame$ParentAge[x] - BaseDataFrame[x,y] > maxIndexAge || ChildrenAgeCountVector[age_index] ==0) {

        cat("Entered loop", "\n", "ParentAge - ChildAge is ", BaseDataFrame$ParentAge[x] - BaseDataFrame[x,y], "\n")

        BaseDataFrame[x,y] <- sample(minChildIndexAge:maxChildIndexAge, 1)
        age_index <- BaseDataFrame[x,y] + 1

        cat("Child Age is ", BaseDataFrame[x,y], "and Index is ", age_index, "\n")

        # close while test
      }
  #
  #
  #     ChildrenAgeCountVector[age_index] = ChildrenAgeCountVector[age_index] - 1
      AgesUsed <- cbind(AgesUsed, BaseDataFrame[x,y])
  #     # print(AgesUsed)

           # closes for column loop
      }

         # closes for numchildren loop
    }
  #
  # # # # force last lot of children to be matched on the basis of first parent age after minimum
  # # # # need to work from minimum child age
  # # # # find first current parent age that is still available
  # # # # will stuff up distribution entered, but if the function has hit this point, the distribution did not fit
  # # #
  # # # for (j in 1:nrow(Children)) {
  # # #
  # # #   # ensure initial age selection is within min and max parent ages
  # # #
  # # #   AgeDifference <- round(runif(1, MinParentAge, MaxParentAge))
  # # #   Children$ParentAge[j] <- Children[[ChildAgeVariable]][j] + AgeDifference
  # # #   age_index <- Children$ParentAge[j]-(minIndexAge -1)
  # # #
  # # #   if (ParentAgeCountVector[age_index] > 0)  {
  # # #
  # # #     ParentAgeCountVector[age_index] <- ParentAgeCountVector[age_index] - 1
  # # #     Children$AgeDifference[j] <- AgeDifference
  # # #
  # # #   } else {
  # # #
  # # #     Children$AgeDifference[j] <- NA
  # # #     Children$ParentAge[j] <- NA
  # # #
  # # #     age_index <- which.max(ParentAgeCountVector)
  # # #     Children$ParentAge[j] <- age_index + (minIndexAge -1)
  # # #     Children$AgeDifference[j] <- Children$ParentAge[j] - Children[[ChildAgeVariable]][j]
  # # #
  # # #     while (!(ParentAgeCountVector[age_index] > 0 && Children$AgeDifference[j] >= MinParentAge && Children$AgeDifference[j] <= MaxParentAge)) {
  # # #
  # # #       age_index <- age_index + round(runif(1,-2,2),0)
  # # #
  # # #       if(age_index < 1) {
  # # #         age_index <- round(length(ParentAgeCountVector)*.2, 0)
  # # # # reduce pool of potentially partnered donors to only those matched to recipients
  #
  #
  # # # OutputDataframe <- rbind(FirstDataframeSplit, SecondDataframeSplit)
  # # #
  # # # #####################################
  # # # #####################################
  # # # # data frame merging ends here
  # # # #####################################
  # # # #####################################
  # # #
  # # #
  # # # return(OutputDataframe)

  return(BaseDataFrame)

#closes function
}

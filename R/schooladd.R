#' Create a data frame of children matched to schools
#' This function creates a data frame of children, with a school allocated to them, based on the characteristics of the child ages in a household (e.g. the presence of twins) and the proportions of children of each age within each school.
#' Two data frames are required. The Children data frame contains the children data, to which the Schools data will be applied.
#' No minimum or maximum child ages are required, as the function limits the ages within the age range across the schools Thus, pre-cleaning the Children data frame is not required.
#' The Schools data frame must be a summary in the form of counts by age within school. Each row is one age only. For example, if a school has children aged 5 to 9 years, there should be 5 rows. #' Co-educational, single-sex, or both types of schools can be included in the Schools data frame. If at least one same-sex school is included, similarly aged children of the opposite sex will be assigned to the equivalent school for the opposite sex, if present, otherwise they will be assigned to a co-educational school. Because of the semi-summary nature of the Schools data frame, this information must be repeated for each row.
#' Children within a family will be assigned to the same school, where the age and/or sex structure requires this, using the probability supplied by the user. Twins are the exception to this rule. Twins and other multiples will be allocated to the same school with probability 1, except when the structure of single-sex schools prevents this. If there are single-sex schools for each twin/multiple, and one child is randomly allocated to a single-sex school, then all those children will be assigned to a single-sex school assuming that is available.The allocation is robust for the number of children in a household who are the same age.
#' The function performs a reasonableness check for child ID, child age, and counts for the number of children of each age versus the number of available age slots across all schools.
#'
#' @export
#' @param Children A data frame containing observations limited to the children to be matched An age column is required. All children in this data frame will be matched to a parent/guardian.
#' @param ChildIDCol The column number for the ID variable in the Children data frame.
#' @param ChildAgeCol The column number for the Age variable in the Children data frame.
#' @param ChildSxCol The column number for the sex indicator for children. This column is used to assign children to the appropriate school type (co-educational or single-sex). The expected values are "F" (female) or "M" (male).
#' @param HouseholdIDCol The column number for the household variable in the Children data frame. This must be provided.
#' @param Schools A data frame containing the school observations.
#' @param SchoolIDCol The column number for the variable in the Schools data frame that contains the name of each school. While these IDs can be numeric, the data must be either character (preferred) or factor (will be converted to character).
#' @param SchoolAgeCol The column number for the Age variable in the Schools data frame. Each student age within the school must be a separate row.
#' @param SchoolRollCol The number of places available for children at that school age, within the school.
#' @param SchoolTypeCol An indicator variable used to determine whether the school is co-educational or single-sex. The expected values are "C" (co-educational), "F" (female only), and "M" (male-only).
#' @param ChildProb If one child is assigned to a same-sex school, the probability that another child in the household is also assigned to a same-sex school. If an eqivalent same-sex school is not available, the other child will be assigned to a co-ed school. The default value is 1, so that all children in the same household will be assigned to same-sex schools, or to co-educational schools. A probability of 0 means that, if one child is assigned to a same-sex school, all other children will be assigned to co-educational schools. The assignment is affected by the number of boy-only and girl-only schools, and the age distribution covered by these schools..
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

schooladd <- function(Children, ChildIDCol, ChildAgeCol, ChildSxCol, HouseholdIDCol = NULL,
                        Schools, SchoolIDCol, SchoolAgeCol, SchoolRollCol, SchoolTypeCol, ChildProb = 1, UserSeed=NULL)
{

  options(dplyr.summarise.inform=F)


  #####################################################################
  #####################################################################
  # Test for any problem ages, stop function if this situation exists
  #####################################################################
  #####################################################################

  ChildrenRenamed <- Children %>%
    rename(ChildID = !! ChildIDCol, ChildAge = !! ChildAgeCol, ChildType = !! ChildSxCol,
           HouseholdID = !! HouseholdIDCol)

  SchoolsRenamed <- Schools %>%
    rename(SchoolID = !! SchoolIDCol, SchoolAge = !! SchoolAgeCol,
           ChildCounts = !! SchoolRollCol, SchoolType = !! SchoolTypeCol) %>%
    mutate(SchoolID = as.character(SchoolID),
           SchoolType = as.character(SchoolType)) %>%
    #mutate(across(where(is.factor), as.character)) %>%
    select(SchoolID, SchoolAge, ChildCounts, SchoolType)

  OriginalSchoolsCounts <- SchoolsRenamed

  ###############################################
  ###############################################
   # get the col names
  ###############################################
  ###############################################

  ChildIDColName <- sym(names(Children[ChildIDCol]))
  ChildAgeColName <- sym(names(Children[ChildAgeCol]))
  HouseholdIDColName <- sym(names(Children[HouseholdIDCol]))
  ChildSxColName <- sym(names(Children[ChildSxCol]))
  SchoolIDColName <- sym(names(Schools[SchoolIDCol]))

  ###############################################
  ###############################################
  # quick test of compatibility of counts
  ###############################################
  ###############################################
  ChildrenCountTest <- ChildrenRenamed %>%
    group_by(ChildAge) %>%
    summarise(AgeCount = n())

  SchoolsCountTest <- SchoolsRenamed %>%
    group_by(SchoolAge) %>%
    summarise(SchoolAgeCount = sum(ChildCounts))

  CountComparison <- full_join(ChildrenCountTest, SchoolsCountTest, by = c("ChildAge" = "SchoolAge")) %>%
    mutate(AgeCount = replace(AgeCount, is.na(AgeCount), 0),
           SchoolAgeCount = replace(SchoolAgeCount, is.na(SchoolAgeCount), 0),
           CountDiff = SchoolAgeCount - AgeCount) %>%
    filter(SchoolAgeCount != 0, AgeCount != 0)

  TooManyKids <- CountComparison %>%
    filter(CountDiff < 0) %>%
    select(ChildAge)

  # TooManyKids <- as_tibble(CountComparison$ChildAge) # testing if loop


  if (!(nrow(TooManyKids)==0)) {

    TooManyKids <- as.vector(TooManyKids)

    stop(paste("The number of children at these ages exceeds the available school roll places: ", shQuote(TooManyKids), collapse=", "))

  }

  MaxSchoolAge <- as.numeric(CountComparison[nrow(CountComparison), 1])

  cat("The minimum school age is", as.numeric(CountComparison[1,1]), "and the maximum school age is ", as.numeric(CountComparison[nrow(CountComparison), 1]), "\n")


  # restrict child and school data frames to these minimum and maximum ages
  # get age range
  AgeRestriction <- CountComparison %>%
    filter(CountDiff >= 0) %>%
    select(ChildAge)

  # apply to children

  ChildrenRenamed <- left_join(AgeRestriction, ChildrenRenamed, by = "ChildAge")


  # NOTE: this removes any school classrooms where NO children of that age exist in the data
  SchoolsRenamed <- left_join(AgeRestriction, SchoolsRenamed, by = c("ChildAge" = "SchoolAge"))

  SchoolsCountColIndex <- as.numeric(which(colnames(SchoolsRenamed) == "ChildCounts"))

  # get rid of the tibbles
  ChildrenRenamed <- as.data.frame(ChildrenRenamed)
  SchoolsRenamed <- as.data.frame(SchoolsRenamed)

  #####################################################################
  # Create household meta data data frame
  #####################################################################
  # get the number of households
  NumberHouseholds <- as.numeric(ChildrenRenamed %>%
                                   dplyr::summarise(Count = n_distinct(HouseholdID)) %>%
                                   pull(Count))

  # get list of household IDs
  HouseholdIDList <- as.data.frame(ChildrenRenamed %>%
                                     distinct(HouseholdID))

#####################################################################
# create counts by sex
# will be used to ensure that selected schools do not decrease
# the availability of
#####################################################################

  ChildrenSexAge <- ChildrenRenamed %>%
    group_by(ChildType, ChildAge) %>%
    summarise(AgeCount = n())

  SchoolSexAge <- SchoolsRenamed %>%
    group_by(SchoolType, ChildAge) %>%
    summarise(SchoolAgeCount = sum(ChildCounts))

  # TODO remove this split, if no merge == shift between fix and not fix
  # merge may fail on first attempt in household

  # this is the first part of the code that requires randomness
  # so seed is applied here

  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  ListofHouseholds <- ChildrenRenamed %>%
    group_by(HouseholdID) %>%
    summarise(NumberKids = n())

  ###############################################
  ###############################################
  ###############################################
  # assignment of schools
  ###############################################
  ###############################################
  ###############################################

   for(i in 1:nrow(ListofHouseholds)) {


    if(exists("UnmatchedSingleSexToMerge")) {
      rm(UnmatchedSingleSexToMerge)
    }

    if(exists("MergedSingleSexToAdd")) {
      rm(MergedSingleSexToAdd)
    }

    if(exists("SingleSexMatchedSchools")) {
      rm(SingleSexMatchedSchools)
    }


    CurrentHousehold <- ListofHouseholds$HouseholdID[i]

    # cat("The current household is", CurrentHousehold, "\n")


      # get the children in the household

      ChildrenInHousehold <- ChildrenRenamed %>%
        filter(HouseholdID == CurrentHousehold)

      NumKidsRemaining <- nrow(ChildrenInHousehold)

      # only need to do this for households with more than one child
      if(NumKidsRemaining >1) {


      # random roll to see if any children in same school, will prioritise the twins
      RandomRollVector <- runif(nrow(ChildrenInHousehold)-1)

      # cat(RandomRollVector, "\n")

      # test number of children who should go to the same school
      NumberSameSchool <- data.frame(RandomRollVector) %>%
        filter(RandomRollVector > (1-ChildProb)) %>%
        summarise(SameSchool = n()) %>%
        pull(SameSchool)


      # fix the number of children that can go to the same school
      if(NumberSameSchool == 1 | NumberSameSchool == 0) {

        NumberSameSchool <- 2

      # } else if (NumberSameSchool == 0) {
      #
      #   NumberSameSchool <- 2

      } else {

        FinalRandomRoll <- runif(1)

        if(FinalRandomRoll > (1-ChildProb)) {

          NumberSameSchool <- NumberSameSchool +1

          # closes if(FinalRandomRoll > (1-ChildProb))
        }


        # closes  if(NumberSameSchool == 1 | NumberSameSchool == 0)
      }

      # closes if(NumKidsRemaining >1)
      } else {

        NumberSameSchool <- 1
      }


      # print(NumberSameSchool)

      # add children to same school
      # need to identify the number of children that can go to the same school

        # get child ages vector
        ChildAges <- as.data.frame(ChildrenInHousehold %>%
          group_by(ChildType, ChildAge) %>%
          summarise(CountsByAge = n()))


        # locate schools that can take the maximum number of children from NumberSameSchool down

         SchoolSubset <- left_join(ChildAges, SchoolsRenamed, by = "ChildAge") %>%
           mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType, "Y", "N")) %>%
           filter(ChildCounts > 0,
                  IsMatch == "Y") %>%
           group_by(SchoolID, ChildAge, SchoolType, ChildCounts) %>%
           summarise(NumberKids = sum(CountsByAge)) %>%
           ungroup() %>%
           mutate(RemainingChildren = ChildCounts - NumberKids) %>%
           filter(RemainingChildren >= 0)

         NumberKidsPerSchool <- SchoolSubset %>%
           group_by(SchoolID) %>%
           summarise(across(c(ChildCounts, NumberKids), sum))

        # cat("PossibleSchools IsMatch is below", "\n")

        # get the co-ed schools

         CoedSchoolsSelected <- SchoolSubset %>%
           filter(SchoolType == "C")


        # work with the single-sex schools

         SingleSexSchoolsSelected <- SchoolSubset %>%
           filter(!(SchoolType =="C"))


          # determine if there are two single sex schools that have the equivalent age
         # in the SchoolsSubset
         FemaleAgesToMatch <- SchoolSubset %>%
           filter(SchoolType == "F")

         # this will only return a non-empty data frame if there is a male school
         # that exists in the SchoolsSubset, ignoring age
         MaleSchoolsToMatch <- SchoolSubset %>%
           filter(SchoolType == "M")


         # will only return a data frame is there is a male school included in the SchoolSubset
         # that covers the age range of any single-sex schools selected
         # may have no matched males - because either
         # 1. the single-sex schools age ranges do not align, or
         # 2. there is only one single-sex school available

        MatchedMales <- left_join(FemaleAgesToMatch, MaleSchoolsToMatch, by = "ChildAge")

        if(!(is.na(MatchedMales$SchoolID.y[1]))) {

          # cat("There is an overlap in the single-sex school ages", "\n")

          SingleSexMatchedSchools <- MatchedMales %>%
            select(SchoolID.x, SchoolID.y) %>%
            distinct() %>%
            tidyr::drop_na() %>%
            left_join(NumberKidsPerSchool, by = c("SchoolID.x" = "SchoolID")) %>%
            rename(ChildCounts.x = ChildCounts, NumberKids.x = NumberKids) %>%
            left_join(NumberKidsPerSchool, by = c("SchoolID.y" = "SchoolID"))  %>%
            rename(ChildCounts.y = ChildCounts, NumberKids.y = NumberKids) %>%
            mutate(ChildCounts = ChildCounts.x + ChildCounts.y,
                   NumberKids = NumberKids.x + NumberKids.y,
                   SchoolID = paste0("CombinedSchool", 1:nrow(.)))


          # cat("SingleSexMatchedSchools okay for household", CurrentHousehold, "\n")

          MergedSingleSexToAdd <- SingleSexMatchedSchools %>%
            select(SchoolID, ChildCounts, NumberKids) %>%
            mutate(SchoolType = "S")

          # cat("MergedSingleSexToAdd okay for household", CurrentHousehold, "\n")

          # note the NA value for the children remaining as this information is absent from the merger
          # of the single-sex schools
        AllSchoolsFromWhichToChoose <- bind_rows(CoedSchoolsSelected, MergedSingleSexToAdd)

        # need to also amend NumberKidsPerSchool as that is the sampling data frame
        # and has not been amended from the code above
        # have to remove the matched single sex schools and add the replacement combo school in
        # remove the school type from the MergedSingleSex
        ComboToAdd <- MergedSingleSexToAdd %>%
          select(-SchoolType)

        NumberKidsPerSchool <- NumberKidsPerSchool %>%
          filter(!(SchoolID %in% c(MatchedMales$SchoolID.x)),
                 !(SchoolID %in% c(MatchedMales$SchoolID.y))) %>%
          bind_rows(ComboToAdd)

        # cat("AllSchoolsFromWhichToChoose okay for household", CurrentHousehold, "\n")


        UnmatchedSingleSexToMerge <- SingleSexSchoolsSelected %>%
          filter(!(SchoolID %in% c(SingleSexMatchedSchools$SchoolID.x)),
                 !(SchoolID %in% c(SingleSexMatchedSchools$SchoolID.y)))

        if(!(is.na(UnmatchedSingleSexToMerge$NumberKids[1]))) {

          # cat("UnmatchedSingleSexToMerge loop entered for for household", CurrentHousehold, "\n")

          # print(str(PossibleSchools))
          # cat("The rows in UnmatchedSingleSexToMerge are", nrow(UnmatchedSingleSexToMerge), "\n")
          # print(str(UnmatchedSingleSexToMerge))
          # cat("The key test value is", UnmatchedSingleSexToMerge$NumberTimes[1], "\n")

          PossibleSchoolsIndicators <- SchoolSubset %>%
            select(SchoolID, SchoolType) %>%
            filter(SchoolID %in% c(UnmatchedSingleSexToMerge$SchoolID))

          AbsentSingleSex <- SchoolSubset %>%
            filter(SchoolID %in% c(UnmatchedSingleSexToMerge$SchoolID)) %>%
            group_by(SchoolID) %>%
            summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
            rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes) %>%
            left_join(PossibleSchoolsIndicators, by = "SchoolID")


        AllSchoolsFromWhichToChoose <- bind_rows(AllSchoolsFromWhichToChoose, AbsentSingleSex)

        # closes  if(!(is.na(UnmatchedSingleSexToMerge$NumberKids[1])))
        }

        # closes if(!(is.na(UnmatchedSingleSexToMerge$SchoolID[1])))
        } else if ("F" %in% SchoolSubset$SchoolType | "M" %in% SchoolSubset$SchoolType){


          # need to do the same if no matching single sex schools for the age ranges

          # cat("Entered some same sex schools loop", "\n")

          SingleSexSchools <- SchoolSubset %>%
            filter(!(SchoolType == "C"))

          AllSchoolsFromWhichToChoose <- bind_rows(CoedSchoolsSelected, SingleSexSchools)

          # closes else after closes if(!(is.na(UnmatchedSingleSexToMerge$SchoolID[1])))
        } else {

          # cat("Entered no-same-sex-schools loop", "\n")

          AllSchoolsFromWhichToChoose <- CoedSchoolsSelected

        }

       # select schools by probability
        # uses simple weight by roll count
        # cat("The random roll is", NumberSameSchool, "for household", CurrentHousehold, "\n")

        # select schools
        # select school combination by probability
        # but limit to those schools that accept the number of children that do not exceed the number
        # to be assigned to the same school

        # cat("The schools from which to choose are", "\n")
        # str(NumberKidsPerSchool)



        while(NumKidsRemaining > 0) {
        # first set of schools to choose

          MaxChildrenCanTake <- (min(NumberSameSchool, max(NumberKidsPerSchool$NumberKids)))

       # extract these schools
       MultiplesSchools <- NumberKidsPerSchool %>%
         filter(NumberKids >= MaxChildrenCanTake)


      # cat("The max children is", MaxChildrenCanTake, "\n")

        # cat("The number of children to the same school", NumberSameSchool, "\n")

        # random select from the MultipleSchools
        # probability based on roll size

        # print(MultiplesSchools$RollCountSum)

        # will always be at least one child that a school can take
        # need to loop through the number of children in the household so that
        # the school assignment is done for each one

        SchoolChosen <- MultiplesSchools %>%
          slice_sample(weight_by = NumberKids, n=1)


        # now need to loop through all the children in the family

        # print(CurrentHousehold)

        # cat("And the school chosen is", "\n")
        # str(SchoolChosen)

        # cat("With possible schools")
        # str(AllSchoolsFromWhichToChoose)

        # create a SchoolChosenDetail data frame when a same-sex combination is selected
        if(exists("SingleSexMatchedSchools") == TRUE) {

           if(isTRUE(SchoolChosen$SchoolID %in% c(SingleSexMatchedSchools$SchoolID))) {

              # cat("Uses the combo single sex school SchoolID", "\n")

          School1 <- SingleSexMatchedSchools %>%
            filter(SchoolID == SchoolChosen$SchoolID) %>%
            select(SchoolID.x)

          SchoolDetail1 <- SchoolSubset %>%
            filter(SchoolID == School1$SchoolID.x)

          School2 <- SingleSexMatchedSchools %>%
            filter(SchoolID == SchoolChosen$SchoolID) %>%
            select(SchoolID.y)

          SchoolDetail2 <- SchoolSubset %>%
            filter(SchoolID == School2$SchoolID.y)

          SchoolChosenDetail <- bind_rows(SchoolDetail1, SchoolDetail2)

          # close if(isTRUE(SchoolChosenDetail$SchoolID %in% c(SingleSexMatchedSchools$SchoolID)))
           } else {

             # cat("Should have else here and SchoolID is", SchoolChosen$SchoolID, "\n")

             SchoolChosenDetail <- AllSchoolsFromWhichToChoose %>%
               filter(SchoolID == SchoolChosen$SchoolID)

         }

        } else {

          # the school is chosen normally as the single-sex combo was not selected

          # cat("All schools from which to chose is", "\n")
          #
          # str(AllSchoolsFromWhichToChoose)
          #
          # cat("No single sex school combo and the possible schools that will be merged are", "\n")
          #
          # str(SchoolSubset)

          SchoolChosenDetail <- AllSchoolsFromWhichToChoose %>%
            filter(SchoolID == SchoolChosen$SchoolID) #%>%
       #     left_join(SchoolSubset, by = c("SchoolID", "SchoolType")) %>%
            # filter(ChildAge %in% c(ChildAges$ChildAge))

          # cat("The file SchoolChosenDetail inside loop is", "\n")
          # str(SchoolChosenDetail)

          # cat("Also entered this final loop", "\n")

          # cat("The school chosen detail is", "\n")
          #
          # print(str(SchoolChosenDetail))

          # close if(exists("SingleSexMatchedSchools") == TRUE)
        }

        # cat("The children in the household are", "\n")
        # str(ChildrenInHousehold)
        #
        # cat("The file SchoolChosenDetail is", "\n")
        # str(SchoolChosenDetail)

        # cat("ChildSchoolMerge IsMatch below line 496", "\n")


        ChildSchoolMerge <- left_join(ChildrenInHousehold, SchoolChosenDetail, by = "ChildAge") %>%
          mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType | SchoolType == "S", "Y", "N")) %>%
          filter(IsMatch == "Y") %>%
          select(-IsMatch)


      # pick the kids that will go to this school, using the max number that can be assigned
        # loop is only entered if there are more school spots than children
        # due to the join, this can only happen if there are multiple children who are the same age
        # and permitted same-sex combinations

        if(nrow(ChildSchoolMerge) > MaxChildrenCanTake) {

          # cat("There are", MaxChildrenCanTake, "for", nrow(ChildSchoolMerge), "school slots", "\n")

          TimesToRepeat <- 1 + nrow(ChildSchoolMerge) - MaxChildrenCanTake

          AgesToLoop <- ChildSchoolMerge %>%
            pull(ChildAge)

          # print(AgesToLoop)

          for(k in 1:TimesToRepeat) {
            youngest = AgesToLoop[k]
            oldest = AgesToLoop[k + MaxChildrenCanTake - 1]

           # cat("Youngest is", youngest, "and oldest is", oldest, "\n")
            AgeDiffs = oldest - youngest

            # print(AgeDiffs)

            if(k == 1) {
              SmallestVector <- k
              SmallestGap <- AgeDiffs

            }

            if (AgeDiffs < SmallestGap) {

              # cat("Loop entered, smallest gap is", SmallestGap, "smallest vector is", SmallestVector, "\n")

              SmallestVector <- k
              SmallestGap <- AgeDiffs

            }

            # cat("Start row for subset is", SmallestVector, "and end row for subset is", (SmallestVector+(MaxChildrenCanTake-1)), "\n")

          }


          ChildSchoolMerge <- ChildSchoolMerge %>%
            slice(SmallestVector:(SmallestVector+(MaxChildrenCanTake-1)))


          # closes if(nrow(ChildSchoolMerge) > MaxChildrenCanTake)
        }


        # merge the school into the household data

        # do the allocation
        if(exists("ChildrenFinalised")) {

          # cat("Children Finalised exists", "\n")

          ChildrenFinalised <- bind_rows(ChildrenFinalised, ChildSchoolMerge)

        } else {

          ChildrenFinalised <- ChildSchoolMerge

          # get column index for injection
          # only need to grab this the once, when the file is constructed
          SchoolsIDColIndex <- as.numeric(which(colnames(ChildrenFinalised) == "SchoolID"))

          SchoolsTypeColIndex <- as.numeric(which(colnames(ChildrenFinalised) == "SchoolType"))

          # closes  if(exists(ChildrenFinalised))
        }

        # reduce the relevant roll count for the school
        # do straight injection of updated count
        # this will update each time the child/ren are allocated
        # so the numbers will always be accurate

        # NOTE: the usual approach is to remove the allocated school from the set available
        # so it does not matter that the role counts aren't updated for that household
        # as the school will not be used again
        # and where the school has to be added in, the updated roll count is used

        SchoolCountSummaries <- ChildSchoolMerge %>%
          group_by(SchoolID, ChildAge) %>%
          summarise(AllocatedCounts = n())

        for(l in 1:nrow(SchoolCountSummaries)) {
        SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountSummaries$SchoolID[l] &
                                             SchoolsRenamed$ChildAge==SchoolCountSummaries$ChildAge[l]))

        SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] -
          SchoolCountSummaries$AllocatedCounts[l]

        SchoolsRenamed <- SchoolsRenamed %>%
          filter(ChildCounts > 0)

        # closes for(l in 1:nrow(SchoolCountSummaries))
        }

        # remove children who are matched
        ChildrenInHousehold <- ChildrenInHousehold %>%
          filter(!(ChildID %in% c(ChildSchoolMerge$ChildID)))

        # cat("The children in household data are", "\n")
        # str(ChildrenInHousehold)

        # update for whether loop continues
        # cat("The number of children allocated was", nrow(ChildSchoolMerge), "\n")

        NumKidsRemaining <- NumKidsRemaining - nrow(ChildSchoolMerge)

        # cat("The number of kids remaining is", NumKidsRemaining, "\n")

        # update the maximum number that can be allocated to the same school

        # stops the recalculation of this information if there are no more children to assign to schools
        # put the code block in the loop below

        if (NumKidsRemaining > 0) {
        NumberSameSchool <- max(NumberSameSchool - nrow(ChildSchoolMerge), 1)

        AllSchoolsFromWhichToChoose <- AllSchoolsFromWhichToChoose %>%
          filter(!(SchoolID == SchoolChosen$SchoolID),
                 ChildAge %in% c(ChildrenInHousehold$ChildAge))

        if(is.na(AllSchoolsFromWhichToChoose$ChildAge[1])) {

          ChildAges <- as.data.frame(ChildrenInHousehold %>%
                                       group_by(ChildType, ChildAge) %>%
                                       summarise(CountsByAge = n()))

         # construct a non-empty data frame if the schools went to zero because of school
          # age ranges and age ranges of children in the household

            AllSchoolsFromWhichToChoose <- left_join(ChildAges, SchoolsRenamed, by = "ChildAge") %>%
              mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType, "Y", "N")) %>%
              filter(ChildCounts > 0,
                     IsMatch == "Y") %>%
              group_by(SchoolID, ChildAge, SchoolType, ChildCounts) %>%
              summarise(NumberKids = sum(CountsByAge)) %>%
              ungroup() %>%
              mutate(RemainingChildren = ChildCounts - NumberKids)

            # str(ChildAges)
            # str(SchoolsRenamed)
            # str(AllSchoolsFromWhichToChoose)

            # if school slots are small relative to school population
            # and there are single-sex schools and co-ed schools
            # can end up with situation where the only school slot available is at a single-sex school
            # for the opposite sex
            # so fix by doing a straight swap

            if(is.na(AllSchoolsFromWhichToChoose$ChildAge[1])) {

              # cat(CurrentHousehold, "Entered this final fixit loop", "\n")


              # str(SchoolsRenamed)
               # sample one school per remaining child
              IntSchoolsFromWhichToChoose <- left_join(ChildAges, SchoolsRenamed, by = "ChildAge") %>%
                group_by(SchoolID) %>%
                slice(rep(1:n(), first(ChildCounts))) %>%
                left_join(ChildrenInHousehold, by = c("ChildAge", "ChildType")) %>%
                select(ChildID, SchoolID, ChildAge, ChildType, SchoolType) %>%
                group_by(ChildID) %>%
                slice_sample(n = 1) %>%
                ungroup()

               # cat("AllSchoolsFromWhichToChoose", "\n")
              # pick one school to swap in for remaining child
              ChildrenToSwap <- ChildrenFinalised %>%
                filter(SchoolType == "C") %>%
                right_join(IntSchoolsFromWhichToChoose, by = "ChildAge") %>%
                filter(!(ChildType.x == ChildType.y)) %>%
                group_by(ChildID.y) %>%
                slice_sample(n = 1) %>%
                ungroup()

              # process:
              # 1. swap school IDs
              # 2. sub in the new school ID for the donor child/ren, replacing the original
              # 2.A must also change sex allocation
              # 3. create the correctly formatted child merge file for the recipient children
              # 4. add in those recipient children
              # 5. decrease the counts for the schools that were the incorrect sex
              InjectionInformation <- ChildrenToSwap %>%
                select(ChildID.x, SchoolID.y, SchoolType.y)

              for(m in 1:nrow(InjectionInformation)) {

                ChildRowIndex <- as.numeric(which(ChildrenFinalised$ChildID==InjectionInformation$ChildID.x[m]))

                ChildrenFinalised[ChildRowIndex, SchoolsIDColIndex] <- InjectionInformation$SchoolID.y[m]
                ChildrenFinalised[ChildRowIndex, SchoolsTypeColIndex] <- InjectionInformation$SchoolType.y[m]

                # closes for(m in 1:nrow(InjectionInformation))
              }

              # replacement school injected, now fix the child/ren in the current household


              # 3. create the correctly formatted child merge file for the recipient children
              # get children and add the extra columns for merge
              # need to mock up these columns as the schools dataset no longer contains
              # this schoolID/childage combo
              # only incorrect value is SchoolType

              ChildSchoolMerge <- ChildrenToSwap %>%
                select(ChildID.y, SchoolID.x) %>%
                rename(ChildID = ChildID.y,
                       SchoolID = SchoolID.x) %>%
                left_join(ChildrenInHousehold, by = "ChildID") %>%
                mutate(SchoolType = "Z", ChildCounts = 0, NumberKids = 0, RemainingChildren = 0)

              # 4. add in those recipient children
              ChildrenFinalised <- bind_rows(ChildrenFinalised, ChildSchoolMerge)

              # 5. decrease the counts for the schools that were the incorrect sex
              # cat("Fixing school count summaries", "\n")

              SchoolCountSummaries <- ChildrenToSwap %>%
                select(SchoolID.y, ChildAge) %>%
                rename(SchoolID = SchoolID.y) %>%
                group_by(SchoolID, ChildAge) %>%
                summarise(AllocatedCounts = n()) %>%
                left_join(SchoolsRenamed, by = c("SchoolID", "ChildAge")) %>%
                mutate(RemainingChildren = ChildCounts - AllocatedCounts)


              for(l in 1:nrow(SchoolCountSummaries)) {
                SchoolRowIndex <- as.numeric(which(SchoolsRenamed$SchoolID==SchoolCountSummaries$SchoolID[l] &
                                                     SchoolsRenamed$ChildAge==SchoolCountSummaries$ChildAge[l]))

                # cat("School row index is", SchoolRowIndex, "and the school column index is", SchoolsCountColIndex, "\n")

                SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] -
                  SchoolCountSummaries$AllocatedCounts[l]

                SchoolsRenamed <- SchoolsRenamed %>%
                  filter(ChildCounts > 0)

              # closes for(l in 1:nrow(SchoolCountSummaries))
              }

              # remove children who are matched
              ChildrenInHousehold <- ChildrenInHousehold %>%
                filter(!(ChildID %in% c(ChildSchoolMerge$ChildID)))

              AllSchoolsFromWhichToChoose <- bind_rows(SchoolID = "0", "ChildAge" = 0, "SchoolType" = "0",
                                                       "ChildCounts" = 0, "NumberKids" = 0, "RemainingChildren" = 0)

              NumKidsRemaining <- 0

              # cat("The number of children remaining in the household is", nrow(ChildrenInHousehold), "\n")

              # add a child to a school when the addition will be an overcount
              if(nrow(ChildrenInHousehold) > 0) {

              print(list(ChildrenInHousehold$ChildID))

                # options are:
                # 1. school already used, co-ed or same-sex
                # 2. an equivalent same-sex school already used
                # 3. no school used and need to match new school

                # check to see if there is any school that matches the ones the others are in
                SchoolsAlreadyUsed <- ChildrenFinalised %>%
                  filter(HouseholdID == CurrentHousehold) %>%
                  select(SchoolID) %>%
                  unique() %>%
                  left_join(OriginalSchoolsCounts, by = "SchoolID") #%>%
                  filter(SchoolAge %in% ChildrenInHousehold$ChildAge,
                         ChildCounts > 0)

                return(SchoolsAlreadyUsed)

                if(nrow(SchoolsAlreadyUsed) == 0) {

                    FinalSchoolMatch <- ChildrenInHousehold %>%
                      left_join(OriginalSchoolsCounts, by = c("ChildAge" = "SchoolAge")) %>%
                      filter((SchoolType == "C" | SchoolType == ChildType) &
                               ChildCounts > 0)

                      for (m in 1: nrow(ChildrenInHousehold)) {

                          SchoolThatMatched <- FinalSchoolMatch %>%
                            filter(ChildID == ChildrenInHousehold$ChildID[m]) %>%
                            slice_sample(weight_by = ChildCounts, n=1)

                          ChildrenFinalised <- bind_rows(ChildrenFinalised, SchoolThatMatched)


                        # closes for (m in 1: nrow(ChildrenInHousehold))
                      }

                  # closes if(nrow(SchoolsAlreadyUsed) == 0)
                } else {

                  # NOT TESTED

                  # options remaining are
                  # 1. school already used, co-ed or same-sex
                  # 2. an equivalent same-sex school already used


                  # get the age matches
                  InterimSchoolMatches <- ChildrenFinalised %>%
                    filter(HouseholdID == CurrentHousehold) %>%
                    select(SchoolID) %>%
                    unique() %>%
                    filter(SchoolAge %in% ChildrenInHousehold$ChildAge,
                           ChildCounts > 0)

                  # loop through the children matched to no school

                  for (n in 1: nrow(ChildrenInHousehold)) {

                    CurrentChild <- ChildrenInHousehold[n,]

                    SchoolMatchOptions <- CurrentChild %>%
                      left_join(OriginalSchoolsCounts, by = c("ChildAge" = "SchoolAge")) %>%
                      filter(SchoolID %in% c(InterimSchoolMatches$SchoolID))

                    #  # option
                    # 1. school already used, co-ed or same-sex

                    if(SchoolMatchOptions %in% SchoolType == "C" | SchoolType == ChildType) {

                      # random sample one of the schools and add child to the final dataframe


                      # closes if(SchoolMatchOptions %in% SchoolType == "C" | SchoolType == ChildType)
                    } else {

                      # closes else for if(SchoolMatchOptions %in% SchoolType == "C" | SchoolType == ChildType)
                    }

                    # closes for (n in 1: nrow(ChildrenInHousehold))
                  }


                  # closes else for if(nrow(SchoolsAlreadyUsed) == 0)
                }

                  # MissingMatchedSchools <- ChildrenInHousehold %>%
                  #   left_join(OriginalSchoolsCounts, by = c("ChildAge" = "SchoolAge")) %>%
                  #   filter(ChildCounts > 0)
                 # if MoreThanOneOccurrence has multiple matches, need to join those

                 # if (nrow(MoreThanOneOccurrence) == 0) {
                 #
                 #   FinalSchoolMatch <- ChildrenInHousehold %>%
                 #     left_join(OriginalSchoolsCounts, by = c("ChildAge" = "SchoolAge")) %>%
                 #     filter((SchoolType == "C" | SchoolType == ChildType) &
                 #              ChildCounts > 0)
                 #
                 #   for (m in 1: nrow(ChildrenInHousehold)) {
                 #
                 #       SchoolThatMatched <- FinalSchoolMatch %>%
                 #         filter(ChildID == ChildrenInHousehold$ChildID[m]) %>%
                 #         slice_sample(weight_by = ChildCounts, n=1)
                 #
                 #       ChildrenFinalised <- bind_rows(ChildrenFinalised, SchoolThatMatched)
                 #
                 #
                 #     # closes for (m in 1: nrow(ChildrenInHousehold))
                 #   }
#
#                  } else {
#
#
#
#                  # TODO else if MoreThanOneOccurrence > 0
#
#                    # closes the else loop on if MoreThanOneOccurrence > 0
#                  }



#
#                 } else {
#
#                   # TODO if there are children who already have a matching school

                  # closes if(nrow(SchoolsAlreadyUsed) == 0)
                # }

                # closes if(nrow(ChildrenInHousehold) > 0)
              }

              # closes INSIDE if(is.na(AllSchoolsFromWhichToChoose$ChildAge[1]))
             }

          # close if(is.na(AllSchoolsFromWhichToChoose$ChildAge[1]))
        }

        NumberKidsPerSchool <- NumberKidsPerSchool %>%
          filter(SchoolID %in% c(AllSchoolsFromWhichToChoose$SchoolID))


        # cat("The number same school is", NumberSameSchool, "\n")

        # something here about the random roll - number of kids already allocated versus max number kids can take
        # need to update random roll, taking into account of the number of kids already assigned

        # cat("The remaining schools from which to choose are", nrow(AllSchoolsFromWhichToChoose), "\n")

        MaxChildrenCanTake <- max((min(NumberSameSchool, max(AllSchoolsFromWhichToChoose$NumberKids))), 1)

        # cat("The maximum children can take on line 772 is", MaxChildrenCanTake, "\n")
        # cat("The min number same school is", min(NumberSameSchool), "The other is", max(AllSchoolsFromWhichToChoose$NumberKids), "\n")

        # closes if (NumKidsRemaining > 0)
        }


        # closes  while(NumKidsRemaining > 0)
      }


          # closes  if(NumberSameSchool > 1)
      # }


    # closes  for(i in 1:nrow(ChildrenRenamed))

  }
#

  OutputDataframe <- ChildrenFinalised %>%
    select(-c(SchoolType, ChildCounts, NumberKids, RemainingChildren)) %>%
    rename(!!ChildIDColName := ChildID, !!ChildAgeColName := ChildAge,
           !!HouseholdIDColName := HouseholdID, !!ChildSxColName := ChildType,
           !!SchoolIDColName := SchoolID)

  cat("The school allocation by sex is", "\n")
  print(table(ChildrenFinalised$SchoolID, ChildrenFinalised$Sex))

  return(OutputDataframe)
  # closes function
}


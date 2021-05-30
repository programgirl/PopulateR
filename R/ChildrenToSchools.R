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

ChildrenToSchools <- function(Children, ChildIDCol, ChildAgeCol, ChildSxCol, HouseholdIDCol = NULL,
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

  # singleton households, assign these last
  SingletonHouseholds <- ChildrenRenamed %>%
    group_by(HouseholdID) %>%
    summarise(ChildCount = n()) %>%
    filter(ChildCount == 1)

  MultipleChildrenHouseholds <- ChildrenRenamed %>%
    select(HouseholdID) %>%
    filter(!(HouseholdID %in% (SingletonHouseholds$HouseholdID)),
           duplicated(HouseholdID) == FALSE)

  # this is the first part of the code that requires randomness
  # so seed is applied here

  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  # work through the multiple child households first, if these exist

  if(nrow(MultipleChildrenHouseholds > 0)) {

  for(i in 1:nrow(MultipleChildrenHouseholds)) {

    # must delete the data frames constructed for each household
    # if(exists("PossibleSchools")) {
    #   rm(PossibleSchools)
    # }
#
#     if(exists("JoinToMerge")) {
#       rm(JoinToMerge)
#     }

    if(exists("UnmatchedSingleSexToMerge")) {
      rm(UnmatchedSingleSexToMerge)
    }

    if(exists("MergedSingleSexToAdd")) {
      rm(MergedSingleSexToAdd)
    }

    if(exists("SingleSexMatchedSchools")) {
      rm(SingleSexMatchedSchools)
    }


    CurrentHousehold <- MultipleChildrenHouseholds$HouseholdID[i]

    cat("The current household is", CurrentHousehold, "\n")

      # get the children in the household

      ChildrenInHousehold <- ChildrenRenamed %>%
        filter(HouseholdID == CurrentHousehold)

      NumKidsRemaining <- nrow(ChildrenInHousehold)

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

        }


      }


      # print(NumberSameSchool)

      # add children to same school

      # don't think I need this if
      # if(NumberSameSchool > 1) {

      # need to loop through the kids in the household

      # while(NumKidsRemaining > 0) {

        # need to identify the number of children that can go to the same school

        # get child ages vector
        ChildAges <- as.data.frame(ChildrenInHousehold %>%
          group_by(ChildType, ChildAge) %>%
          summarise(CountsByAge = n()))


        # locate schools that can take the maximum number of children from NumberSameSchool down
        # not sure I need this either
        # for(j in 1:nrow(ChildAges)) {

          # cat("Current child age is", ChildAges$ChildAge[j], "and child sex is", ChildAges$ChildType[j],
          #     "and number children that age and sex is", ChildAges$CountsByAge[j], "\n")

         # SchoolSubset <- left_join(ChildAges, SchoolsRenamed, by = "ChildAge") %>%
         #   mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType, "Y", "N")) %>%
         #   filter(ChildCounts > 0,
         #          IsMatch == "Y") %>%
         #   group_by(SchoolID, ChildAge, SchoolType, ChildCounts) %>%
         #   summarise(NumberKids = n()) %>%
         #   ungroup() %>%
         #   mutate(RemainingChildren = ChildCounts - NumberKids) %>%
         #   filter(RemainingChildren >= 0)

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
           # summarise(TotalKids = sum(NumberKids))

        # cat("PossibleSchools IsMatch is below", "\n")

         # NOTE: PossibleSchools did not provide the output needed for further in the function
         # NOTE: now trying to do the function with SchoolSubset instead

          # PossibleSchools <- SchoolsRenamed %>%
          #   filter(ChildAge %in% c(ChildAges$ChildAge)) %>%
          #   right_join(ChildAges, by = c("ChildAge")) %>%
          #     mutate(CountsDecreased = ChildCounts - CountsByAge,
          #            IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType, "Y", "N")) %>%
          #     filter(CountsDecreased > 0,
          #            IsMatch == "Y") %>%
          #   select(-c(ChildType, CountsByAge, CountsDecreased, IsMatch)) %>%
          #   distinct()


        # closes for j loop through selecting schools
        # }

        # get the co-ed schools

         CoedSchoolsSelected <- SchoolSubset %>%
           filter(SchoolType == "C")

        # CoedSchoolsSelected <- PossibleSchools %>%
        #   filter(SchoolType == "C") %>%
        #   group_by(SchoolID) %>%
        #   summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
        #   rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes) %>%
        #   mutate(SchoolType = "C")

        # work with the single-sex schools

         SingleSexSchoolsSelected <- SchoolSubset %>%
           filter(!(SchoolType =="C"))

        # SingleSexSchoolsSelected <- PossibleSchools %>%
        #   filter(!(SchoolType == "C")) %>%
        #   group_by(SchoolID) %>%
        #   summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
        #   rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes)

          # determine if there are two single sex schools that have the equivalent age
         # in the SchoolsSubset
         FemaleAgesToMatch <- SchoolSubset %>%
           filter(SchoolType == "F")

         # this will only return a non-empty data frame if there is a male school
         # that exists in the SchoolsSubset, ignoring age
         MaleSchoolsToMatch <- SchoolSubset %>%
           filter(SchoolType == "M")

         # MaleSchoolsToMatch <- SchoolsRenamed %>%
         #   filter(SchoolType == "M",
         #          SchoolID %in% c(SingleSexSchoolsSelected$SchoolID))


        # FemaleAgesToMatch <- PossibleSchools %>%
        #   filter(SchoolType == "F")
        #
        # MaleSchoolsToMatch <- SchoolsRenamed %>%
        #   filter(SchoolType == "M",
        #          SchoolID %in% c(SingleSexSchoolsSelected$SchoolID))


         # will only return a data frame is there is a male school included in the SchoolSubset
         # that covers the age range of any single-sex schools selected
         # may have no matched males - because either
         # 1. the single-sex schools age ranges do not align, or
         # 2. there is only one single-sex school available

        MatchedMales <- left_join(FemaleAgesToMatch, MaleSchoolsToMatch, by = "ChildAge")

        if(!(is.na(MatchedMales$SchoolID.y[1]))) {

          cat("There is an overlap in the single-sex school ages", "\n")

          SingleSexMatchedSchools <- MatchedMales %>%
            select(SchoolID.x, SchoolID.y) %>%
            distinct() %>%
            left_join(NumberKidsPerSchool, by = c("SchoolID.x" = "SchoolID")) %>%
            rename(ChildCounts.x = ChildCounts, NumberKids.x = NumberKids) %>%
            left_join(NumberKidsPerSchool, by = c("SchoolID.y" = "SchoolID"))  %>%
            rename(ChildCounts.y = ChildCounts, NumberKids.y = NumberKids) %>%
            mutate(ChildCounts = ChildCounts.x + ChildCounts.y,
                   NumberKids = NumberKids.x + NumberKids.y,
                   SchoolID = paste0("CombinedSchool", 1:nrow(.)))

#         # sum the female counts by school
#         SingleSexFemaleSchoolCounts <- FemaleAgesToMatch %>%
#           group_by(SchoolID) %>%
#           summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
#           rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes)
#
#
#         # sum the male counts by school
#         SingleSexMaleSchoolCounts <- MatchedMales %>%
#           select(SchoolID.x, SchoolID.y, ChildCounts.y) %>%
#           group_by(SchoolID.x, SchoolID.y) %>%
#           summarise(across(ChildCounts.y, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
#          rename(RollCountSum = ChildCounts.y_RollCountSum, NumberTimes = ChildCounts.y_NumberTimes) %>%
#           ungroup()
#
#         # only do this if there are matched single-sex schools
#        # if(nrow(SingleSexMaleSchoolCounts) >0) {
#
#         print(CurrentHousehold)
#
#           SingleSexMatchedSchools <- left_join(SingleSexMaleSchoolCounts, SingleSexFemaleSchoolCounts,
#                                                by = c("SchoolID.x" = "SchoolID")) %>%
#             mutate(SchoolID = paste0("CombinedSchool", 1:nrow(.)),
#                    RollCountSum = RollCountSum.x + RollCountSum.y,
#                    NumberTimes = NumberTimes.x + NumberTimes.y)

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

         # used to close  if(nrow(SingleSexMaleSchoolCounts) >0)
        # }

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

        # PossibleSchoolsIndicators <- PossibleSchools %>%
        #   select(SchoolID, SchoolType) %>%
        #   filter(SchoolID %in% c(UnmatchedSingleSexToMerge$SchoolID))
        #
        # AbsentSingleSex <- PossibleSchools %>%
        #   filter(SchoolID %in% c(UnmatchedSingleSexToMerge$SchoolID)) %>%
        #   group_by(SchoolID) %>%
        #   summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
        #   rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes) %>%
        #   left_join(PossibleSchoolsIndicators, by = "SchoolID")


        AllSchoolsFromWhichToChoose <- bind_rows(AllSchoolsFromWhichToChoose, AbsentSingleSex)

        # closes  if(!(is.na(UnmatchedSingleSexToMerge$NumberKids[1])))
        }

        # closes if(!(is.na(UnmatchedSingleSexToMerge$SchoolID[1])))
        } else if ("F" %in% SchoolSubset$SchoolType | "M" %in% SchoolSubset$SchoolType){

        # } else if ("F" %in% PossibleSchools$SchoolType | "M" %in% PossibleSchools$SchoolType){

          # need to do the same if no matching single sex schools for the age ranges

          # cat("Entered some same sex schools loop", "\n")

          SingleSexSchools <- SchoolSubset %>%
            filter(!(SchoolType == "C")) #%>%
            # group_by(SchoolID, SchoolType) %>%
            # summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
            # rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes)

          # SingleSexSchools <- PossibleSchools %>%
          #   filter(!(SchoolType == "C")) %>%
          #   group_by(SchoolID, SchoolType) %>%
          #   summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
          #   rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes)

          AllSchoolsFromWhichToChoose <- bind_rows(CoedSchoolsSelected, SingleSexSchools)

          # closes else after closes if(!(is.na(UnmatchedSingleSexToMerge$SchoolID[1])))
        } else {

          # cat("Entered no-same-sex-schools loop", "\n")

          AllSchoolsFromWhichToChoose <- CoedSchoolsSelected

        }
#
#         cat("The schools from which to choose are", "\n")
#         str(AllSchoolsFromWhichToChoose)

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
        # MaxChildrenCanTake <- (min(NumberSameSchool, max(NumberKidsPerSchool$NumberKids)))

       # extract these schools
       MultiplesSchools <- NumberKidsPerSchool %>%
         filter(NumberKids >= MaxChildrenCanTake)


      # cat("The max children is", MaxChildrenCanTake, "\n")

        # cat("The number of children to the same school", NumberSameSchool, "\n")

        # need to loop through the kids in the household

        # while(NumKidsRemaining > 0) {

          # cat("The number of kids remaining is", print(NumKidsRemaining), "\n")

        # MultiplesSchools <- AllSchoolsFromWhichToChoose %>%
        #   filter(NumberKids >= MaxChildrenCanTake)

        # cat("With", NumKidsRemaining, "kids remaining the multiples schools are:", "\n")
        # str(MultiplesSchools)

     #   cat(str(MultipleSchools), "\n")

        # random select from the MultipleSchools
        # probability based on roll size

        # print(MultiplesSchools$RollCountSum)

        # will always be at least one child that a school can take
        # need to loop through the number of children in the household so that
        # the school assignment is done for each one

        SchoolChosen <- MultiplesSchools %>%
          slice_sample(weight_by = NumberKids, n=1)

        # now need to loop through all the children in the family

        print(CurrentHousehold)


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

             # cat("Should have else here", "\n")

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

        # cat("ChildSchoolMerge IsMatch below", "\n")

        ChildSchoolMerge <- left_join(ChildrenInHousehold, SchoolChosenDetail, by = "ChildAge") %>%
          mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType | SchoolType == "S", "Y", "N")) %>%
          filter(IsMatch == "Y") %>%
          select(-IsMatch)


      # pick the kids that will go to this school, using the max number that can be assigned
        # loop is only entered if there are more school spots than children
        # due to the join, this can only happen if there are multiple children who are the same age
        # and permitted same-sex combinations

        if(nrow(ChildSchoolMerge) > MaxChildrenCanTake) {

          cat("There are", MaxChildrenCanTake, "for", nrow(ChildSchoolMerge), "school slots", "\n")

          TimesToRepeat <- 1 + nrow(ChildSchoolMerge) - MaxChildrenCanTake

          AgesToLoop <- ChildSchoolMerge %>%
            pull(ChildAge)

          print(AgesToLoop)

          for(k in 1:TimesToRepeat) {
            youngest = AgesToLoop[k]
            oldest = AgesToLoop[k + MaxChildrenCanTake - 1]

           cat("Youngest is", youngest, "and oldest is", oldest, "\n")
            AgeDiffs = oldest - youngest

            print(AgeDiffs)

            if(k == 1) {
              SmallestVector <- k
              SmallestGap <- AgeDiffs

            }

            if (AgeDiffs < SmallestGap) {

              cat("Loop entered, smallest gap is", SmallestGap, "smallest vector is", SmallestVector, "\n")

              SmallestVector <- k
              SmallestGap <- AgeDiffs

            }

            cat("Start vector for smallest is", SmallestVector, "\n")


            ChildSchoolMerge <- ChildSchoolMerge %>%
              slice(SmallestVector:(SmallestVector+(MaxChildrenCanTake-1)))

          }



#           #########################################################
#           #########################################################
#           # replacing this block
#           #########################################################
#           #########################################################
          #
          # # only do this for the coed schools
          # # presence of same-sex schools needs to be handled separately
          #
          # # need to account for twins, triplets etc here
          # # check for duplicated and subset these if present
#
#           # prioritise the kids who are the same age
#           CheckForMultiples <- ChildSchoolMerge %>%
#             group_by(ChildAge) %>%
#             summarise(NumberKidsThatAge = n()) %>%
#             filter(NumberKidsThatAge > 1)
#
#           # cat("The multiples are", nrow(CheckForMultiples), "\n")
#
#           # need to handle "multiples" and "not multiples" separately
#           # code below assigns the not-multiples
#           if(!(is.na(CheckForMultiples$ChildAge[1]))) {
#
#             cat("There are", nrow(CheckForMultiples), "multiples", "\n")
#
#             # while school may have too many rows, there may not be enough kids
#             # the same age
#             # does this matter?
#             ChildSchoolMerge <- ChildSchoolMerge %>%
#               slice_sample(weight_by = ChildCounts, n=MaxChildrenCanTake)
#
#             # handle case when multiple children same age
#             # Still trying to work through the logic of sampling from a school where there are double-ups on age,
#             # but there may be
#             # 1. More kids needed than simply those at the same age (need to resample from school at different age/s) and
#             # 2. Fewer kids needed at the same age, so need to sample from the available spots
#           } else {
#
#             ChildrenToFix <- MaxChildrenCanTake
#
#             while(ChildrenToFix > 0) {
#
#               # get the largest age counts
#               LargestAgeCount <- max(CheckForMultiples$NumberKidsThatAge)
#
#               if(CurrentHousehold == 687) {
#
#                 return(LargestAgeCount)
#               }
#
#               # filter those largest age counts
#               SameAgeSample <- CheckForMultiples %>%
#                 filter(NumberKidsThatAge == LargestAgeCount)
#
#               # random select from the largest age counts
#               AgeToUse <- SameAgeSample %>%
#                 slice_sample(n = 1)
#
#               # restrict the matches to that age
#               ChildSchoolMerge <- ChildSchoolMerge %>%
#                 filter(ChildAge == AgeToUse$ChildAge)
#
#               # random delete one if the number of kids that age is more than the
#               # number for the roll that age in that school
#               # first performed for simple match where all children are at the same school
#
#               if(nrow(ChildSchoolMerge) > ChildrenToFix & exists("SingleSexMatchedSchools") == TRUE) {
#
#                 ChildSchoolMerge <- ChildSchoolMerge %>%
#                   slice_sample(n = ChildrenToFix)
#
#               } else {
#                 # now fix the single-sex school combo problem
#                 # get the counts by school
#                 # some counts may have to be all in one school and some in the other
#                 # 1. there is more than one single-sex school
#                 # 2. a. children may only go to one of the single sex schools or
#                 # 2. b. children may go to one of the single sex schools but only a subset go to the corresponding other single sex school
#
#
#                 # closes if(nrow(ChildSchoolMerge) > ChildrenToFix & exists("SingleSexMatchedSchools") == TRUE)
#               }
#
#
#               MaxMultiplesAge <- max(CheckForMultiples$NumberKidsThatAge)
#
#               # TODO: not sure what I am doing in the row below.
#
#               CombinationMade <- ChildSchoolMerge %>%
#                 filter(ChildAge == MaxMultiplesAge)
#
#               # if the count of kids at the same age was always smaller than the count in the school roll
#               # could do a simple sample, but this may not be the case
#
#
#               # work through the remaining kids
#               # if(nrow(CombinationMade) > )
#
#                 ChildrenToFix <- ChildrenToFix - nrow(CombinationMade)
#
#                   # something here that redoes the check for multiples content
#
#                 # something here about testing whether all the kids are selected
#
#                 # closes while(ChildrenToFix > 0)
#             }
#
#             #closes if(is.na(CheckForMultiples$ChildAge[1]))
#           }
#
#
#           # there may also be assignment of children who are not multiples
#           # e.g. twins and one other child go to the same school
#           # the extra child/children must also be retained as a match
#
#           # get the ChildSchoolMerge with the largest number of matches by age
#
#
#
#           # irrelevant as all matches are used
# #         } else {
# #
# #           # just a simple random sample if the condition above doesn't occur
# #           ChildSchoolMerge <- SchoolChosenDetail %>%
# #             slice_sample(weight_by = ChildCounts, n=MaxChildrenCanTake)
# #
#           #########################################################
#           #########################################################
#           # end replacing this block
#           #########################################################
#           #########################################################


          # closes if(nrow(ChildSchoolMerge) > MaxChildrenCanTake)
        }





        # merge the school into the household data

        # do the allocation
        if(exists("ChildrenFinalised")) {

          # cat("Children Finalised exists", "\n")

          ChildrenFinalised <- bind_rows(ChildrenFinalised, ChildSchoolMerge)

        } else {

          ChildrenFinalised <- ChildSchoolMerge

          # closes  if(exists(ChildrenFinalised))
        }


         # reduce the relevant roll count for the school
        # do straight injection of updated count


          # may have multiple kids that can go to the same school but only one/few can,
          # cut down the number of rows to the maximum number that can be accepted

        # remove children who are matched
        ChildrenInHousehold <- ChildrenInHousehold %>%
          filter(!(ChildID %in% c(ChildSchoolMerge$ChildID)))

        # cat("The children in household data are", "\n")
        # str(ChildrenInHousehold)

        # removed matched school from the choices available
        # and restrict those who contain the remaining ages
        # cat("Before restricting the schools there are", "\n")
        # str(AllSchoolsFromWhichToChoose)

        AllSchoolsFromWhichToChoose <- AllSchoolsFromWhichToChoose %>%
          filter(!(SchoolID == SchoolChosen$SchoolID),
                 ChildAge %in% c(ChildrenInHousehold$ChildAge))


        NumberKidsPerSchool <- NumberKidsPerSchool %>%
          filter(SchoolID %in% c(AllSchoolsFromWhichToChoose$SchoolID))

        # if(CurrentHousehold == 672) {
        #
        #   return(AllSchoolsFromWhichToChoose)
        # }

        # cat("The remaining schools are", "\n")
        # str(AllSchoolsFromWhichToChoose)
        # str(NumberKidsPerSchool)

        # update for whether loop continues
        # cat("The number of children allocated was", nrow(ChildSchoolMerge), "\n")

        NumKidsRemaining <- NumKidsRemaining - nrow(ChildSchoolMerge)

        # cat("The number of kids remaining is", NumKidsRemaining, "\n")

        # update the maximum number that can be allocated to the same school

        # stops the recalculation of this information if there are no more children to assign to schools
        if (NumKidsRemaining > 0) {
        NumberSameSchool <- max(NumberSameSchool - nrow(ChildSchoolMerge), 1)

        # cat("The number same school is", NumberSameSchool, "\n")

        # something here about the random roll - number of kids already allocated versus max number kids can take
        # need to update random roll, taking into account of the number of kids already assigned

        # cat("The remaining schools from which to choose are", nrow(AllSchoolsFromWhichToChoose), "\n")

        MaxChildrenCanTake <- max((min(NumberSameSchool, max(AllSchoolsFromWhichToChoose$NumberKids))), 1)

        # cat("The maximum children can take on line 779 is", MaxChildrenCanTake, "\n")
        # cat("The min number same school is", min(NumberSameSchool), "The other is", max(AllSchoolsFromWhichToChoose$NumberKids), "\n")

        # closes if (NumKidsRemaining > 0)
        }


        # closes  while(NumKidsRemaining > 0)
      }


          # closes  if(NumberSameSchool > 1)
      # }


    # closes  for(i in 1: nrow(MultipleChildrenHouseholds))
  }
#



    # closes if(nrow(MultipleChildrenHouseholds > 0)
  }


  return(ChildrenFinalised)
  # closes function
}


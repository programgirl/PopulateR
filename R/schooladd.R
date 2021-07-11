#' Create a data frame of children matched to schools
#' This function creates a data frame of children and matching schools. By default, all similarly-aged children in the same household will be matched to the same school. If one child is matched to a same-sex school, then all similarly aged children will also be matched to a same-sex school. This includes opposite-sex children.
#' Two data frames are required: one for the children and one for the schools.
#' No minimum or maximum child ages are required, as the function limits the ages within the age range across the schools. Thus, pre-cleaning the children data frame is not required.
#' The Schools data frame must be a summary in the form of counts by age within school. Each row is one age only. For example, if a school has children aged 5 to 9 years, there should be 5 rows. Any combination of co-educational and single-sex schools can be used.
#'
#' @export
#' @param children A data frame containing the children to be paired with a parent/guardian.
#' @param chlidcol The column number for the ID variable in the children data frame.
#' @param chlagecol The column number for the age variable in the children data frame.
#' @param chlsxcol The column number for the sex indicator for children. This column is used to assign children to the appropriate school type (co-educational or single-sex). The expected values are "F" (female) or "M" (male).
#' @param hhidcol The column number for the household identifier variable in the parent data frame.
#' @param schools A data frame containing the school observations.
#' @param schidcol The column number for the ID variable in the schools data frame. If the IDs are factors, these will be converted to character.
#' @param schagecol The column number for the Age variable in the schools data frame..
#' @param schrollcol The number of places available for children at that school age, within the school.
#' @param schtypecol An indicator variable used to determine whether the school is co-educational or single-sex. The expected values are "C" (co-educational), "F" (female only), and "M" (male-only).
#' @param childprob If one child is assigned to a same-sex school, the probability that another child in the household is also assigned to a same-sex school. If an equivalent same-sex school is not available, the other child will be assigned to a co-ed school. The default value is 1, so that all similarly aged children will be assigned to their respective same-sex schools, or all will be to co-educational schools.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

schooladd <- function(children, chlidcol, chlagecol, chlsxcol, hhidcol = NULL, schools, schidcol, schagecol,
                      schrollcol, schtypecol, childprob = 1, UserSeed=NULL)
{

  options(dplyr.summarise.inform=F)


  #####################################################################
  #####################################################################
  # Test for any problem ages, stop function if this situation exists
  #####################################################################
  #####################################################################

  childrenRenamed <- children %>%
    rename(ChildID = !! chlidcol, ChildAge = !! chlagecol, ChildType = !! chlsxcol,
           HouseholdID = !! hhidcol)

  schoolsRenamed <- schools %>%
    rename(SchoolID = !! schidcol, SchoolAge = !! schagecol,
           ChildCounts = !! schrollcol, SchoolType = !! schtypecol) %>%
    mutate(SchoolID = as.character(SchoolID),
           SchoolType = as.character(SchoolType)) %>%
    #mutate(across(where(is.factor), as.character)) %>%
    select(SchoolID, SchoolAge, ChildCounts, SchoolType)

  OriginalschoolsCounts <- schoolsRenamed

  ###############################################
  ###############################################
   # get the col names
  ###############################################
  ###############################################

  chlidcolName <- sym(names(children[chlidcol]))
  chlagecolName <- sym(names(children[chlagecol]))
  hhidcolName <- sym(names(children[hhidcol]))
  chlsxcolName <- sym(names(children[chlsxcol]))
  schidcolName <- sym(names(schools[schidcol]))

  ###############################################
  ###############################################
  # quick test of compatibility of counts
  ###############################################
  ###############################################
  childrenCountTest <- childrenRenamed %>%
    group_by(ChildAge) %>%
    summarise(AgeCount = n())

  schoolsCountTest <- schoolsRenamed %>%
    group_by(SchoolAge) %>%
    summarise(SchoolAgeCount = sum(ChildCounts))

  CountComparison <- full_join(childrenCountTest, schoolsCountTest, by = c("ChildAge" = "SchoolAge")) %>%
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

  childrenRenamed <- left_join(AgeRestriction, childrenRenamed, by = "ChildAge")


  # NOTE: this removes any school classrooms where NO children of that age exist in the data
  schoolsRenamed <- left_join(AgeRestriction, schoolsRenamed, by = c("ChildAge" = "SchoolAge"))

  schoolsCountColIndex <- as.numeric(which(colnames(schoolsRenamed) == "ChildCounts"))

  # get rid of the tibbles
  childrenRenamed <- as.data.frame(childrenRenamed)
  schoolsRenamed <- as.data.frame(schoolsRenamed)

  #####################################################################
  # Create household meta data data frame
  #####################################################################
  # get the number of households
  NumberHouseholds <- as.numeric(childrenRenamed %>%
                                   dplyr::summarise(Count = n_distinct(HouseholdID)) %>%
                                   pull(Count))

  # get list of household IDs
  HouseholdIDList <- as.data.frame(childrenRenamed %>%
                                     distinct(HouseholdID))

#####################################################################
# create counts by sex
# will be used to ensure that selected schools do not decrease
# the availability of
#####################################################################

  childrenSexAge <- childrenRenamed %>%
    group_by(ChildType, ChildAge) %>%
    summarise(AgeCount = n())

  schoolsexAge <- schoolsRenamed %>%
    group_by(SchoolType, ChildAge) %>%
    summarise(SchoolAgeCount = sum(ChildCounts))

  # TODO remove this split, if no merge == shift between fix and not fix
  # merge may fail on first attempt in household

  # this is the first part of the code that requires randomness
  # so seed is applied here

  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

  ListofHouseholds <- childrenRenamed %>%
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

    if(exists("SingleSexMatchedschools")) {
      rm(SingleSexMatchedschools)
    }


    CurrentHousehold <- ListofHouseholds$HouseholdID[i]

    # cat("The current household is", CurrentHousehold, "\n")


      # get the children in the household

      childrenInHousehold <- childrenRenamed %>%
        filter(HouseholdID == CurrentHousehold)

      NumKidsRemaining <- nrow(childrenInHousehold)

      # only need to do this for households with more than one child
      if(NumKidsRemaining >1) {


      # random roll to see if any children in same school, will prioritise the twins
      RandomRollVector <- runif(nrow(childrenInHousehold)-1)

      # cat(RandomRollVector, "\n")

      # test number of children who should go to the same school
      NumberSameSchool <- data.frame(RandomRollVector) %>%
        filter(RandomRollVector > (1-childprob)) %>%
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

        if(FinalRandomRoll > (1-childprob)) {

          NumberSameSchool <- NumberSameSchool +1

          # closes if(FinalRandomRoll > (1-childprob))
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
        ChildAges <- as.data.frame(childrenInHousehold %>%
          group_by(ChildType, ChildAge) %>%
          summarise(CountsByAge = n()))


        # locate schools that can take the maximum number of children from NumberSameSchool down

         schoolsubset <- left_join(ChildAges, schoolsRenamed, by = "ChildAge") %>%
           mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType, "Y", "N")) %>%
           filter(ChildCounts > 0,
                  IsMatch == "Y") %>%
           group_by(SchoolID, ChildAge, SchoolType, ChildCounts) %>%
           summarise(NumberKids = sum(CountsByAge)) %>%
           ungroup() %>%
           mutate(Remainingchildren = ChildCounts - NumberKids) %>%
           filter(Remainingchildren >= 0)

         NumberKidsPerSchool <- schoolsubset %>%
           group_by(SchoolID) %>%
           summarise(across(c(ChildCounts, NumberKids), sum))

        # cat("Possibleschools IsMatch is below", "\n")

        # get the co-ed schools

         CoedschoolsSelected <- schoolsubset %>%
           filter(SchoolType == "C")


        # work with the single-sex schools

         SingleSexschoolsSelected <- schoolsubset %>%
           filter(!(SchoolType =="C"))


          # determine if there are two single sex schools that have the equivalent age
         # in the schoolsSubset
         FemaleAgesToMatch <- schoolsubset %>%
           filter(SchoolType == "F")

         # this will only return a non-empty data frame if there is a male school
         # that exists in the schoolsSubset, ignoring age
         MaleschoolsToMatch <- schoolsubset %>%
           filter(SchoolType == "M")


         # will only return a data frame is there is a male school included in the schoolsubset
         # that covers the age range of any single-sex schools selected
         # may have no matched males - because either
         # 1. the single-sex schools age ranges do not align, or
         # 2. there is only one single-sex school available

        MatchedMales <- left_join(FemaleAgesToMatch, MaleschoolsToMatch, by = "ChildAge")

        if(!(is.na(MatchedMales$SchoolID.y[1]))) {

          # cat("There is an overlap in the single-sex school ages", "\n")

          SingleSexMatchedschools <- MatchedMales %>%
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


          # cat("SingleSexMatchedschools okay for household", CurrentHousehold, "\n")

          MergedSingleSexToAdd <- SingleSexMatchedschools %>%
            select(SchoolID, ChildCounts, NumberKids) %>%
            mutate(SchoolType = "S")

          # cat("MergedSingleSexToAdd okay for household", CurrentHousehold, "\n")

          # note the NA value for the children remaining as this information is absent from the merger
          # of the single-sex schools
        AllschoolsFromWhichToChoose <- bind_rows(CoedschoolsSelected, MergedSingleSexToAdd)

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

        # cat("AllschoolsFromWhichToChoose okay for household", CurrentHousehold, "\n")


        UnmatchedSingleSexToMerge <- SingleSexschoolsSelected %>%
          filter(!(SchoolID %in% c(SingleSexMatchedschools$SchoolID.x)),
                 !(SchoolID %in% c(SingleSexMatchedschools$SchoolID.y)))

        if(!(is.na(UnmatchedSingleSexToMerge$NumberKids[1]))) {

          # cat("UnmatchedSingleSexToMerge loop entered for for household", CurrentHousehold, "\n")

          # print(str(Possibleschools))
          # cat("The rows in UnmatchedSingleSexToMerge are", nrow(UnmatchedSingleSexToMerge), "\n")
          # print(str(UnmatchedSingleSexToMerge))
          # cat("The key test value is", UnmatchedSingleSexToMerge$NumberTimes[1], "\n")

          PossibleschoolsIndicators <- schoolsubset %>%
            select(SchoolID, SchoolType) %>%
            filter(SchoolID %in% c(UnmatchedSingleSexToMerge$SchoolID))

          AbsentSingleSex <- schoolsubset %>%
            filter(SchoolID %in% c(UnmatchedSingleSexToMerge$SchoolID)) %>%
            group_by(SchoolID) %>%
            summarise(across(ChildCounts, list(RollCountSum = sum, NumberTimes = ~n()))) %>%
            rename(RollCountSum = ChildCounts_RollCountSum, NumberTimes = ChildCounts_NumberTimes) %>%
            left_join(PossibleschoolsIndicators, by = "SchoolID")


        AllschoolsFromWhichToChoose <- bind_rows(AllschoolsFromWhichToChoose, AbsentSingleSex)

        # closes  if(!(is.na(UnmatchedSingleSexToMerge$NumberKids[1])))
        }

        # closes if(!(is.na(UnmatchedSingleSexToMerge$SchoolID[1])))
        } else if ("F" %in% schoolsubset$SchoolType | "M" %in% schoolsubset$SchoolType){


          # need to do the same if no matching single sex schools for the age ranges

          # cat("Entered some same sex schools loop", "\n")

          SingleSexschools <- schoolsubset %>%
            filter(!(SchoolType == "C"))

          AllschoolsFromWhichToChoose <- bind_rows(CoedschoolsSelected, SingleSexschools)

          # closes else after closes if(!(is.na(UnmatchedSingleSexToMerge$SchoolID[1])))
        } else {

          # cat("Entered no-same-sex-schools loop", "\n")

          AllschoolsFromWhichToChoose <- CoedschoolsSelected

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

          MaxchildrenCanTake <- (min(NumberSameSchool, max(NumberKidsPerSchool$NumberKids)))

       # extract these schools
       Multiplesschools <- NumberKidsPerSchool %>%
         filter(NumberKids >= MaxchildrenCanTake)


      # cat("The max children is", MaxchildrenCanTake, "\n")

        # cat("The number of children to the same school", NumberSameSchool, "\n")

        # random select from the Multipleschools
        # probability based on roll size

        # print(Multiplesschools$RollCountSum)

        # will always be at least one child that a school can take
        # need to loop through the number of children in the household so that
        # the school assignment is done for each one

        SchoolChosen <- Multiplesschools %>%
          slice_sample(weight_by = NumberKids, n=1)


        # now need to loop through all the children in the family

        # print(CurrentHousehold)

        # cat("And the school chosen is", "\n")
        # str(SchoolChosen)

        # cat("With possible schools")
        # str(AllschoolsFromWhichToChoose)

        # create a SchoolChosenDetail data frame when a same-sex combination is selected
        if(exists("SingleSexMatchedschools") == TRUE) {

           if(isTRUE(SchoolChosen$SchoolID %in% c(SingleSexMatchedschools$SchoolID))) {

              # cat("Uses the combo single sex school SchoolID", "\n")

          School1 <- SingleSexMatchedschools %>%
            filter(SchoolID == SchoolChosen$SchoolID) %>%
            select(SchoolID.x)

          SchoolDetail1 <- schoolsubset %>%
            filter(SchoolID == School1$SchoolID.x)

          School2 <- SingleSexMatchedschools %>%
            filter(SchoolID == SchoolChosen$SchoolID) %>%
            select(SchoolID.y)

          SchoolDetail2 <- schoolsubset %>%
            filter(SchoolID == School2$SchoolID.y)

          SchoolChosenDetail <- bind_rows(SchoolDetail1, SchoolDetail2)

          # close if(isTRUE(SchoolChosenDetail$SchoolID %in% c(SingleSexMatchedschools$SchoolID)))
           } else {

             # cat("Should have else here and SchoolID is", SchoolChosen$SchoolID, "\n")

             SchoolChosenDetail <- AllschoolsFromWhichToChoose %>%
               filter(SchoolID == SchoolChosen$SchoolID)

         }

        } else {

          # the school is chosen normally as the single-sex combo was not selected

          # cat("All schools from which to chose is", "\n")
          #
          # str(AllschoolsFromWhichToChoose)
          #
          # cat("No single sex school combo and the possible schools that will be merged are", "\n")
          #
          # str(schoolsubset)

          SchoolChosenDetail <- AllschoolsFromWhichToChoose %>%
            filter(SchoolID == SchoolChosen$SchoolID) #%>%
       #     left_join(schoolsubset, by = c("SchoolID", "SchoolType")) %>%
            # filter(ChildAge %in% c(ChildAges$ChildAge))

          # cat("The file SchoolChosenDetail inside loop is", "\n")
          # str(SchoolChosenDetail)

          # cat("Also entered this final loop", "\n")

          # cat("The school chosen detail is", "\n")
          #
          # print(str(SchoolChosenDetail))

          # close if(exists("SingleSexMatchedschools") == TRUE)
        }

        # cat("The children in the household are", "\n")
        # str(childrenInHousehold)
        #
        # cat("The file SchoolChosenDetail is", "\n")
        # str(SchoolChosenDetail)

        # cat("ChildSchoolMerge IsMatch below line 496", "\n")


        ChildSchoolMerge <- left_join(childrenInHousehold, SchoolChosenDetail, by = "ChildAge") %>%
          mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType | SchoolType == "S", "Y", "N")) %>%
          filter(IsMatch == "Y") %>%
          select(-IsMatch)


      # pick the kids that will go to this school, using the max number that can be assigned
        # loop is only entered if there are more school spots than children
        # due to the join, this can only happen if there are multiple children who are the same age
        # and permitted same-sex combinations

        if(nrow(ChildSchoolMerge) > MaxchildrenCanTake) {

          # cat("There are", MaxchildrenCanTake, "for", nrow(ChildSchoolMerge), "school slots", "\n")

          TimesToRepeat <- 1 + nrow(ChildSchoolMerge) - MaxchildrenCanTake

          AgesToLoop <- ChildSchoolMerge %>%
            pull(ChildAge)

          # print(AgesToLoop)

          for(k in 1:TimesToRepeat) {
            youngest = AgesToLoop[k]
            oldest = AgesToLoop[k + MaxchildrenCanTake - 1]

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

            # cat("Start row for subset is", SmallestVector, "and end row for subset is", (SmallestVector+(MaxchildrenCanTake-1)), "\n")

          }


          ChildSchoolMerge <- ChildSchoolMerge %>%
            slice(SmallestVector:(SmallestVector+(MaxchildrenCanTake-1)))


          # closes if(nrow(ChildSchoolMerge) > MaxchildrenCanTake)
        }


        # merge the school into the household data

        # do the allocation
        if(exists("childrenFinalised")) {

          # cat("children Finalised exists", "\n")

          childrenFinalised <- bind_rows(childrenFinalised, ChildSchoolMerge)

        } else {

          childrenFinalised <- ChildSchoolMerge

          # get column index for injection
          # only need to grab this the once, when the file is constructed
          schoolsIDColIndex <- as.numeric(which(colnames(childrenFinalised) == "SchoolID"))

          schoolsTypeColIndex <- as.numeric(which(colnames(childrenFinalised) == "SchoolType"))

          # closes  if(exists(childrenFinalised))
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
        SchoolRowIndex <- as.numeric(which(schoolsRenamed$SchoolID==SchoolCountSummaries$SchoolID[l] &
                                             schoolsRenamed$ChildAge==SchoolCountSummaries$ChildAge[l]))

        schoolsRenamed[SchoolRowIndex, schoolsCountColIndex] <- schoolsRenamed[SchoolRowIndex, schoolsCountColIndex] -
          SchoolCountSummaries$AllocatedCounts[l]

        schoolsRenamed <- schoolsRenamed %>%
          filter(ChildCounts > 0)

        # closes for(l in 1:nrow(SchoolCountSummaries))
        }

        # remove children who are matched
        childrenInHousehold <- childrenInHousehold %>%
          filter(!(ChildID %in% c(ChildSchoolMerge$ChildID)))

        # cat("The children in household data are", "\n")
        # str(childrenInHousehold)

        # update for whether loop continues
        # cat("The number of children allocated was", nrow(ChildSchoolMerge), "\n")

        NumKidsRemaining <- NumKidsRemaining - nrow(ChildSchoolMerge)

        # cat("The number of kids remaining is", NumKidsRemaining, "\n")

        # update the maximum number that can be allocated to the same school

        # stops the recalculation of this information if there are no more children to assign to schools
        # put the code block in the loop below

        if (NumKidsRemaining > 0) {
        NumberSameSchool <- max(NumberSameSchool - nrow(ChildSchoolMerge), 1)

        AllschoolsFromWhichToChoose <- AllschoolsFromWhichToChoose %>%
          filter(!(SchoolID == SchoolChosen$SchoolID),
                 ChildAge %in% c(childrenInHousehold$ChildAge))

        if(is.na(AllschoolsFromWhichToChoose$ChildAge[1])) {

          ChildAges <- as.data.frame(childrenInHousehold %>%
                                       group_by(ChildType, ChildAge) %>%
                                       summarise(CountsByAge = n()))

         # construct a non-empty data frame if the schools went to zero because of school
          # age ranges and age ranges of children in the household

            AllschoolsFromWhichToChoose <- left_join(ChildAges, schoolsRenamed, by = "ChildAge") %>%
              mutate(IsMatch = ifelse(SchoolType == "C" | SchoolType == ChildType, "Y", "N")) %>%
              filter(ChildCounts > 0,
                     IsMatch == "Y") %>%
              group_by(SchoolID, ChildAge, SchoolType, ChildCounts) %>%
              summarise(NumberKids = sum(CountsByAge)) %>%
              ungroup() %>%
              mutate(Remainingchildren = ChildCounts - NumberKids)

            # str(ChildAges)
            # str(schoolsRenamed)
            # str(AllschoolsFromWhichToChoose)

            # if school slots are small relative to school population
            # and there are single-sex schools and co-ed schools
            # can end up with situation where the only school slot available is at a single-sex school
            # for the opposite sex
            # so fix by doing a straight swap

            if(is.na(AllschoolsFromWhichToChoose$ChildAge[1])) {

              # cat(CurrentHousehold, "Entered this final fixit loop", "\n")


              # str(schoolsRenamed)
               # sample one school per remaining child
              IntschoolsFromWhichToChoose <- left_join(ChildAges, schoolsRenamed, by = "ChildAge") %>%
                group_by(SchoolID) %>%
                slice(rep(1:n(), first(ChildCounts))) %>%
                left_join(childrenInHousehold, by = c("ChildAge", "ChildType")) %>%
                select(ChildID, SchoolID, ChildAge, ChildType, SchoolType) %>%
                group_by(ChildID) %>%
                slice_sample(n = 1) %>%
                ungroup()

               # cat("AllschoolsFromWhichToChoose", "\n")
              # pick one school to swap in for remaining child
              childrenToSwap <- childrenFinalised %>%
                filter(SchoolType == "C") %>%
                right_join(IntschoolsFromWhichToChoose, by = "ChildAge") %>%
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
              InjectionInformation <- childrenToSwap %>%
                select(ChildID.x, SchoolID.y, SchoolType.y)

              for(m in 1:nrow(InjectionInformation)) {

                ChildRowIndex <- as.numeric(which(childrenFinalised$ChildID==InjectionInformation$ChildID.x[m]))

                childrenFinalised[ChildRowIndex, schoolsIDColIndex] <- InjectionInformation$SchoolID.y[m]
                childrenFinalised[ChildRowIndex, schoolsTypeColIndex] <- InjectionInformation$SchoolType.y[m]

                # closes for(m in 1:nrow(InjectionInformation))
              }

              # replacement school injected, now fix the child/ren in the current household


              # 3. create the correctly formatted child merge file for the recipient children
              # get children and add the extra columns for merge
              # need to mock up these columns as the schools dataset no longer contains
              # this schoolID/childage combo
              # only incorrect value is SchoolType

              ChildSchoolMerge <- childrenToSwap %>%
                select(ChildID.y, SchoolID.x) %>%
                rename(ChildID = ChildID.y,
                       SchoolID = SchoolID.x) %>%
                left_join(childrenInHousehold, by = "ChildID") %>%
                mutate(SchoolType = "Z", ChildCounts = 0, NumberKids = 0, Remainingchildren = 0)

              # 4. add in those recipient children
              childrenFinalised <- bind_rows(childrenFinalised, ChildSchoolMerge)

              # 5. decrease the counts for the schools that were the incorrect sex
              # cat("Fixing school count summaries", "\n")

              SchoolCountSummaries <- childrenToSwap %>%
                select(SchoolID.y, ChildAge) %>%
                rename(SchoolID = SchoolID.y) %>%
                group_by(SchoolID, ChildAge) %>%
                summarise(AllocatedCounts = n()) %>%
                left_join(schoolsRenamed, by = c("SchoolID", "ChildAge")) %>%
                mutate(Remainingchildren = ChildCounts - AllocatedCounts)


              for(l in 1:nrow(SchoolCountSummaries)) {
                SchoolRowIndex <- as.numeric(which(schoolsRenamed$SchoolID==SchoolCountSummaries$SchoolID[l] &
                                                     schoolsRenamed$ChildAge==SchoolCountSummaries$ChildAge[l]))

                # cat("School row index is", SchoolRowIndex, "and the school column index is", schoolsCountColIndex, "\n")

                schoolsRenamed[SchoolRowIndex, schoolsCountColIndex] <- schoolsRenamed[SchoolRowIndex, schoolsCountColIndex] -
                  SchoolCountSummaries$AllocatedCounts[l]

                schoolsRenamed <- schoolsRenamed %>%
                  filter(ChildCounts > 0)

              # closes for(l in 1:nrow(SchoolCountSummaries))
              }

              # remove children who are matched
              childrenInHousehold <- childrenInHousehold %>%
                filter(!(ChildID %in% c(ChildSchoolMerge$ChildID)))

              AllschoolsFromWhichToChoose <- bind_rows(SchoolID = "0", "ChildAge" = 0, "SchoolType" = "0",
                                                       "ChildCounts" = 0, "NumberKids" = 0,
                                                       "Remainingchildren" = 0)

              NumKidsRemaining <- 0

              # cat("The number of children remaining in the household is", nrow(childrenInHousehold), "\n")

              # add a child to a school when the addition will be an overcount
              if(nrow(childrenInHousehold) > 0) {

            #  print(list(childrenInHousehold$ChildID))

                # options are:
                # 1. school already used
                # 2. an equivalent same-sex school already used
                # 3. no school used and need to match new school

                # check to see if there is any school that matches the ones the others are in
                schoolsAlreadyUsed <- childrenFinalised %>%
                  filter(HouseholdID == CurrentHousehold) %>%
                  select(SchoolID) %>%
                  unique() %>%
                  left_join(OriginalschoolsCounts, by = "SchoolID") %>%
                  filter(SchoolAge %in% childrenInHousehold$ChildAge,
                         ChildCounts > 0)

                # Option 3. no school used and need to match new school
                if(nrow(schoolsAlreadyUsed) == 0) {

                    FinalSchoolMatch <- childrenInHousehold %>%
                      left_join(OriginalschoolsCounts, by = c("ChildAge" = "SchoolAge")) %>%
                      filter((SchoolType == "C" | SchoolType == ChildType) &
                               ChildCounts > 0)

                      for (m in 1: nrow(childrenInHousehold)) {

                          SchoolThatMatched <- FinalSchoolMatch %>%
                            filter(ChildID == childrenInHousehold$ChildID[m]) %>%
                            slice_sample(weight_by = ChildCounts, n=1)

                          childrenFinalised <- bind_rows(childrenFinalised, SchoolThatMatched)

                        # closes for (m in 1: nrow(childrenInHousehold))
                      }

                  # closes if(nrow(schoolsAlreadyUsed) == 0)
                } else {

                  # NOT TESTED

                  # options remaining are
                  # 1. school already used
                  # 2. an equivalent same-sex school already used

                  # get the age matches
                  InterimSchoolMatches <- childrenFinalised %>%
                    filter(HouseholdID == CurrentHousehold) %>%
                    select(SchoolID) %>%
                    unique() %>%
                    filter(SchoolAge %in% childrenInHousehold$ChildAge,
                           ChildCounts > 0)

                  # loop through the children matched to no school

                  for (n in 1: nrow(childrenInHousehold)) {

                    CurrentChild <- childrenInHousehold[n,]

                    SchoolMatchOptions <- CurrentChild %>%
                      left_join(OriginalschoolsCounts, by = c("ChildAge" = "SchoolAge")) %>%
                      filter(SchoolID %in% c(InterimSchoolMatches$SchoolID))

                    #  # option
                    # 1. school already used, same-sex

                    if(CurrentChild$ChildType %in% c(SchoolMatchOptions$SchoolType)) {

                      # remove matches that are opposite sex
                      SchoolMatchOptions <- SchoolMatchOptions %>%
                        filter(SchoolType == CurrentChild$ChildType)

                      # random sample one of the schools and add child to the final dataframe

                      SchoolThatMatched <- SchoolMatchOptions %>%
                        slice_sample(weight_by = ChildCounts, n=1)

                      childrenFinalised <- bind_rows(childrenFinalised, SchoolThatMatched)

                      # closes if(SchoolMatchOptions %in% SchoolType == "C" | SchoolType == ChildType)

                    } else {

                      # no matching single-sex schools
                      # look at opposite-sex single-sex school

                      SchoolMatchOptionsSexes <- SchoolMatchOptions %>%
                        filter(!(SchoolType == CurrentChild$ChildType) &
                                !(SchoolType == "C"))

                      # deal with the situation where there are schools of different types
                      if (nrow(SchoolMatchOptionsSexes) > 0) {

                        SchoolThatMatched <- SchoolMatchOptions %>%
                          slice_sample(weight_by = ChildCounts, n=1)

                        childrenFinalised <- bind_rows(childrenFinalised, SchoolThatMatched)

                          # there may be no single-sex schools that match the sex of the child
                        # which means only co-ed schools left

                        # closes if (nrow(SchoolMatchOptionsSexes) > 0)
                          } else {

                            # for the co-ed only schools
                            SchoolThatMatched <- SchoolMatchOptions %>%
                              filter(SchoolType == "C") %>%
                              slice_sample(weight_by = ChildCounts, n=1)

                            childrenFinalised <- bind_rows(childrenFinalised, SchoolThatMatched)

                          # closes else to if (nrow(SchoolMatchOptionsSexes) > 0)
                      }

                      # closes else to if(CurrentChild$ChildType %in% c(SchoolMatchOptions$SchoolType))
                    }

                    # closes for (n in 1: nrow(childrenInHousehold))
                  }

                  # closes else for if(nrow(schoolsAlreadyUsed) == 0)
                }

                   # closes if(nrow(childrenInHousehold) > 0)
              }

              # closes INSIDE if(is.na(AllschoolsFromWhichToChoose$ChildAge[1]))
             }

          # close if(is.na(AllschoolsFromWhichToChoose$ChildAge[1]))
        }

        NumberKidsPerSchool <- NumberKidsPerSchool %>%
          filter(SchoolID %in% c(AllschoolsFromWhichToChoose$SchoolID))


        # cat("The number same school is", NumberSameSchool, "\n")

        # something here about the random roll - number of kids already allocated versus max number kids can take
        # need to update random roll, taking into account of the number of kids already assigned

        # cat("The remaining schools from which to choose are", nrow(AllschoolsFromWhichToChoose), "\n")

        MaxchildrenCanTake <- max((min(NumberSameSchool, max(AllschoolsFromWhichToChoose$NumberKids))), 1)

        # cat("The maximum children can take on line 772 is", MaxchildrenCanTake, "\n")
        # cat("The min number same school is", min(NumberSameSchool), "The other is", max(AllschoolsFromWhichToChoose$NumberKids), "\n")

        # closes if (NumKidsRemaining > 0)
        }


        # closes  while(NumKidsRemaining > 0)
      }


          # closes  if(NumberSameSchool > 1)
      # }


    # closes  for(i in 1:nrow(childrenRenamed))

  }
#

  OutputDataframe <- childrenFinalised %>%
    select(-c(SchoolType, ChildCounts, NumberKids, Remainingchildren)) %>%
    rename(!!chlidcolName := ChildID, !!chlagecolName := ChildAge,
           !!hhidcolName := HouseholdID, !!chlsxcolName := ChildType,
           !!schidcolName := SchoolID)

  cat("The school allocation by sex is", "\n")
  print(table(childrenFinalised$SchoolID, childrenFinalised$Sex))

  return(OutputDataframe)
  # closes function
}


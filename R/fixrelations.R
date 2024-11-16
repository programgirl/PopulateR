#' @importFrom data.table :=
#' @importFrom dplyr all_of arrange bind_cols bind_rows desc filter group_by left_join mutate pull rename rename_all select slice_sample summarise
#' @importFrom magrittr %>%
#' @importFrom rlang sym !!
#' @importFrom tidyr contains
NULL

#' Provides an age structure to relationship status, estimated from age groups
#'
#' Redistributes a user-defined relationship status value between ages, using age groups and other variables (if specified). Within the group definition provided, the marginal totals of the relationship status values are retained.
#' The data frame can include groups where all people have the same relationship status. In this situation, there is no need to restrict the data frame to only those whose relationship status must be redistributed.
#' @export
#' @param people A data frame containing individual people.
#' @param pplid The variable containing the unique identifier for each person.
#' @param pplage The variable containing the ages.
#' @param pplstat The relationship status variable in the people data frame.
#' @param stfixval The value of the relationship status, in the people data frame, that will be adjusted for age. If there are only two relationship status values, the choice does not matter. But if there are three or more values, this is the one value that will be age-corrected.
#' @param props The data frame containing the proportions of people with the stfixval value, by the grpdef.
#' @param propcol The variable in the props data frame that contains the proportions for the relationship status value of interest.
#' @param grpdef A vector containing the combination of grouping variables, in the people dataframe, that defines the marginal totals for relationship status counts. This can be one variable or a string of multiple variables. Include the age-group variable, but not the age variable.
#' @param matchdef A vector containing the same variables as grpdef, except the age variable is substituted for the age-group variable.
#' @param userseed If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.
#'
#' @return A data frame of observations, with one relationship status redistributed so that an age, rather than age group, structure is created.
#'
#' @examples
#' library("dplyr")
#' thegroups <- as.vector("Sex")
#' GroupInfo <- rbind(GroupInfo, list("Male", "Under 20 Years", 19, 19, "Partnered", 0, 19),
#'                    list("Female", "Under 20 Years", 19, 19, "Partnered", 0, 19))
#' RelProps <- interdiff(GroupInfo, pplage = "MidPoints", pplprop = "RelProps", endmin = "MinAge",
#'                       endmax = "MaxAge", grpdef = thegroups)
#' # add in the age groups
#' RelProps <- RelProps %>%
#'   mutate(AgeBand = ifelse(Age==19, "Under 20 Years",
#'                    ifelse(between(Age, 20, 29), "20-29 Years",
#'                    ifelse(between(Age, 30, 39), "30-39 Years",
#'                    ifelse(between(Age, 40, 49), "40-49 Years",
#'                    ifelse(between(Age, 50, 59), "50-59 Years",
#'                    ifelse(between(Age, 60, 69), "60-69 Years",
#'                    ifelse(between(Age, 70, 79), "70-79 Years", "80-90 Years"))))))))
#'
#' # perform separately by sex
#' thejoindef <- c("Age", "Sex")
#' thegroups <- c("Sex", "AgeBand")
#' FinalRels <- fixrelations(BadRels, pplid = "ID", pplage = "Age", pplstat = "Relationship",
#'                           stfixval = "Partnered", props = RelProps, propcol = "Fits",
#'                           grpdef = thegroups, matchdef = thejoindef, userseed = 4)


fixrelations <- function(people, pplid, pplage, pplstat, stfixval, props, propcol, grpdef, matchdef, userseed = NULL) {

  withr::local_options(dplyr.summarise.inform=F)

  # print(names(people))


  #####################################
  # check for missing input information
  #####################################


  if (!pplage %in% names(people)) {
    stop("The age variable in the people data frame does not exist.")
  }

  if (!pplstat %in% names(people)) {
    stop("The relationship variable in the people data frame does not exist.")
  }

  if (!propcol %in% names(props)) {
    stop("The proportions variable in the props data frame does not exist.")
  }

  if (!(all((grpdef) %in% names(people)))) {
    stop("All names in grpdef must exist in the people data frame.")
  }

  if (!(all((matchdef) %in% names(people))) | !(all((matchdef) %in% names(props)))) {
    stop("All names in matchdef must exist in the people and props data frames.")
  }


  # need to do the name change here

  peopleRenamed <- people %>%
    rename(StatusID = !! pplstat,
           PersonID = !! pplid)


  # get the unique set of grouping factors
  PeopleUnique <- people[,grpdef] %>%
    unique()

  # get column names as symbols to use inside data frame subfunctions

  AgeColName <- sym(names(people[pplage]))
  IDColName <- sym(names(people[pplid]))
  StatuscolName <- sym(names(people[pplstat]))
  PropscolName <- sym(names(props[propcol]))



    if (is.factor(people[[pplstat]]) == TRUE) {

    RelationshipLevels <- levels(people[[pplstat]])
    NeededRelLevel <- labels(people[[pplstat]])[match(stfixval, levels(people[[pplstat]]))]

   # cat("Relationship levels are", RelationshipLevels, ", and the extracted relationship level is ", NeededRelLevel, "\n")
   # cat("The information about the input dataframe is",  "\n")
   #
   # str(people)

  }



  # print(class(stfixval))

  if (!is.null(userseed)) {
    set.seed(userseed)
  }

  # loop through the unique rows
  for(i in 1:nrow(PeopleUnique)) {

    # cat("i is", i, "\n")

    # delete previous versions of these data frames
    if(exists("UnderSample")) {
      rm(UnderSample)
    }

    if(exists("OverSample")) {
      rm(OverSample)
      }

    # fix for one grouping variable
    # see https://stackoverflow.com/a/69116009/1030648
    CurrentDef = PeopleUnique[i, , drop=FALSE]

    suppressMessages(CurrentGroup <- left_join(CurrentDef, peopleRenamed, by = c(grpdef)))
    # cat("Group is", "\n")
    # print(CurrentDef)
    #
    # cat("Number of people in the group is", nrow(CurrentGroup), "\n")

    # get everyone in the group
    TotalGroup <- CurrentGroup

  # get the number in each status in the variable of interest
    NumInEachStatus <- CurrentGroup %>%
      group_by(.data$StatusID) %>%
      summarise(NumPerLevel = n())

    # cat("The number of people in each relationship status is ", "\n")
    # print(NumInEachStatus)


    #need to skip the bit below if there is ONLY the partnered status OR there is no partnered status
    if(nrow(NumInEachStatus) == 1 | !(stfixval %in% NumInEachStatus$StatusID)) {

      cat("Group with only one status is", "\n")
      print(CurrentDef)

      NumToFix <- 0

    } else {

   # extract the number of people in the status that needs fixing
    NumToFix <- pull(NumInEachStatus %>%
      filter(.data$StatusID == stfixval) %>%
      select("NumPerLevel"))
    }

    if(length(NumToFix) == 0 & is.integer(NumToFix)) {

      NumToFix <- 0
    }

     # cat("Number of rows in current group is", nrow(CurrentGroup), "and Num in category is", NumToFix, "\n")



    # skip over the groups when the number in the status is the same as the number in the group
    # there is no-one to swap

    if(NumToFix > 0) {

      # cat("The number of rows in the current group is ", nrow(CurrentGroup), "and the number to fix is ", NumToFix, "\n")
      #
      # cat("The num to fix is ", NumToFix, "\n")


    # get the people that are in this subgroup

    MatchingValues <- CurrentGroup %>%
      select(all_of(matchdef)) %>%
      unique()

    suppressMessages(RelevantProps <- left_join(MatchingValues, props, by = c(matchdef)) %>%
      replace(is.na(.), 0))


    # cat("The information about RelevantProps is", "\n")
    # str(RelevantProps)

    InitialCounts <- CurrentGroup %>%
      group_by(!!AgeColName) %>%
      # group_by(!!AgeColName, StatusID) %>%
      # filter(StatusID == stfixval) %>%
      summarise(TotalinStatus = n())

    # cat("The first version of InitialCounts is", "\n")
    # str(InitialCounts)

    StatOnlyCounts <- CurrentGroup %>%
      group_by(!!AgeColName, .data$StatusID) %>%
      filter(.data$StatusID == stfixval) %>%
      summarise(NuminDesStatus = n())

    # cat("StatusID is", StatOnlyCounts$StatusID, "\n")

    # cat("Initial counts construction below", "\n")

    suppressMessages(InitialCounts <- left_join(InitialCounts, StatOnlyCounts))

    # cat("After the left join, the InitialCounts is", "\n")
    # str(InitialCounts)

         # cat("Initial counts constructed", "\n")


  #  if(is.factor(expr(`$`(InitialCounts, !!StatuscolName))) == TRUE) {
    if (is.factor(people[[pplstat]]) == TRUE) {

      # cat("is.factor loop entered", "\n")
      # cat("The relationship value needed is ", stfixval, "\n")
      # cat("The RelevantProps data frame is", "\n")
      # str(RelevantProps)
      # cat("The InitialCounts data frame is", "\n")
      # str(InitialCounts)

     suppressMessages(CountComp <- left_join(RelevantProps, InitialCounts) %>%
        mutate(TotalinStatus = ifelse(is.na(.data$TotalinStatus), 0, .data$TotalinStatus),
               ExpectedCount = round(.data$TotalinStatus * !!PropscolName, 0),
               StatusID = replace(.data$StatusID, is.na(.data$StatusID), !!{{stfixval}}),
              NuminDesStatus = ifelse(is.na(.data$NuminDesStatus), 0, .data$NuminDesStatus)))


    } else {

      suppressMessages(CountComp <- left_join(RelevantProps, InitialCounts) %>%
        mutate(TotalinStatus = ifelse(is.na(.data$TotalinStatus), 0, .data$TotalinStatus),
               ExpectedCount = round(.data$TotalinStatus * !!PropscolName, 0),
               StatusID = ifelse(is.na(.data$StatusID), NeededRelLevel, .data$StatusID),
               NuminDesStatus = ifelse(is.na(.data$NuminDesStatus), 0, .data$NuminDesStatus)))

    }


       # cat("Join RelevantProps and InitialCounts complete", "\n")

    ActualCounts <- sum(CountComp$NuminDesStatus)
    CalculatedCounts <- sum(CountComp$ExpectedCount)

    if(is.na(CalculatedCounts)) {

      # cat("Group is", "\n")
      # print(CurrentDef)

      stop("Current group contains missing values", "\n")
    }
#
#     cat("Actual counts are", ActualCounts, "and the calculated counts are", CalculatedCounts, "\n")
#
#     cat("Calculated counts must be multiplied by ", ActualCounts/CalculatedCounts, "\n")

    TheMultiplier <- ActualCounts/CalculatedCounts

    suppressMessages(CountComp <- CountComp %>%
      mutate(OldExpected = round(.data$TotalinStatus * !!PropscolName, 0),
             ExpectedCount = floor(.data$TotalinStatus * !!PropscolName * TheMultiplier),
             Remainder = (.data$TotalinStatus * !!PropscolName * TheMultiplier) %% 1))

    CountsDiff <- sum(CountComp$NuminDesStatus) - sum(CountComp$ExpectedCount)

    # using this method to ensure that the counts line up
   #  https://stackoverflow.com/a/3956184/1030648

    CountComp <- CountComp %>%
      arrange(desc(.data$Remainder))


    # add 1 to the counts for n == CountsDiff
    # note that this could, theoretically, create an expected count > available people that age
    # need to test

    for(l in 1:nrow(CountComp)) {

      if(CountsDiff > 0) {

      if(CountComp$TotalinStatus[l] >= CountComp$ExpectedCount[l]+ 1) {

        CountComp$ExpectedCount[l] <- CountComp$ExpectedCount[l] + 1
      }

        CountsDiff <- CountsDiff - 1

        # closes if(CountsDiff > 0)
      }

      # closes for(l in 1:nrow(CountComp))
    }



    # TODO need to create a fix if there are not enough values to increase by 1?
    # not sure that this can occur

    # problem is there are no people in the group, so skip over these

    if(ActualCounts > 0) {

    ###########################

    CountComp <- CountComp %>%
      mutate(DiffsNeeded = .data$ExpectedCount - .data$NuminDesStatus)

    ##########################################################################
    # ONLY HERE TO PRODUCE TABLE FOR DISSERTATION
    # LEAVE COMMENTED OUT
    # return(CountComp)
    ##########################################################################



    UndersCount <- CountComp %>%
      filter(.data$DiffsNeeded < 0) %>%
      summarise(Rows = n()) %>%
      pull(.data$Rows)

    OversCount <- CountComp %>%
      filter(.data$DiffsNeeded > 0) %>%
      summarise(Rows = n()) %>%
      pull(.data$Rows)



    # these are the number of ages for each type of mis-count
    # cat("Unders is", UndersCount, "and overs is", OversCount, "\n")
    #

    CheckNoZeros <- sum(abs(CountComp$DiffsNeeded))


    # skip process if there are no differences between desired and actual counts
    # in all ages
    # OR there are no records to sub for the problem ones

    if(CheckNoZeros > 0 & !(UndersCount == 0) & !(OversCount == 0)) {

    CountUnders <- CountComp %>%
      filter(.data$DiffsNeeded < 0)

    # cat("CountComp is", "\n")
    # str(CountComp)

    # get the people to swap

    for(j in 1:nrow(CountUnders)) {

      CurrentTooMany <- CountUnders[j,]



      # cat("Current too many is", CurrentTooMany$DiffsNeeded, "\n")
      #
      # str("CurrentGroup is", "\n")
      # str(CurrentGroup)

      suppressMessages(CurrentSample <- left_join(CurrentTooMany, CurrentGroup, by = c(matchdef)) %>%
                         filter(.data$StatusID.y == !!{{stfixval}})  %>%
                         rename(StatusID = "StatusID.x") %>%
                         select(-"StatusID.y") #%>%
                       #  slice_sample(n = abs(CurrentTooMany$DiffsNeeded), replace = FALSE)
      )

      if(nrow(CurrentSample) > 0) {

        # cat("The number of CurrentSample rows is", nrow(CurrentSample), "\n")
        # cat("The number of people needed for undercounts is", abs(CurrentTooMany$DiffsNeeded), "\n")

        if(nrow(CurrentSample) < abs(CurrentTooMany$DiffsNeeded)) {
          CurrentSample <- CurrentSample

        } else {

        CurrentSample <- CurrentSample %>%
          slice_sample(n = abs(CurrentTooMany$DiffsNeeded), replace = FALSE)

        # closes else for if(nrow(CurrentSample) < abs(CurrentTooMany$DiffsNeeded)) {
        }

      }


      if(exists("UnderSample")) {
        UnderSample <- bind_rows(UnderSample, CurrentSample)

      } else {
        UnderSample <- CurrentSample
      }

      # delete the already sampled people
      CurrentGroup <- CurrentGroup %>%
        filter(!(.data$PersonID %in% c(UnderSample$PersonID)))


    # closes  for(j in 1:nrow(CountUnders))
    }

    # cat("After undersample, current group size is", nrow(CurrentGroup), "and undersample is", nrow(UnderSample), "\n")



    CountOvers <- CountComp %>%
      filter(.data$DiffsNeeded  > 0)


    for(k in 1:nrow(CountOvers)) {

      CurrentTooFew <- CountOvers[k,]

      # cat("Current too few is", CurrentTooFew$DiffsNeeded, "\n")

      suppressMessages(CurrentSample <- left_join(CurrentTooFew, CurrentGroup, by = c(matchdef)) %>%
        filter(! .data$StatusID.y == !!{{stfixval}}) %>%
        mutate(StatusID = .data$StatusID.y) %>%
        select(-c("StatusID.x", "StatusID.y")) # %>%
        # slice_sample(n = abs(CurrentTooFew$DiffsNeeded))
        )


      # only sample from dataframes that have observations
      if(nrow(CurrentSample) > 0) {

        # cat("The number of people needed for overcounts is", abs(CurrentTooMany$DiffsNeeded), "\n")

        # sample if the number needed is less than the number of observations
        if(abs(CurrentTooFew$DiffsNeeded) < nrow(CurrentSample)) {

          CurrentSample <- CurrentSample %>%
            slice_sample(n = abs(CurrentTooFew$DiffsNeeded), replace = FALSE)

        } else {

          # if the number needed equals the number of observations in the data frame or is more
          CurrentSample <- CurrentSample

          # closes if(abs(CurrentTooFew$DiffsNeeded) < nrow(CurrentSample)) {
        }

        # closes if(nrow(CurrentSample) > 0) {
      }

      # cat("Before oversample, current group size is", nrow(CurrentGroup), "\n")


      if(exists("OverSample")) {
        OverSample <- bind_rows(OverSample, CurrentSample)


      } else {
        OverSample <- CurrentSample

      }

      # delete the already sampled people
      CurrentGroup <- CurrentGroup %>%
        filter(!(.data$PersonID %in% c(OverSample$PersonID)))

      # cat("Oversample is ", nrow(OverSample), "\n")


      # closes  for(k in 1:nrow(CountOvers))
    }

    # cat("After oversample, current group size is", nrow(CurrentGroup), "and oversample is", nrow(OverSample), "\n")

    ##################################################
    # unders and overs may not be the same,
    # force them to be the same length - make the larger one the length of the smaller one if so
    ##################################################


    if(nrow(UnderSample) > nrow(OverSample)) {

      UnderSample <- UnderSample %>%
        slice_sample(n = nrow(OverSample), replace = FALSE)
    }

    if(nrow(OverSample) > nrow(UnderSample)) {

      OverSample <- OverSample %>%
        slice_sample(n = nrow(UnderSample), replace = FALSE)
    }

    # cat("Final undersample size is", nrow(UnderSample), "and final oversample size is", nrow(OverSample), "\n")

    # now swapping the ages
    # only need to link age to ID, will wash out in the original data
    # should just be a straight join and then swap
    # remove any ordering

    UnderSample <- UnderSample %>%
      slice_sample(n = nrow(UnderSample), replace = FALSE)


    # cat("Undercount is", (nrow(UnderSample)), "\n")

    OverSample <- OverSample %>%
      slice_sample(n = nrow(OverSample), replace = FALSE)

    # cat("Overcount is", nrow(OverSample), "\n")

    # literally swap the ages in UnderDF and OverDF by row

    UnderSampleAges <- UnderSample %>%
      select(!!AgeColName)

    OverSampleFixed <- OverSample %>%
      select(-!!AgeColName) %>%
      bind_cols(UnderSampleAges)

    OverSampleAges <- OverSample %>%
      select(!!AgeColName)

    UnderSampleFixed <- UnderSample %>%
      select(-!!AgeColName) %>%
      bind_cols(OverSampleAges)


    Fixed <- bind_rows(OverSampleFixed, UnderSampleFixed) %>%
      select(-(c("NuminDesStatus", "ExpectedCount", "DiffsNeeded"))) %>%
      rename_all(list(~gsub("\\.y$", "", .)))


  # add them to the others that haven't been amended

    UnAmended <- CurrentGroup %>%
      filter(!(.data$PersonID %in% c(Fixed$PersonID)))


    # cat("The number fixed is", nrow(Fixed), "and the number unamended is", nrow(UnAmended), "\n")

    # this needs to be a file that takes all the groups

    if(exists("GroupFixed")) {

      GroupFixed <- bind_rows(GroupFixed, UnAmended, Fixed)


    } else {

      GroupFixed <- bind_rows(UnAmended, Fixed)


    # closes  if(exists("GroupFixed"))
    }

    # closes if(CheckNoZeros > 0 & !(UndersCount == 0) & !(OversCount == 0))
    } else {

      if(exists("GroupFixed")) {

        GroupFixed <- bind_rows(GroupFixed, CurrentGroup)

      } else {

        GroupFixed <- CurrentGroup


        # closes  if(exists("GroupFixed"))
      }

      # closes else to  if(CheckNoZeros > 0 & !(UndersCount == 0) & !(OversCount == 0))
    }

    # closes if(ActualCounts > 0)
    }

    # closes if(nrow(CurrentGroup) > NumToFix)

    } else {

      if(exists("GroupFixed")) {

        GroupFixed <- bind_rows(GroupFixed, CurrentGroup)


      } else {

        GroupFixed <- CurrentGroup

       # closes  if(exists("GroupFixed"))
      }

      # closes if(nrow(CurrentGroup) > NumToFix)
    }

    # cat("Size of output group is", nrow(GroupFixed), "\n")

        # closes for(i in 1:nrow(PeopleUnique))
  }

  # people are omitted if the overcount sample and the undercount sample don't have the same number of people
  # and so the larger sample must, itself, be sampled
  # add all these people back in

  MissingPeople <- peopleRenamed %>%
    filter((!(.data$PersonID %in% c(GroupFixed$PersonID))))


  FinalDF <- bind_rows(GroupFixed, MissingPeople) %>%
    arrange(.data$PersonID)


  # fix the col names and remove the extra rows

  # cat("Now outputting the data frame", "\n")

  cat("The relationship variable name is", sym(names(people[pplstat])), "\n" )

  # NOTE: outputs correct relationship variable name in the input data frame

    if (is.factor(people[[pplstat]]) == TRUE) {

    cat("Factor loop entered", "\n")


    OutputDataFrame <- FinalDF %>%
      select(-c("TotalinStatus", {{PropscolName}}, "OldExpected", "Remainder",
                contains(c(".y", ".x")))) %>%
      rename(!!StatuscolName := "StatusID",
             !!IDColName := "PersonID")


  for(m in 1:length(grpdef)) {

    cat("The vector index is", m, "and the string is", grpdef[m], "\n")

    print(is.factor(people[,grpdef[m]]))

    if(is.factor(people[,grpdef[m]]) == TRUE) {

      cat("This is a factor", "\n")

      FactorName <- grpdef[m]
      cat("The factor name is", FactorName, "\n")

      LevelsToUse <- levels(people[,grpdef[m]])

      cat("The levels are", "\n")

      print(LevelsToUse)

      OutputDataFrame[,grpdef[m]] <- factor(OutputDataFrame[,grpdef[m]], levels = c(levels(people[,grpdef[m]])))

    }

    # close for(m in 1:length(grpdef)) {
  }

    #close factor test
    } else {

  OutputDataFrame <- FinalDF %>%
    select(-c("TotalinStatus", {{PropscolName}}, "OldExpected", "Remainder",
              contains(c(".y", ".x")))) %>%
    rename(!!StatuscolName := "StatusID",
           !!IDColName := "PersonID")


  # closes else to factor test
   }

  return(OutputDataFrame)

  #closes function
}

#' Create a data frame of children matched to schools
#' This function creates a data frame of children, with a school allocated to them, based on the characteristics of the child ages in a household (e.g. the presence of twins) and the proportions of children of each age within each school.
#' Two data frames are required. The Children data frame contains the children data, to which the Schools data will be applied.
#' No minimum or maximum child ages are required, as the function limits the ages within the age range across the schools Thus, pre-cleaning the Children data frame is not required.
#' The Schools data frame must be a summary in the form of counts by age within school. Each row is one age only. For example, if a school has children aged 5 to 9 years, there should be 5 rows. #' Co-educational, single-sex, or both types of schools can be included in the Schools data frame. If at least one same-sex school is included, similarly aged children of the opposite sex will be assigned to the equivalent school for the opposite sex, if present, otherwise they will be assigned to a co-educational school. Because of the semi-summary nature of the Schools data frame, this information must be repeated for each row.
#' Children within a family will be assigned to the same school, where the age and/or sex structure requires this. It is possible that some children may be matched to a school with no available slots for that age, resulting in an overcount of children at that age in the school. While the code attempts to prevent this, this outcome may still occur. In this situation, a message will be printed to the console, identifying the problem household. Testing has shown that use of a different set.seed() may remove the problem. Alternatively, a manual fix can be performed after using this function.

#' The function performs a reasonableness check for child ID, child age, and counts for the number of children of each age versus the number of available age slots across all schools..
#'
#' @export
#' @param Children A data frame containing observations limited to the children to be matched An age column is required. All children in this data frame will be matched to a parent/guardian.
#' @param ChildIDVariable The column number for the ID variable in the Children data frame.
#' @param ChildAgeVariable The column number for the Age variable in the Children data frame.
#' @param ChildSexVariable The column number for the sex indicator for children. This column is used to assign children to the appropriate school type (co-educational or single-sex). The expected values are "F" (female) or "M" (male).
#' @param HouseholdIDVariable The column number for the household variable in the Children data frame. This must be provided.
#' @param Schools A data frame containing the school observations.
#' @param SchoolNameVariable The column number for the variable in the Schools data frame that contains the name of each school.
#' @param SchoolAgeVariable The column number for the Age variable in the Schools data frame. Each student age within the school must be a separate row.
#' @param SchoolRollCount The number of places available for children at that school age, within the school.
#' @param SchoolCoEdStatus An indicator variable used to determine whether the school is co-educational or single-sex. The expected values are "C" (co-educational), "F" (female only), and "M" (male-only).

#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.

AddSchools <- function(Children, ChildIDVariable, ChildAgeVariable, ChildSexVariable, HouseholdIDVariable = NULL,
                       Schools, SchoolIDVariable, SchoolAgeVariable, SchoolRollCount, SchoolCoEdStatus, UserSeed=NULL)
  {

  options(dplyr.summarise.inform=F)


  #####################################################################
  #####################################################################
  # Test for any problem ages, stop function if this situation exists
  #####################################################################
  #####################################################################

  ChildrenRenamed <- Children %>%
    rename(ChildID = !! ChildIDVariable, ChildAge = !! ChildAgeVariable, ChildSex = !! ChildSexVariable,
           HouseholdID = !! HouseholdIDVariable)


  SchoolsRenamed <- Schools %>%
    rename(SchoolID = !! SchoolIDVariable, SchoolAge = !! SchoolAgeVariable,
           ChildCounts = !! SchoolRollCount, SchoolType = !! SchoolCoEdStatus) %>%
    mutate_if(is.factor, as.character)

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

  SchoolsRenamed <- left_join(AgeRestriction, SchoolsRenamed, by = c("ChildAge" = "SchoolAge"))


  SchoolsCountColIndex <- as.numeric(which(colnames(SchoolsRenamed) == "ChildCounts"))

  # #####################################################################
  # #####################################################################
  # # Single-value injection function for changing school counts
  # # see https://stackoverflow.com/a/7972038/1030648
  # #####################################################################
  # #####################################################################
  # replace.df <- function(x,y,by,cols=NULL) {
  #   nx <- nrow(x)
  #   ny <- nrow(y)
  #
  #   bx <- x[,by,drop=FALSE]
  #   by <- y[,by,drop=FALSE]
  #   bz <- do.call("paste", c(rbind(bx, by), sep = "\r"))
  #
  #   bx <- bz[seq_len(nx)]
  #   by <- bz[nx + seq_len(ny)]
  #
  #   idx <- match(by,bx)
  #   idy <- match(bx,by)
  #   idy <- idy[!is.na(idy)]
  #
  #   if(is.null(cols)) {
  #     cols <- intersect(names(x),names(y))
  #     cols <- cols[!cols %in% by]
  #   }
  #
  #   x[idx,cols] <- y[idy,cols]
  #   x
  # }


  #####################################################################
  #####################################################################
  # Create a separate vector of age counts for each school
  #####################################################################
  #####################################################################

  # convert the schools file from long to wide ahead of vector construction

  # SchoolsLong <- reshape(SchoolsRenamed, idvar = c("SchoolID", "SchoolType"), v.names = ("ChildAge"), ids = ("ChildCounts"), direction = "wide")
#
#   SchoolsWide <- SchoolsRenamed %>%
#     select(SchoolID, ChildAge, ChildCounts, SchoolType) %>%
#    tidyr::pivot_wider(names_from = ChildAge, values_from = ChildCounts)

  # get the number of households
  NumberHouseholds <- as.numeric(ChildrenRenamed %>%
    dplyr::summarise(Count = n_distinct(HouseholdID)) %>%
    pull(Count))

  # get list of household IDs
  HouseholdIDList <- as.data.frame(ChildrenRenamed %>%
    distinct(HouseholdID))

  # testing on household 1914 as this contains primary and secondary school children.

  WorkingChildren <- ChildrenRenamed %>%
    filter(HouseholdID == HouseholdIDList[30,1])

  # get child age counts for each age in DF, if twins then count == 2 rather than 1

  WorkingChildrenAgeCounts <- WorkingChildren %>%
    select(ChildAge) %>%
    group_by(ChildAge) %>%
    summarise(AgeCount = n())

  #  for (y in 1:nrow(WorkingChildren)) {
  while (!(is.na(WorkingChildren$ChildID[1])) == TRUE) {

      SchoolMatches <- left_join(WorkingChildren, SchoolsRenamed, by = "ChildAge") %>%
        filter(ChildCounts != 0)

      # identify schools that exist multiple times from the join
      # this is the selection for the most number of children in the household to one school

      NumberTimesSchoolSelected <- SchoolMatches %>%
        select(SchoolID, ChildAge) %>%
        group_by(SchoolID) %>%
        summarise(TimesSelected = n())

      # create subset of schools limited to those of the maximum number, and loop
#     for (z in 1:nrow(NumberTimesSchoolSelected)) {

      # extract out first set of schools to match
        MaxSchoolDuplicates <- max(NumberTimesSchoolSelected$TimesSelected)

        FirstSetSchools <- NumberTimesSchoolSelected %>%
          filter(TimesSelected == MaxSchoolDuplicates) %>%
          select(-TimesSelected)

        # create constrained data frame based off SchoolMatches
        RestrictedSchoolMatches <- left_join(FirstSetSchools, SchoolMatches, by = "SchoolID")

        FinalSchoolSelected <- RestrictedSchoolMatches %>%
          slice_max(ChildCounts, n = 1, with_ties = FALSE) %>%
          select(SchoolID)

        FinalSchoolMerged <- left_join(FinalSchoolSelected, SchoolMatches, by = "SchoolID") %>%
          select(ChildID, SchoolID)

        # test code below with pretend multiple values for same age at same school
        # SchoolCountDecreases <- data.frame("SchoolID" = c("Bluestone School", "Bluestone School", "Bluestone School", "New School 1", "New School 2"),
        #                                    "ChildAge" = c(5, 5, 8, 5, 6),
        #                                    "ChildCounts" = c(74, 74, 64, 93, 85), stringsAsFactors = FALSE)
        #
        # SchoolCountsUsed <- SchoolCountDecreases %>%
        #   group_by(SchoolID, ChildAge) %>%
        #   mutate(FinalCounts = ChildCounts - n()) %>%
        #   ungroup() %>%
        #   distinct() %>%
        #   select(-ChildCounts) %>%
        #   rename(ChildCounts = FinalCounts)

        SchoolCountDecreases <- left_join(FinalSchoolSelected, SchoolMatches, by = "SchoolID") %>%
          group_by(SchoolID, ChildAge) %>%
            mutate(FinalCounts = ChildCounts - n()) %>%
            ungroup() %>%
            distinct() %>%
            select(-ChildCounts) %>%
            rename(ChildCounts = FinalCounts)

        cat("Before the loop the column value is", SchoolsChildAgeColIndex, "\n")

        for (a in 1:nrow(SchoolCountDecreases)) {

          SchoolRowIndex <- as.numeric(which((SchoolsRenamed$SchoolID==SchoolCountDecreases$SchoolID[a]) &
                                    (SchoolsRenamed$ChildAge==SchoolCountDecreases$ChildAge[a])))

            SchoolsRenamed[SchoolRowIndex, SchoolsCountColIndex] <- SchoolCountDecreases$ChildCounts[a]

        }




        # create child data frame with school joined

        CurrentMatchedChildren <- left_join(FinalSchoolMerged, WorkingChildren, by = "ChildID")

        if (exists("FinalMatchedChildren")) {

          FinalMatchedChildren <- bind_rows(FinalMatchedChildren, CurrentMatchedChildren)

        } else {

          FinalMatchedChildren <- CurrentMatchedChildren

          # closes if statement
        }

        WorkingChildren <- WorkingChildren %>%
          filter(!(ChildID %in%  FinalMatchedChildren$ChildID))

        # closes for z statement
       # }

      #closes for y statement
  }

  # # split out the households
     # for (x in 1:NumberHouseholds) {
       #
       #   WorkingChildren <- ChildrenRenamed %>%
       #     filter(HouseholdID == HouseholdIDList[x,1])

       # get child age counts for each age in DF, if twins then count == 2 rather than 1

       # WorkingChildrenAgeCounts <- WorkingChildren %>%
       #   select(ChildAge) %>%
       #   group_by(ChildAge) %>%
       #   summarise(AgeCount = n())
       #
       #   if (nrow(WorkingChildren > 1)) {
       #
       #   # match in relevant schools
       #   for (y in 1:nrow(WorkingChildren)) {
       #
       #     SchoolMatches <- left_join(WorkingChildren, SchoolsRenamed, by = "ChildAge")
       #
       #     #closes for y statement
       #   }
       #
       #     # if nrow(Children) else statements below
       #   } else { # this is for households with only one child
       #
       #     #closes else statement
       #   }
       #
       #
       #   # closes for x statement
       # }



     return(SchoolsRenamed)

    }

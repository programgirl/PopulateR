#'
#'




AddChildLnLoop <- function(Children, ChildIDVariable, ChildAgeVariable, Parents, ParentIDVariable, ParentAgeVariable,
  meanlogUsed, sdlogUsed,  MinParentAge = NULL, MaxParentAge = NULL, MinPropRemain = 0, DyadIDValue = NULL,
  HouseholdNumVariable= NULL, UserSeed=NULL, pValueToStop = .01, NumIterations = 1000000)

{

  options(dplyr.summarise.inform=F)

  # content check
  if (!any(duplicated(Children[ChildIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the child data frame must be supplied.")
  }

  if (!is.numeric(ChildAgeVariable)) {
    stop("Both the child ID and the child age column numbers must be supplied.")
  }

  if (!any(duplicated(Parents[ParentIDVariable])) == FALSE) {
    stop("The column number for the ID variable in the parent data frame must be supplied.")
  }

  if (is.null(HouseholdNumVariable)) {
    stop("A name for the household count variable must be supplied.")
  }

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # Child ID variable
  ChildIDColName <- sym(names(Children[ChildIDVariable]))

  # Child age variable
  ChildAgeColName <- sym(names(Children[ChildAgeVariable]))

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

    Parents <- Parents %>%
      filter(({{ParentAgeColName}} - minChildAge) >= MinParentAge)
  }

  if (!(is.null(MaxParentAge))) {

    Parents <- Parents %>%
      filter(({{ParentAgeColName}} - maxChildAge) <= MaxParentAge)
  }

  # get counts for each single age from the parent data frame
  # ensure that any requirements to not use a particular number of counts per age is incorporated
  # ensure all parent ages are represented in the data frame of counts
  # use the minimum and maximum values to create an age sequence from MinParentAge to MaxParentAge

  ParentCounts <- Parents %>%
    group_by_at(ParentAgeVariable) %>%
    summarise(AgeCount=n()) %>%
    mutate(AgeCount = floor(AgeCount*(1-MinPropRemain))) %>%
    #ungroup() #%>%
    complete({{ParentAgeColName}}:=seq(min({{ParentAgeColName}}), max({{ParentAgeColName}})),
             fill = list(AgeCount = 0))

  minIndexAge <- as.integer(ParentCounts[1,1])
  maxIndexAge <- as.integer(ParentCounts[nrow(ParentCounts),1])



  ParentAgeCountVector <- ParentCounts$AgeCount




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

#  cat("minimum age is ", MinParentAge, " and maximum age is ", MaxParentAge)

  for (j in 1:nrow(Children)) {

 #      if (!(is.null(MinParentAge)) & !(is.null(MaxParentAge))) {
         AgeDifference <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))

     #if (isTRUE(AgeDifference >= MinParentAge & AgeDifference <=  MaxParentAge)) {
        MatchedAge <- Children[[ChildAgeVariable]][j] + AgeDifference
       # age_index <- MatchedAge-(minIndexAge -1)

        # if (isTRUE(ParentAgeCountVector[age_index] > 0)) {
        #   Children$AgeDifference[j] <- AgeDifference
        #   Children$MatchedAge[j] <- MatchedAge
        #   Children$AgeIndex[j] <- age_index
        #   ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1
        #
        #
        #  } else {
        #
        #    Children$MatchedAge[j] <- NA
         #   Children$AgeIndex[j] <- NA
#
#
#            # will be else if (minparentage but not max) {}
#
#            # } else if { (max but not min) {
#
#            #} else both are null
#
#            Children$MatchedAge[j] <- NA
#
      #    }
      #
      # }

     # }

  }


  #   if (Children$AgeDifference[j] >= minIndexAge & Children$MatchedAge[j] <=  maxIndexAge) {
  #
  #
  #
  #
  #
  #
  #
  #
  #        }
  #
  #
  #   } else {
  #
  #     Children$MatchedAge[j] <- NA
  #   }
  #
  # }

  # previous code that doesn't limit the age differences for matching to those within the paramters
  # for (j in 1:nrow(Children)) {
  #   Children$AgeDifference[j] <- round(rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed))
  #   Children$MatchedAge[j] <- Children[[ChildAgeVariable]][j] + Children$AgeDifference[j]
  #   if (Children$AgeDifference[j] >= minIndexAge & Children$MatchedAge[j] <=  maxIndexAge) {
  #      age_index <- Children$MatchedAge[j]-(minIndexAge-1)
  #      if (ParentAgeCountVector[age_index]==0) {
  #        Children$MatchedAge[j] <- NA
  #
  #        } else {
  #
  #          ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1
  #
  #        }
  #
  #
  #   } else {
  #
  #     Children$MatchedAge[j] <- NA
  #   }
  #
  # }

# create second attempt to match only those who didn't match the first time
# use same distribution

  # ChildrenMatched <- Children %>%
  #   filter(!(is.na(Children$MatchedAge))) #%>%
 # # select(-c(AgeDifference, MatchedAge))

  # Children <- Children %>%
  #   filter(is.na(Children$MatchedAge)) %>%
  #   select(-c(AgeDifference, MatchedAge))


  # redo code with reduced dataframe

#   for (j in 1:nrow(Children)) {
#     Children$AgeDifference[j] <- rlnorm(1, meanlog=meanlogUsed, sdlog=sdlogUsed)
#     Children$MatchedAge[j] <- round(Children[[ChildAgeVariable]][j] + Children$AgeDifference[j])
#     if (Children$MatchedAge[j] >= minIndexAge & Children$MatchedAge[j] <=  maxIndexAge) {
#       age_index <- Children$MatchedAge[j]-(minIndexAge-1)
#       if (ParentAgeCountVector[age_index]==0) {
#         Children$MatchedAge[j] <- NA
#
#       } else {
#
#   #     ParentAgeCountVector[age_index] = ParentAgeCountVector[age_index] - 1
#   #
#       }
#   #
#   #
#     } else {
#
#       Children$MatchedAge[j] <- NA
#     }
#
#   }

  return(Children)


}



# # the mean is set to 2 years difference, with men generally being older than women
# # the method is:
# # 1. get the counts of women by single age
# # 2. construct two lists of these so that one is the age and the other is the corresponding count
# # 3. generate the skewed normal rng for the man, and the matching age is his age plus the rng number (rounded)
# # 4. as the minimum age is 18, the matching age index is matching age - 17
# # 5. check if there is still a non-zero count in the age list
# # 6a. if so, keep the age as the matched age and decrease the count by 1 in the count list
# # 6b. if the matching age has a count of 0 in the table, set the matched age to NA
# # 7. the non-matched ages will be the May-December matches - check this (make sure there aren't too many non-matches
#
# # assign the matched ages to the males, the females will be randomly matched to these
# # interactive skew plot at http://azzalini.stat.unipd.it/SN/plot-SN1.html
# set.seed(101013)
# for (j in 1:nrow(Partnered2PHHDiffSexMales)) {
#   Partnered2PHHDiffSexMales$AgeDifference[j] <- rsn(1,xi=0, omega=3, alpha=4)
#   Partnered2PHHDiffSexMales$MatchedAge[j] <- round(Partnered2PHHDiffSexMales$AssignedAge[j] - Partnered2PHHDiffSexMales$AgeDifference[j])
#   if (Partnered2PHHDiffSexMales$MatchedAge[j] < 18) Partnered2PHHDiffSexMales$MatchedAge[j] = 18
#   if (Partnered2PHHDiffSexMales$MatchedAge[j] > 90) Partnered2PHHDiffSexMales$MatchedAge[j] = 90
#   age_index <- Partnered2PHHDiffSexMales$MatchedAge[j]-17
#   if (Partnered2PHHCounts[age_index]==0) {
#     Partnered2PHHDiffSexMales$MatchedAge[j] = 0
#   } else {
#     Partnered2PHHCounts[age_index] = Partnered2PHHCounts[age_index] - 1
#
#   }
# }
#


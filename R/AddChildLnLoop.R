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
  ParentCounts <- Parents %>%
    group_by_at(ParentAgeVariable) %>%
    summarise(AgeCount=n()) %>%
    mutate(AgeCount = floor(AgeCount*(1-MinPropRemain)))

  # ensure all parent ages are represented in the data frame of counts
  # use the minimum and maximum values to create an age sequence from MinParentAge to MaxParentAge
  ParentAgeSeq <- seq(min(ParentCounts[,1]),  max(ParentCounts[,1]), by =1)

  #left join on sequence, forcing any NULL counts to be zero

  FinalParentCounts <- ParentAgeSeq %>%
    left_join(ParentCounts, by = c("ParentAgeSeq" = "{{ParentAgeVariable}}")) %>%
    mutate_if(is.numeric,coalesce,0)



  return(ParentCounts)
# ensure that all ages from parent minium age to parent maximum age are i t

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

  # 1. get the counts of parents by single age (completed above)
  # 2. construct two lists of these so that one is the age and the other is the corresponding count
  # 3. generate the random age match for the child: this is child's age plus the rng number (floor)
  # 4.  as the minimum age is 18, the matching age index is matching age - 17
  # 5. check if there is still a non-zero count in the age list
  # 6a. if so, keep the age as the matched age and decrease the count by 1 in the count list
  # 6b. if the matching age has a count of 0 in the table, set the matched age to NA
  # 7. the non-matched ages will be the May-December matches - check this (make sure there aren't too many non-matches)



}



#
#
# # match remaining men and women marked as "matched" - there will be some non-matches as more women are recorded as partnered
# # use a skewed normal distribution
# # a normal distribution won't retain high enough peaks at standard age differences between partners
# # will use the sn package to generate a skewed normal distribution - this retains the high peaks at the mean values
# # the mean is set to 2 years difference, with men generally being older than women
# # the method is:
# # 1. get the counts of women by single age
# # 2. construct two lists of these so that one is the age and the other is the corresponding count
# # 3. generate the skewed normal rng for the man, and the matching age is his age plus the rng number (rounded)
# # 4. as the minimum age is 18, the matching age index is matching age - 17
# # 5. check if there is still a non-zero count in the age list
# # 6a. if so, keep the age as the matched age and decrease the count by 1 in the count list
# # 6b. if the matching age has a count of 0 in the table, set the matched age to NA
# # 7. the non-matched ages will be the May-December matches - check this (make sure there aren't too many non-matches)
#
# # get counts for each single age for the women, for women not in same-sex households
# Partnered2PHHDiffSexFemCounts <- Partnered2PHH %>%
#   filter(RELATIONSHIP=="Partnered", SEX=="Female", !(ID %in% Partnered2PHHSameSexWomen$ID)) %>%
#   group_by(AssignedAge) %>%
#   summarise(AgeCount=n()) #%>%
# #spread(AssignedAge,AgeCount)
#
# Partnered2PHHCounts <- Partnered2PHHDiffSexFemCounts$AgeCount
#
# # remove same-sex households from the 2-person household partnered data
# Partnered2PHHDiffSexMales <- Partnered2PHH %>%
#   filter(RELATIONSHIP=="Partnered", SEX=="Male", !(ID %in% Partnered2PHHSameSexMen$ID)) %>%
#   mutate(
#     AgeDifference=0
#   )
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
# # check matched ages actually matched
# table(Partnered2PHHDiffSexMales$MatchedAge)
# # 329/nrow(Partnered2PHHDiffSexMales)
# # 9% of males not matched when rsn(1,xi=0, omega=3, alpha=4)
#
# Partnered2PHHCounts
# # some large pockets of women not matched
# # plot the matched ages of men and women
# Partnered2PHHMatched1 <- Partnered2PHHDiffSexMales %>%
#   filter(MatchedAge > 0) %>%
#   select(ID, AssignedAge, MatchedAge) %>%
#   gather(key="Age.Type", value="Age", -ID)
#
# ggplot(Partnered2PHHMatched1, aes(x=Age, fill=Age.Type)) +
#   geom_density(col=NA, alpha=0.35) +
#   scale_fill_manual( values = c("blue","pink")) +
#   labs(title="Age distribution of matched partnered adults in 2-person households\nrsn(1,xi=0, omega=3, alpha=4)",
#        x ="Age (years)")
#
# # plot the ages of men unmatched and the ages of women not matched
# Partnered2PHHUnmatched <- Partnered2PHHDiffSexMales %>%
#   filter(MatchedAge==0) %>%
#   select(ID, SEX, AssignedAge)
#
# IncorrectlyUnpartnered2PHHWomen <- data.frame(AssignedAge=c(Partnered2PHHDiffSexFemCounts$AssignedAge), SEX="Female", Count=Partnered2PHHCounts)
# IncorrectlyUnpartnered2PHHWomen <- IncorrectlyUnpartnered2PHHWomen %>%
#   group_by(AssignedAge, SEX) %>%
#   ungroup(AssignedAge, SEX)
#
#
#
#
# ################################################################################################
# # look at children and unpartnered women's ages
# Partnered2PHHChildren <- Master_household_file_Timaru_2013_census_data %>%
#   filter(AGEBAND %in% c(1:4) | AGEBAND==5 & HRSWORKED=="Not Working", INHABITANTS==2)

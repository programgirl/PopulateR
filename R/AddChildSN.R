# # ChildDataframe
# # ChildAgeVariable = column number of child age
# # xiUsed =
# # OmegaUsed =
# # AlphaUsed =
# # MotherDataframe
# # MotherIDVariable = column number of mother ID
# # MotherAgeVariable = column number of mother age
# # PropMothers = the proportion of children who are assigned to mothers, the remainder will be assigned to fathers
# # MinPropRemain = the minimum proportion of people, at each age, who are not parents
# # UserSeed=NULL
#
# GetMotherAges <- function(ChildDataframe, ChildAgeVariable, xiUsed=NULL, OmegaUsed=NULL,
#                           AlphaUsed=NULL, MotherDataframe, MotherIDVariable, MotherAgeVariable,
#                           PropMothers, MinPropRemain,UserSeed=NULL){
#
#   options(dplyr.summarise.inform=F)
#
#   SummaryData <- as.data.frame(ChildDataframe[,ChildAgeVariable])
#
#   ChildCount <- nrow(ChildDataframe)
#
#   # construct dataframe of  mother age probabilities
#   if (!is.null(UserSeed)) {
#     set.seed(UserSeed)
#   }
#
#   for(i in 1:nrow(SummaryData)) {
#     SummaryData$MotherAge[i] <-  round(rsn(1,xiUsed, OmegaUsed,AlphaUsed),0)
#   }
#
#   # SummaryData <- SummaryData %>%
#   #   tidyr::unnest(MotherAge) %>%
#   #   mutate(Diff = SummaryData$MotherAge - SummaryData[,1]) %>%
#   #   sample_n(ChildCount*PropMothers) %>%
#   #   #group_by(MotherAge) %>%
#   #   #summarise(CountMotherAge = n())
#   #   group_by(Diff) %>%
#   #   summarise(DiffCount = n())
#
#
#   return(SummaryData)
#
#   # test whether counts are available in mother dataframe
#
#    MotherSample <- MotherDataframe %>%
#     group_by(.[[MotherAgeVariable]]) %>%
#     summarise(MotherAgeCount = n())
#
#    # NumbersCheck <- left_join(SummaryData, rename_at(MotherSample, 1, ~ names(SummaryData)[1]), by = "Diff") %>%
#    # mutate(Available = MotherAgeCount - AgeDiffCount) %>%
#    #   filter((Available/(MotherAgeCount) < MinPropRemain))
#
#    #NumbersCheck <- left_join(SummaryData, rename_at(MotherSample, 1, ~ names(SummaryData)[1]), by = "MotherAge") %>%
#
#
#  #  correct the problem mother ages so that the MinPropRemain requirement is met
#
#   FinalMotherNums <- left_join(SummaryData, NumbersCheck, by="Diff") %>%
#     mutate(MatNumsUsed = ifelse(!(is.na(Available)), floor(MotherAgeCount * MinPropRemain),
#                                 AgeDiffCount.x),
#            MatAge = Diff) %>%
#     select(MatAge, MatNumsUsed) %>%
#     arrange(MatAge)
#
#  # ensure that the correct number of children are allocated to mothers, compared to fathers
#
#   MotherCountReq <- round(PropMothers * ChildCount,0)
#
#   if(sum(FinalMotherNums$MatNumsUsed) != MotherCountReq)
#      Difference <- sum(FinalMotherNums$MatNumsUsed) - MotherCountReq
#
#
#   if(Difference < 0) {
#
#     ExtraMatNums <-  MotherDataframe %>%
#       filter(between(.[[MotherAgeVariable]],  min(FinalMotherNums$MatAge),
#               max(FinalMotherNums$MatAge))) %>%
#       group_by(.[[MotherAgeVariable]]) %>%
#       summarise(TotalCount = n()) %>%
#       mutate(MaxMums = floor(TotalCount/2)) %>%
#       select(-TotalCount)
#
#
#     ResampleMatNums <- left_join(rename_at(ExtraMatNums, 1, ~ names(FinalMotherNums)[1]),
#                                  FinalMotherNums, by = "MatAge") %>%
#      replace(is.na(.),0) %>%
#      mutate(MumsLeft = MaxMums - MatNumsUsed) %>%
#      filter(MumsLeft > 0)
#
#   }
#
#  # now random sample children to match to mothers
#  # remaining children will match to fathers
#
#   ChildrenToMothers <- sample_n(ChildDataframe, sum(FinalMotherNums$MatNumsUsed))
#
#   # random sample mothers based on permissible mothers counts by age
#
#  MaternalSubset <- MotherDataframe %>%
#    filter(.[[MotherAgeVariable]] %in% FinalMotherNums$MatAge) %>%
#    arrange(.[[MotherAgeVariable]]) %>%
#    group_by(.[[MotherAgeVariable]]) %>%
#    tidyr::nest() %>%
#    ungroup() %>%
#    mutate(MatNumsToMatch = c(FinalMotherNums$MatNumsUsed))
#
#  # #
#  # # # MotherAgeCounts <- FinalMotherNums$MatNumsUsed
#  # # # FinalMothers <- sample_n(MaternalSubset)
#  # #
#  # # # subset the maternal data so that only the ages required are needed
#  # #
#  # #
#  # # # do the random sampling to subset mothers
#  # #
#  # #
#  # # # set.seed()
#  # # # for (j in 1:nrow(MotherDataframe)) {
#  # # #   age_index <- sample(length(MotherAgeCounts), 1, prob=MotherAgeCounts)
#  # # #   MotherAgeCounts[age_index] = MotherAgeCounts[age_index] - 1
#  # # # }
#  # # #  MothersToChildren <-
#  # #
#  # #  # assume numbers missing are fathers
#  # #  # use mean age difference between fathers and mothers to adjust sampling for fathers
#  #
#  #
#
# }
#

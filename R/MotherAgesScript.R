# function(ChildDataframe, AgeVariableIndex, xiUsed=NULL, OmegaUsed=NULL,
#          AlphaUsed=NULL, MotherDataframe, MotherAge, MotherID){
#
#   SummaryData <- as.data.frame(ChildDataframe[,AgeVariableIndex])
#
#   for(i in 1:nrow(SummaryData)) {
#     SummaryData$MotherAge[i] <-  round(rsn(1,xiUsed, OmegaUsed,AlphaUsed),0)
#   }
#
#   SummaryData <- SummaryData %>%
#     tidyr::unnest(MotherAge) %>%
#     mutate(Diff = SummaryData$MotherAge - SummaryData[,1]) %>%
#     group_by(Diff) %>%
#     summarise(AgeDiffCount = n())
#
#   return(SummaryData)
#
#   MotherSample <- MotherDataframe %>%
#     group_by(.[[MotherAge]]) %>%
#     summarise(MotherAgeCount = n())
#
#   return(MotherSample)
#
# }

library("dplyr")

readRDS(file = "JOB_09626_CONZUL_rerun.rds")

# convert the file from wide to long
LongChildGuardianFile <- JOB_09626_CONZUL_rerun %>%
  tidyr::gather(MotherAgeGroup, RRCount, `Less than 18 Years`:`65 Years and Over`, factor_key=TRUE) %>%
  filter(Area == "Timaru Urban Area", MotherAgeGroup != "Less than 18 Years", `Age of Child` != "Total", RRCount !=0) %>%
  select(-c("Total 1", "No Matches", "Multiple Matches", "Total 2")) %>%
  mutate(RRCount = tidyr::replace_na(RRCount, 0),
         YoungestMotherAge = as.numeric(sub(".*?(\\d+).*", "\\1", MotherAgeGroup)),
         OldestMotherAge = as.numeric(sub("([0-9]+).*?([0-9]+).*", "\\2", MotherAgeGroup)), 
         OldestMotherAge = ifelse(YoungestMotherAge == 65, 69, OldestMotherAge),
         YoungestChildAge = as.numeric(sub(".*?(\\d+).*", "\\1", `Age of Child`)),
         OldestChildAge = as.numeric(sub("([0-9]+).*?([0-9]+).*", "\\2", `Age of Child`)),
         MinMotherAge = YoungestMotherAge - OldestChildAge,
         MaxMotherAge = OldestMotherAge - YoungestChildAge,
         CountMotherAges = MaxMotherAge - MinMotherAge +1,
         ChildCountDistribute = RRCount / CountMotherAges
  ) %>%
  filter(MinMotherAge >15)

AgeAtChildbirth <- seq(min(LongChildGuardianFile$MinMotherAge), max(LongChildGuardianFile$MaxMotherAge), 1)
ChildbirthAgeCounts <- rep(0, length(AgeAtChildbirth))

set.seed(161018)
for (j in 1:nrow(LongChildGuardianFile)) {
  for (k in LongChildGuardianFile$MinMotherAge[j]:LongChildGuardianFile$MaxMotherAge[j]) {
    Count[k] = LongChildGuardianFile$ChildCountDistribute[j]
    vector_index = LongChildGuardianFile$MinMotherAge[j] - 16
    ChildbirthAgeCounts[vector_index] = ChildbirthAgeCounts[vector_index] + Count[k]
  }
  
}

saveRDS(JOB_09626_CONZUL_rerun, "JOB_09626_CONZUL_rerun.rds")
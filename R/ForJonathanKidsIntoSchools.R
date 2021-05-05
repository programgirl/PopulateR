load("~/Sync/PhD/PopSim/data/KidsIntoSchools.RData")

TheChildrenFinalised538 <- ChildrenToSchools(KidsIntoSchools, ChildIDCol = 4, ChildAgeCol = 5, ChildSxCol = 9,
                                             HouseholdIDCol = 8, SchoolsToUse, SchoolIDCol = 2, SchoolAgeCol = 6,
                                             SchoolRollCol = 7, SchoolTypeCol = 3, ChildProb = .8, UserSeed = 4)

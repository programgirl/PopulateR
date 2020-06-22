# Getting files into the project for testing
HH4P <- readRDS("~/Sync/PhD/PopSim/R/HH4P.Rds")

# bring in data frames from synethetic population
File7FemaleCouple <- readRDS("~/Sync/PhD/PopSim/R/File7FemaleCouple.rds")
File7MaleCouple <- readRDS("~/Sync/PhD/PopSim/R/File7MaleCouple.rds")

# also using diffr to check files
# use example diffr("~/Sync/PhD/PopSim/R/OppSexN.R", "~/Sync/PhD/PopSim/R/OppositeSexNewN.R")

diffr("~/Sync/PhD/PopSim/R/OppSexSN.R", "~/Sync/PhD/PopSim/R/OppSexN.R")

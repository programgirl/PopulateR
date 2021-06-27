DataForJonathan <- readRDS("SocialnetworksData.rds")

SoFar <- Networks(DataForJonathan, IDCol=6, AgeCol=9, NetworkCol = 11, MeanUsed=0, SDUsed=2,
                          ProbSameNetwork = .5, NetworkVariable = "Networks", UserSeed=2020, pValueToStop=.001,
                          NumIterations=1000000)

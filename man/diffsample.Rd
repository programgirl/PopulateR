% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diffsample.R
\name{diffsample}
\alias{diffsample}
\title{Sample from groups, when the sample size for each group is different}
\usage{
diffsample(people, pplage, sampledf, smplage, smplcounts, userseed = NULL)
}
\arguments{
\item{people}{A data frame containing individual people.}

\item{pplage}{The variable containing the ages, in the people data frame.}

\item{sampledf}{A data frame containing ages and sample size counts.}

\item{smplage}{The variable containing the ages, in the sampledf data frame.}

\item{smplcounts}{The variable containing the sample size counts, in the sampledf data frame.}

\item{userseed}{If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.}
}
\value{
A data frame of people sampled according to the age sample sizes required.
}
\description{
Produces samples by group, enabling different sample sizes to be specified for each group. Sampling without replacement is used. While the function example is based on sampling by age, in practice sampling can be performed using any variable of choice. Only one grouping variable is used.
}
\examples{

SampleNeeded <- data.frame(Age = c(16, 17, 18),
                           NumNeeded = c(5, 10, 15))
SampledAdolescents <- diffsample(WorkingAdolescents, pplage = "Age", sampledf = SampleNeeded,
                                 smplage = "Age", smplcounts = "NumNeeded", userseed = 4)

table(SampledAdolescents$Age)

}

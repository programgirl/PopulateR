% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createemp.R
\name{createemp}
\alias{createemp}
\title{Create employers, each with employee counts}
\usage{
createemp(
  employers,
  industry,
  indsmin,
  indsmax,
  pplmin,
  pplmax,
  stffname = NULL,
  cpyname = NULL,
  userseed = NULL
)
}
\arguments{
\item{employers}{A data frame containing aggregate data on employers.}

\item{industry}{The variable containing the types of employers. This can be an industry code.}

\item{indsmin}{The variable containing the minimum number of employees in each industry.}

\item{indsmax}{The variable containing the maximum number of employees in each industry.}

\item{pplmin}{The variable containing the minimum number of staff in each industry.}

\item{pplmax}{The variable containing the maximum number of staff in each industry.}

\item{stffname}{The variable name to use for the staff counts for each employer.}

\item{cpyname}{The variable name to use for the companies.}

\item{userseed}{If specified, this will set the seed to the number provided. If not, the normal set.seed() function will be used.}
}
\value{
#'A data frames of synthetic companies, with the number of employees and a mock company name.
}
\description{
Constructs individual employers from aggregate counts, such as number of employers per employer type. Employer type is often industry, such as "Sheep, Beef Cattle and Grain Farming". Within each employer type, the number of employers is extracted. The number of employees is then randomly assigned to each of those employers, using the total employee count for that industry. A randomisation method is used to ensure that the company counts can be quite dissimilar across the employers within a type. However, this is constructed by the ratio of employers to employees. If the number of employers is similar to the number of employees, the number of employees will tend to be 1 for each employer.
}
\examples{

library("dplyr")

TownshipEmployment <- createemp(AllEmployers, industry = "ANZSIC06", indsmin = "minCo",
                                indsmax = "maxCo", pplmin = "minStaff", pplmax = "maxStaff",
                                stffname="NumEmployees", cpyname="Company", userseed = 4)
}

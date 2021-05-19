#' Create a subset of observations containing only same-sex couples
#'
#' This is a wrapper for randomly sampling observations into same-sex couples.
#' It is mainly used for data frames that contain a subset of observations that require over-sampling.
#' However, it can also be used to generate a simple random sample of observations.
#' An even number of observations is output.
#'
#' @export
#' @param dataframe A data frame containing observations limited to one sex that includes an age column.
#' @param AgeCol The column number of the data frame that contains the ages.
#' @param ProbSameSex The probability of any observation being assigned to a same-sex couple.
#' @param UpWeightProp The proportion of individuals who are to be over-sampled.
#' @param UpWeightLowerAge The youngest age for the over-sampling. Required if UpWeightProp value is provided.
#' @param UpWeightUpperAge The oldest age for the over-sampling. Required if UpWeightProp value is provided.
#' @param IDStartValue The starting number for generating a variable that identifies the observations in a couple. Must be numeric.
#' @param HouseholdNumVariable The column name for the household variable. This must be supplied in quotes.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data frame of an even number of observations for allocation into same-sex couples. If IDStartValue is specified, household allocation will be performed.
#'
#' @examples
#' PersonDataframe <- data.frame(cbind(PersonID = c(1:1000),
#'
AddEmployer <- function(Employers, EmployerTypeCol, EmployerCountCol, EmployeeCountCol, UserSeed = NULL) {

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # Recipient ID variable
  EmployerTypeColName <- sym(names(Employers[EmployerTypeCol]))

  EmployerRenamed <- Employers %>%
    rename(CompanyCode = !! EmployerTypeCol,
           CompanyCts = !! EmployerCountCol,
           StaffCts = !! EmployeeCountCol)

  # remove all rows with 0 employer or 0 employee counts
  EmployerRenamed <- EmployerRenamed %>%
    filter(CompanyCts > 0 & StaffCts > 0)

  #####################################
  #####################################
  # Construct the separate companies
  #####################################
  #####################################

  #####################################
  # Expand so that each row is a different company
  # there are 360,999 employers so new data frame must have
  # 360,999 rows
  #####################################

  for (i in 1:nrow(EmployerRenamed)) {

    CurrentCompany <- as.data.frame(EmployerRenamed$CompanyCode[i])

    NumberEmployers <- as.numeric(EmployerRenamed$CompanyCts[i])
    NumberStaff <- as.numeric(EmployerRenamed$CurrentCompany[i])

    cat("Company", CurrentCompany$CompanyCode, "number employers", NumberEmployers, "number employees", NumberStaff, "\n")

    # fix problem if there are more employers than there are employees
    # can happen, e.g. sole enterprises, partnerships with no employees
    # etc
    if(NumberStaff < NumberEmployers) {
      NumberEmployers <- NumberStaff
    }

    # next step is to see whether there are the same number of companies as there are employees
    # if the counts are the same, add one staff member to each company

    if (NumberEmployers == NumberStaff) {

    cat("The number of employers is", NumberEmployers, "and the number of staff is", NumberStaff, "\n")

      InternalEmployer <- CurrentCompany %>%
        slice(rep(1), each = NumberEmployers)

    } else {

    RandomRollVector <- runif(NumberEmployers)

    EmployeePropVector <- RandomRollVector

    # closes else under if (NumberEmployers == NumberStaff)
    }


    if (i == 1) {

      return(InternalEmployer)
    }


  }

#
#  return(EmployerRenamed)
}

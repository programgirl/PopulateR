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

  # put in the starting value for the paste0 used in the company name
  Paste0Value <- 1

  if (!is.null(UserSeed)) {
    set.seed(UserSeed)
  }

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

    CurrentCompany <- EmployerRenamed %>%
      filter(row_number() == i)

    NumberEmployers <- as.numeric(CurrentCompany$CompanyCts)
    NumberStaff <- as.numeric(CurrentCompany$StaffCts)

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
        slice(rep(seq_len(n()), NumberEmployers)) %>%
        mutate(StaffCts = 1,
               CompanyName = paste0("Company", Paste0Value:(Paste0Value+nrow(.)-1)))

      # get max paste0 value so that the starting company name is updated for the next loop
      EndPaste0Value <- InternalEmployer %>%
        slice_tail(n = 1) %>%
        mutate(ValueIs =  as.numeric(gsub("[^[:digit:].]", "",  CompanyName))) %>%
        pull(ValueIs)

      cat("The last company number is", EndPaste0Value, "\n")


      Paste0Value <- EndPaste0Value + 1

      # add the employers to the final dataframe

      if(exists("OutputDataframe")) {

        OutputDataframe <- bind_rows(OutputDataframe, InternalEmployer)

      } else {

        OutputDataframe <- InternalEmployer

        # closes else for if(exists("OutputDataframe"))
      }

    } else {

      cat("The number of employers is", NumberEmployers, "and the number of staff is", NumberStaff, "\n")

      # below doesn't work when number of staff is only slightly greater than number of employees
      # does not return enough values
      # AchievedCompSize <- as.vector(table(sample(1:NumberEmployers, size = NumberStaff, replace = T)))

      AchievedCompSize <- rmultinom(n = 1, size = NumberStaff, prob = rep(1/NumberEmployers, NumberEmployers))

      while(0 %in% AchievedCompSize) {

        BaseCompSize <- rep(1, NumberEmployers)
        NewNumStaff <- NumberStaff - sum(BaseCompSize)

        AddedCompSize <- rmultinom(n = 1, size = NewNumStaff, prob = rep(1/NumberEmployers, NumberEmployers))

        AchievedCompSize <- BaseCompSize + AddedCompSize

        # closes while(0 %in% AchievedCompSize)
      }

    InternalEmployer <- CurrentCompany %>%
      slice(rep(seq_len(n()), NumberEmployers)) %>%
     mutate(CompanyName = paste0("Company", Paste0Value:(Paste0Value+nrow(.)-1)),
            StaffCts = AchievedCompSize)

    EndPaste0Value <- InternalEmployer %>%
      slice_tail(n = 1) %>%
      mutate(ValueIs =  as.numeric(gsub("[^[:digit:].]", "",  CompanyName))) %>%
      pull(ValueIs)

    Paste0Value <- EndPaste0Value + 1

    # add the employers to the final dataframe

    if(exists("OutputDataframe")) {

      OutputDataframe <- bind_rows(OutputDataframe, InternalEmployer)

    } else {

      OutputDataframe <- InternalEmployer

      # closes else for if(exists("OutputDataframe"))
    }

    # closes else under if (NumberEmployers == NumberStaff)
    }



    # closes for (i in 1:nrow(EmployerRenamed))
  }

#
 return(OutputDataframe)
}

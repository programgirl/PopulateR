#' Create a data frame of individual employers, each with aggregate employee counts.
#'
#' This function constructs individual employers from aggregate counts, such as number of employers per employer type. Employer type is often industry, such as "Sheep, Beef Cattle and Grain Farming". Within each employer type, the number of employers is extracted. The number of employees is then randomly assigned to each of those employers, using the total employee count for that industry. A randomisation method is used to ensure that the company counts can be quite dissimilar across the employers within a type. However, this is constructed by the ratio of employers to employees. If the counts are similar, in this case the number of employees will tend to be 1 for each employer. A non-zero count of employees is returned for each employer.
#'
#' The function removes any employer types with either no employees or no employers. This situation can occur due to random rounding in official statistics. Where the number of employers exceeds the number of employees, the former count is replaced by the employee count. Thus, no pre-processing of the data frame is required.
#'
#' @export
#' @param Employers A data frame containing aggregate data on employers.
#' @param EmployerTypeCol The column number for the types of employers. This can be an industry code.
#' @param EmployerCountCol The column number containing the aggregate counts of employers by employer type.
#' @param EmployeeCountCol The column number containing the aggregate counts of employees by employer type.
#' @param UserSeed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return A data frame of synthetic companies, with randomised employee counts.
#'
#' @examples
#' PersonDataframe <- data.frame(cbind(PersonID = c(1:1000),
#'
CreateEmployers <- function(Employers, EmployerTypeCol, EmployerCountCol, EmployeeCountCol, UserSeed = NULL) {

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

  # rename the variables

  OutputDataframe <- OutputDataframe %>%
    rename()

#
 return(OutputDataframe)
}

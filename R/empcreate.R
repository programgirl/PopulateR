#' Create a data frame of individual employers, each with aggregate employee counts.
#'
#' This function constructs individual employers from aggregate counts, such as number of employers per employer type. Employer type is often industry, such as "Sheep, Beef Cattle and Grain Farming". Within each employer type, the number of employers is extracted. The number of employees is then randomly assigned to each of those employers, using the total employee count for that industry. A randomisation method is used to ensure that the company counts can be quite dissimilar across the employers within a type. However, this is constructed by the ratio of employers to employees. If the counts are similar, in this case the number of employees will tend to be 1 for each employer.
#'
#' @export
#' @param employers A data frame containing aggregate data on employers.
#' @param emptypecol The column number for the types of employers. This can be an industry code.
#' @param empnumcol The column number containing the aggregate counts of employers by employer type.
#' @param staffnumcol The column number containing the aggregate counts of employees by employer type.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return #'A list of three data frames $Companies contains the data frame of synthetic companies, with the number of employees and a mock company name. $Overcounts contains the companies where the number of employers in an industry exceeds the number of employees. This is an informational data frame provides the original values for employee and employee counts. These industries are included in the $Companies file. The count of employers is reset to the count of employees, resulting in synthetic companies with employees count of 1. $NoEmps contains industries with employer counts but no employee counts, employee counts but no employer counts, and a combination of no employers and no employees. These are excluded from the $Companies data frame. Industries with 0 employee and 0 employer counts is likely due to a standardised list of industries being used for all geographic regions. The existence of employers with no employees is indicative of sole-trader/director-only companies. The presence of employees but no employers suggests a data accuracy problem.
#'
#' @examples
#' library("dplyr")
#'
#' TownshipEmployment <- empcreate(AllEmployers, emptypecol = 1, empnumcol = 2, staffnumcol = 3, userseed = 4)

empcreate <- function(employers, emptypecol, empnumcol, staffnumcol, userseed = NULL) {

  #####################################
  #####################################
  # get column names as symbols to use inside data frame subfunctions
  #####################################
  #####################################

  # employer type variable
  emptypecolName <- sym(names(employers[emptypecol]))

  # employer counts column name
  empnumcolName <- sym(names(employers[empnumcol]))

  # staff count column name
  staffnumcolName <- sym(names(employers[staffnumcol]))

  #####################################
  #####################################
  # data cleaning
  #####################################
  #####################################

  employerRenamed <- employers %>%
    rename(CompanyCode = !! emptypecol,
           CompanyCts = !! empnumcol,
           StaffCts = !! staffnumcol)

  # put the 0 counts into a file

  # put employers and employees with 0 counts into a file

 Nocounts <- employerRenamed %>%
   filter(CompanyCts == 0 | StaffCts == 0)

  # remove all rows with 0 employer or 0 employee counts
  employerRenamed <- employerRenamed %>%
    filter(CompanyCts > 0 & StaffCts > 0)

  # put in the starting value for the paste0 used in the company name
  Paste0Value <- 1

  if (!is.null(userseed)) {
    set.seed(userseed)
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

  for (i in 1:nrow(employerRenamed)) {

    CurrentCompany <- employerRenamed %>%
      filter(row_number() == i)

    Numberemployers <- as.numeric(CurrentCompany$CompanyCts)
    NumberStaff <- as.numeric(CurrentCompany$StaffCts)

    # cat("Company", CurrentCompany$CompanyCode, "number employers", Numberemployers, "number employees", NumberStaff, "\n")


    # fix problem if there are more employers than there are employees
    # can happen, e.g. sole enterprises, partnerships with no employees
    # etc
    if(NumberStaff < Numberemployers) {

      if(exists("OvercountEmployers")) {

        OvercountEmployers <- bind_rows(OvercountEmployers, CurrentCompany)

      } else {

        OvercountEmployers <- CurrentCompany

        # closes else to if(NumberStaff < Numberemployers)
      }

      # cat("The number of employers is", Numberemployers, "and the number of staff is", NumberStaff, "\n")
      Numberemployers <- NumberStaff
    }

    # next step is to see whether there are the same number of companies as there are employees
    # if the counts are the same, add one staff member to each company

    if (Numberemployers == NumberStaff) {

      # cat("The number of employers is", Numberemployers, "and the number of staff is", NumberStaff, "\n")

      Internalemployer <- CurrentCompany %>%
        slice(rep(seq_len(n()), Numberemployers)) %>%
        mutate(StaffCts = 1,
               CompanyName = paste0("Company", Paste0Value:(Paste0Value+nrow(.)-1)))

      # get max paste0 value so that the starting company name is updated for the next loop
      EndPaste0Value <- Internalemployer %>%
        slice_tail(n = 1) %>%
        mutate(ValueIs =  as.numeric(gsub("[^[:digit:].]", "",  CompanyName))) %>%
        pull(ValueIs)

      # cat("The last company number is", EndPaste0Value, "\n")


      Paste0Value <- EndPaste0Value + 1

      # add the employers to the final dataframe

      if(exists("OutputDataframe")) {

        OutputDataframe <- bind_rows(OutputDataframe, Internalemployer)

      } else {

        OutputDataframe <- Internalemployer

        # closes else for if(exists("OutputDataframe"))
      }

    } else {

      # cat("The number of employers is", Numberemployers, "and the number of staff is", NumberStaff, "\n")


      AchievedCompSize <- as.integer(rmultinom(n = 1, size = NumberStaff, prob = rep(1/Numberemployers, Numberemployers)))


      while(0 %in% AchievedCompSize) {

        BaseCompSize <- rep(1, Numberemployers)
        NewNumStaff <- NumberStaff - sum(BaseCompSize)

        AddedCompSize <- as.integer(rmultinom(n = 1, size = NewNumStaff, prob = rep(1/Numberemployers, Numberemployers)))

        AchievedCompSize <- BaseCompSize + AddedCompSize

        # closes while(0 %in% AchievedCompSize)
      }

      Internalemployer <- CurrentCompany %>%
        slice(rep(seq_len(n()), Numberemployers)) %>%
        mutate(CompanyName = paste0("Company", Paste0Value:(Paste0Value+nrow(.)-1)),
               StaffCts = AchievedCompSize)

      EndPaste0Value <- Internalemployer %>%
        slice_tail(n = 1) %>%
        mutate(ValueIs =  as.numeric(gsub("[^[:digit:].]", "",  CompanyName))) %>%
        pull(ValueIs)

      Paste0Value <- EndPaste0Value + 1

      # add the employers to the final dataframe

      if(exists("OutputDataframe")) {

        OutputDataframe <- bind_rows(OutputDataframe, Internalemployer)

      } else {

        OutputDataframe <- Internalemployer

        # closes else for if(exists("OutputDataframe"))
      }

      # closes else under if (Numberemployers == NumberStaff)
    }



    # closes for (i in 1:nrow(employerRenamed))
  }

  # rename the variables

  OutputDataframe <- OutputDataframe %>%
    rename(!!emptypecolName := CompanyCode, !!staffnumcolName := StaffCts) %>%
    dplyr::select(-CompanyCts)

  if(exists("OvercountEmployers")) {

    OvercountEmployers <- OvercountEmployers %>%
      rename(!!empnumcolName :=CompanyCts, !!staffnumcolName := StaffCts,
             !!emptypecolName := CompanyCode)

  }

  if(exists("Nocounts")) {

    Nocounts <- Nocounts %>%
      rename(!!empnumcolName :=CompanyCts, !!staffnumcolName := StaffCts,
             !!emptypecolName := CompanyCode)

  }

  # if(exists("Nocounts")) {
  #
  #   return(Nocounts)
  # }

  MergedList <- list()

  MergedList$Companies <- OutputDataframe
  MergedList$Overcount <- OvercountEmployers
  MergedList$NoEmps <- Nocounts

  return(MergedList)

}

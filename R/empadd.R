#' Create a data frame of people with employers added
#'
#' accepts aggregate or disaggregate employee data
#' unemployed people can be included in the input data frame
#' unemployed people will have an employer set to 0.
#' FIX BELOW
#'
#' NOTE THAT EVERY COLUMN IN THE EMPLOYERS DF IS ADDED TO THE WORKER DF.
#' This function constructs individual employers from aggregate counts, such as number of employers per employer type. Employer type is often industry, such as "Sheep, Beef Cattle and Grain Farming". Within each employer type, the number of employers is extracted. The number of employees is then randomly assigned to each of those employers, using the total employee count for that industry. A randomisation method is used to ensure that the company counts can be quite dissimilar across the employers within a type. However, this is constructed by the ratio of employers to employees. If the counts are similar, in this case the number of employees will tend to be 1 for each employer.
#' END OF TO FIX
#'
#' @export
#' @param employers A data frame containing employer data.
#' @param empid The column number for the employer ID.
#' @param empcount The column number that provides the number of employees for each employer.
#' @param workers A data frame containing the people that must be matched to employers.
#' @wrkid The column number for the unique value that identifies unique people.
#' @param hourscol The column number containing the hours worked by each person Must be an ordered factor or numeric. The levels/values must be ascending for hours worked. This is output as an ordered factor.
#' @param hoursmin The relevant factor level/number from hourscol representing the workers. Anything lower than this level/number will be treated as unemployed.
#' @param userseed The user-defined seed for reproducibility. If left blank the normal set.seed() function will be used.
#'
#' @return
#'FIX BELOW
#'
#'    #'A list of three data frames $Companies contains the data frame of synthetic companies, with the number of employees and a mock company name. $Overcounts contains the companies where the number of employers in an industry exceeds the number of employees. This is an informational data frame provides the original values for employee and employee counts. These industries are included in the $Companies file. The count of employers is reset to the count of employees, resulting in synthetic companies with employees count of 1. $NoEmps contains industries with employer counts but no employee counts, employee counts but no employer counts, and a combination of no employers and no employees. These are excluded from the $Companies data frame. Industries with 0 employee and 0 employer counts is likely due to a standardised list of industries being used for all geographic regions. The existence of employers with no employees is indicative of sole-trader/director-only companies. The presence of employees but no employers suggests a data accuracy problem.
#'
#' @example
#' library("dplyr")
#'
#' TownshipEmployment <- empcreate(AllEmployers, emptypecol = 1, empnumcol = 2, staffnumcol = 3, userseed = 4)
#' END OF TO FIX

empadd <- function(employers, empid, empcount, workers, wrkid, hourscol, hoursmin, userseed = NULL) {

  # check if hourscol is an ordered factor or numeric

  # setup

  employersRenamed <- employers %>%
    rename(EmployerID = !! empid, NumStaff = !! empcount)

  # employer variable names
  empidcolName <- sym(names(employers[empid]))

  # workers variable names
  wrkidcolName <- sym(names(workers[wrkid]))

  wrkhrscolName <- sym(names(workers[hourscol]))

  # expand the employer data frame to one row per employee

  employersRenamed <- tidyr::uncount(employersRenamed, NumStaff)

   workersRenamed <- workers %>%
    rename(IntHours = !! hourscol,
           workersid = !! wrkid)

   if (is.ordered(workersRenamed$IntHours) == FALSE &
       is.numeric(workersRenamed$IntHours) == FALSE) {

     stop("Hours worked must be an ordered factor or numeric.")
   }

   workersWorking <- workers %>%
     rename(IntHours = !! hourscol,
            workersid = !! wrkid) %>%
     filter(as.integer(IntHours) >= hoursmin)

   workersUnemployed <- workers %>%
     rename(IntHours = !! hourscol,
            workersid = !! wrkid) %>%
     filter(!(workersid %in% c(workersWorking$workersid)))

   # fix the 0 employer id for unemployed
   # does "0" if factor and 0 if numeric on employerid
   if(is.ordered(class(employers[empid])) == TRUE) {

     workersUnemployed <- workersUnemployed %>%
       mutate(EmployerID = "0")

   } else {

     workersUnemployed <- workersUnemployed %>%
       mutate(EmployerID = 0)

     # closes if(is.ordered(class(employers[empid]))
   }

   # check if the employer list can take all the workers
  if (nrow(employersRenamed) < nrow(workersWorking)) {

    CountDiff <- nrow(workersWorking) - nrow(employersRenamed)

    ExtraEmployers <- employersRenamed %>%
      slice_sample(n = CountDiff)

    employersRenamed <- bind_rows(employersRenamed, ExtraEmployers)

  }

   # shuffle the rows of the employer data frame

   ShuffleCount <- nrow(workersWorking)

   employersRenamed <- employersRenamed %>%
     slice_sample(n = ShuffleCount)


   OutputDataframe <- bind_cols(workersWorking, employersRenamed)



   OutputDataframe <- bind_rows(OutputDataframe, workersUnemployed)


   if (is.ordered(workers[,hourscol]) == TRUE) {

     #  cat("Hours worked is a factor")

     HoursLabels <- levels(workers[,hourscol])

     OutputDataframe <- OutputDataframe %>%
       mutate(IntHours = factor(IntHours, labels = c(HoursLabels), order = TRUE))

     #close factor test for hours worked variable
   }


   OutputDataframe <- OutputDataframe %>%
     rename(!!wrkidcolName := workersid,
            !!wrkhrscolName := IntHours,
            !!empidcolName := EmployerID)

   # Unemployed <-


return(OutputDataframe)

}

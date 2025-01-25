#' @importFrom dplyr across all_of  arrange bind_rows filter group_by left_join mutate pull rename row_number select slice_sample ungroup
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @importFrom tidyselect matches where
NULL

#' Interpolate ages from age group medians
#'
#' The node ages for each age group are defined by the user, along with the age group values. The ages are then imputed from these nodes.
#' Zero values at both extremes must be included. For example, for the age group 20-24 years, the pplprop value is for pplage. if the first non-zero relationship probability is for the age group 20-24 years, and the previous age group is 15-19 years, pplprop==0 for pplage==19.
#'For each age group, there must be a minimum and maximum age specified. This provides the interpolation range for each age group. For the anchoring 0 values, the minimum and maximum ages are the same. In this example, for pplage==19, endmin==19, and endmax==19.
#' If there is no zero for older ages, as the final node value occurs inside the age group, the function assumes that the last node-to-node should be used to extrapolate for the ages older than the oldest node value. For example, if the last node value is for 90 years of age, but the oldest age is 95 years, the function will assume the same slope for ages 91 through 95 years.
#' The function can perform a separate interpolation for groups, for example, a separate interpolation can be performed for each sex. The function is flexible for the number of variables that can be used to define groups. If only one interpolation is required, the same grpdef value should be used for each row in the data frame.
#'
#' While the function is designed to interpolate proportions, in practice it can interpolate any values. The limitation is that the function performs no rounding. Integer node values may produce non-integer estimates.
#'
#' @export
#' @param nodes A data frame containing all grouping variables, the node ages for each group, and the associated node values.
#' @param pplage The variable containing the node ages.
#' @param pplprop The variable containing the node values.
#' @param endmin The variable that contains the minimum age for each group.
#' @param endmax The variable that contains the maximum age for each group.
#' @param grpdef A character vector containing the names of the grouping variables.
#'
#' @return A data frame containing the fitted values, by age within group.
#'
#' @examples
#'
#' library("dplyr")
#'
#' # create the expected proportion of people in relationships, by age within sex
#' thegroups <- as.vector("Sex")
#' RelProps <- interdiff(GroupInfo, pplage = "MidPoints", pplprop = "RelProps", endmin = "MinAge",
#'                       endmax = "MaxAge", grpdef = thegroups)


interdiff <- function(nodes, pplage, pplprop, endmin, endmax, grpdef) {

  withr::local_options(dplyr.summarise.inform = FALSE)


  # Subset the node dataframe

  PeopleUnique <- nodes %>%
    select(matches(grpdef)) %>%
    unique()


  #####################################
  # check for missing input information
  #####################################

  if (!pplage %in% names(nodes)) {
    stop("Age variable in the nodes data frame is incorrect.")
  }

  if (!pplprop %in% names(nodes)) {
    stop("Node variable in the nodes data frame is incorrect.")
  }

  if (!endmin %in% names(nodes)) {
    stop("Minimum age variable name in the ageranges data frame is incorrect.")
  }

  if (!endmax %in% names(nodes)) {
    stop("Maximum age variable name in the ageranges data frame is incorrect.")
  }


  # get column names as symbols to use inside data frame subfunctions

  agevarname <- sym(names(nodes[pplage]))


  # #####################################
  # #####################################
  # # rename variables so don't need to use quosures inside code
  # #####################################
  # #####################################

  peopleRenamed <- as.data.frame(nodes %>%
    rename(OriginalAge = !! pplage,
           OriginalProp = !! pplprop,
           MinAge = !! endmin,
           MaxAge = !! endmax))



  # loop through the unique rows
  for(i in 1:nrow(PeopleUnique)) {


    # below returns a vector if there is only one grouping variable
    # CurrentDef <- PeopleUnique[i,]


    # fix for one grouping variable
    # see https://stackoverflow.com/a/69116009/1030648
    CurrentDef = PeopleUnique[i, , drop = FALSE]

    WorkingAgeMin <- min(as.numeric(left_join(CurrentDef, peopleRenamed, by = c(grpdef)) %>%
                                     select("MinAge") %>%
                                      pull()))


    WorkingAgeMax <- max(as.numeric(left_join(CurrentDef, peopleRenamed, by = c(grpdef)) %>%
                                      select("MaxAge") %>%
                                      pull()))

    GroupInfo <- CurrentDef %>%
      mutate(across(where(is.factor), as.character))


    GroupInfo <- as.character(GroupInfo[1,])

    # create data frame with ages

    GroupInfo <- CurrentDef %>%
      mutate(across(where(is.factor), as.character))


    GroupInfo <- as.character(GroupInfo[1,])

    CurrentGroup <- left_join(CurrentDef, peopleRenamed, by = c(grpdef))

    # need to sort CurrentGroup by age
    CurrentGroup <- CurrentGroup %>%
      arrange(.data$OriginalAge)


    # dataframe of defined probs constructed

    # count number of rows in the group, need to handle one-row groups separately to
    # prevent an NA error
    NumRowsInCurrentGroup <- nrow(CurrentGroup)

    # work through the CurrentGroup dataframe
    if(NumRowsInCurrentGroup == 1) {

      CurrentDF <- data.frame(Age = c(seq(from = WorkingAgeMin, to = WorkingAgeMax, by =1)))

      startpt <-  CurrentGroup$OriginalProp[1]

      CurrentDF$Fits <- rep(startpt, nrow(CurrentDF))

      # add in group information
      ExpandedGroup <- CurrentDef %>%
        slice_sample(n = nrow(CurrentDF), replace = TRUE)

      CurrentDF <- bind_cols(CurrentDF, ExpandedGroup)

      CurrentMinAge <- min(CurrentDF$Age)
      CurrentMaxAge <- max(CurrentDF$Age)


      if(exists("GroupDF")) {

        GroupDF <-  bind_rows(GroupDF, CurrentDF)

      } else {

        GroupDF <- CurrentDF

        # closes else to if(exists("GroupDF")) {
      }


    } else {

    for(j in 1:(nrow(CurrentGroup)-1)) {


      startpt = CurrentGroup$OriginalAge[j]
      endpt = CurrentGroup$OriginalAge[(j+1)]

      ptdiff <- endpt - startpt

      startval <- CurrentGroup$OriginalProp[j]
      endval = CurrentGroup$OriginalProp[(j+1)]

      valdiff <- endval - startval

      # do the estimates for the group
      # get the relevant inflexion points

      mininflex <- startpt

      maxinflex <- endpt

      #do the next bit by whole numbers
      minintval <- ceiling(mininflex)
      maxintval <- floor(maxinflex)

      sloperange <- maxinflex - mininflex

      slopeval <- valdiff/sloperange

      if(j ==(nrow(CurrentGroup)-1) & WorkingAgeMax > maxinflex) {

        CurrentDF <- data.frame(Age = c(seq(from = minintval, to = WorkingAgeMax, by =1)))

      } else {

      CurrentDF <- data.frame(Age = c(seq(from = minintval, to = maxintval, by =1)))

      # closes else to  if(j ==(nrow(CurrentGroup)-1))
      }


      CurrentDF <- CurrentDF %>%
        mutate(Fits = startval + (.data$Age - mininflex) * slopeval)

      ExpandedGroup <- CurrentDef %>%
        slice_sample(n = nrow(CurrentDF), replace = TRUE)

      CurrentDF <- bind_cols(CurrentDF, ExpandedGroup)

      CurrentMinAge <- min(CurrentDF$Age)
      CurrentMaxAge <- max(CurrentDF$Age)


      if(exists("GroupDF")) {

        GroupDF <-  bind_rows(GroupDF, CurrentDF)

      } else {

        GroupDF <- CurrentDF

        # closes else to if(exists("GroupDF")) {
      }

      # closes  for(j in 1:(nrow(CurrentGroup)-1))
    }

      # closes else for if(NumRowsInCurrentGroup == 1)
    }


    # closes for(i in 1:nrow(PeopleUnique))

  }

  # remove any duplicates that occur due to ages, rather than half ages, being used as edges
  # removal of mid-points not working

  GroupDF <- GroupDF %>%
    group_by(.data$Age,across(all_of(grpdef))) %>%
    filter(row_number() == 1) %>%
    ungroup()

  # closes function

  return(GroupDF)

}

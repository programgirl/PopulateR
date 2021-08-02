#' School leavers in the Canterbury Region, 2009 to 2018
#'
#' A dataset produced from the official statistics of school leavers produced by the Ministry of Education
#' The Ministry of Education's data are licensed by the Ministry of Education for reuse under the
#' Creative Commons Attribution 4.0 International licence
#'
#' @format A data frame with 120 rows and 4 variables
#' \describe{
#'     \item{YearLeft}{The year for the school leaver count}
#'     \item{Sex}{The sex for the school leaver count}
#'     \item{Age}{The age for the school leaver count}
#'     \item{Total}{The count of adolescents who left school in that year, of that age and sex}
#' }
#' "LeftSchool"
#'
#'
#'#' Mock households with a network size for each person
#'
#' A dataset developed from the Township dataset in the package. Contains realistic household structures.
#'
#' @format  A data frame with 8,439 rows and 7 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#'     \item{Household}{The household identifier for the person}
#'     \item{NetworkSize}{The number of people in that person's social network}
#'}
#' "Networks"
#'
#' Sex/Age pyramid for the Canterbury Region, 2018
#'
#' A dataset produced from combining aggregate 2018 census datasets, using tablecode 8277,
#' from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}.
#' Stats NZ’s data are licensed by Stats NZ for reuse under the Creative Commons Attribution
#' 4.0 International licence.
#'
#' @format A data frame with 14 observations and 4 variables
#' \describe{
#'     \item{Sex}{The sex relating to the count}
#'     \item{Age group}{String variable of age plus the text " years"}
#'     \item{Value}{The count of adolescents of that age and sex}
#'     \item{Age}{The age relating to that count}
#' }
#' "RegionalStructure"
#'
#'
#' Synthetic people in relationships in Timari District
#'
#' A dataset produced from combining aggregate 2018 census data, using tablecodes 8277 and 8395,
#' from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}.
#' Stats NZ’s data are licensed by Stats NZ for reuse under the Creative Commons Attribution
#' 4.0 International licence.
#'
#' @format A data frame with 46,293 rows and 6 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Age.group}{Age group in five-year bands}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{LowerAge}{The youngest age in the Age.group}
#'     \item{UpperAge}{The oldest age in the Age.group}
#'     \item{ID}{The unique identifier for the person}
#' }
#' "Relationships"
#'
#' Sex/age pyramid data for Timaru District
#'
#' A dataset produced from aggregate 2018 census data, using tablecode 8277,
#' from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}.
#' Stats NZ’s data are licensed by Stats NZ for reuse under the Creative Commons Attribution
#' 4.0 International licence.
#'
#' @format A data frame with 190 rows and 4 variables
#' \describe{
#'     \item{Age.group}{Age group in five-year bands}
#'     \item{Sex}{Sex relating to the count}
#'     \item{Value}{The number of people that age and sex}
#'     \item{Age}{Age relating to the count}
#' }
#' "SingleAges"
#'
#'
#' Random selection of 10,000 synthetic people in the Timaru District from the 2018 census
#'
#' A dataset produced from combining aggregate 2018 census datasets, using tablecodes 8277, 8395, and 8460
#' from the official Stats NZ table builder website \url{http://nzdotstat.stats.govt.nz/wbos/Index.aspx}.
#' Stats NZ’s data are licensed by Stats NZ for reuse under the Creative Commons Attribution
#' 4.0 International licence.
#'
#' @format A data frame with 10,000 rows and 5 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{The age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#' }
#' "Township"
#'
#'
#' Adolescents with a school stats and employment hours
#' A dataset of synthetic people aged between 15 and 24, produced bycombining the aggregate 2018 census
#' datasets 8277, 8395, and 8460, with school leavers data produced by the Ministry of Education
#' Stats NZ’s data are licensed by Stats NZ for reuse under the Creative Commons Attribution
#' 4.0 International licence.
#' The Ministry of Education's data are licensed by the Ministry of Education for reuse under the
#' Creative Commons Attribution 4.0 International licence
#'
#' @format A data frame of 1,079 observations and 6 variables
#' \describe{
#'     \item{Sex}{Sex of the person}
#'     \item{Relationship}{Relationship status of the person}
#'     \item{ID}{The unique identifier for the person}
#'     \item{Age}{Age of the person}
#'     \item{HoursWorked}{The number of hours worked in employment, per week}
#'     \item{SchoolStatus}{Whether the person is attending school}
#' }
#' "WorkingAdolescents"

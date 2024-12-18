
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PopulateR

<!-- badges: start -->
<!-- badges: end -->

The goal of PopulateR is to create a synthetic population of people,
which can then be used for modelling. As access to microdata can be
difficult, the package assumes only frequency tables are available. The
requirement for using the package is that a data frame of synthetic
individuals already exists. This can be created by using a frequency
table with counts or weights, and then using those counts (weights) to
construct the population.

The functions have been created assuming that the process to be used
is - add ages to people (agedis). - choose which people are in education
(addind). - create couples using only age groups (fastmatch). This
creates a couples household. - create couples, or add a single child to
a parent, using a normal or skew normal distribution based on ages
(pairnorm). This creates a household containing two people. - add a
single child to a pre-existing couple, using a four-parameter beta
distribution (pairbeta4Num), or a normal or skew normal distribution
(pairnormNum). - add a single child to a sole parent, using a
four-parameter beta distribution (pairbeta4). This creates sole parent
households. - add multiple children to an existing couple
(pairmultNum). - add multiple children to a sole parent (pairmultNum).
This creates sole parent households. - add children to schools
(addschool). - add an extra person to a pre-existing household
(otherNum). - create a household of unrelated people (other). This
creates households. - create employers (createemp). - add an employer to
each worker (addemp). - create a contact network (addnetwork).

The package is modular, and some functions may not be needed, depending
on the detail in the frequency tables used. For example, if the
population has a pre-existing age structure, the agedis function is not
needed.

For example, relationship status may only be provided by age group.
After adding ages, there is likely to be a random pattern of the
proportion of people in a relationship, by age. fixrelations can be used
to create an age pattern, within age group, within sex, so that there is
a monotonic increase, or decrease in the proportion of people in a
relationship, by age within sex. When an education indicator (addind)
has been added, some people in full-time education may also be working
full-time hours. fixhours adjusts the hours worked so that people in
education do not have an usually long working week.

There are two helper functions included. diffsample samples without
replacement, when different groups require different sample counts.
interdiff interpolates proportions between mean values for groups.

## Installation

You can install the development version of PopulateR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("programgirl/PopulateR")
```

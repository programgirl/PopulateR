# produce graphs for the PopulateR article

library(dplyr)
library(readr)
library(ggplot2)




#####################################
# fixrelations
#####################################

PropPart <- BadRels %>%
  group_by(Sex, AgeBand, MinAge, MaxAge, Relationship) %>%
  summarise(NuminRel = n()) %>%
  mutate(RelProps = NuminRel/sum(NuminRel)) %>%
  filter(Relationship == "Partnered") %>%
  select(-NuminRel) %>%
  ungroup()

PropRelAgeR2 <- BadRels %>%
  group_by(Sex, Age, Relationship) %>%
  summarise(NuminRel = n()) %>%
  mutate(RelProps = round(NuminRel/sum(NuminRel), 2)) %>%
  filter(Relationship == "Partnered") %>%
  select(-NuminRel) %>%
  ungroup()

OrigRels <- ggplot() +
  geom_segment(data = PropPart, aes(x = MinAge, y = RelProps, xend = MaxAge, yend = RelProps, colour = Sex,
  ), linewidth = 1) +
  geom_point(data = PropRelAgeR2, aes(x = Age, y = RelProps, colour = Sex)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100)) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1), limits = c(0,1)) +
  labs(x = "Age (years)", y = "Proportion partnered") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#    ggsave(OrigRels, file="~/Sync/PhD/ThesisVersions/PopSimArticle/OrigRels.pdf", width = 12.25, height = 7.15, units = "in")

# plot the data for the fixed output for relationships
# need the function example run first.

FinalRelAge <- FinalRels %>%
  group_by(Sex, Age, Relationship) %>%
  summarise(NuminRel = n()) %>%
  mutate(RelProps = round(NuminRel/sum(NuminRel), 2)) %>%
  filter(Relationship == "Partnered") %>%
  select(-NuminRel) %>%
  ungroup()

FinalRelPlot <- ggplot() +
  geom_segment(data = PropPart, aes(x = MinAge, y = RelProps, xend = MaxAge, yend = RelProps, colour = Sex,
  ), linewidth = 1) +
  geom_point(data = FinalRelAge, aes(x = Age, y = RelProps, colour = Sex)) +
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100), limits = c(0,100)) +
  scale_y_continuous(breaks = c(0, .2, .4, .6, .8, 1), limits = c(0,1)) +
  labs(x = "Age (years)", y = "Proportion partnered") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#    ggsave(FinalRelPlot, file="~/Sync/PhD/ThesisVersions/PopSimArticle/FinalRelPlot.pdf", width = 12.25, height = 7.15, units = "in")










#####################################
# fixhours
#####################################

Original <- WorkingAdolescents %>%
  group_by(SchoolStatus, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# from the example in the function

Fixed <- OneGroup %>%
  group_by(SchoolStatus, HoursWorked) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

AllHoursValues <- Fixed %>%
  tidyr::expand(SchoolStatus, HoursWorked)

Fixed <- Fixed %>%
  right_join(AllHoursValues) %>%
  mutate(across(where(is.numeric), ~tidyr::replace_na(., 0)))



##########################
# graphs now placed inside the article
# this is the previous version that did the placement inside the R code
# also now have 2 graphs
##########################
library(cowplot)
library(ggplot2)

OriginalGraph <- ggplot(Original, aes(x=HoursWorked, y = freq,
                                      fill = SchoolStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
  coord_cartesian(ylim = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of 15-19 year olds",
       fill = "Person in school?") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
                   guide = guide_axis(angle = 90)) +
  theme(text = element_text(size = 18),
        plot.margin = unit(c(1,0,0,0), "cm"))

FixedGraph <- ggplot(Fixed, aes(x=HoursWorked, y = freq,
                                fill = SchoolStatus)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values=c("#5e3c99", "#fdb863")) +
  coord_cartesian(ylim = c(0, .8)) +
  labs(x="Hours worked per week", y = "Proportion of 15-19 year olds",
       fill = "Person in school?") +
  scale_x_discrete(labels= c("0", "1-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60+"),
                   guide = guide_axis(angle = 90)) +
  theme(text = element_text(size = 18),
        plot.margin = unit(c(1,0,0,0), "cm"))

BothPlots <- cowplot::plot_grid(OriginalGraph + theme(legend.position = "none"),
                                FixedGraph + theme(legend.position = "none"),
                                labels = c("Original", "Adjusted"),
                                label_size = 16,
                                align = "h",
                                hjust = -2)

BothPlotsLegend <- cowplot::get_legend(OriginalGraph +
                                         guides(color = guide_legend(nrow = 1)) +
                                         theme(legend.position = "bottom"))

BothPlotsFinal <- cowplot::plot_grid(BothPlots, BothPlotsLegend,
                                     ncol = 1, rel_heights = c(1, .1))

# ggsave(BothPlotsFinal, file="~/Sync/PhD/ThesisVersions/PopSimArticle/SchoolWorkHours.pdf")
# detach packages
# detach("package:cowplot", unload = TRUE)
# detach("package:ggplot2", unload = TRUE)
######################



# compare the non-parametric ordinal variation associations.
# demonstrate lack of link between school, work, age
# Ordered hours is being correctly assessed as an ordinal variable
# original data
cor.test(as.numeric(WorkingAdolescents$HoursWorked), as.numeric(WorkingAdolescents$SchoolStatus),
         method = "kendall")
# fixed data
cor.test(as.numeric(OneGroup$HoursWorked), as.numeric(OneGroup$SchoolStatus),
         method = "kendall",
         exact=FALSE)










#####################################
# pairnorm and onwards need to be redone
#####################################

####################################
# Graph and differences for the opposite sex
# NEEDS THE OUTPUT FROM THE CODE EXAMPLES FILE
#####################################

# now doing a combined graph for both examples
OppSexAgeDiffPlotValues1 <- Couples1 %>%
  group_by(HouseholdID) %>%
  arrange(desc(Sex), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age))),
         Source = "Normal") %>%
  filter(Sex == "Female")

OppSexAgeDiffPlotValues2 <- Couples2 %>%
  group_by(HouseholdID) %>%
  arrange(desc(Sex), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age))),
         Source = "Normal") %>%
  filter(Sex == "Female")

AgeDiffs1 <- ggplot(OppSexAgeDiffPlotValues1, aes (x = AgeDiff)) +
  geom_bar(fill = "#5e3c99") +
  xlim(-15, 15) +
  labs(x="Age difference, years, male age - female age", y = "Number of couples") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")
# ggsave(AgeDiffs1, file="~/Sync/PhD/Thesis2020/PopSimArticle/OppSexAgeDiffs1.pdf", width = 12.25, height = 7.15, units = "in")

AgeDiffs2 <- ggplot(OppSexAgeDiffPlotValues2, aes (x = AgeDiff)) +
  geom_bar(fill = "#fdb863") +
  xlim(-15,15) +
  labs(x="Age difference, years, male age - female age", y = "Number of couples") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

# ggsave(AgeDiffs2, file="~/Sync/PhD/Thesis2020/PopSimArticle/OppSexAgeDiffs2.pdf", width = 12.25, height = 7.15, units = "in")

# get quantile values
quantile(OppSexAgeDiffPlotValues1$AgeDiff)
quantile(OppSexAgeDiffPlotValues2$AgeDiff)



####################################
# Graph and differences parents and kids
# NEEDS THE OUTPUT FROM THE CODE EXAMPLES FILE
# from pairbeta4
#####################################
# graph the single child, 4 parameter beta age difference, children smaller

ParentKidDiffsCh1 <- ChildAllMatched$Matched %>%
  group_by(Household) %>%
  arrange(desc(Age), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age)))) %>%
  filter(AgeDiff > 0)


PKAgeDiffs1 <- ggplot(ParentKidDiffsCh1, aes (x = AgeDiff)) +
  geom_bar(fill = "#5e3c99") +
  labs(x="Age difference, parent age - child age", y = "Number of parent-child pairs") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgeDiffs1, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgeDiffs1.pdf", width = 12.25, height = 7.15, units = "in")


# plot the parent ages
ParentAges1 <- ChildAllMatched$Matched %>%
  group_by(Household) %>%
  filter(Age == max(Age))

PKAgesG1 <- ggplot(ParentAges1, aes (x = Age)) +
  geom_bar(fill = "#fdb863") +
  xlim(10, 70) +
  labs(x="Current age of parent", y = "Number of parents") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgesG1, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgesG1.pdf", width = 12.25, height = 7.15, units = "in")

# get numbers for the article
# age at childbirth
min(ParentKidDiffsCh1$AgeDiff)
max(ParentKidDiffsCh1$AgeDiff)
median(ParentKidDiffsCh1$AgeDiff)

#current age of parent
min(ParentAges1$Age)
max(ParentAges1$Age)
median(ParentAges1$Age)

# table of parent ages
table(ParentAges1$Age)


# graph the single child, 4 parameter beta age difference, children larger

ParentKidDiffsCh2 <- ChildAllMatched2$Matched %>%
  group_by(Household) %>%
  arrange(desc(Age), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age)))) %>%
  filter(AgeDiff > 0)


PKAgeDiffs2 <- ggplot(ParentKidDiffsCh2, aes (x = AgeDiff)) +
  geom_bar(fill = "#5e3c99") +
  labs(x="Age difference, parent age - child age", y = "Number of parent-child pairs") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgeDiffs2, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgeDiffs2.pdf", width = 12.25, height = 7.15, units = "in")


# plot the parent ages
ParentAges2 <- ChildAllMatched2$Matched %>%
  group_by(Household) %>%
  filter(Age == max(Age))

PKAgesG2 <- ggplot(ParentAges2, aes (x = Age)) +
  geom_bar(fill = "#fdb863") +
  xlim(10, 80) +
  labs(x="Current age of parent", y = "Number of parents") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgesG2, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgesG2.pdf", width = 12.25, height = 7.15, units = "in")

# get numbers for the article
# age at childbirth
min(ParentKidDiffsCh2$AgeDiff)
max(ParentKidDiffsCh2$AgeDiff)
median(ParentKidDiffsCh2$AgeDiff)

#current age of parent
min(ParentAges2$Age)
max(ParentAges2$Age)
median(ParentAges2$Age)

# table of parent ages
table(ParentAges2$Age)













# second, from child yes
#####################################
# graph the single child, skew normal age difference

ParentKidDiffsCh2 <- ChildMatchedID$Matched %>%
  group_by(HouseholdID) %>%
  arrange(desc(Age), .by_group = TRUE) %>%
  mutate(AgeDiff = -(Age - lag(Age, default = first(Age)))) %>%
  filter(AgeDiff > 0)


PKAgeDiffs2 <- ggplot(ParentKidDiffsCh2, aes (x = AgeDiff)) +
  geom_bar(fill = "#fdb863") +
  xlim(10, 70) +
  labs(x="Current age of parent", y = "Number of parents") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgeDiffs2, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgeDiffs2.pdf", width = 12.25, height = 7.15, units = "in")

# get age of parents now
ParentsID2 <- ChildMatchedID$Matched %>%
  filter(Relationship == "Partnered")

PKAgesG2 <-  ggplot(ParentsID2, aes (x = Age)) +
  geom_bar(fill = "#fdb863") +
  xlim(10,70) +
  labs(x="Current age of parent", y = "Number of parents") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.title = element_blank(),
        legend.text = element_text(size = 18),
        legend.position = "bottom")

#   ggsave(PKAgesG2, file="~/Sync/PhD/Thesis2020/PopSimArticle/PKAgesG2.pdf", width = 12.25, height = 7.15, units = "in")

rm(ParentKidDiffs, AgeDiffs, ParentsID, ParentAges)






####################################
# Graph and differences parents and kids
# multiple children households
# NEEDS THE OUTPUT FROM THE CODE EXAMPLES FILE
#####################################
# generic testing

Adultsnotmatched <- KidsMatched$Adults
Kidsnotmatched <- KidsMatched$Children
TheMatched <- KidsMatched$Matched

Inboth <- inner_join(Adultsnotmatched, KidsMatched$Matched)

# doubled up parents?
TestIDSize <- KidsMatched$Matched %>%
  group_by(ID) %>%
  summarise(numinid = n())

TestHHSize <- KidsMatched$Matched %>%
  group_by(HouseholdID) %>%
  summarise(numinhouse = n())

rm(Adultsnotmatched, Kidsnotmatched, TheMatched, Inboth, TestIDSize, TestHHSize)

# Project
# Carolyn Dapper


library("rio")
library("dplyr")
library("stringr")
library("ggplot2")
library("stargazer")



# Data:
# U.S. Court of Appeals Database
# University of South Carolina
# Phase one: 1925-1996

db <- import("data/cta96_stata.dta")


########################## TABLE OF CONTENTS #############################

## 1: Strings
## 2: Variables
## 3: Datasets 
## 4: Plots
## 5: Tests


#################################### 1 ###################################
################################# STRINGS ################################

## Create strings for appel1, appel2, respond1, respond2, isolating demographic info

db <- mutate(db,
             appel1_string <- as.character(appel1),
             appel1_type = as.integer(str_sub(appel1_string, 1, 1)),
             appel1_gender = as.integer(str_sub(appel1_string, 2, 2)),
             appel1_race = as.integer(str_sub(appel1_string, 3, 3)),
             appel1_citizen = as.integer(str_sub(appel1_string, 4, 4)),
             appel1_income = as.integer(str_sub(appel1_string, 5, 5)))

db <- mutate(db,
             appel2_string <- as.character(appel2),
             appel2_type = as.integer(str_sub(appel2_string, 1, 1)),
             appel2_gender = as.integer(str_sub(appel2_string, 2, 2)),
             appel2_race = as.integer(str_sub(appel2_string, 3, 3)),
             appel2_citizen = as.integer(str_sub(appel2_string, 4, 4)),
             appel2_income = as.integer(str_sub(appel2_string, 5, 5)))

db <- mutate(db,
             respond1_string <- as.character(respond1),
             respond1_type = as.integer(str_sub(respond1_string, 1, 1)),
             respond1_gender = as.integer(str_sub(respond1_string, 2, 2)),
             respond1_race = as.integer(str_sub(respond1_string, 3, 3)),
             respond1_citizen = as.integer(str_sub(respond1_string, 4, 4)),
             respond1_income = as.integer(str_sub(respond1_string, 5, 5)))

db <- mutate(db,
             respond2_string <- as.character(respond2),
             respond2_type = as.integer(str_sub(respond2_string, 1, 1)),
             respond2_gender = as.integer(str_sub(respond2_string, 2, 2)),
             respond2_race = as.integer(str_sub(respond2_string, 3, 3)),
             respond2_citizen = as.integer(str_sub(respond2_string, 4, 4)),
             respond2_income = as.integer(str_sub(respond2_string, 5, 5)))

################################## 2 ####################################
############################## VARIABLES ################################

# database with only natural persons as respondents or appellants (database_naturalpersons)

db_np <- db[db$appel1_type == 7 | 
              db$appel2_type == 7 | 
              db$respond1_type == 7 | 
              db$respond2_type == 7, ]

# remove observations without ascertained decision

db_np <- db_np[!is.na(db_np$treat) &
                 db_np$treat != 10, ]

# group and relabel race

db_np$appel1_rlabel <- 0
db_np$appel1_rlabel[db_np$appel1_race == 1] <- "Caucasian"
db_np$appel1_rlabel[db_np$appel1_race == 2] <- "Black"
db_np$appel1_rlabel[db_np$appel1_race == 3 | 
                        db_np$appel1_race == 4] <- "Native American"
db_np$appel1_rlabel[db_np$appel1_race == 5 | 
                        db_np$appel1_race == 6] <- "Asian"
db_np$appel1_rlabel[db_np$appel1_race == 7 | 
                        db_np$appel1_race == 8] <- "Hispanic"

# group and relabel income

db_np$appel1_ilabel <- 0
db_np$appel1_ilabel[db_np$appel1_income == 1 | 
                        db_np$appel1_income == 2] <- "Poor"
db_np$appel1_ilabel[db_np$appel1_income == 3 | 
                        db_np$appel1_income == 4] <- "Wealthy"
db_np$appel1_ilabel[db_np$appel1_income == 5] <- "Middle"

# group and relabel counsel

db_np$counsel_lab <- 0
db_np$counsel_lab[db_np$counsel1 == 1] <- "None"
db_np$counsel_lab[db_np$counsel1 == 2 | db_np$counsel1 == 3] <- "Court Appointed or Public Defender"
db_np$counsel_lab[db_np$counsel1 == 4] <- "Private"
db_np$counsel_lab[db_np$counsel1 == 5 | db_np$counsel1 == 6] <- "Government"
db_np$counsel_lab[db_np$counsel1 == 7] <- "Interest Group"
db_np$counsel_lab[db_np$counsel1 == 8] <- 0

# group and relabel decision
# variable  excludes "in part" decisions

db_np$decision <- 0
db_np$decision[db_np$treat == 1 | 
                  db_np$treat == 8] <- "Affirmed"
db_np$decision[db_np$treat == 2 | 
                  db_np$treat == 3 | 
                  db_np$treat == 4 | 
                  db_np$treat == 7] <- "Reversed"

db_np <- db_np[db_np$decision != 0, ]



################################## 3 ################################## 
############################### DATASETS ##############################

####################################
# database with race and income ascertained

db_npri <- db_np[db_np$appel1_race != 0 & 
                   db_np$appel1_race != 9 & 
                   db_np$appel1_income != 0 , ]

# remove cases with missing values

db_npri <- db_npri[!is.na(db_npri$appel1_income) | !is.na(db_npri$appel1_race), ]
db_npri <- db_npri[db_npri$appel1_race != 0 & 
                     db_npri$appel1_race != 9 & 
                     db_npri$appel1_income != 0 &
                     db_npri$appel1_gender != 0, ]

db_npri <- db_npri[db_npri$appel1_ilabel != 0, ]
db_npri <- db_npri[db_npri$appel1_rlabel != 0, ]


####################################
# database with only ascertained race (not income)

db_npr <- db_np[db_np$appel1_race != 0 & 
                   db_np$appel1_race != 9 &
                  db_np$treat != 10 &
                  db_np$treat != 0, ]

db_npr[db_npr %in% c(0, 9)] <- NA

db_npr <- db_npr[!is.na(db_npr$appel1_race), ]
db_npr <- db_npr[db_npr$appel1_rlabel != 0, ]


####################################
# dataset with ascertained income (not race)

db_npi <- db_np
db_npi <- db_npi[db_npi$appel1_income != 0, ]
db_npi <- db_npi[!is.na(db_npi$appel1_income), ]

db_npi <- db_npi[db_npi$appel1_ilabel!= 0, ]


####################################
# dataset with income, race, and decision ascertained (excludes "in part" decisions)

db_reg <- db_npri
db_reg <- db_reg[db_reg$treat != 5 & db_reg$treat != 6 & db_reg$treat != 0, ]

db_reg <- db_reg[db_reg$counsel1 != 8, ]


# reversed dummy
# 1 = reversed
db_reg$reversed_dum <- 0
db_reg$reversed_dum[db_reg$treat == 1 | 
                      db_reg$treat == 8] <- 0
db_reg$reversed_dum[db_reg$treat == 2 | 
                      db_reg$treat == 3 | 
                      db_reg$treat == 4 | 
                      db_reg$treat == 7] <- 1

############################## 4 ##############################
############################ Plots ############################

########################
# all decisions v. race
# with race dataset

decision_plot_race <- ggplot(db_npr, 
                             aes(x = appel1_rlabel, 
                                 fill = decision, 
                                 y = (((..count..)/sum(..count..)) * 100))) +
  geom_bar(position = "dodge") +
  labs(x = "Racial Groups", y = "Percent of Appellants") +
  scale_fill_discrete(guide = guide_legend(title = "Decision"))

decision_plot_race

#######################
# all decisions v. income
# income only dataset

decision_plot_income <- ggplot(db_npi, 
                              aes(x = appel1_ilabel, 
                                  fill = decision, 
                                  y = (((..count..)/sum(..count..)) * 100))) +
  geom_bar(position = "dodge") +
  labs(x = "Income Level", y = "Percent of Appellants") +
  scale_fill_discrete(guide = guide_legend(title = "Decision")) 

decision_plot_income

################################## 5 ####################################
################################ Tests ################################## 

############################
# income difference in proportion test

table(db_npi$appel1_ilabel, db_npi$decision)

# proportion of poor losing:winning to everyone else

poor_A <- 1433
n_A <- 3014

poor_R <- 671
n_R <- 1422

prop.test(c(poor_A, poor_R), c(n_A, n_R))

# proportion of middle class losing:winning to everyone else

middle_A <- 1198
middle_R <- 488

prop.test(c(middle_A, middle_A), c(n_A, n_R))

# proportion of wealthy losing:winning to everyone else

wealthy_A <- 383
wealthy_R <- 263

prop.test(c(wealthy_A, wealthy_R), c(n_A, n_R))

#############################
# race difference in proportion test

table(db_npr$appel1_rlabel, db_npr$decision)

white_A <- 35
n_A <- 877

white_R <- 18
n_R <- 427

prop.test(c(white_A, white_R), c(n_A, n_R))

# black

black_A <- 118
black_R <- 83

prop.test(c(black_A, black_R), c(n_A, n_R))

table(db_npi$appel1_ilabel, db_npi$decision)

# asian

asian_A <- 174
asian_R <- 85

prop.test(c(asian_A, asian_R), c(n_A, n_R))

# hispanic

hispanic_A <- 431
hispanic_R <- 186

prop.test(c(hispanic_A, hispanic_R), c(n_A, n_R))

# native american

namerican_A <- 119
namerican_R <- 55

prop.test(c(namerican_A, namerican_R), c(n_A, n_R))


#################################### 6 ###################################
############################### Regressions ##############################

# ethnicity into factor variable

db_reg$ethnicity <- factor(db_reg$appel1_rlabel, 
                           labels = c("Asian", "Black", "Caucasian", "Hispanic", "Native American"))
db_reg$ethnicity <- relevel(db_reg$ethnicity, ref = 3)


# counsel into factor variable

db_reg$counsel_type <- factor(db_reg$counsel_lab,
                              labels = c("Court Appointed or Public Defender",
                              "Government", "Interest Group", "None", "Private"))

db_reg$counsel_type <- relevel(db_reg$counsel_type, ref = 4)

# income into factor variable

db_reg$income <- factor(db_reg$appel1_ilabel,
                        labels = c("Poor", "Wealthy", "Middle"))

db_reg$income <- relevel(db_reg$income, ref = 3)

# race regression

race_reg <- lm(data = db_reg,
               formula = reversed_dum ~ ethnicity)

summary(race_reg)

# income regression

income_reg <- lm(data = db_reg,
                 formula = reversed_dum ~ income)

summary(income_reg)

# counsel regression

counsel_reg <- lm(data = db_reg,
                  formula = reversed_dum ~ counsel_type)

summary(counsel_reg)

# all of them regression


reg <- lm(data = db_reg, 
          formula = reversed_dum ~ ethnicity + income + counsel_type)

summary(reg)


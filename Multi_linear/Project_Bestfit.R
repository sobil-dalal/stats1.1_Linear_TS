setwd("/Users/sobil/Documents/MSC/Sem 1/Statistics for Data Analytics/Lab/Project/Multi_linear")
remove(list = ls())

#
cellular <- read.csv("Cellular.csv", stringsAsFactors = FALSE)
cellular <- cellular[,-c(2,4)]

#
other <- read.csv("Gross national income per capita (PPP int. $).csv", stringsAsFactors = FALSE)
other <- other[,-c(2,4)]
cellular <- merge(cellular, other, by = "Country.or.Area")
colnames(cellular) <- c("Country.or.Area","cellular","income")

#
other <- read.csv("Population_medain_age.csv", stringsAsFactors = FALSE)
other <- other[,-c(2,4)]
cellular <- merge(cellular, other, by = "Country.or.Area")
colnames(cellular) <- c("Country.or.Area","cellular","income","popul.med.age")

#
other <- read.csv("Population_Urban.csv", stringsAsFactors = FALSE)
other <- other[,-c(2,4)]
cellular <- merge(cellular, other, by = "Country.or.Area")
colnames(cellular) <- c("Country.or.Area","cellular","income","popul.med.age","popul.urban")

#
other <- read.csv("PopulationInThousand.csv", stringsAsFactors = FALSE)
other <- other[,-c(2,4)]
cellular <- merge(cellular, other, by = "Country.or.Area")
colnames(cellular) <- c("Country.or.Area","cellular","income","popul.med.age","popul.urban","popul.in.thous")

cellular <- cellular[,-1]

# checking distribution & outliers
summary(cellular)
boxplot(cellular$cellular) # no outliers
hist(cellular$cellular) # normally distributed

# checking correlation
library(psych)
psych::pairs.panels(cellular)

# Concept of PARSIMONY 
# checking the best fit predictors
library(leaps)
library(car)
bstFits1 <- regsubsets(cellular ~ 
                         income*popul.med.age*popul.urban*popul.in.thous + 
                         I(income^2) + I(popul.med.age^2) + I(popul.urban^2) + I(popul.in.thous^2), 
                       data = cellular, nbest = 1, nvmax = 4)
par(mfrow = c(1,1))
subsets(bstFits1, statistic = "adjr2")
plot(bstFits1, scale = "adjr2")

# analysing the bstFits1 suggest - lm(formula = cellular ~ popul.med.age + popul.urban + popul.med.age:popul.urban, data = cellular) model






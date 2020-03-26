setwd("/Users/sobil/Documents/MSC/Sem 1/Statistics for Data Analytics/Lab/Project/Multi_linear")
remove(list = ls())

library(psych)
library(leaps)
library(car)
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
str(cellular)

sapply(cellular, sd, na.rm = TRUE)
cat("Size : ", nrow(cellular) , " obs. of " , length(cellular), "variables")

hist(cellular$cellular) # normally distributed

par(mfrow = c(1,2))
hist(cellular$cellular, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Cellular subscription",
     main = "Histogram")
lines(density(cellular$cellular), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")
boxplot(cellular$cellular, main = "Box plot", ylab = "Cellular subscription") # no outliers

# checking correlation
psych::pairs.panels(cellular)

# Model 1
cellular.fit1 <- lm(cellular ~ income*popul.med.age*popul.urban*popul.in.thous + I(income^2) + I(popul.med.age^2) + I(popul.urban^2) + I(popul.in.thous^2), data = cellular)
summary(cellular.fit1)

# Using step process
cellular.fit2 <- step(cellular.fit1)
summary(cellular.fit2)

# Manually eliminating insignificant variable (concept of parsimony)
cellular.fit3 <- update(cellular.fit2, ~ . - income:popul.med.age:popul.urban)
summary(cellular.fit3)

# Using step process
cellular.fit4 <- step(cellular.fit3)
summary(cellular.fit4)

# Manually eliminating insignificant variable (concept of parsimony)
cellular.fit5 <- update(cellular.fit4, ~ . - popul.med.age:popul.in.thous)
summary(cellular.fit5)

# Using step process
cellular.fit6 <- step(cellular.fit5)
summary(cellular.fit6)

# Manually eliminating insignificant variable (concept of parsimony)
cellular.fit7 <- update(cellular.fit6, ~ . - income:popul.med.age)
summary(cellular.fit7)

# Using step process
cellular.fit8 <- step(cellular.fit7)
summary(cellular.fit8)

# Manually eliminating insignificant variable (concept of parsimony)
cellular.fit9 <- update(cellular.fit8, ~ . - popul.in.thous )
summary(cellular.fit9)

# checking plot
par(mfrow =c(2,2))
plot(cellular.fit9)

# checking statstics
# F-statistic: 
# Gauss– Markov Assumptions - Errors have constant variance, which is known as homoscedasticity
ncvTest(cellular.fit9)
# There is no autocorrelation between errors :: 
durbinWatsonTest(cellular.fit9)
# Predictor variables must be independent of the error term (Omitted variable bias!) :: 
# We assume that our errors are normally distributed :: From Q-Q plot and histogram
par(mfrow = c(1,1))

hist(cellular.fit9$residuals, # histogram
     col="peachpuff", # column color
     border="black",
     prob = TRUE, # show densities instead of frequencies
     xlab = "Residuals",
     main = "Residuals Normality check")
lines(density(cellular.fit9$residuals), # density plot
      lwd = 2, # thickness of line
      col = "chocolate3")

hist(cellular.fit9$residuals) # is near to normal distribution
# We assume there is no multicollinearity between predictors ::
vif(cellular.fit9)
# The third assumption we make is that we have no influential data points :: 
cooks.distance(cellular.fit9)
influencePlot(cellular.fit9)

# dropping influential/ problamatic variables i.e. 76,78,90
cellular <- cellular[- c(76,78,90),]
# setting the index of the rows in seq
row.names(cellular) <- 1:159


# again the cellular.fit9
cellular.fit9 <- lm(formula = cellular ~ popul.med.age + popul.urban + popul.med.age:popul.urban, 
                    data = cellular)
summary(cellular.fit9)

# checking plot
par(mfrow =c(2,2))
plot(cellular.fit9)

# checking statstics
# F-statistic: 
# Gauss– Markov Assumptions - Errors have constant variance, which is known as homoscedasticity
ncvTest(cellular.fit9)
# There is no autocorrelation between errors :: 
durbinWatsonTest(cellular.fit9)
# Predictor variables must be independent of the error term (Omitted variable bias!) :: 
# We assume that our errors are normally distributed :: From Q-Q plot and histogram
par(mfrow = c(1,1))
hist(cellular.fit9$residuals) # is near to normal distribution
# We assume there is no multicollinearity between predictors ::
vif(cellular.fit9)
# The third assumption we make is that we have no influential data points :: 
summary(cooks.distance(cellular.fit9))
influencePlot(cellular.fit9)

# after removing these points, the statistics and plot became poor to original.


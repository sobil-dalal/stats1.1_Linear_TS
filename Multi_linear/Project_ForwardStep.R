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

# Primary model with intercept only
cellular.fit1 <- lm(cellular ~ . , data = cellular)
summary(cellular.fit1)

# Using step forward
cellular.fit2 <- step(cellular.fit1, scope = . ~ .^2 ~ .^3, direction = "forward")
summary(cellular.fit2)

# Manually eliminating insignificant variable (concept of parsimony)
cellular.fit3 <- update(cellular.fit2, ~ . - income:popul.med.age)
summary(cellular.fit3)

# Using step process
cellular.fit4 <- step(cellular.fit3)
summary(cellular.fit4)

# Manually eliminating insignificant variable (concept of parsimony)
cellular.fit5 <- update(cellular.fit4, ~ . - popul.in.thous)
summary(cellular.fit5)

# got similar coefficients

par(mfrow = c(2,2))
plot(cellular.fit1)

# checking statstics
# F-statistic: 
# Gauss– Markov Assumptions - Errors have constant variance, which is known as homoscedasticity
ncvTest(cellular.fit1)
# There is no autocorrelation between errors :: 
durbinWatsonTest(cellular.fit1)
# Predictor variables must be independent of the error term (Omitted variable bias!) :: 
# We assume that our errors are normally distributed :: From Q-Q plot and histogram
par(mfrow = c(1,1))
hist(cellular.fit1$residuals) # is near to normal distribution
# We assume there is no multicollinearity between predictors ::
vif(cellular.fit1)
# The third assumption we make is that we have no influential data points :: 
cooks.distance(cellular.fit1)
influencePlot(cellular.fit1)
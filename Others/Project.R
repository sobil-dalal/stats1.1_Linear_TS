remove(list = ls())

# importing the data from CSV (raw)
data <- read.csv("/Users/sobil/Documents/MSC/Sem 1/Statistics for Data Analytics/Lab/Project/GDP.csv", stringsAsFactors = FALSE)

# checking summary and structure of data
str(data)
summary(data)

# creating a new data set with dependent and independent predictor variables
# Change in inventory = Gross capital formation - Gross fixed capital formation (including Acquisitions less disposals of valuables) :: thus removing Change in Inventory column (correlation coefficient is small)
# General government final consumption expenditure = Final consumption expenditure - Household consumption expenditure (including Non-profit institutions serving households) :: will check 2 columns and 1 columns indivally (as both combination has good correlation)
gdp <- data.frame(F.cons.expend = data$Final.consumption.expenditure, 
                  House.cons = data$Household.consumption.expenditure..including.Non.profit.institutions.serving.households.,
                  Gross.cap.form = data$Gross.capital.formation,
                  Gross.fix.cap.form = data$Gross.fixed.capital.formation..including.Acquisitions.less.disposals.of.valuables.,
                  Exp.goods.services = data$Exports.of.goods.and.services,
                  Imp.goods.services = data$Imports.of.goods.and.services,
                  GDP =data$Gross.Domestic.Product..GDP.)
str(gdp)
summary(gdp)

gdp.boxplot <- boxplot(gdp$GDP) # looking at the box plot,we have positive outliers
gdp.boxplot$out

# removing the outliers
gdp<- subset(gdp,! gdp$GDP  %in% gdp.boxplot$out)
gdp.boxplot <- boxplot(gdp$GDP) # looking at the box plot,we have positive outliers
gdp.boxplot$out
gdp<- subset(gdp,! gdp$GDP  %in% gdp.boxplot$out)
gdp.boxplot <- boxplot(gdp$GDP) # looking at the box plot,we have positive outliers
gdp.boxplot$out

# exploratory analysis
plot(gdp$GDP) # scatter plot is almost random
hist(gdp$GDP) # Plot is postively skewed and is not normally distributed

# checking the realtionship between variables
library(psych)
pairs.panels(gdp) # suggests, variables are highly correalted to GDP individually

# Primary model - without making any changes
gdp.fit1 <- lm(GDP~.,data = gdp)
summary(gdp.fit1)
par(mfrow = c(2,2))
plot(gdp.fit1) # Normal Q-Q plot : Errors are not normally distributed : which gives us more evidence for suggesting that the dependent value is not normally distributed

# Preliminary analyses were performed to ensure no violation of the assumptions of normality, linearity and homoscedasticity. 
# Now trying sqare root of GDP column for normal distribution
par(mfrow = c(1,1))
hist((1/gdp$GDP))
hist(sqrt(gdp$GDP)) # By taking square-root of GDP - got near to normal distribution

# Model2
gdp.fit2 <- lm(formula = sqrt(GDP) ~ ., data = gdp)
summary(gdp.fit2)
par(mfrow = c(2,2))
plot(gdp.fit2) # Normal Q-Q plot : Errors are approximatelly -normally distributed

# Model 3 : applying backward step 
gdp.fit3<- step(gdp.fit2)
summary(gdp.fit3)
par(mfrow = c(2,2))
plot(gdp.fit3)

# Concept of PARSIMONY 
# checking the best fit predictors
library(leaps)
library(car)
bstFits1 <- regsubsets(sqrt(GDP) ~ . + sqrt(F.cons.expend)*sqrt(House.cons)*sqrt(Gross.cap.form)*sqrt(Gross.fix.cap.form)*sqrt(Exp.goods.services)*sqrt(Imp.goods.services) , data = gdp, nbest = 2, nvmax = 6, really.big = TRUE)
par(mfrow = c(1,1))
subsets(bstFits1, statistic = "adjr2")

# after checking above subset from 3 and above are almost similar
bstFits2 <- regsubsets(sqrt(GDP) ~ . + sqrt(F.cons.expend)*sqrt(House.cons)*sqrt(Gross.cap.form)*sqrt(Gross.fix.cap.form)*sqrt(Exp.goods.services)*sqrt(Imp.goods.services) , data = gdp, nbest = 1, nvmax = 3, really.big = TRUE)
par(mfrow = c(1,1))
subsets(bstFits2, statistic = "adjr2", max.size = 5)

# setting the index of the rows in seq
row.names(gdp) <- 1:173

# Model4
gdp.fit4 <- lm(formula = sqrt(GDP) ~ sqrt(F.cons.expend) + sqrt(Gross.cap.form):sqrt(Exp.goods.services) + sqrt(F.cons.expend):sqrt(Imp.goods.services), data = gdp)
summary(gdp.fit4)
par(mfrow = c(2,2))
plot(gdp.fit4)

# checking statstics
# F-statistic: 7.307e+04 on 3 and 160 DF,  p-value: < 2.2e-16 :: therefore, egression model is significantly better at predicting values of the outcome than using the mean.
# Gauss– Markov Assumptions - Errors have constant variance, which is known as homoscedasticity
ncvTest(gdp.fit4)
# There is no autocorrelation between errors :: 
durbinWatsonTest(gdp.fit4)
# Predictor variables must be independent of the error term (Omitted variable bias!) :: 
# We assume that our errors are normally distributed :: From Q-Q plot and histogram
par(mfrow = c(1,1))
hist(gdp.fit4$residuals) # is near to normal distribution
# We assume there is no multicollinearity between predictors ::
vif(gdp.fit4)
# The third assumption we make is that we have no influential data points :: 
cooks.distance(gdp.fit4)
influencePlot(gdp.fit4, scale = 3, main = "gdp.fit4")

# dropping influential/ problamatic variables i.e. 76, 96, 150, 120, 127
gdp <- gdp[- c(76, 96, 150, 120, 127),]
# setting the index of the rows in seq
row.names(gdp) <- 1:168

# again applying the same model to new data set
# Model5
gdp.fit5 <- lm(formula = sqrt(GDP) ~ sqrt(F.cons.expend) + sqrt(Gross.cap.form):sqrt(Exp.goods.services) + sqrt(F.cons.expend):sqrt(Imp.goods.services), data = gdp)
summary(gdp.fit5)
par(mfrow = c(2,2))
plot(gdp.fit5) 

# dropping influential/ problamatic variables i.e. 84, 94, 126, 150
gdp <- gdp[- c(84, 94, 126, 150),]
# setting the index of the rows in seq
row.names(gdp) <- 1:164

# again applying the same model to new data set
# Model6
gdp.fit6 <- lm(formula = sqrt(GDP) ~ sqrt(F.cons.expend) + sqrt(Gross.cap.form):sqrt(Exp.goods.services) + sqrt(F.cons.expend):sqrt(Imp.goods.services), data = gdp)
summary(gdp.fit6)
par(mfrow = c(2,2))
plot(gdp.fit6)

# dropping influential/ problamatic variables i.e. 23, 35, 36, 143
gdp <- gdp[- c(23, 35, 36, 143),]
# setting the index of the rows in seq
row.names(gdp) <- 1:160

# again applying the same model to new data set
# Model7
gdp.fit7 <- lm(formula = sqrt(GDP) ~ sqrt(F.cons.expend) + sqrt(Gross.cap.form):sqrt(Exp.goods.services) + sqrt(F.cons.expend):sqrt(Imp.goods.services), data = gdp)
summary(gdp.fit7)
par(mfrow = c(2,2))
plot(gdp.fit7)

# checking statstics
# F-statistic: 7.307e+04 on 3 and 160 DF,  p-value: < 2.2e-16 :: therefore, egression model is significantly better at predicting values of the outcome than using the mean.
# Gauss– Markov Assumptions - Errors have constant variance, which is known as homoscedasticity
ncvTest(gdp.fit7)
# There is no autocorrelation between errors :: 
durbinWatsonTest(gdp.fit7)
# Predictor variables must be independent of the error term (Omitted variable bias!) :: 
# We assume that our errors are normally distributed :: From Q-Q plot and histogram
par(mfrow = c(1,1))
hist(gdp.fit7$residuals) # is near to normal distribution
# We assume there is no multicollinearity between predictors ::
vif(gdp.fit7)
# The third assumption we make is that we have no influential data points :: 
cooks.distance(gdp.fit7)










remove(list = ls())

# reading the gdp file
data <- read.csv("/Users/sobil/Documents/MSC/Sem 1/Statistics for Data Analytics/Lab/Project/Time-Series/GDP_Ireland.csv")
str(data)
summary(data)

# converting the predictor to billion figures
data$Gross.Domestic.Product..GDP. <- data$Gross.Domestic.Product..GDP./1000000000
summary(data)

# sorting the data
data <- data[order(data$Year),]
row.names(data) <- 1:49

# creating time series data and analysing
gdp <- ts(data = data$Gross.Domestic.Product..GDP., start = 1970, end = 2018, frequency = 1)

par(mfrow = c(2,2))
# checking the plots
plot(gdp, main = "Normal")
library(fpp2)
# smotthing the plot for checking Moving average
plot(ma(gdp,3), main = "q = 3")
plot(ma(gdp,5), main = "q = 5")
plot(ma(gdp,7), main = "q = 7")



# Models
# Simple Exponential Model 1 (Only level)- 
par(mfrow = c(1,1))
gdp.1.ses <- ses(gdp, h = 3)
gdp.1.ses
round(accuracy(gdp.1.ses),3)
autoplot(gdp.1.ses)
autoplot(gdp.1.ses,) + autolayer(fitted(gdp.1.ses), series = "Fitted")

gdp.1.ets <- ets(gdp,model = "ANN")
gdp.1.ets
forecast(gdp.1.ets, h = 3)
round(accuracy(gdp.1.ets),3)

# Holts Linear Trend Model 2 (level + trend)- 
gdp.2.holt <- holt(gdp, h = 3)
gdp.2.holt
round(accuracy(gdp.2.holt),3)
autoplot(gdp.2.holt)
autoplot(gdp.2.holt,) + autolayer(fitted(gdp.2.holt), series = "Fitted")

gdp.2.ets <- ets(gdp,model = "AAN")
gdp.2.ets
forecast(gdp.2.ets, h = 3)
round(accuracy(gdp.2.ets),3)

# Holts Model with damped Trend Model 3 (level + trend + damped)- 
gdp.3.holt.d <- holt(gdp, h = 3, damped = TRUE, PI=FALSE)
gdp.3.holt.d
round(accuracy(gdp.3.holt.d),3)
autoplot(gdp.3.holt.d)
autoplot(gdp.3.holt.d,) + autolayer(fitted(gdp.3.holt.d), series = "Fitted")

# ETS method Model
gdp.ets <- ets(gdp, model = "ZZZ")
gdp.ets
forecast(gdp.ets, h = 3)
round(accuracy(gdp.ets),3)
autoplot(gdp.ets) + autolayer(fitted(gdp.ets), series = "Fitted")





autoplot(gdp)

# checking best number of difference in time series
ndiffs(gdp) # 2

# diff = 1
dgdp <- diff(gdp)
autoplot(dgdp)
# checking stationarity
library(tseries)
adf.test(dgdp) # not stationary

# diff = 2
ddgdp <- diff(gdp, lag = 2)
autoplot(ddgdp)
# checking stationarity
adf.test(ddgdp) # stationary
# thus d = 2



# Chossing p and q value
ggtsdisplay(ddgdp)

# Model 1
ddgdp.1 <- arima(x = gdp, order = c(1,2,0))
ddgdp.1
round(accuracy(ddgdp.1),3)

# Model 2
ddgdp.2 <- arima(x = gdp, order = c(4,2,0))
ddgdp.2
round(accuracy(ddgdp.2),3) #####-------BEST---------############

# Checking auto ARIMA
ddgdp.3.auto <- auto.arima(gdp)
ddgdp.3.auto
round(accuracy(ddgdp.3.auto),3)



# Evaluating model
qqnorm(ddgdp.2$residuals,main = "Arima (p,d,q) = (4,2,0)")
qqline(ddgdp.2$residuals)
Box.test(x= ddgdp.2$residuals, type="Ljung-Box")







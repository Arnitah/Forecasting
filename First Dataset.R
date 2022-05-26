###############REFERENCE###########################################################################
###############GitHub - Stat-Wizards/Forcasting-A-Time-Series-Stock-Market-Data:  #################
###A Comparative Analysis of a Real Close-price Stock Market Data with The Classical Time Series,##
################Machine Learning and a Single Layered Neural Network Model.########################
#########https://github.com/Stat-Wizards/Forcasting-A-Time-Series-Stock-Market-Data################

# Libraries
library(highcharter)
library(dplyr)
library(prophet)
library(lubridate)
library(ggplot2)
library(quantmod)
library(rugarch)
library(tseries)
library(forecast)
library(tsfknn)


#data extraction and stock market indicators
setwd("C:/Users/eomuvwie/OneDrive - IESEG/Forecasting/Project") 

stock <- getSymbols("MSFT", src="yahoo", auto.assign=FALSE, from="2015-01-01", to="2022-04-30")
date = index(stock)
date = as.Date(date)
head(stock)


# Candle stick chart
chartSeries(stock, theme = "white", name = "MSFT 2015-2022", TA= NULL)
chartSeries(stock,theme = "white",TA=c(addVo(),addBBands(),addMACD()))


#ADF TEST 
print(adf.test(stock$MSFT.Close))

#Plot ACF and PACF
par(mfrow = c(1, 2))
acf(stock$MSFT.Close)
pacf(stock$MSFT.Close)
par(mfrow = c(1, 1))


## Applying auto.arima() to the dataset 
modelfit <-auto.arima(stock$MSFT.Close, lambda = "auto")
summary(modelfit)

# Diagnostics on Residuals
plot(resid(modelfit),ylab="Residuals",main="Residuals(Arima(5,1,2)) vs. Time")

# Histogram of Residuals & Normality Assumption
hist(resid(modelfit),freq=12,ylim=c(0,9500),main="Histogram of Residuals")
e=resid(modelfit)
curve(dnorm(x, mean=mean(e), sd=sd(e)), add=TRUE, col="darkred")

# Diagnostics tests for Arima
tsdiag(modelfit)

# Box test for lag=2
Box.test(modelfit$residuals, lag= 2, type="Ljung-Box")
Box.test(modelfit$residuals, type="Ljung-Box")
plot(as.ts(stock$MSFT.Close))
lines(modelfit$fitted,col="red")

#Dataset forecasting  for the  next  60  days
price_forecast <- forecast(modelfit,h=60)
plot(price_forecast)
head(price_forecast$mean)
head(price_forecast$upper)
head(price_forecast$lower)

#Dividing the data into train & test sets , applying the model
N = length (stock$MSFT.Close)
n = 0.8*N
train = stock$MSFT.Close[1:n, ]
test = stock$MSFT.Close[(n+1):N,]
trainarimafit <- auto.arima(train$MSFT.Close ,lambda= "auto")
summary(trainarimafit)
predlen= length(test)
trainarima_fit <- forecast(trainarimafit, h= predlen)

#Plotting mean predicted  values vs real data
meanvalues<- as.vector(trainarima_fit$mean)
precios <- as.vector(test$MSFT.Close)
plot(meanvalues, type = "l",col="red")
lines(precios, type = "l")

#GARCH Model Forecasting
#Dataset forecast upper first 5 values using GARCH
fitarfima = autoarfima(data = stock$MSFT.Close, ar.max = 5, 
                       ma.max = 2,criterion = "AIC", method = "full")
fitarfima$fit
#define the model
garch11closeprice=ugarchspec(variance.model=list(garchOrder=c(1,1)),
                             mean.model=list(armaOrder=c(3,2)))
#estimate model 
garch11closepricefit=ugarchfit(spec=garch11closeprice, data=stock$MSFT.Close)

#conditional volatility plot
plot.ts(sigma(garch11closepricefit), ylab="sigma(t)", col="blue")

#Model akike 
infocriteria(garch11closepricefit)
#Normal residuals
garchres <- data.frame(residuals(garch11closepricefit))
plot(garchres$residuals.garch11closepricefit)

#Standardized residuals
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE))
#Normal Q plot
qqnorm(garchres$residuals.garch11closepricefit,standardize=TRUE)
qqline(garchres$residuals.garch11closepricefit,standardize=TRUE)

#Squared standardized residuals Ljung Box
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE)^2)
Box.test(garchres$residuals.garch11closepricefit..standardize...TRUE..2, type="Ljung-Box")

#GARCH Forecasting
garchforecast <- ugarchforecast(garch11closepricefit, n.ahead = 60)
plot(garchforecast) #Selection: 1

## PROPHET Forecasting 
##Dataframe creation and model application 
df <- data.frame(ds = index(stock),
                 y = as.numeric(stock[,4]))
prophet_pred = prophet(df, daily.seasonality = TRUE)
future = make_future_dataframe(prophet_pred,periods=60)
fcastprophet = predict(prophet_pred,future)

#Creating train prediction dataset to compare real data
dataprediction = data.frame(fcastprophet$ds,fcastprophet$yhat)
trainlen = length(stock$MSFT.Close)
dataprediction = dataprediction[c(1:trainlen),]

#Visualizing train prediction vs real data
p= ggplot()+
  geom_smooth(aes(x= dataprediction$fcastprophet.ds,method = 'gam',y=stock$MSFT.Close),
              colour="blue",level=0.99,fill="#69b3a2",se=T)+
  geom_point(aes(x= dataprediction$fcastprophet.ds,y=dataprediction$fcastprophet.yhat))+
  xlab("ds")+
  ylab("y= MSFT.Close")+
  ggtitle("Training Prediction vs. Real Data:Prophet")
p

#Creating Cross Validation
accuracy(dataprediction$fcastprophet.yhat,df$y)
prophet_plot_components(prophet_pred,fcastprophet)

#KNN regression Time Series Forcasting 
#Dataframe creation and model application
df <- data.frame(ds = index(stock),
                 y = as.numeric(stock[,4]))

predknn <- knn_forecasting(df$y, h = 60, lags = 1:50, k = 70, msas = "MIMO")

#Train set model accuracy
ro <- rolling_origin(predknn)
print(ro$global_accu)
autoplot(predknn)

#Fitting nnetar
lambda = BoxCox.lambda(stock$MSFT.Close)
dnn_fit = nnetar(stock[,4],lambda=lambda)
dnn_fit
fcast = forecast(dnn_fit,PI=T,h=60)
autoplot(fcast)
accuracy(dnn_fit)
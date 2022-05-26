#Methods adopted from past projects in Forecasting done by Carla Chahine 2020/2021: C:\Users\eomuvwie\OneDrive - IESEG\Forecasting\Carl Chahine_ Forecasting Report
#Also Adopeted from https://rpubs.com/RatherBit/90267

# Libraries for code
library(readxl)
library(fpp2)/
library(dplyr)
library(TSstudio)

#install.packages("TSstudio")

# Working directory.
setwd("C:/Users/eomuvwie/OneDrive - IESEG/Forecasting/Project") 

# ########## #
# Airpass BE #
# ########## #
data <- read_excel("DataSets2022.xlsx", sheet="Airpass")
plot(data)
Airpass <- ts(data[,2], frequency = 12, start = c(2003, 1), end = c(2021, 10))
str(Airpass)


#Time series plot
ts_plot(Airpass,
        title = "Yearly number of Air Passengers BE",
        Xtitle = "Year",
        Ytitle = "Volume of Air Passengers BE")

# Split the data in training and test set
Airpass_train <- window(Airpass, end=c(2017, 12))
Airpass_test  <- window(Airpass, start=c(2018, 1), end= c(2020, 2))

# Retrieve the length of the test set
h <- length(Airpass_test)

# Plot the data
plot(Airpass, col="red")
lines(Airpass_test, col="blue")

#Season plot of the data
ggseasonplot(Airpass, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal plot",
           ylab="Airpass BE",
           col=rainbow(20), 
           pch=19)

#Using a box plot to see possible seasonal effect
boxplot(Airpass~cycle(Airpass), xlab = "Month", ylab = "AirPassengers", main = "Average Air Passengers per Month", names = month.abb, col = "blue")

#using a polar plot to better compare this pattern
ggseasonplot(Airpass,polar=TRUE)

#monthly seasonal plot
monthplot(Airpass, main="Seasonal subseries plot", 
          ylab = "Airpass BE",
          xlab="Quarter", type="l")

#Plot ACF and PACF
tsdisplay(Airpass)

# Checking for transformations
BoxCox.lambda(Airpass) 
plot(BoxCox(Airpass,lambda=0))
plot(Airpass / monthdays(Airpass)) # Do not need transformation


#Ljung Box Test
#Box.test(Airpass, lag = 12, type = "Ljung") # Using lag = 12 because of seasonality

# seasonal naive and  Residuals plot
sn <- snaive(Airpass_train, h=h)
a = accuracy(sn,Airpass_test)[,c(2,3,5,6)]
plot(sn)
checkresiduals(sn) 

# STL decomposition
fit_stl1 <- stl(Airpass_train, s.window="periodic", t.window = 12, robust = TRUE)
my_stl1 <- predict(fit_stl1, h=h)
plot(fit_stl1)
plot(my_stl1)
b = accuracy(my_stl1,Airpass_test)[,c(2,3,5,6)]
checkresiduals(my_stl1)

fit_stl <- stl(Airpass_train, s.window="periodic", t.window = 6, robust = TRUE)
my_stl <- predict(fit_stl, h=h)
plot(fit_stl)
plot(my_stl)
c = accuracy(my_stl,Airpass_test)[,c(2,3,5,6)]
checkresiduals(my_stl)

fit_stl <- stl(Airpass_train, s.window="periodic", t.window = 3, robust = TRUE)
my_stl <- forecast(fit_stl, h=h)
plot(my_stl)
d = accuracy(my_stl,Airpass_test)
checkresiduals(my_stl)

fit_rwdrift <- rwf(Airpass_train, drift = TRUE,h=h)
plot(fit_rwdrift)
e = accuracy(fit_rwdrift,Airpass_test)[,c(2,3,5,6)]
checkresiduals(fit_rwdrift)

# ETS FORECAST without trans
my_ets <- ets(Airpass_train, model="AAA")
my_ets <- forecast(my_ets, h=h)
plot(my_ets)
f = accuracy(my_ets, Airpass_test)[,c(2,3,5,6)]
summary(my_ets)
checkresiduals(my_ets)

## ETS FORECAST with trans
my_ets <- ets(Airpass_train, lambda = 0, model = 'AAA')
my_ets <- forecast(my_ets, h=h)
plot(my_ets)
g = accuracy(my_ets, Airpass_test)[,c(2,3,5,6)]
checkresiduals(my_ets)

# ARIMA models
fit1 <- auto.arima(Airpass_train)
fit1 <- forecast(fit1,h=h)
auto_arima <- accuracy(fit1, Airpass_test)[,c(2,3,5,6)]
checkresiduals(fit1)
plot(fit1)

#Comapring both the values of auto arima and arima
results = data.frame()

for (i in c(0,1,2)) {
  for (j in c(0,1,2)) {
    for (k in c(0,1,2)) {
      for (l in c(0,1,2)) {
        for (m in c(0,1,2)) {
          for (n in c(0,1,2)) {
            
            tryCatch({ 
              
              # Fitting the model
              fit = Arima(Airpass_train, order = c(i,j,k), seasonal = c(l,m,n), include.drift=TRUE)
              fcast = forecast(fit, h=h)
              a = accuracy(fcast, Airpass_test)['Test set',2]
              results = rbind(results, a)
              
              # Finding the values of i,j,k,l,m,n
              for (z in nrow(results)){
                if (results[z,] == min(results[z,])){
                  results[z,'b1'] = i
                  results[z,'b2'] = j
                  results[z,'b3'] = k
                  results[z,'b4'] = l
                  results[z,'b5'] = m
                  results[z,'b6'] = n
                }  
              }
              
              
              
            }, error=function(e){})
          }
        }
      }
    }
  }
}

# Checking the best results
results[results$X272.491169094517 == min(results$X272.491169094517),]

a = Arima(Airpass_train, order = c(2,2,0), seasonal = c(0,2,2), include.drift=TRUE)
b = forecast(a, h = h)
plot(b)
checkresiduals(a)
accuracy(b, Airpass_test)

# Best model is below
a = Arima(Airpass, order = c(2,2,0), seasonal = c(0,2,2), include.drift=TRUE)
b = forecast(a, h = 26)
plot(b)
checkresiduals(a)
accuracy(b)[,c(2,3,5,6)]

# plot training set
# Split the data in training and test set
Air_train <- window(Airpass, end=c(2017, 12))
Air_test  <- window(Airpass, start=c(2020, 3), end= c(2021, 10))
plot(Airpass_train, main="AirPassengers", ylab="", xlab="Months")

# plot forecasting for 5 years according to four methods
lines(meanf(Air_train,h=48)$mean, col=4)
lines(rwf(Air_train,h=48)$mean, col=2)
lines(rwf(Air_train,drift=TRUE,h=48)$mean, col=3)
lines(snaive(Air_train,h=48)$mean, col=5)

# legend
legend("topleft", lty=1, col=c(4,2,3, 5),
       legend=c("Mean method","Naive method","Drift method", "Seasonal naïve method"),bty="n")
       
       # the test set
       lines(Air_test, col="red")
       
#Comparing the accuracy
# Mean method
accuracy(meanf(Air_train,h=48), Air_test)

# Naive method
accuracy(rwf(Air_train,h=48), Air_test)

# Drift method
accuracy(rwf(Air_train,drift=TRUE,h=48), Air_test)

# Seasonal naïve method
accuracy(snaive(Air_train,h=48), Air_test)


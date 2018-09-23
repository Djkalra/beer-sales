install.packages("tseries")
install.packages("forecast")
install.packages("xts")

library(tseries)
library(forecast)
library(xts)

#Read the csv file using read.csv function
rawdata<-read.csv("doolally_pos_master_staging_3rd_August.csv")

#Subset the data for beer for lets say Hefeweizen for Andheri location using the subset function
beerdata<-subset(rawdata,rawdata$category_name=="beer" & rawdata$correct_beer_name=="hefeweizen" & rawdata$location_name=="andheri",select=c(order_date,subtotal))

#Check the structure of the subset and view the records in it using the str and View command
str(beerdata)
View(beerdata)

#COnvert the subset to monthly data using xts and as.weekly functions
beerdata1<-as.xts(beerdata$subtotal,order.by=as.Date(beerdata$order_date))
beerdata_monthly<-apply.monthly(beerdata1,sum)

#Enter the column name for sales as "Sales" using Colnames function
colnames("beerdata_monthly")<-c("Sales")

View(beerdata_monthly)

#Convert the data into time series using ts function and plot the same using plot.ts
beer_ts<-ts(beerdata_monthly$Sales,start = c(2015,11),end = c(2018,8),frequency=12)
plot.ts(beer_ts)

#From the plot we dont see any seasonality and therefore do the Dikcey Fueller test to check if the data is stationary or not using the adf.test() function
Stat_check<-adf.test(beer_ts)
Stat_check

#THe high p-value suggests that the series is not stationary, hence we do first order differening using the diff() function
stat_check_diff<-diff(beer_ts)

#Applying Dickey-Fueller test on the differenced series again and checking the P-value
Stat_check<-adf.test(stat_check_diff)
Stat_check

#The p-value of the differenced series is quite low suggesting that now the series is stationary
#Checking acf and pacf on the differenced series to find the p and q inputs for the ARIMA 
acf<-acf(stat_check_diff)
pacf<-pacf(stat_check_diff)

#From acf and pacf plots, we get the p factor as 2 and q factor as 1 and d as 1 since we did first order differencing
#Using ARIMA model with p=2, q=1 & d=1
model<-arima(beer_ts,c(2,1,1))

#Using forecast() function to predict the sales for next 5 months till end of 2018
beer_forecast<-forecast(model,5)
beer_forecast

#Using predict for plotting the forecast
pred<-predict(model,n.ahead=5)
ts.plot(beer_ts,pred$pred)

#Calculating accuracy using accuracy() function
accuracy(model)
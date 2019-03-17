##########################################################################################################################
#################################                                #########################################################
################################# TIME SERIES CASE STUDY SOLUTION ########################################################
#################################                                #########################################################
##### TEAM MEMBERS #######################################################################################################
##### BHARATH KUMAR B.S      #############################################################################################
##### MUKUL TAMTA            #############################################################################################
##### SURYA PRAKASH TRIPATHI #############################################################################################
##### VARUN  AHALAWAT        #############################################################################################
##########################################################################################################################
##########################################################################################################################

##INSTALL AND LOAD PACKAGES

#install.packages("tidyr")
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("gridExtra")
# install.packages("forecast")
# install.packages("tseries")
# install.packages("graphics")
# install.packages("stats")

library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(forecast)
library(tseries)
require(graphics)
library(stats)




#########################STEP 1 : READ FILE AND CHECK THE STRUCTURE OF DATA ########################################################################################

superStore_Data<-read.csv("Global Superstore.csv",stringsAsFactors = FALSE)

str(superStore_Data)

head(superStore_Data)

#######################STEP 2 : DATA CLEANING AND UNDERSTANDING ###################################################################################################

####A. FIXING COLUMNS - DELETE UNNECESSARY COLUMNS

##A1.REMOVE COLUMNS WHICH HAVE ONLY NA's.WE WON'T BE ABLE TO DERIVE ANYTHING FROM THEM.
##  ALSO REMOVE COLUMNS WHICH HAVE SINGLE  VALUE FOR ALL ROWS

filter_cols <- apply(superStore_Data, 2, function(x) !all(length(unique(x))==1| is.na(x) | x == 0))
superStore_Data<-superStore_Data[,filter_cols]

## NO COLUMNS HAVE NA VALUES OR UNIQUE VALUES,HENCE NONE OF THEM ARE REMOVED.

####B. FIX MISSING VALUES (DELETE ROWS, COLUMNS)

##B1. DELETING COLUMNS WHICH HAVE MORE THAN 70% OF DATA AS N\A,NULL

sapply(superStore_Data, function(x) sum(is.na(x)))
superStore_Data<-superStore_Data[, -which(colMeans(is.na(superStore_Data)) > 0.7)]

## POSTAL CODE HAS MORE THAN 70% VALUES OF NA.HENCE REMOVED IT.

####C. DATE FIELDS ARE IN CHARACTER FORMAT.WE NEED TO CONVERT IT INTO R FORMAT.

superStore_Data$Order.Date  <- as.Date(parse_date_time(x = superStore_Data$Order.Date, orders = c("%d-%m-%Y","%m-%d-%Y")),format = "%m-%d-%y")
superStore_Data$Ship.Date  <- as.Date(parse_date_time(x = superStore_Data$Ship.Date, orders = c("%d-%m-%Y","%m-%d-%Y")), format = "%m-%d-%y")


####D. DERIVE MONTH FROM ORDER DATE SINCE WE NEED TO AGGREGATE ON MONTHLY BASIS.

superStore_Data$Order.Month <- as.integer(format(superStore_Data$Order.Date,"%m"))
superStore_Data$Order.Year <- as.integer(format(superStore_Data$Order.Date,"%y"))

  
#######################STEP 3 :DATA PREPARATION #############################################################################################################

##We would need to first segment the whole dataset into the 21 subsets based on the market and the customer segment level

#### A . LETS CHECK THE MARKET LEVEL.
unique(superStore_Data$Market)

### 7 MARKETS
### "US"     "APAC"   "EU"     "Africa" "EMEA"   "LATAM"  "Canada"

#### B. CHECK THE CUSTOMER SEGMENT LEVEL

unique(superStore_Data$Segment)

### 3 SEGMENTS
### "Consumer"    "Corporate"   "Home Office"


####C. DIVIDE DATASET INTO SUBSETS

Aggregate_DataSet<- superStore_Data %>% group_by(Market,Segment,Order.Month,Order.Year) %>% 
                    summarise(Monthly_Profit = sum(Profit), Monthly_Quantity = sum(Quantity), Monthly_Sales = sum(Sales))


####D. FIND NET PROFIT,CONSISTENT PROFIT(AVG PROFIT) AND CO-EFFICIENT OF VARIANCE.

profit_DataSet <- Aggregate_DataSet %>% group_by(Market,Segment) %>% 
                  summarise(CV = sd(Monthly_Profit)/mean(Monthly_Profit),NetProfit = sum(Monthly_Profit), AvgProfit = mean(Monthly_Profit))

profit_DataSet <- profit_DataSet[order(profit_DataSet$CV,-profit_DataSet$NetProfit,-profit_DataSet$AvgProfit),]

profit_DataSet

### WE HAVE TO PICK THE SEGMENTS WITH LEAST CV WHICH WILL BE THE 2 MOST PROFITABLE SEGMENTS

# SEGMENT         CV        NETPROFIT    AVG PROFIT
# EU Consumer   0.6243052   188687.707   3930.9939
# APAC Consumer 0.6321323   222817.560   4642.0325


## EU CONSUMER AND APAC CONSUMER ARE THE MOST PROFITABLE SEGMENTS




#######################STEP 3 : MODEL BUILDING ################################################################################################################


####A. BEFORE WE START OUR MODEL BUILDING LETS CREATE REQUIRED DATASETS
## WE SHOULD NOT CONSIDER LAST 6 MONTHS DATA FOR MODEL BUILDING SINCE IT WILL BE USED FOR EVALUATION

## EU CONSUMER SALES DATA - MONTH WISE.

Eu_Consumer_Sales_Data <-  superStore_Data %>% filter(Market == 'EU',Segment == 'Consumer') %>%
                                 group_by(Order.Year,Order.Month) %>%
                                 summarise(Sales = sum(Sales))

Eu_Consumer_Sales_Data$ID <- seq.int(nrow(Eu_Consumer_Sales_Data))


## EU CONSUMER QUANTITY DATA - MONTH WISE

Eu_Consumer_Qty_Data <-  superStore_Data %>% filter(Market == 'EU',Segment == 'Consumer') %>%
                                    group_by(Order.Year,Order.Month) %>%
                                    summarise(Quantity = sum(Quantity))

Eu_Consumer_Qty_Data$ID <- seq.int(nrow(Eu_Consumer_Qty_Data))


## APAC CONSUMER SALES DATA - MONTH WISE.

Apac_Consumer_Sales_Data <-  superStore_Data %>% filter(Market == 'APAC',Segment == 'Consumer') %>%
                                   group_by(Order.Year,Order.Month) %>%
                                   summarise(Sales = sum(Sales))


Apac_Consumer_Sales_Data$ID <- seq.int(nrow(Apac_Consumer_Sales_Data))


## APAC CONSUMER QUANTITY DATA - MONTH WISE

Apac_Consumer_Qty_Data <- superStore_Data %>% filter(Market == 'APAC',Segment == 'Consumer') %>%
                                     group_by(Order.Year,Order.Month) %>%
                                     summarise(Quantity = sum(Quantity))

Apac_Consumer_Qty_Data$ID <- seq.int(nrow(Apac_Consumer_Qty_Data))


##### MODEL BUILDING STARTS
 

## FOR CLASSICAL DECOMPOSITION WE MUST SMOOTH THE SERIES.

smoothSeries <- function(timeSeries)
{
  w <-1
  smoothedseries <- stats::filter(timeSeries, 
                           filter=rep(1/(2-w+1),(2-w+1)), 
                           method='convolution', sides=2)
  
  #Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(timeSeries)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  return (smoothedseries)
}


########################################################################################################################
###  MODEL BUILDING FOR APAC CONSUMER SALES               ##############################################################
########################################################################################################################

## CREATE TIME SERIES FOR APAC SALES

## Total Time series for Apac_Sales
Apac_Sales_total_ts <- ts(Apac_Consumer_Sales_Data$Sales)

## We will not consider last 6 months data for model building.
Apac_Sales_indata <- Apac_Consumer_Sales_Data[1:42,]

## Time Series for Apac Sales
Apac_Sales_Ts <- ts(Apac_Sales_indata$Sales)

# Plot TimeSeries
plot(Apac_Sales_Ts)


#--------CLASSICAL DECOMPOSITION-------------------------------
#--------------------------------------------------------------
## Smoothen the series before applying Classical Decomposition
Apac_Sales_SmoothSeries <- smoothSeries(Apac_Sales_Ts)


#Plot Smoothen Series 
lines(Apac_Sales_SmoothSeries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
Apac_Sales_SmoothedDf <- as.data.frame(cbind(Apac_Sales_indata$ID, as.vector(Apac_Sales_SmoothSeries)))
colnames(Apac_Sales_SmoothedDf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#Apac_Sales_lmfit <- lm(Sales ~ Month, data=Apac_Sales_SmoothedDf)

Apac_Sales_lmfit<- lm(formula = Sales ~ sin(1.04 * Month) * poly(Month,1) + cos(1.04 * Month) * poly(Month,1)
                      + Month, data = Apac_Sales_SmoothedDf)


Apac_Sales_global_pred <- predict.lm(Apac_Sales_lmfit, Month=Apac_Sales_indata$ID)
summary(Apac_Sales_global_pred)
lines(Apac_Sales_indata$ID, Apac_Sales_global_pred, col='red', lwd=2)


#--------------------------------------------------------------------------------------------


#--------------------ARIMA SERIES -----------------------------------------------------------
# --------------------------------------------------------------------------------------------

#Now, let's look at the locally predictable series
#We will model it as an ARIMA series

Apac_Sales_local_pred <- Apac_Sales_Ts-Apac_Sales_global_pred
plot(Apac_Sales_local_pred, col='red', type = "l")
acf(Apac_Sales_local_pred)
acf(Apac_Sales_local_pred, type="partial")
Apac_Sales_armafit <- auto.arima(Apac_Sales_local_pred)

tsdiag(Apac_Sales_armafit)
Apac_Sales_armafit

# Series: Apac_Sales_local_pred 
# ARIMA(0,0,0) with zero mean 
# 
# sigma^2 estimated as 111858951:  log likelihood=-448.78
# AIC=899.57   AICc=899.67   BIC=901.3

#We'll check if the residual series is white noise

Apac_Sales_resi <- Apac_Sales_local_pred-fitted(Apac_Sales_armafit)

adf.test(Apac_Sales_resi,alternative = "stationary")

kpss.test(Apac_Sales_resi)
#The KPSS Test for Level Stationarity shows a p-value>=0.1
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.

#---------------------------------------------------------------------------------------------------------

#-----------MODEL EVALUATION USING MAPE--------------------------------------------------------------------

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

Apac_Consumer_Sales_outdata <- Apac_Consumer_Sales_Data[43:48,]
timevals_out <- Apac_Consumer_Sales_outdata$ID

##------------MODEL EVALUATION FOR CLASSICAL DECOMPOSITION MODEL ----------------------------------------------
Apac_Consumer_Sales_global_pred_out <- predict(Apac_Sales_lmfit,data.frame(Month =timevals_out))

Apac_Consumer_Sales_fcast <- Apac_Consumer_Sales_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
Apac_Sales_MAPE_class_dec <- accuracy(Apac_Consumer_Sales_fcast,Apac_Consumer_Sales_outdata$Sales)
Apac_Sales_MAPE_class_dec

#Let's also plot the predictions along with original values, to get a visual feel of the fit

Apac_Sales_class_dec_pred <- c(ts(Apac_Sales_global_pred),ts(Apac_Consumer_Sales_global_pred_out))
plot(Apac_Sales_total_ts,main = "Apac Sales(Actual vs Forecasted)",xlab = "Time",ylab="Sales",col = "black")
lines(Apac_Sales_class_dec_pred, col = "red")
legend("topleft", legend=c("Actual Sales", "Forecasted Sales"), col=c("Black", "red"), lty=1:1, cex=0.8)


#So, that was classical decomposition, now let's do an ARIMA fit
#-------------------------------------------------------------------------------------------------------------------

#---------MODEL EVALUATION FOR ARIMA -----------------------------------------------------------------------------

autoarima <- auto.arima(Apac_Sales_Ts)
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- Apac_Sales_Ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

Apac_Sales_MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,Apac_Consumer_Sales_outdata$Sales)
Apac_Sales_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

Apac_Sales_auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(Apac_Sales_total_ts,main = "Apac Sales(Actual vs Forecasted)",xlab = "Time",ylab="Sales",col = "black")
lines(Apac_Sales_auto_arima_pred, col = "red")
legend("topleft", legend=c("Actual Sales", "Forecasted Sales"), col=c("Black", "red"), lty=1:1, cex=0.8)


########################################################################################################################
###  MODEL BUILDING FOR APAC CONSUMER QUANTITY           ##############################################################
########################################################################################################################

## CREATE TIME SERIES FOR APAC Qty

## Total Time series for Apac_Qty
Apac_Qty_total_ts <- ts(Apac_Consumer_Qty_Data$Quantity)

## We will not consider last 6 months data for model building.
Apac_Qty_indata <- Apac_Consumer_Qty_Data[1:42,]

## Time Series for Apac Qty
Apac_Qty_Ts <- ts(Apac_Qty_indata$Quantity)

# Plot TimeSeries
plot(Apac_Qty_Ts)


#--------CLASSICAL DECOMPOSITION-------------------------------
#--------------------------------------------------------------
## Smoothen the series before applying Classical Decomposition
Apac_Qty_SmoothSeries <- smoothSeries(Apac_Qty_Ts)


#Plot Smoothen Series 
lines(Apac_Qty_SmoothSeries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
Apac_Qty_SmoothedDf <- as.data.frame(cbind(Apac_Qty_indata$ID, as.vector(Apac_Qty_SmoothSeries)))
colnames(Apac_Qty_SmoothedDf) <- c('Month', 'Qty')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

Apac_Qty_lmfit <- lm(Qty ~ sin(1.13 * Month) * poly(Month,1) + cos(1.13 * Month) * poly(Month,1)
                     + Month, data=Apac_Qty_SmoothedDf)

Apac_Qty_global_pred <- predict(Apac_Qty_lmfit, Month=Apac_Qty_indata$ID)
summary(Apac_Qty_global_pred)
lines(Apac_Qty_indata$ID, Apac_Qty_global_pred, col='red', lwd=2)

#--------------------------------------------------------------------------------------------


#--------------------ARIMA SERIES -----------------------------------------------------------
# --------------------------------------------------------------------------------------------

#Now, let's look at the locally predictable series
#We will model it as an ARIMA series

Apac_Qty_local_pred <- Apac_Qty_Ts-Apac_Qty_global_pred
plot(Apac_Qty_local_pred, col='red', type = "l")
acf(Apac_Qty_local_pred)
acf(Apac_Qty_local_pred, type="partial")
Apac_Qty_armafit <- auto.arima(Apac_Qty_local_pred)

tsdiag(Apac_Qty_armafit)
Apac_Qty_armafit

# Series: Apac_Qty_local_pred 
# ARIMA(0,0,0) with zero mean 
# 
# sigma^2 estimated as 14843:  log likelihood=-261.31
# AIC=524.61   AICc=524.71   BIC=526.35

#We'll check if the residual series is white noise

Apac_Qty_resi <- Apac_Qty_local_pred-fitted(Apac_Qty_armafit)

adf.test(Apac_Qty_resi,alternative = "stationary")

kpss.test(Apac_Qty_resi)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.

#---------------------------------------------------------------------------------------------------------

#-----------MODEL EVALUATION USING MAPE--------------------------------------------------------------------

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

Apac_Consumer_Qty_outdata <- Apac_Consumer_Qty_Data[43:48,]
timevals_out <- Apac_Consumer_Qty_outdata$ID

##------------MODEL EVALUATION FOR CLASSICAL DECOMPOSITION MODEL ----------------------------------------------
Apac_Consumer_Qty_global_pred_out <- predict(Apac_Qty_lmfit,data.frame(Month =timevals_out))

Apac_Consumer_Qty_fcast <- Apac_Consumer_Qty_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
Apac_Qty_MAPE_class_dec <- accuracy(Apac_Consumer_Qty_fcast,Apac_Consumer_Qty_outdata$Quantity)
Apac_Qty_MAPE_class_dec

#Let's also plot the predictions along with original values, to get a visual feel of the fit
Apac_Qty_class_dec_pred <- c(ts(Apac_Qty_global_pred),ts(Apac_Consumer_Qty_global_pred_out))

plot(Apac_Qty_total_ts,main = "Apac Quantity(Actual vs Forecasted)",xlab = "Time",ylab="Quantity",col = "black")
lines(Apac_Qty_class_dec_pred, col = "red")
legend("topleft", legend=c("Actual Quantity", "Forecasted Quantity"), col=c("Black", "red"), lty=1:1, cex=0.8)

#So, that was classical decomposition, now let's do an ARIMA fit
#-------------------------------------------------------------------------------------------------------------------

#---------MODEL EVALUATION FOR ARIMA -----------------------------------------------------------------------------

autoarima <- auto.arima(Apac_Qty_Ts)
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- Apac_Qty_Ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

Apac_Qty_MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,Apac_Consumer_Qty_outdata$Quantity)
Apac_Qty_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

Apac_Qty_auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))

plot(Apac_Qty_total_ts,main = "Apac Quantity(Actual vs Forecasted)",xlab = "Time",ylab="Quantity",col = "black")
lines(Apac_Qty_auto_arima_pred, col = "red")
legend("topleft", legend=c("Actual Quantity", "Forecasted Quantity"), col=c("Black", "red"), lty=1:1, cex=0.8)



########################################################################################################################
###  MODEL BUILDING FOR EU CONSUMER SALES               ##############################################################
########################################################################################################################

## CREATE TIME SERIES FOR EU SALES

## Total Time series for EU_Sales
EU_Sales_total_ts <- ts(Eu_Consumer_Sales_Data$Sales)

## We will not consider last 6 months data for model building.
EU_Sales_indata <- Eu_Consumer_Sales_Data[1:42,]

## Time Series for EU Sales
EU_Sales_Ts <- ts(EU_Sales_indata$Sales)

# Plot TimeSeries
plot(EU_Sales_Ts)


#--------CLASSICAL DECOMPOSITION-------------------------------
#--------------------------------------------------------------
## Smoothen the series before applying Classical Decomposition
EU_Sales_SmoothSeries <- smoothSeries(EU_Sales_Ts)


#Plot Smoothen Series 
lines(EU_Sales_SmoothSeries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
EU_Sales_SmoothedDf <- as.data.frame(cbind(EU_Sales_indata$ID, as.vector(EU_Sales_SmoothSeries)))
colnames(EU_Sales_SmoothedDf) <- c('Month', 'Sales')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

EU_Sales_lmfit <- lm(formula = Sales ~ sin(2 * Month) * poly(Month,1) + cos(2 * Month) * poly(Month,1)
   + Month, data = EU_Sales_SmoothedDf)


EU_Sales_global_pred <- predict(EU_Sales_lmfit, Month=EU_Sales_indata$ID)
summary(EU_Sales_global_pred)
lines(EU_Sales_indata$ID, EU_Sales_global_pred, col='red', lwd=2)

#--------------------------------------------------------------------------------------------


#--------------------ARIMA SERIES -----------------------------------------------------------
# --------------------------------------------------------------------------------------------

#Now, let's look at the locally predictable series
#We will model it as an ARIMA series

EU_Sales_local_pred <- EU_Sales_Ts-EU_Sales_global_pred
plot(EU_Sales_local_pred, col='red', type = "l")
acf(EU_Sales_local_pred)
acf(EU_Sales_local_pred, type="partial")
EU_Sales_armafit <- auto.arima(EU_Sales_local_pred)

tsdiag(EU_Sales_armafit)
EU_Sales_armafit

#We'll check if the residual series is white noise

EU_Sales_resi <- EU_Sales_local_pred-fitted(EU_Sales_armafit)

adf.test(EU_Sales_resi,alternative = "stationary")

kpss.test(EU_Sales_resi)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.

#---------------------------------------------------------------------------------------------------------

#-----------MODEL EVALUATION USING MAPE--------------------------------------------------------------------

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

EU_Consumer_Sales_outdata <- Eu_Consumer_Sales_Data[43:48,]
timevals_out <- EU_Consumer_Sales_outdata$ID

##------------MODEL EVALUATION FOR CLASSICAL DECOMPOSITION MODEL ----------------------------------------------
EU_Consumer_Sales_global_pred_out <- predict(EU_Sales_lmfit,data.frame(Month =timevals_out))

EU_Consumer_Sales_fcast <- EU_Consumer_Sales_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
EU_Sales_MAPE_class_dec <- accuracy(EU_Consumer_Sales_fcast,EU_Consumer_Sales_outdata$Sales)
EU_Sales_MAPE_class_dec

#Let's also plot the predictions along with original values, to get a visual feel of the fit

EU_Sales_class_dec_pred <- c(ts(EU_Sales_global_pred),ts(EU_Consumer_Sales_global_pred_out))

plot(EU_Sales_total_ts,main = "EU Sales(Actual vs Forecasted)",xlab = "Time",ylab="Sales",col = "black")
lines(EU_Sales_class_dec_pred, col = "red")
legend("topleft", legend=c("Actual Sales", "Forecasted Sales"), col=c("Black", "red"), lty=1:1, cex=0.8)


#So, that was classical decomposition, now let's do an ARIMA fit
#-------------------------------------------------------------------------------------------------------------------

#---------MODEL EVALUATION FOR ARIMA -----------------------------------------------------------------------------

autoarima <- auto.arima(EU_Sales_Ts)
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- EU_Sales_Ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

EU_Sales_MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer_Sales_outdata$Sales)
EU_Sales_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit
EU_Sales_auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))

plot(EU_Sales_total_ts,main = "EU Sales(Actual vs Forecasted)",xlab = "Time",ylab="Sales",col = "black")
lines(EU_Sales_auto_arima_pred, col = "red")
legend("topleft", legend=c("Actual Sales", "Forecasted Sales"), col=c("Black", "red"), lty=1:1, cex=0.8)

########################################################################################################################
###  MODEL BUILDING FOR EU CONSUMER QUANTITY           ##############################################################
########################################################################################################################

## CREATE TIME SERIES FOR EU Qty

## Total Time series for EU_Qty
EU_Qty_total_ts <- ts(Eu_Consumer_Qty_Data$Quantity)

## We will not consider last 6 months data for model building.
EU_Qty_indata <- Eu_Consumer_Qty_Data[1:42,]

## Time Series for EU Qty
EU_Qty_Ts <- ts(EU_Qty_indata$Quantity)

# Plot TimeSeries
plot(EU_Qty_Ts)


#--------CLASSICAL DECOMPOSITION-------------------------------
#--------------------------------------------------------------
## Smoothen the series before applying Classical Decomposition
EU_Qty_SmoothSeries <- smoothSeries(EU_Qty_Ts)


#Plot Smoothen Series 
lines(EU_Qty_SmoothSeries, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
EU_Qty_SmoothedDf <- as.data.frame(cbind(EU_Qty_indata$ID, as.vector(EU_Qty_SmoothSeries)))
colnames(EU_Qty_SmoothedDf) <- c('Month', 'Qty')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

EU_Qty_lmfit <- lm(Qty ~ sin(2 * Month) * poly(Month,1) + cos(2 * Month) * poly(Month,1)
                   + Month, data=EU_Qty_SmoothedDf)

EU_Qty_global_pred <- predict(EU_Qty_lmfit, Month=EU_Qty_indata$ID)
summary(EU_Qty_global_pred)
lines(EU_Qty_indata$ID, EU_Qty_global_pred, col='red', lwd=2)

#--------------------------------------------------------------------------------------------


#--------------------ARIMA SERIES -----------------------------------------------------------
# --------------------------------------------------------------------------------------------

#Now, let's look at the locally predictable series
#We will model it as an ARIMA series

EU_Qty_local_pred <- EU_Qty_Ts-EU_Qty_global_pred
plot(EU_Qty_local_pred, col='red', type = "l")
acf(EU_Qty_local_pred)
acf(EU_Qty_local_pred, type="partial")
EU_Qty_armafit <- auto.arima(EU_Qty_local_pred)

tsdiag(EU_Qty_armafit)
EU_Qty_armafit

#We'll check if the residual series is white noise

EU_Qty_resi <- EU_Qty_local_pred-fitted(EU_Qty_armafit)

adf.test(EU_Qty_resi,alternative = "stationary")
#The Augmented Dickey-Fuller Test shows a p-value<0.01. 
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.

kpss.test(EU_Qty_resi)
#The KPSS Test for Level Stationarity shows a p-value>0.1
#This Implies that the residual after extraxting the fitted auto arima for demand is stationairy.

#---------------------------------------------------------------------------------------------------------

#-----------MODEL EVALUATION USING MAPE--------------------------------------------------------------------

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

EU_Consumer_Qty_outdata <- Eu_Consumer_Qty_Data[43:48,]
timevals_out <- EU_Consumer_Qty_outdata$ID

##------------MODEL EVALUATION FOR CLASSICAL DECOMPOSITION MODEL ----------------------------------------------

EU_Consumer_Qty_global_pred_out <- predict(EU_Qty_lmfit,data.frame(Month =timevals_out))

EU_Consumer_Qty_fcast <- EU_Consumer_Qty_global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
EU_Qty_MAPE_class_dec <- accuracy(EU_Consumer_Qty_fcast,EU_Consumer_Qty_outdata$Quantity)
EU_Qty_MAPE_class_dec

#Let's also plot the predictions along with original values, to get a visual feel of the fit

EU_Qty_class_dec_pred <- c(ts(EU_Qty_global_pred),ts(EU_Consumer_Qty_global_pred_out))

plot(EU_Qty_total_ts,main = "EU Quantity(Actual vs Forecasted)",xlab = "Time",ylab="Quantity",col = "black")
lines(EU_Qty_class_dec_pred, col = "red")
legend("topleft", legend=c("Actual Quantity", "Forecasted Quantity"), col=c("Black", "red"), lty=1:1, cex=0.8)

#So, that was classical decomposition, now let's do an ARIMA fit
#-------------------------------------------------------------------------------------------------------------------

#---------MODEL EVALUATION FOR ARIMA -----------------------------------------------------------------------------

autoarima <- auto.arima(EU_Qty_Ts)
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- EU_Qty_Ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)


#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

EU_Qty_MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer_Qty_outdata$Quantity)
EU_Qty_MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

EU_Qty_auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))


plot(EU_Qty_total_ts,main = "EU Quantity(Actual vs Forecasted)",xlab = "Time",ylab="Quantity",col = "black")
lines(EU_Qty_auto_arima_pred, col = "red")
legend("topleft", legend=c("Actual Quantity", "Forecasted Quantity"), col=c("Black", "red"), lty=1:1, cex=0.8)

########################################################################################################################
### Forecasting Demand and Sales for the future 6 month Period. ########################################################
########################################################################################################################

future_month_period = c(49:54)
future_months<- c("Jan2015", "Feb2015", "Mar2015", "Apr2015", "May2015", "June2015")

#----------- APAC-CONSUMER SALES -------------------------------------------------------------------
#A. Classical Decomposition Model Evaluation Result
Apac_Sales_MAPE_class_dec

# ME     RMSE      MAE      MPE   MAPE
# Test set 8865.763 15492.62 12370.81 9.821857 19.266

#B. Auto ARIMA Model Evaluation Results
Apac_Sales_MAPE_auto_arima

# ME     RMSE      MAE      MPE     MAPE
# Test set 15848.24 22755.75 18780.19 19.73091 27.68952


#APAC-CONSUMER SALES Model Evaluation RESULT
#from the results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 8% on the Auto Arima model.
Apac_Sales_Forecast<- predict.lm(Apac_Sales_lmfit, data.frame(Month = future_month_period))
Apac_Sales_Forecast <- data.frame(future_months,Apac_Sales_Forecast)
colnames(Apac_Sales_Forecast) <- c("MONTH","SALES")
Apac_Sales_Forecast

#         MONTH               SALES
# 1       Jan2015            50646.76
# 2       Feb2015            44482.53
# 3       Mar2015            48454.72
# 4       Apr2015            59582.58
# 5       May2015            67693.45
# 6      June2015            65338.72

####---------------------------------------------------------------------------------------------####

#----------APAC-CONSUMER QUANTITY -------------------------------------------------------------------
#A. Classical Decomposition Model Evaluation Result

Apac_Qty_MAPE_class_dec

# ME     RMSE      MAE      MPE     MAPE
# Test set 131.0174 192.5819 172.6545 12.94513 23.98944

#B. Auto ARIMA Model Evaluation Results
Apac_Qty_MAPE_auto_arima
 
# ME     RMSE      MAE       MPE     MAPE
# Test set 12 174.3722 147.6667 -7.362467 26.24458

#APAC-CONSUMER QUANTITY Model Evaluation RESULT
#from the results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 3% on the Auto Arima model.
Apac_Qty_Forecast<- predict.lm(Apac_Qty_lmfit, data.frame(Month = future_month_period))
Apac_Qty_Forecast <- data.frame(future_months,Apac_Qty_Forecast)
colnames(Apac_Qty_Forecast) <- c("MONTH","QUANTITY")
Apac_Qty_Forecast

#      MONTH QUANTITY
# 1  Jan2015 506.8535
# 2  Feb2015 556.1589
# 3  Mar2015 704.1219
# 4  Apr2015 793.8252
# 5  May2015 725.6161
# 6 June2015 578.0357


#----------- EU-CONSUMER SALES -------------------------------------------------------------------
#A. Classical Decomposition Model Evaluation Result
EU_Sales_MAPE_class_dec

# ME     RMSE      MAE      MPE     MAPE
# Test set 5900.185 9868.728 6712.747 8.662211 11.20403

#B. Auto ARIMA Model Evaluation Results
EU_Sales_MAPE_auto_arima

# ME     RMSE     MAE    MPE    MAPE
# Test set 12935.21 19499.14 16687.6 17.678 28.9226


#EU-CONSUMER SALES Model Evaluation RESULT
#from the results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 17% on the Auto Arima model.
EU_Sales_Forecast<- predict.lm(EU_Sales_lmfit, data.frame(Month = future_month_period))
EU_Sales_Forecast <- data.frame(future_months,EU_Sales_Forecast)
colnames(EU_Sales_Forecast) <- c("MONTH","SALES")
EU_Sales_Forecast

#     MONTH    SALES
# 1  Jan2015 39070.59
# 2  Feb2015 40267.48
# 3  Mar2015 49252.02
# 4  Apr2015 41408.35
# 5  May2015 40273.62
# 6 June2015 50970.96


#----------EU-CONSUMER QUANTITY -------------------------------------------------------------------
#A. Classical Decomposition Model Evaluation Result

EU_Qty_MAPE_class_dec

#B. Auto ARIMA Model Evaluation Results
EU_Qty_MAPE_auto_arima

#EU-CONSUMER QUANTITY Model Evaluation RESULT
#from the results it is clear that the Classical Decomposition Model Performs better.
#Classical Decomposition Forecasting model provides a MAPE reduction of 3% on the Auto Arima model.
EU_Qty_Forecast<- predict.lm(EU_Qty_lmfit, data.frame(Month = future_month_period))
EU_Qty_Forecast <- data.frame(future_months,EU_Qty_Forecast)
colnames(EU_Qty_Forecast) <- c("MONTH","QUANTITY")
EU_Qty_Forecast

#      MONTH QUANTITY
# 1  Jan2015 486.8408
# 2  Feb2015 510.7291
# 3  Mar2015 638.8632
# 4  Apr2015 519.8834
# 5  May2015 507.8283
# 6 June2015 661.7200

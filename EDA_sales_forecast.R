rm(list = ls())

## Libraries----
library(fpp3)
library(GGally)
library(ggfortify)
library(fable)
library(forecast)
library(zoo)
library(xts)
library(dplyr)



## Set working directory----
# Set the working directory to the folder containing the CSV file
setwd("C:/test_input/")


##Load, Cleaning & Merge----
#Load the CSV file with ';' as the delimiter
sales_data <- read.csv("sales_data.csv", sep = ";")
store_master <- read.csv("store_master.csv", sep = ";")

#Fixing characters in latitude & longitude using gsub()
store_master <- store_master %>%
  mutate(latitude = gsub(",", ".", latitude))
store_master <- store_master %>%
  mutate(longitude = gsub(",", ".", longitude))

####Merge datasets  ----
#merge imported and modified datasets and create a df with sales and store

sales_store <- left_join(sales_data, store_master, by = "store")
sales_store <- sales_store %>%
  mutate(unit_price = gsub(",", ".", unit_price))

#change class of items, stores and categories to character from numeric to characters
#because they are categorical values 
sales_store <- sales_store %>%
  mutate(store = as.character(store)) %>%
  mutate(item = as.character(item)) %>%
  mutate(item_category = as.character(item_category))%>%
  mutate(unit_price = as.numeric(unit_price)) %>%
  mutate(date = as.Date(date))






---------
  
  
  
  
  
  
  
  ##Selecting ITEMs to forecast----
# 10 items with the most available historical records have been considered for analysis
# Group the data by 'item' and count the number of unique 'date' records for each item


item_counts <- sales_store %>%
  group_by(item) %>%
  summarise(date_count = n_distinct(date)) %>% arrange(desc(date_count))


selected_10_items <- as.list(head(item_counts[1], 10))
selected_10_items <- as.numeric(selected_10_items$item)

item_list = selected_10_items #selected items


# create tsibble for selected items & aggregate 'qty' and 'unite_price' by 'date' 
# calculate 'total_qty' and 'unit_price' value for each item per day














## ITEM 1 -------------------------
### EDA----
item_1_df <- sales_store %>% #filter sales_store to get item specific data
  filter(item == item_list[1], date >= as.Date("2017-01-01") & date <= as.Date("2019-12-31"))

item_1_df <- item_1_df %>% #arrange in desc order
  arrange(item_1_df, desc(date))

#using aggregate functions for items, get the sum of 'qty' and mean for 'unit_price' in each day
sum_qty_df_item1 <- item_1_df %>%
  group_by(date) %>%
  summarise(total_qty = sum(qty), unit_price = mean(unit_price))

#head(sum_qty_df_item1) # View the new dataframe

tsibble_item1<- as_tsibble(sum_qty_df_item1, index = date) #convert to tsibble

# Plots 
p1_qty <-autoplot(tsibble_item1, .vars = total_qty) + labs(title = "Item 1")
p1_qty


item_list
item_list[1]

tsibble_item1 %>% #fix the class of date and identify index
  mutate(date = as_date(date)) %>%
  as_tsibble(index = date)

#### Histogram----
#Check histogram for 'total_qty' and 'unit_price'
hist1 <- ggplot(data = tsibble_item1, aes(x = total_qty)) +
  geom_histogram() +
  labs(x = "Total Quantity", y = "Frequency", title = "Histogram of Total Quantity")
hist1


#### Correlogram---- 
ts_data_item1 <- tsibble_item1 %>% 
  as_tsibble(index = date) %>%
  fill_gaps()

acf_1_lag365 <- ts_data_item1 %>% 
  ACF(total_qty, lag_max = 365) %>% autoplot()
acf_1_lag365 #there is seasonal pattern according to ACF
# "The variable "total_qty" shows a statically significant correlation with the most recent observed data, 
# suggests that forecasting methods that rely on recent observations tend to yield better results.
# Additionally, there is a noticeable seasonal pattern on weekly dataframe."


#### Scatterplot---- 
#study correlation between 'unit_price' and 'total_qty'
scatter_plot1 <- tsibble_item1 %>%
  ggplot(aes(x=total_qty, y=unit_price)) + geom_point() +
  labs(x = "Total Quantity for Item", y = "Unit Price")
scatter_plot1
#correlation is not significant 


#drop 'price_unit' column and setting index
tsble_itm1_drpd <- tsibble_item1 %>% select(date, total_qty) %>% 
  as_tsibble(index=date)

#### Decomposition----
#additive decomposition because the magnitude of seasonal fluctuation does not vary with level

tsble_itm1_drpd <- tsble_itm1_drpd %>% # fill gaps 
  as_tsibble(index = date) %>%
  fill_gaps()
dcmp_item1 <- tsble_itm1_drpd %>%  
  mutate(total_qty = na.aggregate(total_qty, FUN = median)) %>% 
  model(stl = STL(total_qty))
#components(dcmp_item1)
autoplot(components(dcmp_item1))


###Forecast -------------------------------------------

####converting data to ts----- 
##Creating xts object and fixing missing date values
tsble_itm1_drpd_xts <- xts(tsble_itm1_drpd$total_qty, tsble_itm1_drpd$date)

start_date_item1 <- first(tsble_itm1_drpd$date) #find start date
end_date_item1 <- last(tsble_itm1_drpd$date)    #find end date

##create a seq of dates in order, without missing date value
dates_xts_item1 <- seq(as.Date(start_date_item1), as.Date(end_date_item1), by="days") 

##convert to date df and merge them by left join to add all probable missing date values
##convert dates to df
df_temp_1_1 <- as.data.frame(dates_xts_item1) 
colnames(df_temp_1_1) <- "date"

##merge, interpolate NA values & conversions
df_temp_1_2 <- as.data.frame(tsble_itm1_drpd)
df_dates_merged_item1 <- left_join(df_temp_1_1, df_temp_1_2, by = "date")

xtsdates_merged_item1 <- xts(df_dates_merged_item1$total_qty, df_dates_merged_item1$date)
xtsdates_merged_item1 <- na.approx(xtsdates_merged_item1) 

##conversions for date, create ts and break date to YYYY, MM, DD
date_obj_item1_1 <- as.Date(start_date_item1) 
year <- as.numeric(format(date_obj_item1_1, "%Y"))
month <- as.numeric(format(date_obj_item1_1, "%m"))
day <- as.numeric(format(date_obj_item1_1, "%d"))

startdate_for_ts1 <- c(year, month, day) #start date as vector

date_obj_item1_2 <- as.Date(end_date_item1) 
year <- as.numeric(format(date_obj_item1_2, "%Y"))
month <- as.numeric(format(date_obj_item1_2, "%m"))
day <- as.numeric(format(date_obj_item1_2, "%d"))

##time-series for item
enddate_for_ts1 <- c(year, month, day) #end date as vector
ts_item1 <- ts(xtsdates_merged_item1, frequency = 365, start =startdate_for_ts1, end = enddate_for_ts1)


####Define test and train sets----
test_no_item1 <- round(nrow(ts_item1)*0.2) 
test_item1 <- tail(ts_item1, test_no_item1) #20% of last obs as test

train_no_item1<- round(nrow(ts_item1))-test_no_item1 
train_item1<- head(ts_item1, train_no_item1) #80% of first obs as train



####Fit models ----

###ARIMA
fit1_arima <- auto.arima(train_item1) %>%
  forecast(h = test_no_item1)

###SARIMA
p <- 1  # AR order -- The model uses previous value as predictor bc of ACF
d <- 0  # Differencing order -- bc the plot is close to stationary the original data is assumed to be already stationary. if there is no significant trend, d can be set to 0 
q <- 1  # MA order -- It represents the number of lagged forecast errors (residuals) to include in the model
P <- 1  # Seasonal AR order -- indicating that the model includes the lagged value of the seasonal component
D <- 1  # Seasonal differencing order -- When the time series shows clear seasonality D can be set to a positive value
Q <- 1  # Seasonal MA order,  model includes the lagged residuals of the seasonal component.
S <- 7  # Seasonal period (e.g.,7 for weekly seasonality)

fit1_sarima <- arima(train_item1, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S)) %>%
  forecast(h = test_no_item1)  # Adjust the forecast horizon

###Snaive
fit1_snaive <- snaive(train_item1, h=test_no_item1)

###Mean
fit1_mean <- meanf(train_item1, h=test_no_item1) 


###RWF
fit1_rwf <- rwf(train_item1, drift = TRUE, h=test_no_item1)

###ma
ma_model_1 <- ma(train_item1, order = 1)
fit1_ma <- forecast(ma_model_1, h=test_no_item1)

###ema
ema_model_1 <- HoltWinters(train_item1, beta = FALSE, gamma = FALSE)
# Forecast future values using the EMA model
fit1_ema <- forecast(ema_model_1, h = test_no_item1)

###PLOTS
plot_f1 <- autoplot(ts_item1) +
  autolayer(fit1_mean, series = 'MEAN', PI = FALSE) +
  autolayer(fit1_snaive, series = 'SNAIVE', PI = FALSE) +
  autolayer(fit1_sarima, series = 'SARIMA', PI = FALSE) +
  autolayer(fit1_arima, series = 'ARIMA', PI = FALSE) +
  autolayer(fit1_rwf, series = 'RWF', PI = FALSE) +
  autolayer(fit1_ema, series = 'ema', PI = FALSE) +
  xlab("Year") + ylab("Total_Qty") +
  ggtitle("Forecasts for Item 1 Quantity Sold")

plot_f1

####Accuracy----

accuracy(fit1_mean, test_item1)[2, c('RMSE', 'MAE')]
accuracy(fit1_snaive, test_item1)[2, c('RMSE', 'MAE')]
accuracy(fit1_sarima, test_item1)[2, c('RMSE', 'MAE')]
accuracy(fit1_rwf, test_item1)[2, c('RMSE', 'MAE')]
accuracy(fit1_arima, test_item1)[2, c('RMSE', 'MAE')]
accuracy(fit1_ema, test_item1)[2, c('RMSE', 'MAE')]

####BEST FORECASTS: 
checkresiduals(fit1_sarima)


####Estimated Values----

fit1_final <- arima(ts_item1, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = S)) %>%
  forecast(h = 30)


####Table & Graph-----

plot(fit1_final, PI = FALSE)

# Extract the point forecasts from the forecast object
point_forecasts_1 <- as.numeric(fit1_final$mean)

# Determine the forecast horizon length
horizon_length <- length(point_forecasts_1)

##create a seq of dates in order for forecast data
dates_forecast1 <- seq(as.Date(end_date_item1) + 1, by = "day", length.out = horizon_length)

# Create a tsibble object using the point forecasts and dates
forecast_1_final <- tsibble(Date = dates_forecast1, Quantity = point_forecasts_1) %>%
  mutate(Item = item_list[1])


autoplot(forecast_1_final) #plot final forecast 

print(forecast_1_final, n=30) #forecast values table

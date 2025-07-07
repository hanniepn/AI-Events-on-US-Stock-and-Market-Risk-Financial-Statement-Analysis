# Author: Margaret Nguyen
# Date: 04/30/2025
# The Fama-French Three Factor model

# Clear the environment
rm(list = ls())

# Turn off scientific notation
options(scipen = 999)

# Set the working directory
setwd('/Users/margaret06/Downloads/ECON 314 PROJECT')

# Suppress warnings
options(warn = -1)
suppressPackageStartupMessages(library(tidyquant))

# Fix errors
conflictRules('dplyr', exclude = 'lag')

# Install packages
library(readxl) 
library(tidyquant)
library(dplyr)
library(lubridate)

#Write a function to calculate daily return
dret = function(ticker){
  pricedata = getSymbols(ticker, from = "2016-01-01",
                         to = "2024-12-31",warnings = FALSE,
                         auto.assign = FALSE) 
  pricedata=as.data.frame(pricedata)
  n = nrow(pricedata) # number of rows
  m = ncol(pricedata) # number of columns
  for (i in 1:(n-1)){
    pricedata[i+1,m+1] = log(pricedata[i+1,m]/pricedata[i,m])
  }
  colnames(pricedata)[m+1] = c("Daily.Return")
  pricedata[-1,]
  
  #Generate YYYYMM
  dailyreturn = cbind(rownames(pricedata),pricedata[,7])
  dailyreturn[,1] = format(as.Date(dailyreturn[,1]),'%Y%m%d')
  dailyreturn = as.data.frame(dailyreturn)
  dailyreturn[,3] = substr(format(dailyreturn[,1]),1,6)
  head(dailyreturn)
  dailyreturn[,2] = round(as.numeric(dailyreturn[,2]),digits = 6)
  colnames(dailyreturn)[1:3] = c("Date","logret","YYYYMM")
  dailyreturn = na.omit(dailyreturn)
  dailyreturn = dailyreturn[1:2] # remove YYYYMM column
  dailyreturn
}

# Access data
NVDAret = dret("NVDA")
TSLAret = dret("TSLA")
HSYret = dret("HSY")
COSTret = dret("COST")
MARret = dret("MAR")
mktret = dret("^GSPC")

# Colnames
names(NVDAret)

# Access to the Fama-French Factor Data: 
# https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html#Research
ffdata = read.csv("F-F_Research_Data_Factors_daily.csv",header = TRUE,skip = 3, sep = ",")

# Data rows
nrow(ffdata)

# View dataframe
head(ffdata)
tail(ffdata)

# lapply function: Apply a Function over a List or Vector
ffdata[,2:5] = lapply(ffdata[,2:5],as.numeric)
ffdata[,2:5] = ffdata[,2:5]/100 # to decimal

# Convert to numeric
ffdata[,1] = as.numeric(ffdata[,1])
ff_data = ffdata

# Filter Date
ff_data = subset(ff_data, ff_data[,1] >= 20160101 & ff_data[,1] <= 20241231)
colnames(ff_data)[1:2] = c("Date", "MKT_RF")

# Merge the dataframe
NVDAestdata = merge(ff_data, NVDAret, by="Date")
head(NVDAestdata)

TSLAestdata = merge(ff_data, TSLAret, by="Date")
head(TSLAestdata)

HSYestdata = merge(ff_data, HSYret, by="Date")
head(HSYestdata)

COSTestdata = merge(ff_data, COSTret, by="Date")
head(COSTestdata)

MARestdata = merge(ff_data, MARret, by="Date")
head(MARestdata)

### SUMMARY STATISTICS ###
# Basic summary (min, Q1, median, mean, Q3, max)
# Fama-French 3-factor data are the same across stocks
NVDAestdata_summary_stats <- summary(NVDAestdata[, -1])  # Exclude 'Date' column
print(NVDAestdata_summary_stats)

# Exclude the Date column and calculate standard deviation
nvda_sd <- sapply(NVDAestdata[, -1], sd, na.rm = TRUE)
print(nvda_sd)

# Exclude the Date column and calculate standard deviation
tsla_sd <- sapply(TSLAestdata[, -1], sd, na.rm = TRUE)
print(tsla_sd)

# Exclude the Date column and calculate standard deviation
hsy_sd <- sapply(HSYestdata[, -1], sd, na.rm = TRUE)
print(hsy_sd)

# Exclude the Date column and calculate standard deviation
cost_sd <- sapply(COSTestdata[, -1], sd, na.rm = TRUE)
print(cost_sd)

# Exclude the Date column and calculate standard deviation
mar_sd <- sapply(MARestdata[, -1], sd, na.rm = TRUE)
print(mar_sd)

### Key AI milestone ###
# Deepfake Obama (Jordan Peele) video on 2018-04-17
# Trump signed EO 13859 on AI on 2019-02-11
# Alexa Conversations launched on 2020-07-22
# ChatGPT released on 2022-11-30

# Function to create data for each event
add_post_event_dummy <- function(data, date_col, event_date, new_col_name) {
  data <- data %>%
    mutate({{ new_col_name }} := ifelse({{ date_col }} >= event_date, 1, 0)) %>%
    relocate({{ new_col_name }}, .before = 5)
  return(data)
}

# Create data related to a specified AI event
### Deepfake Obama (Jordan Peele) video on 2018-04-17 ###
NVDAestdata_deepfake <- add_post_event_dummy(
  data = NVDAestdata,
  date_col = Date,
  event_date = 20180417,
  new_col_name = Post_AI_deepfake
)

TSLAestdata_deepfake <- add_post_event_dummy(
  data = TSLAestdata,
  date_col = Date,
  event_date = 20180417,
  new_col_name = Post_AI_deepfake
)

HSYestdata_deepfake <- add_post_event_dummy(
  data = HSYestdata,
  date_col = Date,
  event_date = 20180417,
  new_col_name = Post_AI_deepfake
)

COSTestdata_deepfake <- add_post_event_dummy(
  data = COSTestdata,
  date_col = Date,
  event_date = 20180417,
  new_col_name = Post_AI_deepfake
)

MARestdata_deepfake <- add_post_event_dummy(
  data = MARestdata,
  date_col = Date,
  event_date = 20180417,
  new_col_name = Post_AI_deepfake
)

### Trump signed EO 13859 on AI on 2019-02-11 ###
NVDAestdata_EO <- add_post_event_dummy(
  data = NVDAestdata,
  date_col = Date,
  event_date = 20190211,
  new_col_name = Post_AI_EO
)

TSLAestdata_EO <- add_post_event_dummy(
  data = TSLAestdata,
  date_col = Date,
  event_date = 20190211,
  new_col_name = Post_AI_EO
)

HSYestdata_EO <- add_post_event_dummy(
  data = HSYestdata,
  date_col = Date,
  event_date = 20190211,
  new_col_name = Post_AI_EO
)

COSTestdata_EO <- add_post_event_dummy(
  data = COSTestdata,
  date_col = Date,
  event_date = 20190211,
  new_col_name = Post_AI_EO
)

MARestdata_EO <- add_post_event_dummy(
  data = MARestdata,
  date_col = Date,
  event_date = 20190211,
  new_col_name = Post_AI_EO
)

### Alexa Conversations launched on 2020-07-22 ###
NVDAestdata_alexa <- add_post_event_dummy(
  data = NVDAestdata,
  date_col = Date,
  event_date = 20200722,
  new_col_name = Post_AI_alexa
)

TSLAestdata_alexa <- add_post_event_dummy(
  data = TSLAestdata,
  date_col = Date,
  event_date = 20200722,
  new_col_name = Post_AI_alexa
)

HSYestdata_alexa <- add_post_event_dummy(
  data = HSYestdata,
  date_col = Date,
  event_date = 20200722,
  new_col_name = Post_AI_alexa
)

COSTestdata_alexa <- add_post_event_dummy(
  data = COSTestdata,
  date_col = Date,
  event_date = 20200722,
  new_col_name = Post_AI_alexa
)

MARestdata_alexa <- add_post_event_dummy(
  data = MARestdata,
  date_col = Date,
  event_date = 20200722,
  new_col_name = Post_AI_alexa
)

### ChatGPT released on 2022-11-30 ###
NVDAestdata_chatgpt <- add_post_event_dummy(
  data = NVDAestdata,
  date_col = Date,
  event_date = 20221130,
  new_col_name = Post_AI_chatgpt
)

TSLAestdata_chatgpt <- add_post_event_dummy(
  data = TSLAestdata,
  date_col = Date,
  event_date = 20221130,
  new_col_name = Post_AI_chatgpt
)

HSYestdata_chatgpt <- add_post_event_dummy(
  data = HSYestdata,
  date_col = Date,
  event_date = 20221130,
  new_col_name = Post_AI_chatgpt
)

COSTestdata_chatgpt <- add_post_event_dummy(
  data = COSTestdata,
  date_col = Date,
  event_date = 20221130,
  new_col_name = Post_AI_chatgpt
)

MARestdata_chatgpt <- add_post_event_dummy(
  data = MARestdata,
  date_col = Date,
  event_date = 20221130,
  new_col_name = Post_AI_chatgpt
)

### Generalized Fama-French 3 Factor Model Estimation for 2015 (Dec 10) ###
ff3_model <- function(data, stock_name, 
                      short_start, short_end, 
                      long_start, long_end) {
  
  ### SHORT-TERM MODEL ###
  short_data = subset(data, 
                      data[,1] >= short_start & 
                        data[,1] <= short_end)
  
  x_short = as.matrix(short_data[,2:5])  # Factors matrix
  y_short = short_data[,7] - short_data[,6]  # Excess returns
  
  short_model = lm(y_short ~ x_short)
  
  ### LONG-TERM MODEL ###
  long_data = subset(data,
                     data[,1] >= long_start & 
                       data[,1] <= long_end)
  
  x_long = as.matrix(long_data[,2:5])  # Factors matrix
  y_long = long_data[,7] - long_data[,6]  # Excess returns
  
  long_model = lm(y_long ~ x_long)
  
  ### Return Results ###
  results = list(
    stock = stock_name,
    short_model = summary(short_model),
    long_model = summary(long_model),
    short_data_range = c(short_start, short_end),
    long_data_range = c(long_start, long_end)
  )
  
  return(results)
}


### PRE-COVID: 2020 (March 11) ###
### For long-term estimation: Deepfake Obama (Jordan Peele) video on 2018-04-17 ###
nvda_results_deepfake <- ff3_model(NVDAestdata_deepfake, "NVDA",
                                   20190102, 20200310, 
                                   20160311, 20200310)

tsla_results_deepfake <- ff3_model(TSLAestdata_deepfake, "TSLA",
                                   20190102, 20200310, 
                                   20160311, 20200310)

hsy_results_deepfake <- ff3_model(HSYestdata_deepfake, "HSY",
                                  20190102, 20200310, 
                                  20160311, 20200310)

cost_results_deepfake <- ff3_model(COSTestdata_deepfake, "COST",
                                   20190102, 20200310, 
                                   20160311, 20200310)

mar_results_deepfake <- ff3_model(MARestdata_deepfake, "MAR",
                                  20190102, 20200310, 
                                  20160311, 20200310)

## Display results
nvda_results_deepfake$long_model
tsla_results_deepfake$long_model
hsy_results_deepfake$long_model
cost_results_deepfake$long_model
mar_results_deepfake$long_model

# Check how many observations
data = NVDAestdata_deepfake
long_data = subset(data,
                   data[,1] >= 20160311 & 
                     data[,1] <= 20200310)
nrow(long_data)

### For short-term estimation: Trump signed EO 13859 on AI on 2019-02-11 ###
nvda_results_EO <- ff3_model(NVDAestdata_EO, "NVDA",
                             20190102, 20200310, 
                             20160311, 20200310)

tsla_results_EO <- ff3_model(TSLAestdata_EO, "TSLA",
                             20190102, 20200310, 
                             20160311, 20200310)

hsy_results_EO <- ff3_model(HSYestdata_EO, "HSY",
                            20190102, 20200310, 
                            20160311, 20200310)

cost_results_EO <- ff3_model(COSTestdata_EO, "COST",
                             20190102, 20200310, 
                             20160311, 20200310)

mar_results_EO <- ff3_model(MARestdata_EO, "MAR",
                            20190102, 20200310, 
                            20160311, 20200310)

## Display results
nvda_results_EO$short_model
tsla_results_EO$short_model
hsy_results_EO$short_model
cost_results_EO$short_model
mar_results_EO$short_model

# Check how many obs 
data1 = NVDAestdata_EO
short_data = subset(data1, 
                    data1[,1] >= 20190102 & 
                      data1[,1] <= 20200310)
nrow(short_data)

### POST-COVID: 2020 (March 11) ###
### For short-term estimation: Alexa Conversations launched on 2020-07-22 ###
nvda_results_alexa <- ff3_model(NVDAestdata_alexa, "NVDA",
                                20200311, 20210310, 
                                20200311, 20240310)

tsla_results_alexa <- ff3_model(TSLAestdata_alexa, "TSLA",
                                20200311, 20210310, 
                                20200311, 20240310)

hsy_results_alexa <- ff3_model(HSYestdata_alexa, "HSY",
                               20200311, 20210310, 
                               20200311, 20240310)

cost_results_alexa <- ff3_model(COSTestdata_alexa, "COST",
                                20200311, 20210310, 
                                20200311, 20240310)

mar_results_alexa <- ff3_model(MARestdata_alexa, "MAR",
                               20200311, 20210310, 
                               20200311, 20240310)

## Display results
nvda_results_alexa$short_model
tsla_results_alexa$short_model
hsy_results_alexa$short_model
cost_results_alexa$short_model
mar_results_alexa$short_model

# Check how many obs 
data2 = NVDAestdata_alexa
short_data1 = subset(data2, 
                    data2[,1] >= 20190102 & 
                      data2[,1] <= 20200310)
nrow(short_data1)


### For long-term estimation: ChatGPT released on 2022-11-30 ###
nvda_results_chatgpt <- ff3_model(NVDAestdata_chatgpt, "NVDA",
                                  20200311, 20210310, 
                                  20200311, 20240310)

tsla_results_chatgpt <- ff3_model(TSLAestdata_chatgpt, "TSLA",
                                  20200311, 20210310, 
                                  20200311, 20240310)

hsy_results_chatgpt <- ff3_model(HSYestdata_chatgpt, "HSY",
                                 20200311, 20210310, 
                                 20200311, 20240310)

cost_results_chatgpt <- ff3_model(COSTestdata_chatgpt, "COST",
                                  20200311, 20210310, 
                                  20200311, 20240310)

mar_results_chatgpt <- ff3_model(MARestdata_chatgpt, "MAR",
                                 20200311, 20210310, 
                                 20200311, 20240310)

## Display results
nvda_results_chatgpt$long_model
tsla_results_chatgpt$long_model
hsy_results_chatgpt$long_model
cost_results_chatgpt$long_model
mar_results_chatgpt$long_model

# Check how many observations
data3 = NVDAestdata_chatgpt
long_data1 = subset(data3,
                   data3[,1] >= 20160311 & 
                     data3[,1] <= 20200310)
nrow(long_data1)
